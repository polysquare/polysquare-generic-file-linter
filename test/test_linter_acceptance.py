# /test/test_linter_acceptance.py
#
# Acceptance tests for polysquarelinter.linter, actually runs
# main() and checks results of various functions.
#
# Disable no-self-use in tests as all test methods must be
# instance methods and we don't necessarily have to use a matcher
# with them.
# pylint:disable=no-self-use
#
# See /LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings."""

import doctest

import errno

import os

import shutil

import sys

import tempfile

from contextlib import contextmanager

from iocapture import capture

from nose_parameterized import parameterized

from polysquarelinter import linter

from testtools import TestCase
from testtools.matchers import (DocTestMatches,
                                Equals as TTEqMatcher,
                                MatchesSetwise)


# Pychecker complains about the Equals matcher failing to override comparator
# so do that here
class Equals(TTEqMatcher):  # suppress(R0903)
    """Matcher which tests equality."""

    def __init__(self, matchee):
        """Forward matchee to parent class."""
        super(Equals, self).__init__(matchee)

    def comparator(self, expected, other):
        """Check that expected == other."""
        return other == expected


class LinterFailure(Exception):
    """Exception raised when the linter reports a message."""

    def __init__(self, message, repl):
        """Initialize exception with mesh and replacement."""
        super(LinterFailure, self).__init__()
        self.message = message
        self.replacement = repl

    def __str__(self):
        """Represent as string."""
        return str("{0}".format(self.message))


def run_with_kwargs_as_switches(func, *args, **kwargs):
    """Run :func: with :kwargs: converted to switches."""
    arguments = list(args)

    def _convert_kv_to_switches(key, value):
        """Convert a key-value pair to command-line switches."""
        append_args = ["--{0}".format(key).replace("_", "-")]

        type_dispatch = {
            bool: [],
            list: value,
            str: [value]
        }

        # We assume that the types in type_dispatch are the only types
        # we'll encounter, all others will throw an exception.
        append_args += type_dispatch[type(value)]
        return append_args

    for key, value in kwargs.items():
        arguments += _convert_kv_to_switches(key, value)

    return func(arguments)


def run_linter_main(filename, **kwargs):
    """Run linter.main() (as an integration test)."""
    arguments = [filename]
    return run_with_kwargs_as_switches(linter.main, *arguments, **kwargs)


class TestLinterAcceptance(TestCase):
    """Acceptance tests for linter.main()."""

    def __init__(self, *args, **kwargs):  # pylint:disable=super-on-old-class
        """Initialize class variables."""
        super(TestLinterAcceptance, self).__init__(*args, **kwargs)
        self._temporary_file = None

    def setUp(self):  # suppress(N802)
        """Create a temporary file."""
        from six import StringIO

        super(TestLinterAcceptance, self).setUp()
        self._temporary_file = tempfile.mkstemp()
        self.patch(sys, "stdout", StringIO())

        os.environ["JOBSTAMPS_DISABLED"] = "1"

    def tearDown(self):  # suppress(N802)
        """Remove temporary file.

        Note that we need to ensure that the file is closed
        first, so if it hasn't been opened yet, we won't get
        EBADF. Otherwise we'll get EBADF and we can safely
        ignore it.
        """
        try:
            os.close(self._temporary_file[0])
        except OSError as error:
            if error.errno != errno.EBADF:   # suppress(PYC90)
                raise error

        os.remove(self._temporary_file[1])
        super(TestLinterAcceptance, self).tearDown()

    def test_parallelization_path(self):
        """Generate expected number of errors when running in parallel."""
        contents = ("#\n"
                    "#\n"
                    "# Description\n"
                    "#\n"
                    "# See LICENCE.md for Copyright information\n"
                    "\n")

        temporary_dir = tempfile.mkdtemp(prefix=os.path.join(os.getcwd(),
                                                             "technical"))
        self.addCleanup(lambda: shutil.rmtree(temporary_dir))
        files_to_lint = []

        for i in range(0, 20):
            with open(os.path.join(temporary_dir,
                                   "file{0}".format(i)), "w") as lint_file:
                lint_file.write(contents)
                files_to_lint.append(os.path.realpath(lint_file.name))

        result = run_with_kwargs_as_switches(linter.main,
                                             *files_to_lint,
                                             whitelist="headerblock/copyright")

        self.assertEqual(result, 20)

    def test_inline_suppressions_above(self):
        """Check inline suppressions work above the error-generating line."""
        contents = ("#\n"
                    "#\n"
                    "# Description\n"
                    "#\n"
                    "# suppress(headerblock/copyright)\n"
                    "# See LICENCE.md for Copyright information\n"
                    "\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 whitelist=["headerblock/copyright"])

        self.assertEqual(result, 0)

    def test_handle_empty_files(self):
        """Handle empty files with appropriate error message."""
        contents = ""

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1])

        # There should be a failure exit status, since empty
        # files will trigger errors in the linter.
        self.assertEqual(result, 1)

    def test_inline_suppressions_beside(self):
        """Check inline suppressions work beside the error-generating line."""
        contents = ("#\n"
                    "#\n"
                    "# Description\n"
                    "#\n"
                    "# See LICENCE.md for Copyright information"
                    "# suppress(headerblock/copyright)\n"  # on the same line
                    "\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 whitelist=["headerblock/copyright"])

        self.assertEqual(result, 0)

    def test_blacklist(self):
        """Check that blacklisting a test causes it not to run."""
        contents = ("#\n"
                    "#\n"
                    "# Description\n"
                    "#\n"
                    "# See /LICENCE.md for Copyright information\n"
                    "\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 blacklist=["headerblock/filename",
                                            "file/spelling_error"])

        self.assertEqual(result, 0)

    def test_whitelist_pass(self):
        """Check that white-listing a test causes only it to run."""
        contents = ("#\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 whitelist=["file/newline_last_char"])

        self.assertEqual(result, 0)

    def test_whitelist_fail(self):
        """Check that white-listing a test causes only it to run."""
        contents = ("#")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 whitelist=["file/newline_last_char"])

        self.assertEqual(result, 1)

    def test_fix_what_you_can(self):
        """Check that --fix-what-you-can modifies file correctly."""
        contents = ("#")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        run_linter_main(self._temporary_file[1],
                        whitelist=["file/newline_last_char"],
                        fix_what_you_can=True)

        with open(self._temporary_file[1], "r") as processed_file:
            self.assertEqual("#\n", processed_file.read())

    @parameterized.expand([c for c in linter.LINTER_FUNCTIONS.keys()])
    def test_show_checks(self, check):
        """Check that --checks shows a specified check."""
        if check != list(linter.LINTER_FUNCTIONS.keys())[-1]:
            final_ellipsis = " ..."
        else:
            final_ellipsis = ""

        doctest_contents = ("... * {0}{1}").format(check, final_ellipsis)

        with capture() as captured:
            self.patch(sys, "exit", lambda _: None)
            linter.main(["--checks"])

            self.assertThat(captured.stdout,  # suppress(PYC70)
                            DocTestMatches(doctest_contents,
                                           doctest.ELLIPSIS |
                                           doctest.NORMALIZE_WHITESPACE |
                                           doctest.REPORT_NDIFF))

    def test_log_technical_words_over_two_files(self):
        """Check that --log-technical-words combines found technical words."""
        @contextmanager
        def in_dir(directory):
            """Perform actions in the context of directory."""
            last_directory = os.getcwd()
            os.chdir(directory)
            try:
                yield
            finally:
                os.chdir(last_directory)

        temporary_dir = tempfile.mkdtemp(prefix=os.path.join(os.getcwd(),
                                                             "technical"))
        self.addCleanup(lambda: shutil.rmtree(temporary_dir))

        with in_dir(temporary_dir):
            first_file_path = os.path.join(temporary_dir, "first_file.txt")
            second_file_path = os.path.join(temporary_dir, "second_file.txt")
            tech_terms_path = os.path.join(temporary_dir,
                                           "technical_terms.txt")

            with open(first_file_path, "w") as f:
                f.write("#\n technical_term_one shared_technical_term\n")

            with open(second_file_path, "w") as f:
                f.write("#\n technical_term_two shared_technical_term\n")

            run_with_kwargs_as_switches(linter.main,
                                        first_file_path,
                                        second_file_path,
                                        whitelist=["file/spelling_error"],
                                        log_technical_terms_to=tech_terms_path)

            with open(tech_terms_path, "r") as f:
                logged_technical_terms = f.read().splitlines()

            self.assertThat(logged_technical_terms,
                            MatchesSetwise(Equals("technical_term_one"),
                                           Equals("technical_term_two"),
                                           Equals("shared_technical_term")))
