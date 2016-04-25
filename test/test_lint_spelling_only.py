# /test/test_lint_spelling_only.py
#
# Test cases for the spellcheck-linter utility.
#
# Disable no-self-use in tests as all test methods must be
# instance methods and we don't necessarily have to use a matcher
# with them.
# pylint:disable=no-self-use
#
# See /LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings."""

import doctest

import os

import shutil

import sys

import tempfile

from iocapture import capture

from polysquarelinter import lint_spelling_only

from testtools import TestCase
from testtools.matchers import DocTestMatches


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


def run_lint_spelling_only_main(filename, **kwargs):
    """Run lint_spelling_only.main with filename and kwargs as switches."""
    arguments = [filename]
    return run_with_kwargs_as_switches(lint_spelling_only.main,
                                       *arguments,
                                       **kwargs)


class TestLintSpellingOnlyAcceptance(TestCase):
    """Acceptance tests for lint_spelling_only.main()."""

    def __init__(self, *args, **kwargs):  # pylint:disable=super-on-old-class
        """Initialize class variables."""
        super(TestLintSpellingOnlyAcceptance, self).__init__(*args, **kwargs)
        self._temporary_file = None
        self._tech_words_file = None

    @classmethod
    def setUpClass(cls):  # suppress(N802)
        """Create a temporary directory to store word graph caches."""
        # This is the name of the directory that we want to
        # place our files in.
        word_cache_dir = os.path.join(os.getcwd(), "spelling_only_wordcache")
        cache_dir = tempfile.mkdtemp(prefix=word_cache_dir)
        cls.cache_dir = cache_dir

    @classmethod
    def tearDownClass(cls):
        """Remove cache directories."""
        shutil.rmtree(cls.cache_dir)

    def setUp(self):  # suppress(N802)
        """Create a temporary file."""
        from six import StringIO

        super(TestLintSpellingOnlyAcceptance, self).setUp()
        self._last_directory = os.getcwd()
        self._temp_directory = tempfile.mkdtemp()
        os.chdir(self._temp_directory)
        self._temporary_file = os.path.join(os.getcwd(), "file_to_lint.txt")
        self._tech_words_file = os.path.join(os.getcwd(),
                                             "techincal_terms.txt")

        # Ensure candidate file has been created
        with open(self._temporary_file, "w"):
            pass

        with open(self._tech_words_file, "w"):
            pass

        # Patch stdout to not go anywhere
        self.patch(sys, "stdout", StringIO())

        # Finally, disable caching
        os.environ["JOBSTAMPS_DISABLED"] = "1"

    def tearDown(self):  # suppress(N802)
        """Remove temporary file."""
        os.chdir(self._last_directory)
        shutil.rmtree(self._temp_directory)
        super(TestLintSpellingOnlyAcceptance, self).tearDown()

    def _run_with_cache(self, filename, **kwargs):
        """Wrap run_lint_spelling_only_main and use caching automatically."""
        kwargs["spellcheck_cache"] = self.__class__.cache_dir
        return run_lint_spelling_only_main(filename, **kwargs)

    def test_exit_failure_on_spelling_errors(self):
        """Exit with failure on spelling errors."""
        with open(self._temporary_file, "w") as f:
            f.write("splelling error\n")

        result = self._run_with_cache(self._temporary_file)
        self.assertEqual(result, 1)

    def test_exit_failure_on_technical_word_misuse(self):
        """Exit with failure on technical word misuse."""
        with open(self._temporary_file, "w") as f:
            f.write("technical_looking_word\n")

        result = self._run_with_cache(self._temporary_file,
                                      technical_terms=self._tech_words_file)
        self.assertEqual(result, 1)

    def test_report_spelling_failure(self):
        """Report spelling failures."""
        with open(self._temporary_file, "w") as f:
            f.write("splelling error\n")

        with capture() as captured:
            self._run_with_cache(self._temporary_file)

            self.assertThat(captured.stdout,  # suppress(PYC70)
                            DocTestMatches("""... [file/spelling_error] ...""",
                                           doctest.ELLIPSIS |
                                           doctest.NORMALIZE_WHITESPACE |
                                           doctest.REPORT_NDIFF))

    def test_report_technical_word_misuse(self):
        """Report technical word misuse."""
        with open(self._temporary_file, "w") as f:
            f.write("technical_looking_word\n")

        with capture() as captured:
            self._run_with_cache(self._temporary_file,
                                 technical_terms=self._tech_words_file)

            self.assertThat(captured.stdout,  # suppress(PYC70)
                            # suppress(file/spelling_error)
                            DocTestMatches("""... technical_looking_word"""
                                           """...""",
                                           doctest.ELLIPSIS |
                                           doctest.NORMALIZE_WHITESPACE |
                                           doctest.REPORT_NDIFF))

    def test_exit_success_on_technical_word_found_in_list(self):
        """Exit with success when a technical word is found."""
        with open(self._temporary_file, "w") as f:
            f.write("technical_looking_word\n")

        with open(self._tech_words_file, "w") as f:
            f.write("technical_looking_word\n")

        result = self._run_with_cache(self._temporary_file,
                                      technical_terms=self._tech_words_file)
        self.assertEqual(result, 0)

    def test_allow_technical_words_in_user_dictionary(self):
        """Allow technical terms in user dictionary."""
        with open(os.path.join(os.getcwd(), "DICTIONARY"), "w") as f:
            f.write("technical_looking_word\n")

        with open(self._tech_words_file, "w") as f:
            f.write("technical_looking_word\n")

        result = self._run_with_cache(self._temporary_file)

        self.assertEqual(result, 0)
