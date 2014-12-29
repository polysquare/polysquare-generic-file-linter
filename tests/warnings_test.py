# /tests/warnings_test.py
#
# Test cases for each warning in polysquare-generic-file-linter
#
# Disable no-self-use in tests as all test methods must be
# instance methods and we don't necessarily have to use a matcher
# with them.
# pylint:  disable=no-self-use
#
# See LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings."""

import os

import tempfile

from polysquarelinter import linter

from testtools import (ExpectedException, TestCase)


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


def run_linter_throw(relative_path, contents, whitelist=None, blacklist=None):
    """Run linter.lint and throws if it reports a message."""
    errors = linter.lint(relative_path,
                         contents,
                         whitelist=whitelist,
                         blacklist=blacklist)

    if len(errors):
        raise LinterFailure("{0}:{1} [{2}]".format(relative_path,
                                                   errors[0][1].line,
                                                   errors[0][0]),
                            (errors[0][1].line, errors[0][1].replacement))

    return True


def run_linter_main(filename, **kwargs):
    """Run linter.main() (as an integration test)."""
    arguments = [filename]

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

    return linter.main(arguments)


# Needed to silence pychecker warning - pychecker doesn't detect
# that self.assertRaises was overridden and thinks that the exception
# object returned is not the exception itself but unittest's old
# _ExceptionContext object
def replacement(exception):
    """Get replacement stored in exception."""
    assert exception.__class__.__name__ == "LinterFailure"
    return exception.replacement


class TestFilenameHeaderWarnings(TestCase):

    """Test case for filenames being at the top of a header."""

    def test_lint_pass(self):
        """Check that headerblock/filename passes.

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n",
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    def test_ignore_shebangs(self):
        """Check that headerblock/filename passes, ignoring shebangs.

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "#!/usr/bin/env bash\n# /path/to/file\n#\n",
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    def test_lint_fail_malformed(self):
        """Check that headerblock/filename fails.

        Test fails where /path/to/file is not in the header on the first line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "# path/to/file_wrong\n#\n",
                             whitelist=["headerblock/filename"])

    def test_lint_fail_nocomment(self):
        """Check that headerblock/filename fails.

        Test fails where /path/to/file is not in the header on the first line
        """
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "aabb\nbbcc",
                             whitelist=["headerblock/filename"])

    def test_lint_fail_short(self):
        """Check that headerblock/filename fails.

        Test fails where there are no lines
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "",
                             whitelist=["headerblock/filename"])

    def test_suggest_filename(self):
        """Suggest the filename on headerblock/filename failure."""
        def get_replacement():
            """Get relacement for first line of headerblock."""
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/filename"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (1, "# /path/to/file\n"))


class TestSpaceBetweenHeaderAndDescWarnings(TestCase):

    """Test case for a single blank comment between top and body of header."""

    def test_lint_pass(self):
        """Check that headerblock/desc_space passes.

        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n# Text",
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    def test_ignore_shebangs(self):
        """Check that headerblock/desc_space passes, ignoring shebangs.

        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  ("#!/usr/bin/env bash\n"
                                   "# /path/to/file\n#\n# Text"),
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    def test_lint_fail_malformed(self):
        """Check that headerblock/desc_space fails.

        Test fail where there is not a single blank comment on the second line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/desc_space"])

    def test_lint_fail_short(self):
        """Check that headerblock/desc_space fails.

        Test fail where there are not even two lines
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n",
                             whitelist=["headerblock/desc_space"])

    def test_suggest_insert_break(self):
        """Suggest a blank comment line on headerblock/desc_space failure."""
        def get_replacement():
            """Get relacement for lack of break."""
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/desc_space"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, "#\n# Text"))


class TestSpaceDescAndCopyrightWarnings(TestCase):

    """Test case for a single blank comment between bottom and body."""

    def test_lint_pass(self):
        """Check that headerblock/space_copyright passes.

        Test passes where there is a single blank comment on the second
        last line
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n# Text\n\n",
                                  whitelist=["headerblock/space_copyright"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Check that headerblock/desc_space_copyright fails.


        Test fails where there is not a single blank comment on the second
        last line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "# Text\n# Text\n # Text\n\n",
                             whitelist=["headerblock/space_copyright"])

    def test_lint_fail_no_headerblock(self):
        """RuntimeError where file does not have headerblock."""
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "\n",
                             whitelist=["headerblock/space_copyright"])

    def test_suggest_insert_break(self):
        """Suggest a blank comment line for headerblock/space_copyright."""
        def get_replacement():
            """Get relacement for lack of break."""
            run_linter_throw("path/to/file",
                             "# Text\n# Text\n# Text\n\n",
                             whitelist=["headerblock/space_copyright"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, "#\n# Text\n"))


class TestCopyrightNotice(TestCase):

    """Test case for Copyright notice at end of header block."""

    def test_lint_pass(self):
        """Check that headerblock/copyright passes.

        Test passes where "See LICENCE.md for Copyright information" appears
        at the end of the header block
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n# "
                                  "See LICENCE.md for Copyright "
                                  "information\n\n",
                                  whitelist=["headerblock/copyright"])
        self.assertTrue(result)

    def test_lint_pass_c89(self):
        """Check that headerblock/copyright passes for C89 style comments.

        Test passes where "See LICENCE.md for Copyright information" appears
        at the end of the header block (along with */)
        """
        result = run_linter_throw("path/to/file",
                                  "/* /path/to/file\n *\n * "
                                  "See LICENCE.md for Copyright "
                                  "information */\n\n",
                                  whitelist=["headerblock/copyright"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Check that headerblock/copyright fails.

        Test fails where "See LICENCE.md for Copyright information" does not
        appear at the end of the header block
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# No Copyright Notice\n\n",
                             whitelist=["headerblock/copyright"])

    def test_lint_fail_no_end(self):
        """headerblock/copyright fails where headerblock has no ending."""
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n#",
                             whitelist=["headerblock/copyright"])

    def test_suggest_replacement(self):
        """Check a replacement is suggested if line has LICENCE or Copyright.

        The entire line should be replaced where it has LICENCE or Copyright
        as the user probably intended (but typoed) to write the copyright
        notice
        """
        def get_replacement():
            """Get relacement for partial Copyright notice."""
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# No Copyright Notice\n\n",
                             whitelist=["headerblock/copyright"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, "# See LICENCE.md for Copyright information\n"))

    def test_suggest_newline(self):
        """Check a new line is suggested where line has no notice.

        A new line should be suggested as no Copyright notice was even inserted
        in the first place
        """
        def get_replacement():
            """Get relacement for lack of Copyright notice."""
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# Other\n\n",
                             whitelist=["headerblock/copyright"])

        expected_repl = "# Other\n# See LICENCE.md for Copyright information\n"
        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, expected_repl))


class TestNewlineAsLastChar(TestCase):

    r"""Test case for \n as last char of file."""

    def test_lint_pass(self):
        r"""Check that file/newline_last_char passes.

        Test passes where "\n" is the last character in the file
        """
        result = run_linter_throw("path/to/file",
                                  "#\n#\n",
                                  whitelist=["file/newline_last_char"])
        self.assertTrue(result)

    def test_lint_fail(self):
        r"""Check that file/newline_last_char false.

        Test fails where "\n" is not the last character in the file
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n#",
                             whitelist=["file/newline_last_char"])

    def test_suggest_newline(self):
        """Suggest a newline on the last line for file/newline_last_char."""
        def get_replacement():
            """Get relacement for lack of trailing newline."""
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["file/newline_last_char"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, "# Text\n"))


class TestLinterAcceptance(TestCase):

    """Acceptance tests for linter.main()."""

    def __init__(self, *args, **kwargs):
        """"Initialize class variables."""
        cls = TestLinterAcceptance
        super(cls, self).__init__(*args,  # pylint:disable=R0903
                                  **kwargs)
        self._temporary_file = None

    def setUp(self):  # NOQA
        """Create a temporary file."""
        super(TestLinterAcceptance, self).setUp()
        self._temporary_file = tempfile.mkstemp()

    def tearDown(self):  # NOQA
        """Remove temporary file."""
        os.remove(self._temporary_file[1])
        super(TestLinterAcceptance, self).tearDown()

    def test_blacklist(self):
        """Check that blacklisting a test causes it not to run."""
        contents = ("#\n"
                    "#\n"
                    "# Description\n"
                    "#\n"
                    "# See LICENCE.md for Copyright information\n"
                    "\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 blacklist=["headerblock/filename"])

        self.assertEqual(result, 0)

    def test_whitelist_pass(self):
        """Check that whitelisting a test causes only it to run."""
        contents = ("#\n")

        with os.fdopen(self._temporary_file[0], "a+") as process_file:
            process_file.write(contents)

        result = run_linter_main(self._temporary_file[1],
                                 whitelist=["file/newline_last_char"])

        self.assertEqual(result, 0)

    def test_whitelist_fail(self):
        """Check that whitelisting a test causes only it to run."""
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
