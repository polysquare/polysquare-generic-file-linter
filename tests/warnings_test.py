# /tests/warnings_test.py
#
# Test cases for each warning in polysquare-generic-file-linter
#
# See LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings"""

from polysquarelinter import linter
import unittest


class LinterFailure(Exception):
    """Exception raised when the linter reports a message"""
    def __init__(self, message):
        super(LinterFailure, self).__init__()
        self.message = message

    def __str__(self):
        return str("{0}".format(self.message))


def run_linter_throw(relative_path, contents, whitelist=None, blacklist=None):
    """Runs linter.lint and throws if it reports a message"""
    def throw_message(message):
        """Raises LinterFailure when called"""
        raise LinterFailure(message)

    return linter.lint(relative_path,
                       contents,
                       whitelist=whitelist,
                       blacklist=blacklist,
                       report=throw_message)


class TestFilenameHeaderWarnings(unittest.TestCase):
    """Test case for filenames being at the top of a header"""
    def test_lint_pass(self):
        """Checks that headerblock/filename passes

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n",
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Checks that headerblock/filename fails

        Test fails where /path/to/file is not in the header on the first line
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "# path/to/file_wrong\n#\n",
                             whitelist=["headerblock/filename"])


class TestSpaceBetweenHeaderAndDescWarnings(unittest.TestCase):
    """Test case for a single blank comment between top and body of header"""
    def test_lint_pass(self):
        """Checks that headerblock/desc_space passes


        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n# Text",
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Checks that headerblock/desc_space fails


        Test fail where there is not a single blank comment on the second line
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/desc_space"])


class TestCopyrightNotice(unittest.TestCase):
    """Test case for Copyright notice at end of header block"""
    def test_lint_pass(self):
        """Checks that headerblock/copyright passes


        Test passes where "See LICENCE.md for Copyright information" appears
        at the end of the header block
        """
        result = run_linter_throw("path/to/file",
                                  "# /path/to/file\n#\n# "
                                  "See LICENCE.md for Copyright "
                                  "information\n\n",
                                  whitelist=["headerblock/copyright"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Checks that headerblock/copyright fails


        Test fails where "See LICENCE.md for Copyright information" does not
        appear at the end of the header block
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# No Copyright Notice\n\n",
                             whitelist=["headerblock/copyright"])


class TestNewlineAsLastChar(unittest.TestCase):
    """Test case for \n as last char of file"""
    def test_lint_pass(self):
        """Checks that file/newline_last_char passes


        Test passes where "\n" is the last character in the file
        """
        result = run_linter_throw("path/to/file",
                                  "#\n#\n",
                                  whitelist=["file/newline_last_char"])
        self.assertTrue(result)

    def test_lint_fail(self):
        """Checks that file/newline_last_char false


        Test fails where "\n" is not the last character in the file
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n#",
                             whitelist=["file/newline_last_char"])

if __name__ == "__main__":
    unittest.main()
