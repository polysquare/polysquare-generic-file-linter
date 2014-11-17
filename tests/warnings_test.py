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
    def __init__(self, message, replacement):
        super(LinterFailure, self).__init__()
        self.message = message
        self.replacement = replacement

    def __str__(self):
        return str("{0}".format(self.message))


def run_linter_throw(relative_path, contents, whitelist=None, blacklist=None):
    """Runs linter.lint and throws if it reports a message"""

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

    def test_lint_fail_malformed(self):
        """Checks that headerblock/filename fails

        Test fails where /path/to/file is not in the header on the first line
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "# path/to/file_wrong\n#\n",
                             whitelist=["headerblock/filename"])

    def test_lint_fail_nocomment(self):
        """Checks that headerblock/filename fails

        Test fails where /path/to/file is not in the header on the first line
        """
        with self.assertRaises(RuntimeError):
            run_linter_throw("path/to/file",
                             "aabb\nbbcc",
                             whitelist=["headerblock/filename"])

    def test_lint_fail_short(self):
        """Checks that headerblock/filename fails

        Test fails where there are no lines
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "",
                             whitelist=["headerblock/filename"])

    def test_suggest_filename(self):
        """Suggest the filename on headerblock/filename failure"""
        with self.assertRaises(LinterFailure) as exception_context:
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/filename"])

        self.assertEqual(exception_context.exception.replacement,
                         (1, "# /path/to/file\n"))


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

    def test_lint_fail_malformed(self):
        """Checks that headerblock/desc_space fails


        Test fail where there is not a single blank comment on the second line
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/desc_space"])

    def test_lint_fail_short(self):
        """Checks that headerblock/desc_space fails


        Test fail where there are not even two lines
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "#\n",
                             whitelist=["headerblock/desc_space"])

    def test_suggest_insert_whitespace(self):
        """Suggest a blank comment line on headerblock/desc_space failure"""
        with self.assertRaises(LinterFailure) as exception_context:
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["headerblock/desc_space"])

        self.assertEqual(exception_context.exception.replacement,
                         (2, "#\n# Text"))


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

    def test_lint_pass_c89(self):
        """Checks that headerblock/copyright passes for C89 style comments


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
        """Checks that headerblock/copyright fails


        Test fails where "See LICENCE.md for Copyright information" does not
        appear at the end of the header block
        """
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# No Copyright Notice\n\n",
                             whitelist=["headerblock/copyright"])

    def test_lint_fail_no_end(self):
        """headerblock/copyright fails where headerblock has no ending"""
        with self.assertRaises(LinterFailure):
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n#",
                             whitelist=["headerblock/copyright"])

    def test_suggest_replacement(self):
        """Checks a replacement is suggested where line has LICENCE or Copyright


        The entire line should be replaced where it has LICENCE or Copyright
        as the user probably intended (but typoed) to write the copyright
        notice
        """
        with self.assertRaises(LinterFailure) as exception_context:
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# No Copyright Notice\n\n",
                             whitelist=["headerblock/copyright"])

        self.assertEqual(exception_context.exception.replacement,
                         (3, "# See LICENCE.md for Copyright information\n"))

    def test_suggest_newline(self):
        """Checks a new line is suggested where line has no notice


        A new line should be suggested as no Copyright notice was even inserted
        in the first place
        """
        with self.assertRaises(LinterFailure) as exception_context:
            run_linter_throw("path/to/file",
                             "# /path/to/file\n#\n# Other\n\n",
                             whitelist=["headerblock/copyright"])

        replacement = "# Other\n# See LICENCE.md for Copyright information\n"
        self.assertEqual(exception_context.exception.replacement,
                         (3, replacement))


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

    def test_suggest_newline(self):
        """Suggest a newline on the last line for file/newline_last_char"""
        with self.assertRaises(LinterFailure) as exception_context:
            run_linter_throw("path/to/file",
                             "#\n# Text",
                             whitelist=["file/newline_last_char"])

        self.assertEqual(exception_context.exception.replacement,
                         (2, "# Text\n"))

if __name__ == "__main__":
    unittest.main()
