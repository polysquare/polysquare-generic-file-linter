# /test/test_warnings.py
#
# Test cases for each warning in polysquare-generic-file-linter
#
# Disable no-self-use in tests as all test methods must be
# instance methods and we don't necessarily have to use a matcher
# with them.
# pylint:disable=no-self-use
#
# See /LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings."""

import os

import shutil

import tempfile

from collections import defaultdict, namedtuple

from nose_parameterized import param, parameterized

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

FormatStyle = namedtuple("FormatStyle", "start mid end")
_KNOWN_STYLES = [
    param(FormatStyle(start="#", mid="#", end="")),
    param(FormatStyle(start="rem", mid="rem", end="")),
    param(FormatStyle(start="//", mid="//", end="")),
    param(FormatStyle(start="/*", mid=" *", end=" */"))
]


def style_format(script, style):
    """Format script for style, replace keys in script with comments."""
    return script.format(s=style.start, m=style.mid, e=style.end)


def run_linter_throw(relative_path,
                     contents,
                     style,
                     **kwargs):
    """Run linter.lint and throws if it reports a message."""
    kwargs = defaultdict(lambda: None, **kwargs)
    functions = dict(linter.linter_functions_from_filters(kwargs["whitelist"],
                                                          kwargs["blacklist"]))
    tool_options = linter.tool_options_from_global(kwargs, 1)
    errors = linter.lint(relative_path,
                         style_format(contents, style),
                         functions,
                         **dict(tool_options))

    if len(errors):
        description = errors[0][1].description
        raise LinterFailure("{0}:{1} - {2} [{3}]".format(relative_path,
                                                         errors[0][1].line,
                                                         description,
                                                         errors[0][0]),
                            (errors[0][1].line, errors[0][1].replacement))

    return True


# Needed to silence pychecker warning - pychecker doesn't detect
# that self.assertRaises was overridden and thinks that the exception
# object returned is not the exception itself but unittest's old
# _ExceptionContext object # suppress(file/spelling_error)
def replacement(exception):
    """Get replacement stored in exception."""
    assert exception.__class__.__name__ == "LinterFailure"
    return exception.replacement


class DisableStampingTestCase(TestCase):
    """Base TestCase class that explicitly disables job caching."""

    def setUp(self):  # suppress(N802)
        """Disable job caching."""
        super(DisableStampingTestCase, self).setUp()
        os.environ["_POLYSQUARE_GENERIC_FILE_LINTER_NO_STAMPING"] = "1"


class TestFilenameHeaderWarnings(DisableStampingTestCase):
    """Test case for file names being at the top of a header."""

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_pass(self, style):
        """Check that headerblock/filename passes.

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "{s} /path/to/file\n{m}{e}\n",
                                  style,
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_ignore_shebangs(self, style):
        """Check that headerblock/filename passes, ignoring shebangs.

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "#!/usr/bin/env bash\n"
                                  "{s} /path/to/file\n{m}{e}\n",
                                  style,
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_ignore_at_echo(self, style):
        """Check that headerblock/filename passes, ignoring @ echo off.

        Test passes where /path/to/file is in the header on the first line
        """
        result = run_linter_throw("path/to/file",
                                  "@echo off\n"
                                  "{s} /path/to/file\n{m}{e}\n",
                                  style,
                                  whitelist=["headerblock/filename"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_malformed(self, style):
        """Check that headerblock/filename fails.

        Test fails where /path/to/file is not in the header on the first line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s} path/to/file_wrong\n{m}{e}\n",
                             style,
                             whitelist=["headerblock/filename"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_nocomment(self, style):
        """Check that headerblock/filename fails.

        Test fails where /path/to/file is not in the header on the first line
        """
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "aabb\nbbcc",
                             style,
                             whitelist=["headerblock/filename"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_short(self, style):
        """Check that headerblock/filename fails.

        Test fails where there are no lines
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "",
                             style,
                             whitelist=["headerblock/filename"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_filename(self, style):
        """Suggest the file name on headerblock/filename failure."""
        def get_replacement():
            """Get replacement for first line of headerblock."""
            run_linter_throw("path/to/file",
                             "{s}\n{m} Text{e}",
                             style,
                             whitelist=["headerblock/filename"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (1, style_format("{s} /path/to/file\n", style)))


class TestSpaceBetweenHeaderAndDescWarnings(DisableStampingTestCase):
    """Test case for a single blank comment between top and body of header."""

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_pass(self, style):
        """Check that headerblock/desc_space passes.

        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  "{s} /path/to/file\n{m}\n{m} Text{e}",
                                  style,
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_ignore_shebangs(self, style):
        """Check that headerblock/desc_space passes, ignoring shebangs.

        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  "#!/usr/bin/env bash\n"
                                  "{s} /path/to/file\n{m}\n{m} Text{e}",
                                  style,
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_ignore_at_echo(self, style):
        """Check that headerblock/desc_space passes, ignoring @ echo off.

        Test passes where there is a single blank comment on the second line
        """
        result = run_linter_throw("path/to/file",
                                  "@echo off\n"
                                  "{s} /path/to/file\n{m}\n{m} Text{e}",
                                  style,
                                  whitelist=["headerblock/desc_space"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_malformed(self, style):
        """Check that headerblock/desc_space fails.

        Test fail where there is not a single blank comment on the second line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s}\n{m} Text{e}",
                             style,
                             whitelist=["headerblock/desc_space"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_short(self, style):
        """Check that headerblock/desc_space fails.

        Test fail where there are not even two lines
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s}{e}\n",
                             style,
                             whitelist=["headerblock/desc_space"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_insert_break(self, style):
        """Suggest a blank comment line on headerblock/desc_space failure."""
        def get_replacement():
            """Get replacement for lack of break."""
            run_linter_throw("path/to/file",
                             "{s}\n{m} Text{e}",
                             style,
                             whitelist=["headerblock/desc_space"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, style_format("{m}\n{m} Text{e}", style)))


class TestSpaceDescAndCopyrightWarnings(DisableStampingTestCase):
    """Test case for a single blank comment between bottom and body."""

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_pass(self, style):
        """Check that headerblock/space_copyright passes.

        Test passes where there is a single blank comment on the second
        last line
        """
        result = run_linter_throw("path/to/file",
                                  "{s} /path/to/file\n{m}\n{m} Text\n\n",
                                  style,
                                  whitelist=["headerblock/space_copyright"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail(self, style):
        """Check that headerblock/desc_space_copyright fails.

        Test fails where there is not a single blank comment on the second
        last line
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s} Text\n{m} Text\n {m} Text\n{e}\n",
                             style,
                             whitelist=["headerblock/space_copyright"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_no_headerblock(self, style):
        """RuntimeError where file does not have headerblock."""
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "\n",
                             style,
                             whitelist=["headerblock/space_copyright"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_insert_break(self, style):
        """Suggest a blank comment line for headerblock/space_copyright."""
        def get_replacement():
            """Get replacement for lack of break."""
            run_linter_throw("path/to/file",
                             "{s} Text\n{m} Text\n{m} Text{e}\n\n",
                             style,
                             whitelist=["headerblock/space_copyright"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, style_format("{m}\n{m} Text{e}\n", style)))


class TestCopyrightNotice(DisableStampingTestCase):
    """Test case for Copyright notice at end of header block."""

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_pass(self, style):
        """Check that headerblock/copyright passes.

        Test passes where "See /LICENCE.md for Copyright information" appears
        at the end of the header block
        """
        result = run_linter_throw("path/to/file",
                                  "{s} /path/to/file\n{m}\n{m} "
                                  "See /LICENCE.md for Copyright "
                                  "information{e}\n\n",
                                  style,
                                  whitelist=["headerblock/copyright"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail(self, style):
        """Check that headerblock/copyright fails.

        Test fails where "See /LICENCE.md for Copyright information" does not
        appear at the end of the header block
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s} /path/to/file\n"
                             "{m}\n{m} No Copyright Notice{e}\n\n",
                             style,
                             whitelist=["headerblock/copyright"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail_no_end(self, style):
        """headerblock/copyright fails where headerblock has no ending."""
        with ExpectedException(RuntimeError):
            run_linter_throw("path/to/file",
                             "{s} /path/to/file\n{m}\n{m}{e}",
                             style,
                             whitelist=["headerblock/copyright"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_replacement(self, style):
        """Check a replacement is suggested if line has LICENCE or Copyright.

        The entire line should be replaced where it has LICENCE or Copyright
        as the user probably intended (but with a typographical error) to write
        the copyright notice
        """
        def get_replacement():
            """Get replacement for partial Copyright notice."""
            run_linter_throw("path/to/file",
                             "{s} /path/to/file\n"
                             "{m}\n{m} No Copyright Notice{e}\n\n",
                             style,
                             whitelist=["headerblock/copyright"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, style_format("{m} See /LICENCE.md for Copyright "
                                          "information{e}\n", style)))

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_newline(self, style):
        """Check a new line is suggested where line has no notice.

        A new line should be suggested as no Copyright notice was even inserted
        in the first place
        """
        def get_replacement():
            """Get replacement for lack of Copyright notice."""
            run_linter_throw("path/to/file",
                             "{s} /path/to/file\n{m}\n{m} Other{e}\n\n",
                             style,
                             whitelist=["headerblock/copyright"])

        expected_repl = ("{m} Other\n"
                         "{m} See /LICENCE.md for Copyright information{e}\n")
        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (3, style_format(expected_repl, style)))


class TestNewlineAsLastChar(DisableStampingTestCase):
    r"""Test case for \n as last char of file."""

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_pass(self, style):
        r"""Check that file/newline_last_char passes.

        Test passes where "\n" is the last character in the file
        """
        result = run_linter_throw("path/to/file",
                                  "{s}\n{m}{e}\n",
                                  style,
                                  whitelist=["file/newline_last_char"])
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_lint_fail(self, style):
        r"""Check that file/newline_last_char false.

        Test fails where "\n" is not the last character in the file
        """
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             "{s}\n{m}{e}",
                             style,
                             whitelist=["file/newline_last_char"])

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_newline(self, style):
        """Suggest a newline on the last line for file/newline_last_char."""
        def get_replacement():
            """Get replacement for lack of trailing newline."""
            run_linter_throw("path/to/file",
                             "{s}\n{m} Text{e}",
                             style,
                             whitelist=["file/newline_last_char"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, style_format("{m} Text{e}\n", style)))


class TestTrailingWhitespace(DisableStampingTestCase):
    """Test case for trailing whitespace."""

    def test_lint_pass(self):
        """File passes when there is no trailing whitespace."""
        contents = "contents\ncontents\n"
        result = run_linter_throw("path/to/file",
                                  contents,
                                  FormatStyle("#", "#", ""),
                                  whitelist=["file/trailing_whitespace"])
        self.assertTrue(result)

    @parameterized.expand(["contents     \ncontents\n",
                           "contents\n    \ncontents"])
    def test_lint_fail(self, contents):
        """File fails when there is trailing whitespace."""
        with ExpectedException(LinterFailure):
            run_linter_throw("path/to/file",
                             contents,
                             FormatStyle("#", "#", ""),
                             whitelist=["file/trailing_whitespace"])

    @parameterized.expand(["contents     \ncontents\n",
                           "    \ncontents"])
    def test_suggest_no_trailing_whitespace(self, contents):
        """File fails when there is trailing whitespace."""
        def get_replacement():
            """Get replacement for trailing newline."""
            run_linter_throw("path/to/file",
                             contents,
                             FormatStyle("#", "#", ""),
                             whitelist=["file/trailing_whitespace"])

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (1, contents.splitlines(True)[0].replace(" ", "")))


class TestSpellingErrors(DisableStampingTestCase):
    """Test case for spelling errors."""

    @classmethod
    def setUpClass(cls):  # suppress(N802)
        """Create a temporary directory to store word graph caches."""
        # This is the name of the directory that we want to
        # place our files in.
        word_cache_dir = os.path.join(os.getcwd(), "wordcache")
        cls.cache_dir = tempfile.mkdtemp(prefix=word_cache_dir)

    @classmethod
    def tearDownClass(cls):
        """Remove cache directories."""
        shutil.rmtree(cls.cache_dir)

    def _spellcheck_lint(self, text, style):
        """Wrap run_linter_throw and only run spellcheck.

        This function also provides the path to the word cache file which
        should speed up operation substantially.
        """
        cache_dir = self.__class__.cache_dir
        return run_linter_throw("path/to/file",
                                text,
                                style,
                                whitelist=["file/spelling_error"],
                                options={
                                    "spellcheck_cache": cache_dir
                                })

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_inside_comment(self, style):
        """Find spelling errors inside comment."""
        with ExpectedException(LinterFailure):
            self._spellcheck_lint("{s}\n{m} splelling mistake{e}", style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_error_comments_starting_inside_string(self, style):
        """No spelling errors where comment block starts inside string."""
        result = self._spellcheck_lint("{s}{e}\n\"{s}\" splelling",
                                       style)
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_error_inside_double_quoted_string(self, style):
        """No spelling errors inside single double-quote string."""
        result = self._spellcheck_lint("{s}{e}\n\"splelling\"",
                                       style)
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_error_inside_single_quoted_string(self, style):
        """No spelling errors inside single single-quote string."""
        result = self._spellcheck_lint("{s}{e}\n'splelling'",
                                       style)
        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_correction_inside_comment(self, style):
        """Correct spelling errors inside comment."""
        def get_replacement():
            self._spellcheck_lint("{s}\n{m} splelling mistake{e}", style)

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, style_format("{m} spelling mistake{e}",
                                          style)))

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_in_double_quote_docstring(self, style):
        """Find spelling errors on double quoted string."""
        with ExpectedException(LinterFailure):
            self._spellcheck_lint("{s}{e}\ncall(\"\"\"splelling\"\"\")\n",
                                  style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_correction_double_quote_docstring(self, style):
        """Correct spelling errors inside double quoted docstring."""
        def get_replacement():
            self._spellcheck_lint("{s}{e}\ncall(\"\"\"splelling\"\"\")\n",
                                  style)

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2,
                          style_format("call(\"\"\"spelling\"\"\")\n",
                                       style)))

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_in_multiline_docstring(self, style):
        """Find spelling errors in multi-line docstring."""
        with ExpectedException(LinterFailure):
            self._spellcheck_lint("{s}{e}\ncall(\"\"\"splelling\n"
                                  "mistake\"\"\")\n",
                                  style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_correction_multiline_docstring(self, style):
        """Correct spelling errors in multi-line docstring."""
        def get_replacement():
            self._spellcheck_lint("{s}{e}\ncall(\"\"\"splelling\n"
                                  "mistake\"\"\")\n",
                                  style)

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2,
                          style_format("call(\"\"\"spelling\n",
                                       style)))

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_in_docstring(self, style):
        """Find spelling errors on docstring."""
        with ExpectedException(LinterFailure):
            self._spellcheck_lint("{s}{e}\n\"\"\"splelling mistake\"\"\")\n",
                                  style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_for_technical_terms_not_in_context(self, style):
        """Technical terms not in same indent context are not valid words."""
        with ExpectedException(LinterFailure):
            content = "{s}{e}\n\"\"\"technicalterm\"\"\""
            self._spellcheck_lint(content, style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_mistakes_for_technical_terms_in_context(self, style):
        """Technical terms used in rest of file are valid words."""
        content = "{s}{e}\n\"\"\"technicalterm\"\"\")\n\n technicalterm\n"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_for_referenced_symbol_not_in_context(self,
                                                                   style):
        """Technical terms not referenced in context are invalid."""
        with ExpectedException(LinterFailure):
            content = "{s}{e}\n\"\"\"@technical_term\"\"\""
            self._spellcheck_lint(content, style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_mistakes_for_referenced_symbol_in_context(self,
                                                                   style):
        """Technical terms used in rest of file are valid words."""
        content = "{s}{e}\n\"\"\"@technical_term\"\"\")\n\n technical_term\n"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_no_spelling_mistakes_after_docstring(self, style):
        """No spelling errors after docstring."""
        content = "{s}{e}\n\"\"\"spelling mistake\"\"\")\n\n msspltword\n"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_apostrophe_in_comments(self, style):
        """Handle apostrophes in comments correctly."""
        content = "{s}There's{e}\n msspltword\n{s} We're {e}"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_comments_in_quotes(self, style):
        """Handle comments in quotes correctly."""
        content = "{s}{e}\n\"{s} There{e}\"\n msspltword\n"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_comments_in_multiline_quotes(self, style):
        """Handle comments in multi-line quotes correctly."""
        content = "{s}{e}\n\"{s} There\n{e}\"\n msspltword\n"
        result = self._spellcheck_lint(content, style)

        self.assertTrue(result)

    @parameterized.expand(_KNOWN_STYLES)
    def test_spelling_mistake_in_single_quote_string(self, style):
        """Find spelling errors on single quoted docstring."""
        with ExpectedException(LinterFailure):
            self._spellcheck_lint("{s}{e}\ncall('''splelling mistake''')\n",
                                  style)

    @parameterized.expand(_KNOWN_STYLES)
    def test_suggest_correction_single_quote_string(self, style):
        """Correct spelling errors inside single quoted docstring."""
        def get_replacement():
            self._spellcheck_lint("{s}{e}\ncall('''splelling mistake''')\n",
                                  style)

        exception = self.assertRaises(LinterFailure, get_replacement)
        self.assertEqual(replacement(exception),
                         (2, style_format("call('''spelling mistake''')\n",
                                          style)))
