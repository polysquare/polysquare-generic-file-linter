# /polysquarelinter/linter.py
#
# Entry point for linter.
#
# See LICENCE.md for Copyright information
"""Main module for linter."""

import argparse

import os

import re

import sys

from collections import namedtuple

_ALL_COMMENT = r"(/\*| \*\/| \*|#|//|rem)"
_HDR_COMMENT = r"(/\*|#|//|rem)"
_MDL_COMMENT = r"( \*\/| \*|#|//|rem)"
_FTR_COMMENT = r"(/\*| \*)"


def _comment_type_from_line(line):
    """Return the "comment header" (eg ' * ', '# ', 'rem ', '// ', '/* ').

    This header goes before the content of a start of the
    line in a replacement.
    """
    regex = re.compile(r"^{0}".format(_ALL_COMMENT))
    match = regex.match(line)
    if match:
        return "{0} ".format(line[match.start():match.end()])

    raise RuntimeError("Unable to find comment header for {0}".format(line))


def _end_comment_type_from_line(line):
    """Return the "comment footer" (eg ' */'').

    This header goes after the content of a start of the
    line in a replacement that would be at the end of a header block.
    """
    regex = re.compile(r"^{0}".format(_FTR_COMMENT))
    match = regex.match(line)
    if match:
        return " */"

    return ""


LinterFailure = namedtuple("LinterFailure", "description line replacement")


def _line_is_shebang(line):
    """Return true if line is a shebang."""
    regex = re.compile(r"^(#!|@echo off).*$")
    if regex.match(line):
        return True

    return False


def _filename_in_headerblock(relative_path, contents):
    """Check for a filename in a header block.

    like such:
    # /path/to/filename
    """
    check_index = 0

    if len(contents) > 0:
        if _line_is_shebang(contents[0]):
            check_index = 1

    if len(contents) < check_index + 1:
        description = ("Document cannot have less than "
                       "{0} lines").format(check_index + 1)
        return LinterFailure(description, 1, replacement=None)

    regex = re.compile(r"^{0} \/".format(_HDR_COMMENT) +
                       re.escape(relative_path) + "$")
    if not regex.match(contents[check_index]):
        description = "The filename /{0} must be the first line of the header"
        return LinterFailure(description.format(relative_path),
                             check_index + 1,
                             _comment_type_from_line(contents[check_index]) +
                             "/{0}\n".format(relative_path))


def _match_space_at_line(line):
    """Return a re.match object if an empty comment was found on line."""
    regex = re.compile(r"^{0}$".format(_MDL_COMMENT))
    return regex.match(line)


def _space_in_headerblock(relative_path, contents):
    """Check for space between the filename in a header block and description.

    like such:
    # /path/to/filename
    #
    # Description
    """
    del relative_path

    check_index = 1

    if len(contents) > 0:
        if _line_is_shebang(contents[0]):
            check_index = 2

    if len(contents) < check_index + 1:
        description = ("Document cannot have less "
                       "than {0} lines").format(check_index + 1)
        return LinterFailure(description, 1, replacement=None)

    candidate = contents[check_index]

    if not _match_space_at_line(candidate):
        description = "The second line must be an empty comment"
        return LinterFailure(description, check_index + 1,
                             _comment_type_from_line(candidate)[:-1] + "\n" +
                             candidate)


def _find_last_line_index(contents):
    """Find the last line of the headerblock in contents."""
    lineno = 0
    headerblock = re.compile(r"^{0}.*$".format(_ALL_COMMENT))
    while headerblock.match(contents[lineno]):
        if lineno + 1 == len(contents):
            raise RuntimeError("No end of headerblock in file")
        lineno = lineno + 1

    if lineno < 2:
        raise RuntimeError("Headerblock must have at least two lines")

    return lineno - 1


def _space_before_copyright(relative_path, contents):
    """Check for a space between the last line and description.

    like such
    # Description
    #
    # Last Line
    """
    del relative_path

    last_line = _find_last_line_index(contents)
    if not _match_space_at_line(contents[last_line - 1]):
        comment = _comment_type_from_line(contents[last_line])[:-1]
        description = "The second last line must be an empty comment"
        return LinterFailure(description, last_line + 1,
                             comment + "\n" + contents[last_line])


def _copyright_end_of_headerblock(relative_path, contents):
    """Check for copyright notice at end of headerblock."""
    del relative_path

    lineno = _find_last_line_index(contents)
    notice = "See LICENCE.md for Copyright information"
    regex = re.compile(r"^{0} {1}( .*$|$)".format(_MDL_COMMENT, notice))
    if not regex.match(contents[lineno]):
        description = "The last of the header block line must have the "\
                      "following notice: {0}"
        replacement = None
        comment = _comment_type_from_line(contents[lineno])
        comment_footer = _end_comment_type_from_line(contents[lineno])
        # If the last line has the words "Copyright" or "LICENCE.md" the
        # user probably attempted to add a notice, but failed, so just
        # suggest replacing the whole line
        if re.compile(r"^.*(Copyright|LICENCE.md).*$").match(contents[lineno]):
            replacement = "{0}{1}\n".format(comment, notice + comment_footer)
        # Put the copyright notice on a new line
        else:
            # Strip off the footer and the \n at the end of the line.
            repl_contents = contents[lineno][:-(len(comment_footer) + 1)]
            replacement = "{0}\n{1}{2}\n".format(repl_contents,
                                                 comment,
                                                 notice + comment_footer)

        return LinterFailure(description.format(notice),
                             lineno + 1,
                             replacement)


def _newline_end_of_file(relative_path, contents):
    r"""Check that every file ends with a single \n."""
    del relative_path
    last_line = len(contents) - 1
    if not contents[last_line].endswith("\n"):
        return LinterFailure(r"No \n at end of file",
                             last_line + 1,
                             "{0}\n".format(contents[last_line]))


LINTER_FUNCTIONS = {
    "headerblock/filename": _filename_in_headerblock,
    "headerblock/desc_space": _space_in_headerblock,
    "headerblock/space_copyright": _space_before_copyright,
    "headerblock/copyright": _copyright_end_of_headerblock,
    "file/newline_last_char": _newline_end_of_file
}


def lint(relative_path_to_file,
         contents,
         whitelist=None,
         blacklist=None):
    r"""Actually lints some file contents.

    relative_path_to_file should
    contain the relative path to the file being linted from the root
    source directory. Contents should be a raw string with \ns
    Set report to change the default reporter - by default nothing is
    reported.
    """
    contents_lines = contents.splitlines(True)
    linter_functions = LINTER_FUNCTIONS

    def _keyvalue_pair_if(dictionary, condition):
        """Return a key-value pair in dictionary if condition matched."""
        return {
            k: v for (k, v) in dictionary.items() if condition(k)
        }

    def _check_list(check_list, cond):
        """Return function testing against a list if the list exists."""
        def _check_against_list(key):
            """Return true if list exists and condition passes."""
            return cond(check_list, key) if check_list is not None else True

        return _check_against_list

    linter_functions = _keyvalue_pair_if(linter_functions,
                                         _check_list(whitelist,
                                                     lambda l, k: k in l))
    linter_functions = _keyvalue_pair_if(linter_functions,
                                         _check_list(blacklist,
                                                     lambda l, k: k not in l))

    linter_errors = []
    for (code, function) in linter_functions.items():
        error = function(relative_path_to_file, contents_lines)
        if error:
            linter_errors.append((code, error))

    return linter_errors


class ShowAvailableChecksAction(argparse.Action):  # pylint:disable=R0903

    """If --checks is encountered, just show available checks and exit."""

    def __call__(self, parser, namespace, values, option_string=None):
        """Show available checks on --checks."""
        if option_string == "--checks":
            sys.stdout.write("Available option are:\n")
            for item in LINTER_FUNCTIONS.keys():
                sys.stdout.write(" * {0}\n".format(item))

            sys.exit(0)


def _parse_arguments(arguments=None):
    """Return a parser context result."""
    parser = argparse.ArgumentParser(description="Lint for Polysquare "
                                     "style guide")
    parser.add_argument("--checks",
                        nargs=0,
                        action=ShowAvailableChecksAction,
                        help="list available checks")
    parser.add_argument("files",
                        nargs="*",
                        metavar=("FILE"),
                        help="read FILE",
                        type=argparse.FileType("r+"))
    parser.add_argument("--whitelist",
                        nargs="*",
                        help="list of checks that should only be run",
                        default=None)
    parser.add_argument("--blacklist",
                        nargs="*",
                        help="list of checks that should never be run",
                        default=None)
    parser.add_argument("--fix-what-you-can",
                        action="store_const",
                        const=True)

    return parser.parse_args(arguments)


def _report_lint_error(error, file_path):
    """Report a linter error."""
    line = error[1].line
    code = error[0]
    description = error[1].description
    sys.stdout.write("{0}:{1} [{2}] {3}".format(file_path,
                                                line,
                                                code,
                                                description))


def _apply_replacement(error, found_file, file_lines):
    """Apply a single replacement."""
    fixed_lines = file_lines
    fixed_lines[error[1].line - 1] = error[1].replacement
    concatenated_fixed_lines = "".join(fixed_lines)

    # Only fix one error at a time
    found_file.seek(0)
    found_file.write(concatenated_fixed_lines)
    found_file.truncate()


def main(arguments=None):
    """Entry point for the linter."""
    result = _parse_arguments(arguments)

    num_errors = 0
    for found_file in result.files:
        file_path = os.path.abspath(found_file.name)
        file_contents = found_file.read()
        file_lines = file_contents.splitlines(True)
        try:
            errors = lint(file_path[len(os.getcwd()) + 1:],
                          file_contents,
                          result.whitelist,
                          result.blacklist)
        except RuntimeError as err:
            msg = "RuntimeError in processing {0} - {1}".format(file_path,
                                                                str(err))
            raise RuntimeError(msg)
        for error in errors:
            _report_lint_error(error, file_path)
            if result.fix_what_you_can and error[1].replacement is not None:
                _apply_replacement(error, found_file, file_lines)
                sys.stdout.write(" ... FIXED\n")
                break

            sys.stdout.write("\n")

        num_errors += len(errors)

    return num_errors
