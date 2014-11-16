# /polysquarelinter/linter.py
#
# Entry point for linter.
#
# See LICENCE.md for Copyright information
""" Main module for linter """

import argparse
import os
import re
import sys


class LinterFailureError(Exception):
    """Exception raised when lint() fails"""

    def __init__(self, code, line, description):
        super(LinterFailureError, self).__init__(self)
        self.description = description
        self.code = code
        self.line = line

    def __str__(self):
        error = ":{self.line} - [{self.code}]: {self.description}"
        return str(error.format(self=self))


class LinterCheckFailure(Exception):
    """Exception raised by individual linters when they fail"""

    def __init__(self, description, line):
        super(LinterCheckFailure, self).__init__(self)
        self.description = description
        self.line = line

    def __str__(self):
        return str("{self.description}".format(self=self))


def _filename_in_headerblock(relative_path, contents):
    """Check for a filename in a header block

    like such:
    # /path/to/filename
    """
    regex = re.compile(r"^(\/\*|#|//) \/" + re.escape(relative_path) + "$")
    if not regex.match(contents[0]):
        description = "The filename /{0} must be the first line of the header"
        raise LinterCheckFailure(description.format(relative_path), 1)


def _space_in_headerblock(relative_path, contents):
    """Check for a space between the filename in a header block and description

    like such:
    # /path/to/filename
    #
    # Description
    """
    del relative_path
    regex = re.compile(r"^( \*\/| \*|#|//)$")
    if not regex.match(contents[1]):
        description = "The second line must be an empty comment"
        raise LinterCheckFailure(description, 2)


def _copyright_end_of_headerblock(relative_path, contents):
    """Check for copyright notice at end of headerblock"""
    del relative_path
    headerblock = re.compile(r"^( \*|#|//).*$")
    lineno = 0
    while headerblock.match(contents[lineno]):
        if lineno + 1 == len(contents):
            raise LinterCheckFailure("No end of headerblock", lineno)
        lineno = lineno + 1

    lineno = lineno - 1
    notice = "See LICENCE.md for Copyright information"
    regex = re.compile(r"^( \*|#|//) {0}$".format(notice))
    if not regex.match(contents[lineno]):
        description = "The last of the header block line must have the "\
                      "following notice: {0}"
        raise LinterCheckFailure(description.format(notice), lineno + 1)


def _newline_end_of_file(relative_path, contents):
    """Check that every file ends with a single \n"""
    del relative_path
    if not contents[len(contents) - 1].endswith("\n"):
        raise LinterCheckFailure(r"No \n at end of file", len(contents))


def run_linter(relative_path, contents, linter_function, code):
    """Runs the linter_function over contents for relative_path

    raises LinterFailureError with its code if it fails
    """
    try:
        linter_function(relative_path, contents)
    except LinterCheckFailure as check_failure:
        raise LinterFailureError(code,
                                 check_failure.line,
                                 check_failure.description)

    return True

LINTER_FUNCTIONS = {
    "headerblock/filename": _filename_in_headerblock,
    "headerblock/desc_space": _space_in_headerblock,
    "headerblock/copyright": _copyright_end_of_headerblock,
    "file/newline_last_char": _newline_end_of_file
}


def _report_nothing(msg):
    """Null reporter"""
    del msg


def lint(relative_path_to_file,
         contents,
         whitelist=None,
         blacklist=None,
         report=_report_nothing):
    """Actually lints some file contents.

    relative_path_to_file should
    contain the relative path to the file being linted from the root
    source directory. Contents should be a raw string with \ns
    Set report to change the default reporter - by default nothing is
    reported.
    """

    contents_lines = contents.splitlines(True)
    linter_functions = LINTER_FUNCTIONS

    # Filter linters, firstly by whitelist
    if whitelist is not None:
        linter_functions = {
            k: v for (k, v) in linter_functions.items() if k in whitelist
        }

    if blacklist is not None:
        linter_functions = {
            k: v for (k, v) in linter_functions.items() if k not in blacklist
        }

    linter_errors = []
    for (code, function) in linter_functions.items():
        try:
            run_linter(relative_path_to_file, contents_lines, function, code)
        except LinterFailureError as linter_error:
            linter_errors.append(linter_error)

    if len(linter_errors) > 0:
        for error in linter_errors:
            report("{0}{1}\n".format(relative_path_to_file, error))

        return False
    else:
        return True


class ShowAvailableChecksAction(argparse.Action):
    """If --checks is encountered, just show available checks and exit"""

    def __call__(self, parser, namespace, values, option_string=None):
        if option_string == "--checks":
            sys.stdout.write("Available option are:\n")
            for item in LINTER_FUNCTIONS.keys():
                sys.stdout.write(" * {0}\n".format(item))

            sys.exit(0)


class ExitStatus(object):
    """A singleton encapsulating our exit status"""

    _instance = None

    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(ExitStatus, cls).__new__(cls,
                                                           *args,
                                                           **kwargs)
            cls._instance.exit_status = 0

        return cls._instance

    def error_reported(self):
        """Call when there is an error. Bumps exit_status"""
        self.exit_status = self.exit_status + 1

    def exit(self):
        """Calls sys.exit with the number of errors encountered"""
        sys.exit(self.exit_status)


def report_errors_at_runtime(msg):
    """Writes error to stderr and bumps ExitStatus


    ExitStatus will reflect how many failures there have been
    """
    sys.stderr.write("{0}".format(msg))
    ExitStatus().error_reported()


def main():
    """Entry point for the linter"""

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
                        type=argparse.FileType("r"))
    parser.add_argument("--whitelist",
                        nargs="*",
                        help="list of checks that should only be run",
                        default=None)
    parser.add_argument("--blacklist",
                        nargs="*",
                        help="list of checks that should never be run",
                        default=None)

    result = parser.parse_args()

    for found_file in result.files:
        lint(os.path.abspath(found_file.name)[len(os.getcwd()) + 1:],
             found_file.read(),
             result.whitelist,
             result.blacklist,
             report=report_errors_at_runtime)

    ExitStatus().exit()
