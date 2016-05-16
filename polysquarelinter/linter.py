# /polysquarelinter/linter.py
#
# Entry point for linter.
#
# See /LICENCE.md for Copyright information
"""Main module for linter."""

import argparse

import errno

import hashlib

import io

import itertools

import multiprocessing

import os

import re

import sys

import tempfile

import traceback

from collections import OrderedDict, defaultdict, namedtuple

from contextlib import closing

from functools import reduce as freduce

from jobstamps import jobstamp

import parmap

from polysquarelinter.spelling import (Dictionary,
                                       SpellcheckError,
                                       spellcheck_region,
                                       spellcheckable_and_shadow_contents,
                                       technical_words_from_shadow_contents,)
import polysquarelinter.valid_words_dictionary as valid_words_dictionary_helper

try:
    from Queue import Queue
except ImportError:
    # suppress(F811,E301,E101,F401,unused-import,import-error)
    from queue import Queue

_ALL_COMMENT = r"(/\*| \*\/| \*|#|//|rem)"
_HDR_COMMENT = r"(/\*|#|//|rem)"
_MDL_COMMENT = r"( \*\/| \*|#|//|rem)"
_FTR_COMMENT = r"(/\*| \*)"


def _comment_type_from_line(line):
    """Return the "comment header" (' * ', '# ', 'rem ', '// ', '/* ').

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


def _filename_in_headerblock(relative_path, contents, linter_options):
    """Check for a filename in a header block.

    like such:
    # /path/to/filename
    """
    del linter_options
    check_index = 0

    if len(contents) > 0:
        if _line_is_shebang(contents[0]):
            check_index = 1

    if len(contents) < check_index + 1:
        description = ("""Document cannot have less than """
                       """{0} lines""").format(check_index + 1)
        return LinterFailure(description, 1, replacement=None)

    header_path = relative_path.replace("\\", "/")
    regex = re.compile(r"^{0} \/{1}$".format(_HDR_COMMENT,
                                             re.escape(header_path)))
    if not regex.match(contents[check_index]):
        description = ("""The filename /{0} must be the """
                       """first line of the header""")
        return LinterFailure(description.format(header_path),
                             check_index + 1,
                             _comment_type_from_line(contents[check_index]) +
                             "/{0}\n".format(header_path))


def _match_space_at_line(line):
    """Return a re.match object if an empty comment was found on line."""
    regex = re.compile(r"^{0}$".format(_MDL_COMMENT))
    return regex.match(line)


def _space_in_headerblock(relative_path, contents, linter_options):
    """Check for space between the filename in a header block and description.

    like such:
    # /path/to/filename
    #
    # Description
    """
    del relative_path
    del linter_options

    check_index = 1

    if len(contents) > 0:
        if _line_is_shebang(contents[0]):
            check_index = 2

    if len(contents) < check_index + 1:
        description = ("""Document cannot have less """
                       """than {0} lines""").format(check_index + 1)
        return LinterFailure(description, 1, replacement=None)

    candidate = contents[check_index]

    if not _match_space_at_line(candidate):
        description = """The second line must be an empty comment"""
        return LinterFailure(description, check_index + 1,
                             _comment_type_from_line(candidate)[:-1] + "\n" +
                             candidate)


def _find_last_line_index(contents):
    """Find the last line of the headerblock in contents."""
    lineno = 0
    headerblock = re.compile(r"^{0}.*$".format(_ALL_COMMENT))

    if not len(contents):
        raise RuntimeError("""File does not not have any contents""")

    while headerblock.match(contents[lineno]):
        if lineno + 1 == len(contents):
            raise RuntimeError("""No end of headerblock in file""")
        lineno = lineno + 1

    if lineno < 2:
        raise RuntimeError("""Headerblock must have at least two lines""")

    return lineno - 1


def _space_before_copyright(relative_path, contents, linter_options):
    """Check for a space between the last line and description.

    like such
    # Description
    #
    # Last Line
    """
    del relative_path
    del linter_options

    last_line = _find_last_line_index(contents)
    if not _match_space_at_line(contents[last_line - 1]):
        comment = _comment_type_from_line(contents[last_line])[:-1]
        description = """The second last line must be an empty comment"""
        return LinterFailure(description, last_line + 1,
                             comment + "\n" + contents[last_line])


def _copyright_end_of_headerblock(relative_path, contents, linter_options):
    """Check for copyright notice at end of headerblock."""
    del relative_path
    del linter_options

    lineno = _find_last_line_index(contents)
    notice = "See /LICENCE.md for Copyright information"
    regex = re.compile(r"^{0} {1}( .*$|$)".format(_MDL_COMMENT, notice))
    if not regex.match(contents[lineno]):
        description = ("""The last of the header block line must have the """
                       """following notice: {0}""")
        replacement = None
        comment = _comment_type_from_line(contents[lineno])
        comment_footer = _end_comment_type_from_line(contents[lineno])
        # If the last line has the words "Copyright" or "/LICENCE.md" the
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


def _newline_end_of_file(relative_path, contents, linter_options):
    r"""Check that every file ends with a single \n."""
    del relative_path
    del linter_options
    last_line = len(contents) - 1
    if not len(contents) or not contents[last_line].endswith("\n"):
        if len(contents):
            last_line_replacement = contents[last_line] + "\n"
        else:
            last_line_replacement = "\n"
        return LinterFailure(r"No \n at end of file",
                             last_line + 1,
                             last_line_replacement)


def _no_trailing_whitespace(relative_path, contents, linter_options):
    r"""Check that every file ends with a single \n."""
    del relative_path
    del linter_options

    trailing_whitespace = re.compile(r"(?<![ \t\r\n])[ \t]+$")

    for index, line in enumerate(contents):
        for space in trailing_whitespace.finditer(line):
            assert space.end() == len(line) - 1
            return LinterFailure("Trailing whitespace",
                                 index + 1,
                                 line[:space.start()] + line[space.end():])


# Disabling too-many-arguments as this is just a helper function
# to populate another named tuple.
#
# suppress(too-many-arguments)
def _populate_spelling_error(word,
                             suggestions,
                             contents,
                             line_offset,
                             column_offset,
                             message_start):
    """Create a LinterFailure for word.

    This function takes suggestions from :suggestions: and uses it to
    populate the message and candidate replacement. The replacement will
    be a line in :contents:, as determined by :line_offset: and
    :column_offset:.
    """
    error_line = contents[line_offset]
    if len(suggestions):
        char_word_offset = (column_offset + len(word))
        replacement = (error_line[:column_offset] +
                       suggestions[0] +
                       error_line[char_word_offset:])
    else:
        replacement = None

    if len(suggestions):
        suggestions_text = (""", perhaps you meant """ +
                            " ".join(suggestions))
    else:
        suggestions_text = ""

    format_desc = message_start + suggestions_text
    return LinterFailure(format_desc,
                         line_offset + 1,
                         replacement)


def _determine_character_offset(line_offset, character_offset, chunk_offset):
    """Apply chunk_offset if line_offset is zero."""
    if line_offset == 0:
        return character_offset + chunk_offset
    else:
        return character_offset


_SPELLCHECK_MESSAGES = {
    SpellcheckError.InvalidWord: """Misspelled word {0}""",
    SpellcheckError.TechnicalWord: """Technical word {0} not used in """
                                   """surrounding body """
}


def _find_spelling_errors_in_chunks(chunks,
                                    contents,
                                    valid_words_dictionary=None,
                                    technical_words_dictionary=None,
                                    user_dictionary_words=None):
    """For each chunk and a set of valid and technical words, find errors."""
    for chunk in chunks:
        for error in spellcheck_region(chunk.data,
                                       valid_words_dictionary,
                                       technical_words_dictionary,
                                       user_dictionary_words):
            col_offset = _determine_character_offset(error.line_offset,
                                                     error.column_offset,
                                                     chunk.column)
            msg = _SPELLCHECK_MESSAGES[error.error_type].format(error.word)
            yield _populate_spelling_error(error.word,
                                           error.suggestions,
                                           contents,
                                           error.line_offset +
                                           chunk.line,
                                           col_offset,
                                           msg)


# suppress(invalid-name)
def _create_technical_words_dictionary(spellchecker_cache_path,
                                       relative_path,
                                       user_words,
                                       shadow):
    """Create Dictionary at spellchecker_cache_path with technical words."""
    technical_terms_set = (user_words |
                           technical_words_from_shadow_contents(shadow))
    technical_words = Dictionary(technical_terms_set,
                                 "technical_words_" +
                                 relative_path.replace(os.path.sep, "_"),
                                 [os.path.realpath(relative_path)],
                                 spellchecker_cache_path)
    return technical_words


def _construct_user_dictionary(global_options, tool_options):
    """Cause dictionary with valid and user words to be cached on disk."""
    del global_options

    spellchecker_cache_path = tool_options.get("spellcheck_cache", None)
    valid_words_dictionary_helper.create(spellchecker_cache_path)


def _drain(queue_to_drain, sentinel=None):
    """Remove all values from queue_to_drain and return as list.

    This uses the trick from
    http://stackoverflow.com/questions/
    1540822/dumping-a-multiprocessing-queue-into-a-list in order to ensure that
    the queue is fully drained.
    """
    queue_to_drain.put(sentinel)
    queued_items = [i for i in iter(queue_to_drain.get, None)]
    return queued_items


def _maybe_log_technical_terms(global_options, tool_options):
    """Log technical terms as appropriate if the user requested it.

    As a side effect, if --log-technical-terms-to is passed to the linter
    then open up the file specified (or create it) and then merge the set
    of technical words that we have now with the technical words already
    in it.
    """
    log_technical_terms_to_path = global_options.get("log_technical_terms_to",
                                                     None)
    log_technical_terms_to_queue = tool_options.get("log_technical_terms_to",
                                                    None)
    if log_technical_terms_to_path:
        assert log_technical_terms_to_queue is not None

        try:
            os.makedirs(os.path.dirname(log_technical_terms_to_path))
        except OSError as error:
            if error.errno != errno.EEXIST:
                raise error

        if not log_technical_terms_to_queue.empty():
            with closing(os.fdopen(os.open(log_technical_terms_to_path,
                                           os.O_RDWR | os.O_CREAT),
                                   "r+")) as terms_file:
                # pychecker can't see through the handle returned by closing
                # so we need to suppress these warnings.
                terms = set(terms_file.read().splitlines())
                new_terms = set(freduce(lambda x, y: x | y,
                                        _drain(log_technical_terms_to_queue)))

                if not terms.issuperset(new_terms):
                    terms_file.seek(0)
                    terms_file.truncate(0)
                    terms_file.write("\n".join(list(terms |
                                                    set(new_terms))))


def _no_spelling_errors(relative_path, contents, linter_options):
    """No spelling errors in strings, comments or anything of the like."""
    block_regexps = linter_options.get("block_regexps", None)
    chunks, shadow = spellcheckable_and_shadow_contents(contents,
                                                        block_regexps)
    cache = linter_options.get("spellcheck_cache", None)
    user_words, valid_words = valid_words_dictionary_helper.create(cache)
    technical_words = _create_technical_words_dictionary(cache,
                                                         relative_path,
                                                         user_words,
                                                         shadow)

    if linter_options.get("log_technical_terms_to"):
        linter_options["log_technical_terms_to"].put(technical_words.words())

    return [e for e in _find_spelling_errors_in_chunks(chunks,
                                                       contents,
                                                       valid_words,
                                                       technical_words,
                                                       user_words) if e]


def _line_suppresses_error_code(line, code):
    """Check if line contains necessary content to suppress code.

    A line suppresses code if it is in the format suppress(code1,code2) etc.
    """
    match = re.compile(r"suppress\((.*)\)").match(line)
    if match:
        codes = match.group(1).split(",")
        return code in codes

    return False


def _error_is_suppressed(error, code, contents):
    """Return true if error is suppressed by an inline suppression."""
    if len(contents) == 0:
        return False

    if error.line > 1:
        # Check above, and then to the side for suppressions
        above = contents[error.line - 2].split("#")
        if len(above) and _line_suppresses_error_code(above[-1].strip(),
                                                      code):
            return True

    aside = contents[error.line - 1].split("#")
    if len(aside) and _line_suppresses_error_code(aside[-1].strip(), code):
        return True

    return False


class _LinterFunction(namedtuple("_LinterFunction",
                                 "function before_all after_all")):
    """A function and some before/after hooks to be called."""

    def __repr__(self):
        """Print function names, but no more."""
        func_name = self.function.__name__ if self.function else "_"
        before_name = self.before_all.__name__ if self.before_all else "_"
        after_name = self.after_all.__name__ if self.after_all else "_"

        return "_LinterFunction({}, {}, {})".format(func_name,
                                                    before_name,
                                                    after_name)


def _linter_function(function, before_all=None, after_all=None):
    """Return a _LinterFunction with before_all and after_all defaulted."""
    return _LinterFunction(function, before_all, after_all)

LINTER_FUNCTIONS = {
    "headerblock/filename": _linter_function(_filename_in_headerblock),
    "headerblock/desc_space": _linter_function(_space_in_headerblock),
    "headerblock/space_copyright": _linter_function(_space_before_copyright),
    "headerblock/copyright": _linter_function(_copyright_end_of_headerblock),
    "file/newline_last_char": _linter_function(_newline_end_of_file),
    "file/trailing_whitespace": _linter_function(_no_trailing_whitespace),
    "file/spelling_error": _linter_function(_no_spelling_errors,
                                            _construct_user_dictionary,
                                            _maybe_log_technical_terms)
}


def lint(relative_path_to_file,
         contents,
         linter_functions,
         **kwargs):
    r"""Actually lints some file contents.

    relative_path_to_file should contain the relative path to the file being
    linted from the root source directory. Contents should be a raw string
    with \n's.
    """
    lines = contents.splitlines(True)

    errors = list()
    for (code, info) in linter_functions.items():
        error = info.function(relative_path_to_file, lines, kwargs)
        if error:
            if isinstance(error, list):
                errors.extend([(code, e) for e in error])
            else:
                errors.append((code, error))

    errors = [e for e in errors if not _error_is_suppressed(e[1], e[0], lines)]
    return sorted(errors, key=lambda e: e[1].line)


def linter_functions_from_filters(whitelist=None,
                                  blacklist=None):
    """Yield tuples of _LinterFunction matching whitelist but not blacklist."""
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

    linter_functions = LINTER_FUNCTIONS
    linter_functions = _keyvalue_pair_if(linter_functions,
                                         _check_list(whitelist,
                                                     lambda l, k: k in l))
    linter_functions = _keyvalue_pair_if(linter_functions,
                                         _check_list(blacklist,
                                                     lambda l, k: k not in l))

    for code, linter_function in linter_functions.items():
        yield (code, linter_function)


class ShowAvailableChecksAction(argparse.Action):  # pylint:disable=R0903
    """If --checks is encountered, just show available checks and exit."""

    def __call__(self, parser, namespace, values, option_string=None):
        """Show available checks on --checks."""
        if option_string == "--checks":
            sys.stdout.write("Available options are:\n")
            for item in LINTER_FUNCTIONS.keys():
                sys.stdout.write(" * {0}\n".format(item))

            sys.exit(0)


def _parse_arguments(arguments=None):
    """Return a parser context result."""
    dir_hash = hashlib.sha1(os.getcwd().encode("utf-8")).hexdigest()

    parser = argparse.ArgumentParser(description="""Lint for Polysquare """
                                     """style guide""")
    parser.add_argument("--checks",
                        nargs=0,
                        action=ShowAvailableChecksAction,
                        help="""list available checks""")
    parser.add_argument("files",
                        nargs="*",
                        metavar=("FILE"),
                        help="""read FILE""",
                        type=str)
    parser.add_argument("--whitelist",
                        nargs="*",
                        help="""list of checks that should only be run""",
                        default=None)
    parser.add_argument("--blacklist",
                        nargs="*",
                        help="""list of checks that should never be run""",
                        default=None)
    parser.add_argument("--fix-what-you-can",
                        action="store_const",
                        help="""fix errors automatically""",
                        const=True)
    parser.add_argument("--spellcheck-cache",
                        help="""path to spell-checking cache file""",
                        default=None)
    parser.add_argument("--log-technical-terms-to",
                        help="""path to file to log technical terms to""",
                        default=None)
    parser.add_argument("--stamp-file-path",
                        help="""path to directory to store cached results""",
                        default=os.path.join(tempfile.gettempdir(),
                                             "jobstamps",
                                             "spellcheck_linter",
                                             dir_hash))
    parser.add_argument("--block-regexps",
                        help="""Regular expressions to exclude from """
                             """all checks.""",
                        nargs="*",
                        default=None)

    return parser.parse_args(arguments)


def _report_lint_error(error, file_path):
    """Report a linter error."""
    line = error[1].line
    code = error[0]
    description = error[1].description
    sys.stdout.write("{0}:{1} [{2}] {3}\n".format(file_path,
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


def _should_use_multiprocessing(num_jobs):
    """Return true if multiprocessing should be enabled."""
    return (not os.getenv("DISABLE_MULTIPROCESSING", None) and
            multiprocessing.cpu_count() < num_jobs and
            multiprocessing.cpu_count() > 2)


class ReprQueue(object):
    """Queue that has a stable __repr__."""

    def __init__(self, queue):
        """Initialize with underlying queue."""
        super(ReprQueue, self).__init__()
        self._queue = queue

    def __repr__(self):
        """Class name of underlying queue."""
        return self._queue.__class__.__name__

    def put(self, item, block=True, timeout=None):
        """Put item into underlying queue."""
        return self._queue.put(item, block, timeout)

    def get(self, block=True, timeout=None):
        """Get item from underlying queue."""
        return self._queue.get(block, timeout)

    def empty(self):
        """Return empty status from underlying queue."""
        return self._queue.empty()


def _obtain_queue(num_jobs):
    """Return queue type most appropriate for runtime model.

    If we are using multiprocessing, that should be
    multiprocessing.Manager().Queue. If we are just using a
    single process, then use a normal queue type.
    """
    if _should_use_multiprocessing(num_jobs):
        return ReprQueue(multiprocessing.Manager().Queue())

    return ReprQueue(Queue())


def tool_options_from_global(global_options, num_jobs):
    """From an argparse namespace, get a dict of options for the tools."""
    internal_opt = ["whitelist", "blacklist", "fix_what_you_can"]
    queue_object = _obtain_queue(num_jobs)
    translate = defaultdict(lambda: (lambda x: x),
                            log_technical_terms_to=lambda _: queue_object)

    tool_options = OrderedDict()

    for key in sorted(global_options.keys()):
        if key not in internal_opt and global_options[key] is not None:
            tool_options[key] = translate[key](global_options[key])

    return tool_options


class FileLinterFailure(namedtuple("LinterFailure", "absolute_path failure")):
    """A class representing a linter failure as occurring on a file."""

    def __lt__(self, other):
        """Return true if self should be sorted less than other."""
        if self.absolute_path == other.absolute_path:
            return self.failure[1].line < other.failure[1].line

        return self.absolute_path < other.absolute_path


def _run_lint_on_file(file_path,
                      linter_functions,
                      tool_options,
                      fix_what_you_can):
    """Run each function in linter_functions on filename.

    If fix_what_you_can is specified, then the first error that has a
    possible replacement will be automatically fixed on this file.
    """
    with io.open(file_path, "r+", encoding="utf-8") as found_file:
        file_contents = found_file.read()
        file_lines = file_contents.splitlines(True)
        try:
            errors = lint(file_path[len(os.getcwd()) + 1:],
                          file_contents,
                          linter_functions,
                          **tool_options)
        except RuntimeError as err:
            msg = ("""RuntimeError in processing """
                   """{0} - {1}""".format(file_path, str(err)))
            errors = [("polysquarelinter/failure",
                       LinterFailure(msg, 0, None))]

        if fix_what_you_can:
            for error_index, error in enumerate(errors):
                if error[1].replacement is not None:
                    _apply_replacement(error, found_file, file_lines)
                    errors[error_index] = (error[0],
                                           LinterFailure(error[1].description +
                                                         " ... FIXED",
                                                         error[1].line,
                                                         error[1].replacement))
                    break

        return [FileLinterFailure(file_path, e) for e in errors]


def _run_lint_on_file_exceptions(file_path,
                                 linter_functions,
                                 tool_options,
                                 fix_what_you_can):
    """Run except function in :linter_functions: on file_path.

    This function calls through to _run_lint_on_file, but prints a traceback
    for any exceptions encountered. It is intended to be used with parmap.
    """
    try:
        return _run_lint_on_file(file_path,
                                 linter_functions,
                                 tool_options,
                                 fix_what_you_can)
    except Exception as exception:
        sys.stderr.write("""Encountered exception """
                         """when processing {}\n""".format(file_path))
        traceback.print_exc()
        raise exception


def _run_lint_on_file_stamped_args(file_path,  # suppress(too-many-arguments)
                                   stamp_file_path,
                                   log_technical_terms_to,
                                   linter_functions,
                                   tool_options,
                                   fix_what_you_can):
    """Return tuple of args and kwargs that function would be called with."""
    dictionary_path = os.path.abspath("DICTIONARY")
    dependencies = [file_path]

    if os.path.exists(dictionary_path):
        dependencies.append(dictionary_path)

    kwargs = OrderedDict()
    kwargs["jobstamps_dependencies"] = dependencies
    kwargs["jobstamps_cache_output_directory"] = stamp_file_path

    if log_technical_terms_to:
        kwargs["jobstamps_output_files"] = [log_technical_terms_to]

    return ((file_path,
             linter_functions,
             tool_options,
             fix_what_you_can),
            kwargs)


def _run_lint_on_file_stamped(*args):
    """Run linter functions on file_path, stamping in stamp_file_path."""
    # We pass an empty dictionary as keyword arguments here to work
    # around a bug in frosted, which crashes when no keyword arguments
    # are passed
    #
    # suppress(E204)
    stamp_args, stamp_kwargs = _run_lint_on_file_stamped_args(*args,
                                                              **{})

    return jobstamp.run(_run_lint_on_file_exceptions,
                        *stamp_args,
                        **stamp_kwargs)


def _ordered(generator, *args, **kwargs):
    """Sort keys of unordered_dict and store in OrderedDict."""
    unordered_dict = {k: v for k, v in generator(*args, **kwargs)}
    keys = sorted(list(dict(unordered_dict).keys()))
    result = OrderedDict()
    for key in keys:
        result[key] = unordered_dict[key]

    return result


def _any_would_run(func, filenames, *args):
    """True if a linter function would be called on any of filenames."""
    if os.environ.get("_POLYSQUARE_GENERIC_FILE_LINTER_NO_STAMPING", None):
        return True

    for filename in filenames:
        # suppress(E204)
        stamp_args, stamp_kwargs = _run_lint_on_file_stamped_args(filename,
                                                                  *args,
                                                                  **{})
        dependency = jobstamp.out_of_date(func,
                                          *stamp_args,
                                          **stamp_kwargs)

        if dependency:
            return True

    return False


def main(arguments=None):   # suppress(unused-function)
    """Entry point for the linter."""
    result = _parse_arguments(arguments)
    linter_funcs = _ordered(linter_functions_from_filters,
                            result.whitelist,
                            result.blacklist)
    global_options = vars(result)
    tool_options = tool_options_from_global(global_options, len(result.files))
    any_would_run = _any_would_run(_run_lint_on_file_exceptions,
                                   result.files,
                                   result.stamp_file_path,
                                   result.log_technical_terms_to,
                                   linter_funcs,
                                   tool_options,
                                   result.fix_what_you_can)

    if any_would_run:
        for linter_function in linter_funcs.values():
            if linter_function.before_all:
                linter_function.before_all(global_options, tool_options)

        use_multiprocessing = _should_use_multiprocessing(len(result.files))
    else:
        use_multiprocessing = False

    if use_multiprocessing:
        mapper = parmap.map
    else:
        # suppress(E731)
        mapper = lambda f, i, *a: [f(*((x, ) + a)) for x in i]

    errors = list(itertools.chain(*mapper(_run_lint_on_file_stamped,
                                          result.files,
                                          result.stamp_file_path,
                                          result.log_technical_terms_to,
                                          linter_funcs,
                                          tool_options,
                                          result.fix_what_you_can)))

    for error in sorted(errors):
        _report_lint_error(error.failure, os.path.relpath(error.absolute_path))

    if any_would_run:
        for linter_funcs in linter_funcs.values():
            if linter_funcs.after_all:
                linter_funcs.after_all(global_options, tool_options)

    return len(errors)
