# /polysquarelinter/lint_spelling_only.py
#
# Entry point for a special linter that only handles spelling errors.
#
# See /LICENCE.md for Copyright information
"""Entry point for a special linter that only handles spelling errors."""

import argparse

import hashlib

import io

import os

import sys

import tempfile

from jobstamps import jobstamp

from polysquarelinter import (spelling,
                              technical_words_dictionary,
                              valid_words_dictionary)
from polysquarelinter.spelling import spellcheck_region


def _filter_disabled_regions(contents):
    """Filter regions that are contained in back-ticks."""
    contents = list(contents)

    in_backticks = False
    contents_len = len(contents)

    index = 0
    while index < contents_len:
        character = contents[index]
        if character == "`":
            # Check to see if we should toggle the in_backticks
            # mode here by looking ahead for another two characters.
            if ((index + 2) < contents_len and
                    "".join(contents[index:index + 3]) == "```"):
                in_backticks = not in_backticks
                index += 3
                continue

        if in_backticks:
            contents[index] = " "

        index += 1

    return "".join(contents)


def spellcheck(contents, technical_terms=None, spellcheck_cache=None):
    """Run spellcheck on the contents of a file.

    :technical_terms: is a path to a file containing a list of "technical"
    terms. These may be symbols as collected from files by using
    the generic linter or other such symbols. If a symbol-like term is
    used within contents and it does not appear in :technical_terms: then
    an error will result.

    :spellcheck_cache: is a path to a directory where graph files generated
    by the spellchecking engine should be stored. It is used for caching
    purposes between invocations, since generating the spellchecking
    graph is an expensive operation which can take a few seconds to complete.
    """
    contents = spelling.filter_nonspellcheckable_tokens(contents)
    contents = _filter_disabled_regions(contents)
    lines = contents.splitlines(True)
    user_words, valid_words = valid_words_dictionary.create(spellcheck_cache)
    technical_words = technical_words_dictionary.create(technical_terms,
                                                        spellcheck_cache)

    return sorted([e for e in spellcheck_region(lines,
                                                valid_words,
                                                technical_words,
                                                user_words)])


def _parse_arguments(arguments=None):
    """Return a parser context result."""
    dir_hash = hashlib.sha1(os.getcwd().encode("utf-8")).hexdigest()

    parser = argparse.ArgumentParser(description="""Find spelling errors""")
    parser.add_argument("files",
                        nargs="*",
                        metavar=("FILE"),
                        help="""read FILE""",
                        type=str)
    parser.add_argument("--spellcheck-cache",
                        help="""path to spell-checking cache file""",
                        default=None)
    parser.add_argument("--technical-terms",
                        help="""path to file to source technical terms from""",
                        default=None)
    parser.add_argument("--stamp-file-path",
                        help="""path to directory to store cached results""",
                        default=os.path.join(tempfile.gettempdir(),
                                             "jobstamps",
                                             "spellcheck_linter",
                                             dir_hash))

    return parser.parse_args(arguments)


_SPELLCHECK_MESSAGES = {
    spelling.SpellcheckError.InvalidWord: """Misspelled word {0}""",
    spelling.SpellcheckError.TechnicalWord: """Technical term {0} not used """
                                            """in any scanned code file"""
}


def _report_spelling_error(error, file_path):
    """Report a spelling error."""
    line = error.line_offset + 1
    code = "file/spelling_error"
    description = _SPELLCHECK_MESSAGES[error.error_type].format(error.word)
    if error.suggestions is not None:
        description = (description +
                       ", perhaps you meant: " +
                       ", ".join(error.suggestions))

    sys.stdout.write("{0}:{1} [{2}] {3}\n".format(file_path,
                                                  line,
                                                  code,
                                                  description))


def main(arguments=None):  # suppress(unused-function)
    """Entry point for the spellcheck linter."""
    dictionary_path = os.path.abspath("DICTIONARY")
    result = _parse_arguments(arguments)

    num_errors = 0
    for found_filename in result.files:
        file_path = os.path.abspath(found_filename)
        with io.open(file_path, "r+", encoding="utf-8") as found_file:
            jobstamps_dependencies = [file_path]

            if os.path.exists(dictionary_path):
                jobstamps_dependencies.append(dictionary_path)

            if (result.technical_terms and
                    os.path.exists(result.technical_terms)):
                jobstamps_dependencies.append(result.technical_terms)

            kwargs = {
                "jobstamps_dependencies": jobstamps_dependencies,
                "jobstamps_cache_output_directory": result.stamp_file_path,
            }

            errors = jobstamp.run(spellcheck,
                                  found_file.read(),
                                  result.technical_terms,
                                  result.spellcheck_cache,
                                  **kwargs)

            for error in errors:
                _report_spelling_error(error, file_path)

            num_errors += len(errors)

    return num_errors
