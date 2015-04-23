# /polysquarelinter/lint_spelling_only.py
#
# Entry point for a special linter that only handles spelling errors.
#
# See /LICENCE.md for Copyright information
"""Entry point for a special linter that only handles spelling errors."""

import argparse

import os

import sys

from polysquarelinter import spelling
from polysquarelinter.spelling import Dictionary, spellcheck_region


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
    lines = contents.splitlines(True)
    user_dictionary = os.path.join(os.getcwd(), "DICTIONARY")
    user_words = spelling.read_dictionary_file(user_dictionary)

    valid_words = Dictionary(spelling.valid_words_set(user_dictionary,
                                                      user_words),
                             "valid_words",
                             dictionary_sources=[user_dictionary],
                             cache=spellcheck_cache)

    # By default, include the user dictionary in the technical terms available.
    technical_terms_set = user_words

    if technical_terms:
        with open(technical_terms) as tech_tf:
            technical_terms_set |= set(tech_tf.read().splitlines())

    technical_words = Dictionary(technical_terms_set,
                                 "technical_words",
                                 dictionary_sources=[technical_terms,
                                                     user_dictionary],
                                 cache=spellcheck_cache)

    return sorted([e for e in spellcheck_region(lines,
                                                valid_words,
                                                technical_words,
                                                user_words)])


def _parse_arguments(arguments=None):
    """Return a parser context result."""
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
    result = _parse_arguments(arguments)

    num_errors = 0
    for found_filename in result.files:
        file_path = os.path.abspath(found_filename)
        with open(file_path, "r+") as found_file:
            errors = spellcheck(found_file.read(),
                                result.technical_terms,
                                result.spellcheck_cache)

            for error in errors:
                _report_spelling_error(error, file_path)

            num_errors += len(errors)

    return num_errors
