# /polysquarelinter/valid_words_dictionary.py
#
# Helper module to create caches and a Dictionary for technical
# words as well as regular english words.
#
# See /LICENCE.md for Copyright information
"""Main module for linter."""

import argparse

import os

import sys

from polysquarelinter.spelling import (Dictionary,
                                       read_dictionary_file,
                                       valid_words_set)


def create(spellchecker_cache_path):
    """Create a Dictionary at spellchecker_cache_path with valid words."""
    user_dictionary = os.path.join(os.getcwd(), "DICTIONARY")
    user_words = read_dictionary_file(user_dictionary)

    valid_words = Dictionary(valid_words_set(user_dictionary, user_words),
                             "valid_words",
                             [user_dictionary],
                             spellchecker_cache_path)

    return (user_words, valid_words)


def _cause_cache_population():  # suppress(unused-function)
    """Cause the cache to be populated with valid words."""
    description = ("""Pre-populate cache for polysquare-generic-file-linter """
                   """and spellcheck-linter.""")
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("cache_path",
                        nargs="*",
                        metavar=("PATH"),
                        help="""PATH to cache""",
                        type=str)
    arguments = parser.parse_args(sys.argv[1:])
    create(arguments.cache_path[0])
    return 0
