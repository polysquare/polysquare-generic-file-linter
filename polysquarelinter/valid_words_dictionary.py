# /polysquarelinter/valid_words_dictionary.py
#
# Helper module to create caches and a Dictionary for regular english words
# and any user defined words.
#
# See /LICENCE.md for Copyright information
"""Helper module to create caches for ordinary and user defined words."""

import os

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
