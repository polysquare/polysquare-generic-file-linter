# /polysquarelinter/cache_populate.py
#
# Populate caches before running multiple checker processes in parallel.
#
# See /LICENCE.md for Copyright information
"""Populate caches before running multiple checker processes in parallel."""

import argparse

import sys

from polysquarelinter import (technical_words_dictionary,
                              valid_words_dictionary)


def main():  # suppress(unused-function)
    """Cause the cache to be populated with valid words."""
    description = ("""Pre-populate cache for polysquare-generic-file-linter """
                   """and spellcheck-linter.""")
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("cache_path",
                        metavar=("PATH"),
                        help="""PATH to cache""",
                        type=str)
    parser.add_argument("--technical-terms",
                        metavar=("FILE"),
                        help="""FILE containing technical terms""",
                        type=str)
    arguments = parser.parse_args(sys.argv[1:])
    valid_words_dictionary.create(arguments.cache_path)
    technical_words_dictionary.create(arguments.technical_terms,
                                      arguments.cache_path)
    return 0
