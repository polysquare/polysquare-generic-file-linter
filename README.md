# Polysquare Generic File Linter #

## Status ##

| Travis CI | AppVeyor | Coverage | PyPI |
|-----------|----------|----------|------|
|[![Travis](https://img.shields.io/travis/polysquare/polysquare-generic-file-linter.svg)]()|[![AppVeyor](https://img.shields.io/appveyor/ci/smspillaz/polysquare-generic-file-linter.svg)]()|[![Coveralls](https://img.shields.io/coveralls/polysquare/polysquare-generic-file-linter.svg)]()|[![PyPI](https://img.shields.io/pypi/v/polysquare-generic-file-linter.svg)]()[![PyPI](https://img.shields.io/pypi/pyversions/polysquare-generic-file-linter.svg)]()|[![License](https://img.shields.io/github/license/polysquare/polysquare-generic-file-linter.svg)]()|

Checks each file passed in for compliance with polysquare style guidelines.

* `headerblock/filename`: Checks that the first line of the file has a line
                          which matches `/path/to/file` from the source root
* `headerblock/desc_space`: Checks that the second line of the headerblock is
                            as empty comment
* `headerblock/space_copyright`: Checks that the second last line of the
                                 headerblock is an empty comment
* `headerblock/copyright`: Checks that the last line of the headerblock
                           contains an appropriate short-form copyright
                           notice
* `file/newline_last_char`: Checks that the last line is just a \n
* `file/spelling_error`: Checks that docstrings and comments do not
                         contain spelling errors or technical-like terms
                         that do not appear in the rest of the source file
* `file/trailing_whitespace`: Checks that no line contains trailing whitespace

## Main Linter Usage ##

    usage: polysquare-generic-file-linter [-h] [--checks]
                                          [--whitelist [LIST [LIST ...]]]
                                          [--blacklist [LIST [LIST ...]]]
                                          [--fix-what-you-can]
                                          [--spellcheck-cache SPELLCHECK_CACHE]
                                          [--log-technical-terms-to LOG]
                                          [--stamp-file-path STAMP_FILE_PATH]
                                          [--block-regexps [BLOCK [BLOCK ...]]]
                                          [FILE [FILE ...]]

    Lint for Polysquare style guide

    positional arguments:
      FILE                  read FILE

    optional arguments:
      -h, --help            show this help message and exit
      --checks              list available checks
      --whitelist [WHITELIST [WHITELIST ...]]
                            list of checks that should only be run
      --blacklist [BLACKLIST [BLACKLIST ...]]
                            list of checks that should never be run
      --fix-what-you-can    fix errors automatically
      --spellcheck-cache SPELLCHECK_CACHE
                            path to spell-checking cache file
      --log-technical-terms-to LOG_TECHNICAL_TERMS_TO
                            path to file to log technical terms to
      --stamp-file-path STAMP_FILE_PATH
                            path to directory to store cached results
      --block-regexps [BLOCK_REGEXES [BLOCK_REGEXES ...]]
                            Regular expressions to exclude from all checks.

## Spell-checking ##

Of some interest to others may be the spell-checking functionality. The
`file/spelling_error` check will scan any inline documentation (docstrings
and comments) in your code for spelling errors and misused technical terms. If
you want a string, (because, for example, it contains user-facing text) to be
considered, just make it a python-style docstring by using three quotes.

### Ordinary words ###

The spell-checker will check any ordinary word, those being words with
roman alphabetical characters and an apostrophe (') against a list of
words in the American English dictionary as generated by
[SCOWL](http://app.aspell.net/create) at level 50 with abbreviations and
hacker-terms on. You can also specify your own domain specific words by
providing a file called `DICTIONARY` in the project root directory. Words
are checked against both lists on a case-insensitive basis.

### Technical words ###

Certain words, once separated by the check, will be treated as "technical"
words as opposed to ordinary words. They will be checked against the list
of valid symbols detected from the surrounding source code. For instance,
the following code will trigger an error, because the term _CustomTerm wasn't
defined in the source file (CustomTerm was, however):

    class CustomTerm:

        """_CustomTerm is a certain type of class."""

### Ignoring certain expressions ###

Sometimes it does not make sense to run spellcheck or check certain expressions
against the list of detected technical words. This is often the case where
comments might contain inline markup or metadata which looks and behaves
like code. The check can be told to ignore anything matching a user-specified
regex in order to handle this case. Just pass the regex to `--block-regexps`.

### Removal of punctuation ###

The check will do its best to remove surrounding punctuation around words
such that only those words are checked against the word lists. However,
punctuation must follow standard English grammar rules in order for words
around them to be considered as ordinary words instead of technical words.
For instance, a space must appear before an opening parenthesis.
But a nested opening parenthesis can appear directly after another opening
The golden rule is that if it looks like something which could be
code, the surrounding words will be treated as code and not as ordinary
English words.

### Speeding up execution ###

[Whoosh](https://bitbucket.org/mchaput/whoosh/src), the spellchecking engine
behind `file/spelling_error` needs to generate some data structures in order to
quickly find corrections for words. Generating these data structures with the
long word list that is shipped with this tool by default can take a few seconds.
Obviously, this would be undesirable if this tool is to be used multiple times
or as part of a script. You can pass `--spellcheck-cache` and a path to a
directory to store cache files to cache the result of these data structures
between invocations.

### Stand-alone spellchecking ###

If you just want to run spellcheck on the code comments and inline
documentation, then you can use the `--whitelist` option to only run that check.
Just pass it with `--whitelist file/spelling_error`.

If you want to run spellcheck on an entire file, a special tool called
`spellcheck-linter` is provided which also serves that purpose. It will check
all ordinary looking words against the user-provided `DICTIONARY` and the
built-in American English dictionary. If `--technical-terms` and a path to a
filename containing technical terms is provided, it will also check that
technical looking terms exist in this file.

### Spellcheck Linter Usage ###

    usage: spellcheck-linter [-h] [--spellcheck-cache SPELLCHECK_CACHE]
                             [--technical-terms TECHNICAL_TERMS]
                             [--technical-terms-dependencies [[DEPENDENCY ...]]]
                             [--stamp-file-path STAMP_FILE_PATH]
                             [FILE [FILE ...]]

    Find spelling errors

    positional arguments:
      FILE                  read FILE

    optional arguments:
      -h, --help            show this help message and exit
      --spellcheck-cache SPELLCHECK_CACHE
                            path to spell-checking cache file
      --technical-terms TECHNICAL_TERMS
                            path to file to source technical terms from
      --stamp-file-path STAMP_FILE_PATH
                            path to directory to store cached results

#### Technical terms ###

A technical terms file is just a list of symbols in a text file. As a matter
of convenience, this can be automatically generated for you by passing
`--log-technical-terms-to` to `polysquare-generic-file-linter` when checking
the inline documentation of those files. The second argument after this
switch should be the path to a filename where technical terms are to be stored.
On each invocation, the union of the current file contents and the technical
terms detected will be written back to the file.

### Embedding the checking API ###

The exported API in `polysquarelinter.spelling` isn't by any means stable
right now, but it can be embedded into an application with some ease.

The `Dictionary` class encapsulates the Whoosh spellchecking API, word-lookup
and caching functionality. The `dictionary_sources` keyword argument indicates
a list of files from which the words in passed to the Dictionary were
sourced from. If any of these files has a newer timestamp than the dictionary
cache, then the dictionary cache will be regenerated.

The `spellcheck_region` function takes a list of lines and runs spellcheck
and a check for invalid technical words on each word in those lines. It will
handle punctuation and other syntactical markets appropriately in either case.
Both the `valid_words_dictionary` and `technical_words_dictionary` can be either
None or an instance of `Dictionary`. The `user_words` argument is simply
a set of words that the user has indicated are always valid.

The `spellcheckable_and_shadow_contents` splits a file into spellcheckable
chunks (made out of `_ChunkInfo`) and "shadow contents", which make up the rest
of the file. The shadow contents are usually just the code around the inline
documentation. The `data` member of `_ChunkInfo` is a list of lines,
effectively representing the region which should be spellchecked. `line_offset`
and `col_offset` indicate the line and column offset into the main contents. If
you are reporting errors, any error in `SpellcheckError` as returned by
`spellcheck_region` will be returned relative to the contents passed to it
and not to the whole file. Use the offsets in `_ChunkInfo` to turn these
into absolute offsets into the file being checked itself.

### Caching ###

Internally, `polysquare-generic-file-linter` and `spellcheck-linter` cache
their results using the [`jobstamps`](https://github.com/polysquare/jobstamps)
library. If you want to redirect where the cache files are written, you
can pass `--stamp-file-path` to either tool.
