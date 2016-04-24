# /polysquarelinter/spelling.py
#
# Implements spell-checking functionality for polysquarelinter.
#
# See /LICENCE.md for Copyright information
"""Implements spell-checking functionality for polysquarelinter.

The main two functions of interest are
spellcheckable_and_shadow_contents and spellcheck_region.

spellcheckable_and_shadow_contents will return a tuple
of both a list of _ChunkInfo classes and a shadow region.

The _ChunkInfo classes, represent a region spanning some lines
of text. The data member of _ChunkInfo is a list of lines covering
that region.

The shadow region is an array of characters with 1-1 mapping on the rest
of the input lines which were not covered by any chunk. This can
be used, for example, to generate a list of "technical words" used
in the rest of the file. technical_words_from_shadow_contents provides
this functionality as a convenience.

spellcheck_region takes a list of lines and spell-checks every
word in that region. If it detects an error it will return
a SpellcheckError indicating the offset into that chunk
the error was detected.
"""

import errno

import functools

import itertools

import os

import re

from collections import namedtuple

from pkg_resources import resource_stream

from whoosh import spelling
from whoosh.automata import fst
from whoosh.filedb.filestore import FileStorage, RamStorage, copy_to_ram

_SPELLCHECKABLE_WORDS = r"^([A-Za-z][a-z']*|[A-Z']*)$"
_VALID_SYMBOL_WORDS = r"^[A-Za-z_][A-Za-z0-9_\.]*$"

# These are intended to be used as "global" variables, as caches. We
# don't want to expose these caches to the user. As such, they should
# not be named with the constant naming convention.
_spellchecker_cache = dict()  # suppress(invalid-name)
_valid_words_cache = dict()  # suppress(invalid-name)
_user_dictionary_cache = dict()  # suppress(invalid-name)


def clear_caches():  # suppress(unused-function)
    """Clear all caches."""
    for _, reader in _spellchecker_cache.values():
        reader.close()

    _spellchecker_cache.clear()
    _valid_words_cache.clear()
    _user_dictionary_cache.clear()


FileCommentSystem = namedtuple("FileCommentSystem", "begin middle end")


def _comment_system_for_file(contents):
    """For file contents, return the comment system."""
    if contents[0] == "#":
        return FileCommentSystem(begin="#", middle="", end="")
    elif contents[:2] == "/*":
        return FileCommentSystem(begin="/*", middle="*", end="*/")
    elif contents[:2] == "//":
        return FileCommentSystem(begin="//", middle="//", end="")
    elif contents[:3] == "rem":
        return FileCommentSystem(begin="rem", middle="rem", end="")
    else:
        raise RuntimeError("Couldn't detect comment "
                           "system from {0}".format(contents[:3]))


@functools.total_ordering
class _Marker(namedtuple("Marker", "line col")):
    """A marker for a point in a text file. Comparable."""

    def __lt__(self, other):
        """True if self is less than other."""
        if self.line == other.line:
            return self.col < other.col

        return self.line < other.line


@functools.total_ordering
class _ChunkInfo(namedtuple("_ChunkInfo", "line column data type")):
    """A chunk of selected text, starting at line/column."""

    # The chunk type.
    #
    # A Shadow chunk exists only to filter out other chunks by virtue
    # of being "first". It shouldn't be considered a region of selected
    # text.
    #
    # A real chunk is a region of selected text.
    Real = 0
    Shadow = 1

    def start(self):
        """Start point of this chunk, as a Marker."""
        return _Marker(line=self.line, col=self.column)

    def end(self):
        """End point of this chunk, as a Marker."""
        if len(self.data) == 1:
            end_col = self.column + len(self.data[0])
        else:
            end_col = len(self.data[-1])

        return _Marker(line=self.line + len(self.data) - 1, col=end_col)

    def __lt__(self, other):
        """True if self is less than other."""
        if self.line == other.line:
            if self.column == other.column:
                # Shadow chunks always end up after real chunks. This
                # will cause them to be excluded under the
                # first-chunk-wins rule.
                return self.type < other.type

            return self.column < other.column

        return self.line < other.line


def _get_in_out_points(lines, chunk_begin_regex, chunk_end_regex):
    """Get a set of line/column markers for each in and out point.

    The in points match :chunk_begin_regex: and the out points match
    :chunk_end_regex:. Note that this function might return duplicated points
    or in and out points which are otherwise invalid. They should be
    filtered such that there are no duplicated points and no repeated
    in points before out points.
    """
    all_in_points = []
    all_out_points = []

    for index, line in enumerate(lines):
        line_out_points = []
        line_in_points = [m.end() for m in re.finditer(chunk_begin_regex,
                                                       line)]

        if not chunk_end_regex:
            if len(line_in_points):
                line_out_points = [len(line)]
        else:
            line_out_points = [m.start() for m in re.finditer(chunk_end_regex,
                                                              line)]

        all_in_points.extend([_Marker(index, c) for c in line_in_points])
        all_out_points.extend([_Marker(index, c) for c in line_out_points])

    return all_in_points, all_out_points


def _find_chunks(lines, chunk_begin_regex, chunk_end_regex, chunk_type):
    """Find chunks delimited by chunk_begin_regex and chunk_end_regex.

    The found chunks are returned as part of a _ChunkInfo struct which
    contains details about the line and character offsets where the chunk
    starts and finishes.

    Note that this function may return overlapping chunks. To avoid that,
    filter the chunks through _filter_overlapping_chunks.

    The "data" member is a list of strings, each going line-by-line.

    The chunk will be assigned a type of :chunk_type:
    """
    all_in_points, all_out_points = _get_in_out_points(lines,
                                                       chunk_begin_regex,
                                                       chunk_end_regex)

    # Filter out alternating in and out points if they are denoted
    # by the same marker.
    if chunk_end_regex == chunk_begin_regex:
        assert len(all_out_points) == len(all_in_points)
        length = len(all_out_points)
        all_out_points = [all_out_points[i] for i in range(1, length, 2)]
        all_in_points = [all_in_points[i] for i in range(0, length, 2)]

    # Now use all the in and out points to generate _ChunkInfo structs
    while len(all_out_points):
        # Move to next in/out points
        in_point = all_in_points.pop(0)
        out_point = all_out_points.pop(0)

        # Move in and out points along such that
        # !((in_point | out_point) & shadow_contents) and
        # in_point > out_point
        while len(all_in_points) and all_in_points[0] <= out_point:
            all_in_points.pop(0)

        while len(all_out_points) and out_point < in_point:
            out_point = all_out_points.pop(0)

        # Nothing here, move on
        if in_point == out_point:
            continue

        assert out_point > in_point

        chunk_data = []
        for line_index in range(in_point.line, out_point.line + 1):
            line = len(lines[line_index])
            in_col = in_point.col if line_index == in_point.line else 0
            out_col = out_point.col if line_index == out_point.line else line

            chunk_data.append(lines[line_index][in_col:out_col])

        yield _ChunkInfo(in_point.line,
                         in_point.col,
                         chunk_data,
                         chunk_type)


def _filter_overlapping_chunks(chunks):
    """Return a list of non-overlapping chunks on a first chunk wins basis.

    A chunk is considered to be overlapping if it starts before a chunk
    prior to it ends. We keep the former chunk.
    """
    chunks = sorted(chunks)
    if len(chunks) < 2:
        return chunks

    # Always take the first chunks
    filtered_chunks = [chunks[0]]

    compare_index = 0
    take_index = 1
    while take_index < len(chunks):
        # Advance take_index if the chunk it refers to starts
        # before the compare_index chunk end point. If this is
        # false, then the chunks do not overlap. As such, assign
        # compare_index to take_index and continue.
        #
        # This is highly sensitive to the sorting order of the chunks.
        # Because docstrings also count as a chunk and will count
        # in the same place as a regular "shadow" string, the shadow
        # string must appear after the docstring does. This will
        # cause the shadow string to be eliminated.
        if chunks[take_index].start() > chunks[compare_index].end():
            filtered_chunks.append(chunks[take_index])
            compare_index = take_index

        take_index += 1

    return filtered_chunks


def _split_line_with_offsets(line):
    """Split a line by delimiter, but yield tuples of word and offset.

    This function works by dropping all the english-like punctuation from
    a line (so parenthesis preceded or succeeded by spaces, periods, etc)
    and then splitting on spaces.
    """
    for delimiter in re.finditer(r"[\.,:\;](?![^\s])", line):
        span = delimiter.span()
        line = line[:span[0]] + " " + line[span[1]:]

    for delimiter in re.finditer(r"[\"'\)\]\}>](?![^\.,\;:\"'\)\]\}>\s])",
                                 line):
        span = delimiter.span()
        line = line[:span[0]] + " " + line[span[1]:]

    for delimiter in re.finditer(r"(?<![^\.,\;:\"'\(\[\{<\s])[\"'\(\[\{<]",
                                 line):
        span = delimiter.span()
        line = line[:span[0]] + " " + line[span[1]:]

    # Treat hyphen separated words as separate words
    line = line.replace("-", " ")

    # Remove backticks
    line = line.replace("`", " ")

    for match in re.finditer(r"[^\s]+", line):
        content = match.group(0)
        if content.strip() != "":
            yield (match.span()[0], content)


def read_dictionary_file(dictionary_path):
    """Return all words in dictionary file as set."""
    try:
        return _user_dictionary_cache[dictionary_path]
    except KeyError:
        if dictionary_path and os.path.exists(dictionary_path):
            with open(dictionary_path, "rt") as dict_f:
                words = set(re.findall(r"(\w[\w']*\w|\w)",
                                       " ".join(dict_f.read().splitlines())))
                return words

        return set()


def valid_words_set(path_to_user_dictionary=None,
                    user_dictionary_words=None):
    """Get a set of valid words.

    If :path_to_user_dictionary: is specified, then the newline-separated
    words in that file will be added to the word set.
    """
    def read_file(binary_file):
        """Read a binary file for its text lines."""
        return binary_file.read().decode("ascii").splitlines()

    try:
        valid = _valid_words_cache[path_to_user_dictionary]
        return valid
    except KeyError:
        words = set()
        with resource_stream("polysquarelinter", "en_US.txt") as words_file:
            words |= set(["".join(l).lower() for l in read_file(words_file)])

        if path_to_user_dictionary:
            # Add both case-sensitive and case-insensitive variants
            # of words in user dictionary as they may be checked as
            # though they are a regular word and a technical word.
            words |= set([w.lower() for w in user_dictionary_words])
            words |= user_dictionary_words

        _valid_words_cache[path_to_user_dictionary] = words
        return words


# Store the corrector and reader in the cache
SpellcheckerCacheEntry = namedtuple("SpellcheckerCacheEntry",
                                    "corrector reader")


def _create_word_graph_file(name, file_storage, word_set):
    """Create a word graph file and open it in memory."""
    word_graph_file = file_storage.create_file(name)
    spelling.wordlist_to_graph_file(sorted(list(word_set)),
                                    word_graph_file)
    return copy_to_ram(file_storage).open_file(name)


def _spellchecker_for(word_set,
                      name,
                      spellcheck_cache_path=None,
                      sources=None):
    """Get a whoosh spellchecker for :word_set:.

    The word graph for this spellchecker will be stored on-disk with
    the unique-name :name: in :spellcheck_cache_path:, if it exists.
    This allows for much faster loading of word graphs after they have been
    pre-populated.

    :sources: is a list of filenames which will be checked to see if they
    are newer than the stored word graph. If they are newer, then the word
    graph gets repopulated.
    """
    if _spellchecker_cache.get(name, None) is not None:
        return _spellchecker_cache[name].corrector

    # Check the modification time of all the paths in :sources: to see
    # if they've been modified since the cache file was created. If so,
    # delete the cache file. This will cause it to be regenerated.
    #
    # Note that this relies on an implementation detail in whoosh, namely
    # that the cache file is always stored at spellcheck_cache_path/name.
    if spellcheck_cache_path:

        # Ensure that the directory has been created
        try:
            os.makedirs(spellcheck_cache_path)
        except OSError as error:
            if error.errno != errno.EEXIST:  # suppress(PYC90)
                raise error

        graph_path = os.path.realpath(spellcheck_cache_path)
        file_storage = FileStorage(graph_path)

        preexisting_cache = os.path.abspath(os.path.join(spellcheck_cache_path,
                                                         name))
        if os.path.exists(preexisting_cache):
            cache_mtime = os.path.getmtime(preexisting_cache)
            for source in sources:
                source_path = os.path.realpath(source)
                if not os.path.exists(source_path):
                    continue

                if os.path.getmtime(source_path) > cache_mtime:
                    file_storage.delete_file(name)
                    break

        try:
            word_graph = copy_to_ram(file_storage).open_file(name)
        except (IOError, NameError):
            word_graph = _create_word_graph_file(name, file_storage, word_set)

    else:
        ram_storage = RamStorage()
        word_graph = _create_word_graph_file(name, ram_storage, word_set)

    reader = fst.GraphReader(word_graph)
    corrector = spelling.GraphCorrector(reader)
    _spellchecker_cache[name] = SpellcheckerCacheEntry(corrector, reader)
    return corrector


def filter_nonspellcheckable_tokens(line, block_out_regexes=None):
    """Return line with paths, urls and emails filtered out.

    Block out other strings of text matching :block_out_regexes: if passed in.
    """
    all_block_out_regexes = [
        r"[^\s]*:[^\s]*[/\\][^\s]*",
        r"[^\s]*[/\\][^\s]*",
        r"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]+\b"
    ] + (block_out_regexes or list())

    for block_regex in all_block_out_regexes:
        for marker in re.finditer(block_regex, line):
            spaces = " " * (marker.end() - marker.start())
            line = line[:marker.start()] + spaces + line[marker.end():]

    return line


def _shadow_contents_from_chunks(contents, chunks, block_out_regexes=None):
    """Remove all contents in spellcheckable :chunks: from contents."""
    shadow_contents = [list(l) for l in contents]
    for chunk in chunks:
        char_offset = chunk.column
        line_offset = 0

        for index, line in enumerate(chunk.data):
            # Block out entire chunk range from shadow_contents
            for character_in_line in range(0, len(line)):
                shadow_line = chunk.line + line_offset
                shadow_char = char_offset + character_in_line
                shadow_contents[shadow_line][shadow_char] = 0

            # Also block out certain regexps from this chunk
            line = filter_nonspellcheckable_tokens(line,
                                                   block_out_regexes)
            chunk.data[index] = line

            line_offset += 1
            char_offset = 0

    return shadow_contents


# There doesn't seem to be a shorter name that can be given to this function
# which also retains its descriptive value. The function return both
# the spellcheckable contents and the remaining contents which are not
# spellcheckable.
#
# suppress(invalid-name)
def spellcheckable_and_shadow_contents(contents, block_out_regexes=None):
    """For contents, split into spellcheckable and shadow parts.

    :contents: is a list of lines in a file.

    The return value is a tuple of (chunks, shadow_contents).
    chunks is a list of _ChunkInfo, each of which contain
    a region of text to be spell-checked. shadow_contents is an array of
    characters and integers. The characters represent nonspellcheckable
    regions and any region which will be subject to spellcheck is denoted
    by a zero in place of that character.
    """
    if not len(contents):
        return ([], [])

    comment_system = _comment_system_for_file(contents[0])
    begin_comment = comment_system.begin.replace("*", r"\*")

    if comment_system.end == "":
        end_comment = None
    else:
        end_comment = comment_system.end.replace("*", r"\*")

    iterables = [
        _find_chunks(contents,
                     r"(?<![^\\]\\)\"",
                     r"(?<![^\\]\\)\"",
                     _ChunkInfo.Shadow),
        _find_chunks(contents,
                     r"(?<![^\\]\\)'",
                     r"(?<![^\\]\\)'",
                     _ChunkInfo.Shadow),
        _find_chunks(contents, r"\"\"\"", r"\"\"\"", _ChunkInfo.Real),
        _find_chunks(contents, r"'''", r"'''", _ChunkInfo.Real),
        _find_chunks(contents, begin_comment, end_comment, _ChunkInfo.Real)
    ]

    # Shadow contents excludes anything in quotes
    chunks = _filter_overlapping_chunks(list(itertools.chain(*iterables)))
    shadow_contents = _shadow_contents_from_chunks(contents,
                                                   chunks,
                                                   block_out_regexes)

    # Only return chunks which are actually spellcheckable and not just
    # quotes which are intended to block out parts of the shadow contents
    chunks = [c for c in chunks if c.type == _ChunkInfo.Real]
    return (chunks, shadow_contents)


def _split_into_symbol_words(sym):
    """Split a technical looking word into a set of symbols.

    This handles cases where technical words are separated by dots or
    arrows, as is the convention in many programming languages.
    """
    punc = r"[\s\-\*/\+\.,:\;=\)\(\[\]\{\}<>\|\?&\^\$@]"
    words = [w.strip() for w in re.split(punc, sym)]
    return words


# There is no shorter function name which will cause this function
# name to lose its descriptive value.
#
# suppress(invalid-name)
def technical_words_from_shadow_contents(shadow_contents):
    """Get a set of technical words from :shadow_contents:.

    :shadow_contents: is an array of shadow contents, as returned by
    spellcheckable_and_shadow_contents.
    """
    technical_words = set()
    for line in shadow_contents:
        # "Fix up" the shadow line by replacing zeros with spaces.
        line = "".join([(lambda c: " " if c == 0 else c)(c) for c in line])
        for sym in _split_into_symbol_words(line):
            if len(sym) and re.compile(_VALID_SYMBOL_WORDS).match(sym):
                technical_words |= set([sym])

    return technical_words


@functools.total_ordering
class SpellcheckError(namedtuple("SpellcheckError",
                                 "word "
                                 "line_offset "
                                 "column_offset "
                                 "suggestions "
                                 "error_type")):
    """A spelling error, relative to a certain region of lines."""

    InvalidWord = 0
    TechnicalWord = 1

    def __lt__(self, other):
        """True if self is less than other."""
        if self.line_offset == other.line_offset:
            return self.column_offset < other.column_offset

        return self.line_offset < other.line_offset


def _error_if_word_invalid(word,
                           valid_words_dictionary,
                           technical_words_dictionary,
                           line_offset,
                           col_offset):
    """Return SpellcheckError if this non-technical word is invalid."""
    word_lower = word.lower()
    valid_words_result = valid_words_dictionary.corrections(word_lower)

    if technical_words_dictionary:
        technical_words_result = technical_words_dictionary.corrections(word)
    else:
        # No technical words available to make an otherwise invalid
        # result value.
        technical_words_result = Dictionary.Result(False, list())

    if not valid_words_result.valid and not technical_words_result.valid:
        return SpellcheckError(word,
                               line_offset,
                               col_offset,
                               valid_words_result.suggestions,
                               SpellcheckError.InvalidWord)


def _error_if_symbol_unused(symbol_word,
                            technical_words_dictionary,
                            line_offset,
                            col_offset):
    """Return SpellcheckError if this symbol is not used in the code."""
    result = technical_words_dictionary.corrections(symbol_word,
                                                    distance=5,
                                                    prefix=0)
    if not result.valid:
        return SpellcheckError(symbol_word,
                               line_offset,
                               col_offset,
                               result.suggestions,
                               SpellcheckError.TechnicalWord)


class Dictionary(object):
    """A dictionary which can find corrections a word set.

    Customize the words available in the dictionary by passing a set of
    words as :words: .

    :dictionary_sources: is list of paths to text files where :words: set
    was sourced from. If :cache: is set, then the dictionary will be loaded
    from an on-disk binary cache loaded inside the path specified by
    :cache: , but will be regenerated if the cache file is older than
    than any of the paths in :dictionary_sources:

    """

    def __init__(self, words, name, dictionary_sources=None, cache=None):
        """Initialize this Dictionary.

        This sets the dictionary's words and creates a corrector
        object, from a cache if possible.
        """
        super(Dictionary, self).__init__()
        self._words = words
        self._corrector = _spellchecker_for(words,
                                            name,
                                            cache,
                                            sources=dictionary_sources)

    Result = namedtuple("Result", "valid suggestions")

    def corrections(self, word, prefix=1, distance=2):
        """Get corrections for word, if word is an invalid word.

        :prefix: is the number of characters the prefix of the word must
        have in common with the suggested corrections.

        :distance: is the character distance the corrections may have between
        the input word. This limits the number of available corrections
        but decreases the correction search space.

        The return value of this function is a Result tuple, with the
        :valid: member indicating whether the input word is a valid one and
        :suggestions: member containing a list of suggestions.
        """
        if word not in self._words:
            return Dictionary.Result(False,
                                     self._corrector.suggest(word,
                                                             prefix=prefix,
                                                             maxdist=distance))
        else:
            return Dictionary.Result(True, list())

    def words(self):
        """Retrieve set of words in this dictionary."""
        return self._words


def spellcheck_region(region_lines,
                      valid_words_dictionary=None,
                      technical_words_dictionary=None,
                      user_dictionary_words=None):
    """Perform spellcheck on each word in :region_lines:.

    Each word will be checked for existence in :valid_words_dictionary:.
    If it is not in :valid_words_dictionary: then corrections will be
    suggested.

    If the word isn't one which is an ordinary word, then it will be checked
    against the available symbols in :technical_words_dictionary: . If it is
    not in :technical_words_dictionary: then corrections will be suggested.
    """
    user_dictionary_words = user_dictionary_words or set()
    spellcheckable_words_regex = re.compile(_SPELLCHECKABLE_WORDS)

    line_offset = 0
    for line in region_lines:
        for col_offset, word in _split_line_with_offsets(line):
            word = word.strip()
            if len(word) == 0:
                continue

            # If this word exists in the user dictionary, then always allow
            # it, even if it might be technical in nature
            if word in user_dictionary_words:
                continue

            if (valid_words_dictionary and
                    spellcheckable_words_regex.match(word)):
                error = _error_if_word_invalid(word,
                                               valid_words_dictionary,
                                               technical_words_dictionary,
                                               line_offset,
                                               col_offset)

                if error:
                    yield error

            # Check for symbols appearing in comments or
            # docstrings.
            elif technical_words_dictionary:
                for symbol_word in _split_into_symbol_words(word):
                    if not re.compile(_VALID_SYMBOL_WORDS).match(symbol_word):
                        continue

                    error = _error_if_symbol_unused(symbol_word,
                                                    technical_words_dictionary,
                                                    line_offset,
                                                    col_offset)

                    if error:
                        yield error

        line_offset += 1
