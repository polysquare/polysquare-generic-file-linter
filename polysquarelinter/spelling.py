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

import abc

import errno

import functools

import os

import re

from collections import namedtuple

from pkg_resources import resource_stream

import six

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


FileCommentSystem = namedtuple("FileCommentSystem", "begin middle end single")


def _comment_system_for_file(contents):
    """For file contents, return the comment system."""
    if contents[0] == "#":
        return FileCommentSystem(begin="#", middle="", end="", single="#")
    elif contents[:2] == "/*":
        return FileCommentSystem(begin="/*", middle="*", end="*/", single="//")
    elif contents[:2] == "//":
        return FileCommentSystem(begin="//", middle="//", end="", single="//")
    elif contents[:3] == "rem":
        return FileCommentSystem(begin="rem",
                                 middle="rem",
                                 end="",
                                 single="rem")
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
class _ChunkInfo(namedtuple("_ChunkInfo", "line column data")):
    """A chunk of selected text, starting at line/column."""

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
            return self.column < other.column

        return self.line < other.line


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
    assert "/" not in name and "\\" not in name

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


STATE_IN_TEXT = 0
STATE_IN_QUOTE = 1
STATE_IN_COMMENT = 2


def _chunk_from_ranges(contents_lines,
                       start_line_index,
                       start_column_index,
                       end_line_index,
                       end_column_index):
    """Create a _ChunkInfo from a range of lines and columns.

    :contents_lines: is the raw lines of a file.
    """
    # If the start and end line are the same we have to compensate for
    # that by subtracting start_column_index from end_column_index
    if start_line_index == end_line_index:
        end_column_index -= start_column_index

    lines = contents_lines[start_line_index:end_line_index + 1]
    lines[0] = lines[0][start_column_index:]
    lines[-1] = lines[-1][:end_column_index]

    return _ChunkInfo(start_line_index,
                      start_column_index,
                      lines)


def _token_at_col_in_line(line, column, token, token_len=None):
    """True if token is at column."""
    if not token_len:
        token_len = len(token)

    remaining_len = len(line) - column

    return (remaining_len >= token_len and
            line[column:column + token_len] == token)


def _is_escaped(line, column, is_escaped):
    """True if token is escaped."""
    return line[column] in ["\"", "'"] and is_escaped


def _transition_from_text_func(comment_system):
    """Return function that causes state transition for comment_system."""
    start_len = len(comment_system.begin)
    single_len = len(comment_system.single)

    begin = comment_system.begin
    single = comment_system.single

    def _transition_from_text(line, line_index, column, is_escaped):
        """Return the new state of the comment parser."""
        if _token_at_col_in_line(line, column, single, single_len):
            return (STATE_IN_COMMENT,
                    (line_index, column + single_len),
                    ParserState.EOL)
        elif _token_at_col_in_line(line, column, begin, start_len):
            return (STATE_IN_COMMENT,
                    (line_index, column + single_len),
                    comment_system.end)
        elif (_token_at_col_in_line(line, column, "\"") or
              _token_at_col_in_line(line, column, "'") and
              not _is_escaped(line, column, is_escaped)):
            # Check here to see whether this is a quote or if this
            # is a spellcheckable line
            if (_token_at_col_in_line(line, column, "\"\"\"") or
                    _token_at_col_in_line(line, column, "'''")):
                return (STATE_IN_COMMENT,
                        (line_index, column + 3),
                        line[column:column + 3])
            else:
                return (STATE_IN_QUOTE,
                        (line_index, column + 1),
                        line[column:column + 1])

        return (STATE_IN_TEXT,
                (0, 0),
                None)

    return _transition_from_text


# suppress(too-few-public-methods)
class ParserState(six.with_metaclass(abc.ABCMeta, object)):
    """An immutable object to represent the state of the comment parser.

    The comment parser moves from left to right over the entire file
    buffer. Its state might change depending on whether or not it hits
    a new line or a comment character.
    """

    EOL = 1

    def __init__(self, started_at, waiting_until):
        """Initialize this ParserState."""
        super(ParserState, self).__init__()
        self._started_at = started_at
        self._waiting_until = waiting_until

    @abc.abstractmethod
    def get_transition(self,  # suppress(too-many-arguments)
                       line,
                       line_index,
                       column,
                       is_escaped,
                       transition_from_text,
                       eof=False):
        """Return a parser state, a move-ahead amount, and an append range.

        If this parser state should terminate and return back to
        the TEXT state, then return that state and also any corresponding
        chunk that would have been yielded as a result.
        """
        raise NotImplementedError("""Cannot instantiate base ParserState""")


# suppress(too-few-public-methods)
class InTextParser(ParserState):
    """A parser that is in the state of parsing non-comment text."""

    def __init__(self):
        """Initialize this InTextParser.

        Only certain underlying state values make sense, so we
        just force them.
        """
        super(InTextParser, self).__init__((0, 0), None)

    def get_transition(self,  # suppress(too-many-arguments)
                       line,
                       line_index,
                       column,
                       is_escaped,
                       transition_from_text,
                       eof=False):
        """Get transition from InTextParser."""
        parser_transition = {
            STATE_IN_COMMENT: InCommentParser,
            STATE_IN_QUOTE: InQuoteParser
        }

        (state,
         start_state_from,
         waiting_until) = transition_from_text(line,
                                               line_index,
                                               column,
                                               is_escaped)

        # We need to move ahead by a certain number of characters
        # if we hit a new state
        if state != STATE_IN_TEXT:
            return (parser_transition[state](start_state_from,
                                             waiting_until),
                    start_state_from[1] - column,
                    None)
        else:
            return (self, 1, None)


# suppress(too-few-public-methods)
class InCommentParser(ParserState):
    """A parser that is in the state of parsing a comment."""

    def get_transition(self,  # suppress(too-many-arguments)
                       line,
                       line_index,
                       column,
                       is_escaped,
                       transition_from_text,
                       eof=False):
        """Get transition from InCommentParser."""
        del line_index
        del transition_from_text

        if self._waiting_until != ParserState.EOL:
            wait_until_len = len(self._waiting_until)
            if (_token_at_col_in_line(line,
                                      column,
                                      self._waiting_until,
                                      wait_until_len) and
                    not _is_escaped(line, column, is_escaped)):

                # Skip ahead to end of this token
                return (InTextParser(),
                        len(self._waiting_until),
                        self._started_at)
        elif self._waiting_until == ParserState.EOL and column == 0:
            # We hit a new line and the state ends here. Return
            # corresponding state
            return (InTextParser(), 0, self._started_at)
        elif eof:
            # We hit the end of the file and were still in a comment
            # state. Grab everything up to here.
            return (InTextParser(), 0, self._started_at)

        # Move ahead by one character otherwise
        return (self, 1, None)


# suppress(too-few-public-methods)
class InQuoteParser(ParserState):
    """A parser that is in the state of parsing a quote."""

    def get_transition(self,  # suppress(too-many-arguments)
                       line,
                       line_index,
                       column,
                       is_escaped,
                       *args,
                       **kwargs):
        """Get transition from InQuoteParser."""
        del line_index
        del args
        del kwargs

        wait_until_len = len(self._waiting_until)
        if (_token_at_col_in_line(line,
                                  column,
                                  self._waiting_until,
                                  wait_until_len) and
                not _is_escaped(line, column, is_escaped)):
            return (InTextParser(), 1, None)

        return (self, 1, None)


def _maybe_append_chunk(chunk_info, line_index, column, contents, chunks):
    """Append chunk_info to chunks if it is set."""
    if chunk_info:
        chunks.append(_chunk_from_ranges(contents,
                                         chunk_info[0],
                                         chunk_info[1],
                                         line_index,
                                         column))


def _find_spellcheckable_chunks(contents,
                                comment_system):
    """Given some contents for a file, find chunks that can be spellchecked.

    This applies the following rules:
     1. If the comment system comments individual lines, that whole line
        can be spellchecked from the point of the comment
     2. If a comment-start marker or triple quote is found, keep going
        until a comment end marker or matching triple quote is found.
    """
    state = InTextParser()
    transition_from_text = _transition_from_text_func(comment_system)

    chunks = []
    for line_index, line in enumerate(contents):
        column = 0
        line_len = len(line)
        escape_next = False

        # We hit a new line. If we were waiting until the end of the line
        # then add a new chunk in here
        (state,
         column_delta,
         chunk_info) = state.get_transition(line,
                                            line_index,
                                            0,
                                            False,
                                            transition_from_text)
        _maybe_append_chunk(chunk_info,
                            line_index - 1,
                            len(contents[line_index - 1]),
                            contents,
                            chunks)
        column += column_delta

        while column < line_len:
            # Check if the next character should be considered as escaped. That
            # only happens if we are not escaped and the current character is
            # a backslash.
            is_escaped = escape_next
            escape_next = not is_escaped and line[column] == "\\"

            (state,
             column_delta,
             chunk_info) = state.get_transition(line,
                                                line_index,
                                                column,
                                                is_escaped,
                                                transition_from_text)

            _maybe_append_chunk(chunk_info,
                                line_index,
                                column,
                                contents,
                                chunks)
            column += column_delta

    last_line_index = len(contents) - 1
    _maybe_append_chunk(state.get_transition(contents[-1],
                                             last_line_index,
                                             len(contents[-1]),
                                             False,
                                             transition_from_text,
                                             eof=True)[2],
                        last_line_index,
                        len(contents[last_line_index]),
                        contents,
                        chunks)

    return chunks


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

    # Shadow contents excludes anything in quotes
    chunks = _find_spellcheckable_chunks(contents, comment_system)
    shadow_contents = _shadow_contents_from_chunks(contents,
                                                   chunks,
                                                   block_out_regexes)

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
                                            re.sub(r"[\/\\]", "_", name),
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
