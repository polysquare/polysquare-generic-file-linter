# /test/test_spelling.py
#
# Test cases for spelling-specific code in
#
# Disable no-self-use in tests as all test methods must be
# instance methods and we don't necessarily have to use a matcher
# with them.
# pylint:disable=no-self-use
#
# See /LICENCE.md for Copyright information
"""Test the linter to ensure that each lint use-case triggers warnings."""

import inspect

import os

import shutil

import tempfile

import time

from nose_parameterized import parameterized

from polysquarelinter import spelling
from polysquarelinter.spelling import SpellcheckError

from testtools import ExpectedException, TestCase
from testtools.matchers import (Contains,
                                Equals as TTEqMatcher,
                                MatchesListwise,
                                MatchesSetwise,
                                Not)


# Pychecker complains about the Equals matcher failing to override comparator
# so do that here
class Equals(TTEqMatcher):  # suppress(R0903)
    """Matcher which tests equality."""

    def __init__(self, matchee):
        """Forward matchee to parent class."""
        super(Equals, self).__init__(matchee)

    def comparator(self, expected, other):
        """Check that expected == other."""
        return other == expected


class WordCacheTestCase(TestCase):
    """A test case base class which keeps a word cache from start to finish."""

    @classmethod
    def setUpClass(cls):  # suppress(N802)
        """Create a temporary directory to store word graph caches."""
        # This is the name of the directory that we want to
        # place our files in.
        word_cache_dir = os.path.join(os.getcwd(), "wordcache")
        cls.cache_dir = tempfile.mkdtemp(prefix=word_cache_dir)

    @classmethod
    def tearDownClass(cls):
        """Remove cache directories."""
        shutil.rmtree(cls.cache_dir)


class TestDictionary(WordCacheTestCase):
    """Test case for spelling.Dictionary object."""

    def test_expected_english_words_corrections(self):
        """Expected corrections returned for misspelled english word."""
        dictionary = spelling.Dictionary(spelling.valid_words_set(),
                                         "english_words",
                                         self.__class__.cache_dir)
        result = dictionary.corrections("splelling")
        self.assertFalse(result.valid)
        self.assertThat(result.suggestions,
                        MatchesSetwise(Equals("spelling"),
                                       Equals("spellings"),
                                       Equals("spieling"),
                                       Equals("spilling"),
                                       Equals("swelling")))

    def test_true_on_valid_word(self):
        """True and empty list returned for valid english word."""
        dictionary = spelling.Dictionary(spelling.valid_words_set(),
                                         "english_words",
                                         self.__class__.cache_dir)
        result = dictionary.corrections("spelling")
        self.assertTrue(result.valid)
        self.assertEqual(result.suggestions, list())


class TestDictionaryWithCustomWords(WordCacheTestCase):
    """Test case for spelling.Dictionary object."""

    def __init__(self, *args, **kwargs):
        """Initialize instance variables for this test case."""
        super(TestDictionaryWithCustomWords, self).__init__(*args, **kwargs)
        self.user_dictionary_path = None

    def setUp(self):  # suppress(N802)
        """Create a dictionary with custom words in it."""
        super(TestDictionaryWithCustomWords, self).setUp()
        self._current_path = os.getcwd()
        dictionary_dir = os.path.join(self._current_path, "user_dictionary")
        self._user_dictionary_dir = tempfile.mkdtemp(prefix=dictionary_dir)
        self.addCleanup(shutil.rmtree, self._user_dictionary_dir)
        self.user_dictionary_path = os.path.join(self._user_dictionary_dir,
                                                 "DICTIONARY")
        self.addCleanup(os.chdir, self._current_path)

    def _user_dictionary_set(self):
        """Get words in user dictionary as set."""
        return spelling.read_dictionary_file(self.user_dictionary_path)

    def test_create_dictionary_with_slashes(self):
        """Dictionary name can contain slashes."""
        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        spelling.Dictionary(valid_words, "has/slashes\\here/")

    def test_spellcheck_without_cache(self):
        """Run spellcheck without cache.

        This is a simple check to ensure that a dictionary can still
        be constructed in-memory.
        """
        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        dictionary = spelling.Dictionary(valid_words,
                                         "english_and_custom_words")
        dictionary.corrections("mzcustomword")

    def test_custom_words_included_in_corrections(self):
        """Custom words included in corrections."""
        with open(self.user_dictionary_path, "w") as user_dictionary:
            user_dictionary.write("mycustomword")

        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        dictionary = spelling.Dictionary(valid_words,
                                         "english_and_custom_words",
                                         cache=self.__class__.cache_dir)
        result = dictionary.corrections("mzcustomword")
        self.assertFalse(result.valid)
        self.assertThat(result.suggestions,
                        MatchesSetwise(Equals("mycustomword")))

    def test_match_with_custom_word(self):
        """Match true on hitting custom word."""
        with open(self.user_dictionary_path, "w") as user_dictionary:
            user_dictionary.write("mycustomword\n")

        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        dictionary = spelling.Dictionary(valid_words,
                                         "english_and_custom_words",
                                         [self.user_dictionary_path],
                                         self.__class__.cache_dir)
        result = dictionary.corrections("mycustomword")
        self.assertTrue(result.valid)
        self.assertEqual(result.suggestions, list())

    def test_invalidate_custom_word_cache(self):
        """Regenerate internal user dictionary on file change."""
        with open(self.user_dictionary_path, "w") as user_dictionary:
            user_dictionary.write("mycustomword\n")

        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        dictionary = spelling.Dictionary(valid_words,
                                         "english_and_custom_words",
                                         [self.user_dictionary_path],
                                         self.__class__.cache_dir)
        dictionary.corrections("mycustomword")

        # Reassign to valid_words and dictionary, such that the caches
        # are re-generated on disk
        time.sleep(1)
        spelling.clear_caches()
        with open(self.user_dictionary_path, "w") as user_dictionary:
            user_dictionary.write("myothercustomword\n")

        valid_words = spelling.valid_words_set(self.user_dictionary_path,
                                               self._user_dictionary_set())
        dictionary = spelling.Dictionary(valid_words,
                                         "english_and_custom_words",
                                         [self.user_dictionary_path],
                                         self.__class__.cache_dir)
        result = dictionary.corrections("myothercustomword")
        self.assertTrue(result.valid)
        self.assertEqual(result.suggestions, list())


class TestSplitSpellcheckableFromShadowContents(TestCase):
    """Test case for the spellcheckable_and_shadow_contents function."""

    @staticmethod
    def shadow_contents_to_string(shadow):
        """Convert shadow contents to a single string."""
        # suppress(PYC70,PYC90)
        return "".join(["".join([c for c in l if c != 0]) for l in shadow])

    def test_spellcheckable_chunks_not_in_shadow_contents(self):
        """Spellcheckable chunk is not found in shadow contents."""
        contents = "# spellcheckable\n shadow".splitlines()
        chunks, shadow = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertThat(self.__class__.shadow_contents_to_string(shadow),
                        Not(Contains(chunks[0].data[0])))

    def test_repeated_comment_markers(self):
        """Handle repeated comment markers."""
        contents = "# spell # checkable\n shadow".splitlines()
        chunks, _ = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertEqual(chunks[0].data[0], " spell # checkable")

    def test_comment_out_markers_occurring_before_in_points(self):
        """Handle comment out points occurring before in points."""
        contents = "/* */\n*/ spell /* checkable */\n shadow".splitlines()
        chunks, _ = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertEqual(chunks[1].data[0], " checkable ")

    def test_partial_quotes_in_comment_out_markers(self):
        """Handle comment out points occurring before in points."""
        contents = "/* ' */\n*/ spell /* checkable */\n shadow".splitlines()
        chunks, _ = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertEqual(chunks[1].data[0], " checkable ")

    def test_single_quoted_regions_found_in_shadow_contents(self):
        """Single quoted chunk is found in shadow contents."""
        contents = "# spellcheckable\n 'quoted' shadow".splitlines()
        _, shadow = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertThat(self.__class__.shadow_contents_to_string(shadow),
                        Contains("quoted"))

    def test_double_quoted_regions_found_in_shadow_contents(self):
        """Double quoted chunk is found in shadow contents."""
        contents = "# spellcheckable\n \"quoted\" shadow".splitlines()
        _, shadow = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertThat(self.__class__.shadow_contents_to_string(shadow),
                        Contains("quoted"))

    @parameterized.expand(list(" .=[](){}<>"))
    def test_split_technical_words_from_shadow_contents(self, character):
        """Split technical words from shadow contents."""
        contents = "#\n first_sym{0}second_sym".format(character).splitlines()
        _, shadow = spelling.spellcheckable_and_shadow_contents(contents)

        self.assertThat(spelling.technical_words_from_shadow_contents(shadow),
                        MatchesSetwise(Equals("first_sym"),
                                       Equals("second_sym")))

    def test_block_out_regions_of_spellcheckable_chunks(self):
        """Block out regions of spellcheckable chunks."""
        contents = "# blocked(region) other\n other_contents".splitlines()
        block_regexes = [
            r"\bblocked\([^\s]*\)"
        ]
        chunks, _ = spelling.spellcheckable_and_shadow_contents(contents,
                                                                block_regexes)

        self.assertThat(chunks[0].data[0],
                        Not(Contains("blocked(region)")))


class TestSpellcheckOnRegion(WordCacheTestCase):
    """Test cases for spellcheck_region function."""

    @classmethod
    def get_dictionaries(cls, contents):
        """Return a tuple of two dictionaries.

        The first dictionary is a Dictionary of valid words. The second
        dictionary is a Dictionary of technical words.
        """
        unique_name = inspect.currentframe().f_back.f_code.co_name
        _, shadow = spelling.spellcheckable_and_shadow_contents(contents)
        valid_words = spelling.Dictionary(spelling.valid_words_set(),
                                          "english_words",
                                          cls.cache_dir)
        tch_set = spelling.technical_words_from_shadow_contents(shadow)
        technical_words = spelling.Dictionary(tch_set,
                                              "technical_words_" + unique_name,
                                              cls.cache_dir)

        return (valid_words, technical_words)

    def test_exception_on_failure_to_detect_comment_system(self):
        """Runtime error raised when comment system can't be detected."""
        contents = "no comment system"
        with ExpectedException(RuntimeError):
            valid, technical = self.__class__.get_dictionaries(contents)
            spelling.spellcheck_region(contents, valid, technical)

    def test_allow_matching_technical_words(self):
        """Spellcheck passes if a technical word is matched."""
        contents = "# technical_word\n technical_word".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors, list())

    def test_error_on_technical_word_not_found(self):
        """Spellcheck error if a technical word used isn't found."""
        contents = "# technical_word\n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.TechnicalWord)

    def test_error_on_ordinary_mispelling(self):
        """Spellcheck error if ordinary word is misspelled."""
        contents = "# splelling\n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.InvalidWord)

    def test_no_error_on_ordinary_word_valid(self):
        """No spellcheck error if ordinary word is valid."""
        contents = "# spelling\n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(0, len(errors))

    def test_ignore_words_with_non_code_symbols(self):
        """Ignore words with non-code symbols in them."""
        contents = "# ~spelling\n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(0, len(errors))

    def test_ignore_standalone_code_symbols(self):
        """Ignore standalone code-like symbols."""
        contents = "# : \n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(0, len(errors))

    def test_always_allow_user_specified_word(self):
        """Allow user specified words even if they appear technical."""
        contents = "# a_technical\n".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical,
                                                        set(["a_technical"]))]
        self.assertEqual(0, len(errors))

    def test_error_on_ordinary_mispelling_no_technical_dictionary(self):
        """Error if ordinary word is misspelled, no technical dictionary."""
        contents = "# splelling\n".splitlines()
        valid, _ = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        None)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.InvalidWord)

    @parameterized.expand(["\"", "'", "(", "<", "{", "["])
    def test_handle_words_after_punctuation(self, punctuation):
        """Handle misspelled word after valid punctuation."""
        contents = "# ({0}missplled\n".format(punctuation).splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.InvalidWord)

    @parameterized.expand(["\"", "'", ")", ">", "}", "]"])
    def test_handle_words_before_punctuation(self, punctuation):
        """Handle misspelled word before valid punctuation."""
        contents = "# missplled{0})\n".format(punctuation).splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.InvalidWord)

    @parameterized.expand([":", ",", ":", ";"])
    def test_handle_words_before_punctuation_before_space(self, punctuation):
        """Handle misspelled word before valid punctuation (before space)."""
        contents = "# missplled{0} \n".format(punctuation).splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertEqual(errors[0].error_type,
                         spelling.SpellcheckError.InvalidWord)

    def test_no_suggesting_technical_words(self):
        """Do not suggest technical words on ordinary word misspelling."""
        contents = "# splelling\n spl_lling".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertThat(errors[0].suggestions,
                        Not(Contains("spl_lling")))

    def test_spelling_errors_inside_triple_quotes(self):
        """Catch spelling errors inside of triple quotes."""
        contents = "#\n\"\"\"splelling\"\"\"".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertThat(errors[0].suggestions,
                        Contains("spelling"))

    def test_no_spelling_errors_words_after_triple_quotes(self):
        """No spelling errors for words after of triple quotes."""
        contents = "#\n\"\"\"\"\"\".format".splitlines()
        valid, technical = self.__class__.get_dictionaries(contents)

        errors = [e for e in spelling.spellcheck_region(contents,
                                                        valid,
                                                        technical)]
        self.assertThat(len(errors), Equals(0))


class TestFilterNonspellcheckableTokens(TestCase):
    """Test cases for filtering out non-spellcheckable tokens."""

    @parameterized.expand(["https://domain.com/u/r/l.ext",
                           "path/to/file",
                           "/path/to/file",
                           "sample.email@address.com"])
    def test_filter_token(self, token):
        """Block out identifiers from text."""
        text = "valid {token} valid".format(token=token)
        blocked = spelling.filter_nonspellcheckable_tokens(text)
        self.assertEqual(blocked,
                         "valid {0} valid".format(" " * len(token)))


class TestSpellcheckErrors(TestCase):
    """Test spellcheck errors."""

    def test_sort_by_line_number(self):
        """Sort SpellcheckError by line number."""
        errors = [
            SpellcheckError("w", 2, 2, list(), SpellcheckError.InvalidWord),
            SpellcheckError("w", 1, 2, list(), SpellcheckError.TechnicalWord)
        ]

        self.assertThat(sorted(errors),
                        MatchesListwise([Equals(e) for e in [errors[1],
                                                             errors[0]]]))

    def test_sort_by_col_number(self):
        """Sort SpellcheckError by column number."""
        errors = [
            SpellcheckError("w", 1, 2, list(), SpellcheckError.InvalidWord),
            SpellcheckError("w", 1, 1, list(), SpellcheckError.TechnicalWord)
        ]

        self.assertThat(sorted(errors),
                        MatchesListwise([Equals(e) for e in [errors[1],
                                                             errors[0]]]))
