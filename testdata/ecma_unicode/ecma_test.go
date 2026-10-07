package ecmaharness

import (
	"testing"
	"time"
	"unicode/utf8"

	"github.com/dlclark/regexp2/v2"
)

func TestQuantifiedCaptures(t *testing.T) {
	// ECMAScript RepeatMatcher clears captures before each repetition and
	// rejects optional empty iterations. Check both full and quick executors.
	for _, tc := range []struct {
		name  string
		re    *regexp2.Regexp
		input string
		// nil means no match; each group lists its complete capture history.
		captures [][]string
	}{
		{"cleared backreference", regexp2.MustCompile(`^(a(b)?)+\2$`, regexp2.ECMAScript|regexp2.Unicode), "aba", [][]string{{"aba"}, {"a"}, nil}},
		{"stale backreference rejected", regexp2.MustCompile(`^(a(b)?)+\2$`, regexp2.ECMAScript|regexp2.Unicode), "abab", nil},
		{"optional capture", regexp2.MustCompile(`^(a(b)?)+$`, regexp2.ECMAScript), "aba", [][]string{{"aba"}, {"a"}, nil}},
		{"backtracking restores captures", regexp2.MustCompile(`^(a(b)?)+a\2$`, regexp2.ECMAScript), "abab", [][]string{{"abab"}, {"ab"}, {"b"}}},
		{"lookahead restores captures", regexp2.MustCompile(`^(?:(?=(a(b)?))\1)+a\2$`, regexp2.ECMAScript), "abab", [][]string{{"abab"}, {"ab"}, {"b"}}},
		{"atomic group restores captures", regexp2.MustCompile(`^(?:(?>(a(b)?))|c)+a\2$`, regexp2.ECMAScript), "abab", [][]string{{"abab"}, {"ab"}, {"b"}}},
		{"greedy empty iteration rejected", regexp2.MustCompile(`^(a?)*$`, regexp2.ECMAScript), "a", [][]string{{"a"}, {"a"}}},
		{"zero repetitions", regexp2.MustCompile(`^(a?)*$`, regexp2.ECMAScript), "", [][]string{{""}, nil}},
		{"lazy empty iteration rejected", regexp2.MustCompile(`^(a?)+?$`, regexp2.ECMAScript), "a", [][]string{{"a"}, {"a"}}},
		{"lazy empty iteration failure", regexp2.MustCompile(`^(a?)*?\1$`, regexp2.ECMAScript), "a", nil},
		{"greedy retries after required empty", regexp2.MustCompile(`^(a??)+\1$`, regexp2.ECMAScript), "aa", [][]string{{"aa"}, {"a"}}},
		{"lazy retries after required empty", regexp2.MustCompile(`^(a??)+?\1$`, regexp2.ECMAScript), "aa", [][]string{{"aa"}, {"a"}}},
		{"required empty repetition", regexp2.MustCompile(`^(a?)+?$`, regexp2.ECMAScript), "", [][]string{{""}, {""}}},
		{"bounded required empty repetition", regexp2.MustCompile(`^(a?){2,3}$`, regexp2.ECMAScript), "a", [][]string{{"a"}, {""}}},
		{"single named capture", regexp2.MustCompile(`^(?:(?<x>a)|b)+$`, regexp2.ECMAScript), "ab", [][]string{{"ab"}, nil}},
		{"nullable branch retries", regexp2.MustCompile(`^(a?b??)*$`, regexp2.ECMAScript), "ab", [][]string{{"ab"}, {"b"}}},
		{"optional lookahead rejected", regexp2.MustCompile(`(?:(?=(abc)))?a`, regexp2.ECMAScript), "abc", [][]string{{"a"}, nil}},
		{"non-ECMA capture history", regexp2.MustCompile(`^(a(b)?)+$`), "aba", [][]string{{"aba"}, {"ab", "a"}, {"b"}}},
		{"non-ECMA backreference", regexp2.MustCompile(`^(a(b)?)+\2$`), "abab", [][]string{{"abab"}, {"ab", "a"}, {"b"}}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			tc.re.MatchTimeout = time.Second
			checkMatch(t, tc.re, []rune(tc.input), tc.captures != nil)
			m, err := tc.re.FindStringMatch(tc.input)
			if err != nil {
				t.Fatal(err)
			}
			if tc.captures == nil {
				return // checkMatch already verified that there is no match.
			}
			if m == nil {
				t.Fatal("expected match")
			}
			groups := m.Groups()
			if len(groups) != len(tc.captures) {
				t.Fatalf("got %d groups, want %d", len(groups), len(tc.captures))
			}
			for i, want := range tc.captures {
				got := groups[i].Captures
				if len(got) != len(want) {
					t.Errorf("group %d has %d captures, want %v", i, len(got), want)
					continue
				}
				for j, capture := range got {
					if capture.String() != want[j] {
						t.Errorf("group %d capture %d = %q, want %q", i, j, capture.String(), want[j])
					}
				}
			}
		})
	}
}

// Calls remain inside tests so they run after the generated engines register.
func TestPropertyLoopBounds(t *testing.T) {
	for _, re := range []*regexp2.Regexp{
		regexp2.MustCompile(`\P{sc=Hira}+`, regexp2.ECMAScript|regexp2.Unicode),
		regexp2.MustCompile(`\P{sc=Hira}?`, regexp2.ECMAScript|regexp2.Unicode),
		regexp2.MustCompile(`\P{sc=Hira}+`, regexp2.ECMAScript|regexp2.Unicode|regexp2.RightToLeft),
	} {
		t.Run(re.String(), func(t *testing.T) {
			checkMatch(t, re, []rune("a"), true)
		})
	}
}

func TestPropertyRangeBounds(t *testing.T) {
	positive := regexp2.MustCompile(`^\p{sc=Khudawadi}$`, regexp2.ECMAScript|regexp2.Unicode)
	negative := regexp2.MustCompile(`^[^\p{sc=Khudawadi}]$`, regexp2.ECMAScript|regexp2.Unicode)
	for _, tc := range []struct {
		ch   rune
		want bool
	}{
		{'a', false},
		{0x112af, false},
		{0x112b0, true},
		{0x112ea, true},
		{0x112eb, false},
		{0x112f0, true},
		{0x112f9, true},
		{0x112fa, false},
	} {
		checkMatch(t, positive, []rune{tc.ch}, tc.want)
		checkMatch(t, negative, []rune{tc.ch}, !tc.want)
	}
}

func TestPropertyASCIIBoundary(t *testing.T) {
	re := regexp2.MustCompile(`^\P{L}$`, regexp2.ECMAScript|regexp2.Unicode)
	for _, ch := range []rune{0x7e, 0x7f, 0x80} {
		checkMatch(t, re, []rune{ch}, true)
	}
	checkMatch(t, re, []rune("a"), false)
}

func TestSurrogateProperty(t *testing.T) {
	for _, re := range []*regexp2.Regexp{
		regexp2.MustCompile(`^\p{Surrogate}$`, regexp2.ECMAScript|regexp2.Unicode),
		regexp2.MustCompile(`\p{Surrogate}+`, regexp2.ECMAScript|regexp2.Unicode),
		regexp2.MustCompile(`\p{Surrogate}+`, regexp2.ECMAScript|regexp2.Unicode|regexp2.RightToLeft),
	} {
		t.Run(re.String(), func(t *testing.T) {
			for _, ch := range []rune{0xd800, 0xdfff} {
				checkMatch(t, re, []rune{ch}, true)
			}
			for _, ch := range []rune{0xd7ff, 0xe000, utf8.RuneError} {
				checkMatch(t, re, []rune{ch}, false)
			}
			checkMatch(t, re, []rune("ab"), false)
		})
	}
	negative := regexp2.MustCompile(`^\P{Surrogate}$`, regexp2.ECMAScript|regexp2.Unicode)
	checkMatch(t, negative, []rune{0xd800}, false)
	checkMatch(t, negative, []rune{utf8.RuneError}, true)
}

func TestPropertyComposition(t *testing.T) {
	for _, tc := range []struct {
		re      *regexp2.Regexp
		yes, no string
	}{
		{regexp2.MustCompile(`^\p{scx=Hira}$`, regexp2.ECMAScript|regexp2.Unicode), "ー", "a"},
		{regexp2.MustCompile(`^\p{sc=Hira}$`, regexp2.ECMAScript|regexp2.Unicode), "あ", "ー"},
		{regexp2.MustCompile(`^\p{scx=Common}$`, regexp2.ECMAScript|regexp2.Unicode), ".", "ー"},
		{regexp2.MustCompile(`^[\P{L}\p{sc=Greek}]+$`, regexp2.ECMAScript|regexp2.Unicode), "1α!", "1a!"},
		{regexp2.MustCompile(`^[\p{Ll}\p{Emoji}]+$`, regexp2.ECMAScript|regexp2.Unicode|regexp2.IgnoreCase), "AΣ😀", "AΣ!"},
		// /u complements the property before folding. Negating the whole class
		// after folding has different behavior.
		{regexp2.MustCompile(`^\P{Lowercase_Letter}$`, regexp2.ECMAScript|regexp2.Unicode|regexp2.IgnoreCase), "a", "İ"},
		{regexp2.MustCompile(`^[^\p{Lowercase_Letter}]$`, regexp2.ECMAScript|regexp2.Unicode|regexp2.IgnoreCase), "!", "a"},
	} {
		t.Run(tc.re.String(), func(t *testing.T) {
			checkMatch(t, tc.re, []rune(tc.yes), true)
			checkMatch(t, tc.re, []rune(tc.no), false)
		})
	}
}

func TestEscapedGroupNames(t *testing.T) {
	for _, re := range []*regexp2.Regexp{
		regexp2.MustCompile(`(?<\u{1D49C}>a)\k<\u{1D49C}>`, regexp2.ECMAScript),
		regexp2.MustCompile(`(?<\uD835\uDC9C>a)\k<\uD835\uDC9C>`, regexp2.ECMAScript),
		regexp2.MustCompile(`(?<\uD835\uDC9C>a)\k<\u{1D49C}>`, regexp2.ECMAScript|regexp2.Unicode),
	} {
		t.Run(re.String(), func(t *testing.T) {
			checkMatch(t, re, []rune("aa"), true)
			checkMatch(t, re, []rune("ab"), false)
			match, err := re.FindStringMatch("aa")
			if err != nil || match == nil {
				t.Fatalf("FindStringMatch: %v, %v", match, err)
			}
			if group := match.GroupByName("\U0001D49C"); group == nil || group.String() != "a" {
				t.Fatalf("decoded group name: %v; want capture a", group)
			}
		})
	}
}

func TestLegacyECMALookaheadQuantifier(t *testing.T) {
	re := regexp2.MustCompile(`(?=a)*a`, regexp2.ECMAScript)
	checkMatch(t, re, []rune("a"), true)
	checkMatch(t, re, []rune("b"), false)
}

func checkMatch(t *testing.T, re *regexp2.Regexp, input []rune, want bool) {
	t.Helper()
	if got, err := re.MatchRunes(input); err != nil || got != want {
		t.Errorf("%s.MatchRunes(%U) = %v, %v; want %v", re, input, got, err, want)
	}
	if got, err := re.FindRunesMatch(input); err != nil || (got != nil) != want {
		t.Errorf("%s.FindRunesMatch(%U) = %v, %v; want match %v", re, input, got, err, want)
	}
	for _, ch := range input {
		if !utf8.ValidRune(ch) {
			return // Strings cannot represent surrogate code points.
		}
	}
	if got, err := re.MatchString(string(input)); err != nil || got != want {
		t.Errorf("%s.MatchString(%q) = %v, %v; want %v", re, string(input), got, err, want)
	}
	if got, err := re.FindStringMatch(string(input)); err != nil || (got != nil) != want {
		t.Errorf("%s.FindStringMatch(%q) = %v, %v; want match %v", re, string(input), got, err, want)
	}
}

func TestDuplicateNamesBoolean(t *testing.T) {
	// Extends Test262 boolean matching coverage to nested and nullable loops
	// and individual numeric references in generated quick matchers:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-test.js
	for _, tc := range []struct {
		re      *regexp2.Regexp
		yes, no string
	}{
		{regexp2.MustCompile(`^(?:(?<x>a)|(?<x>b))+$`, regexp2.ECMAScript), "ab", "abc"},
		{regexp2.MustCompile(`^(?:(?<x>a)|(?<x>b))+\2$`, regexp2.ECMAScript), "abb", "ab"},
		{regexp2.MustCompile(`^(?:(?<x>a)|(?<x>b))+?\1$`, regexp2.ECMAScript), "baa", "ba"},
		{regexp2.MustCompile(`^(?:(?:(?<x>a)|(?<x>b))+|c)+\2$`, regexp2.ECMAScript), "abc", "ab"},
		{regexp2.MustCompile(`^(?:(?<x>a)|(?<x>b)?)*?\k<x>$`, regexp2.ECMAScript), "bb", "b"},
	} {
		t.Run(tc.re.String(), func(t *testing.T) {
			checkMatch(t, tc.re, []rune(tc.yes), true)
			checkMatch(t, tc.re, []rune(tc.no), false)
		})
	}
}
