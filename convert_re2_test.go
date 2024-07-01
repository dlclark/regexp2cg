package main

import (
	"testing"

	"github.com/dlclark/regexp2/syntax"
)

func runNoMatch(t *testing.T, pattern, reExec, input string) {
	m := matchString(t, pattern, reExec, input)
	validateNoMatch(t, pattern, m, input)
}

func runMatch(t *testing.T, pattern, reExec, input, expected string) {
	m := matchString(t, pattern, reExec, input)
	validateMatch(t, pattern, m, expected, input)
}

func TestRE2NamedAscii_Concat(t *testing.T) {
	pattern := "[[:digit:]a]"
	exec := generateAndCompile(t, pattern, syntax.RE2)

	runNoMatch(t, pattern, exec, "b")

	runMatch(t, pattern, exec, "a", " 0: a")

	runNoMatch(t, pattern, exec, "[")

	runMatch(t, pattern, exec, "5", " 0: 5")
}

func TestRE2Dollar_Singleline(t *testing.T) {
	// PCRE allows for \n after the $ and RE2 doesn't
	pattern := `^ac$\n`
	exec := generateAndCompile(t, pattern, syntax.RE2)

	runNoMatch(t, pattern, exec, "ac")
	runNoMatch(t, pattern, exec, "ac\n")
}

func TestRE2Dollar_Multiline(t *testing.T) {
	pattern := `^ac$\n`
	exec := generateAndCompile(t, pattern, syntax.RE2|syntax.Multiline)

	runNoMatch(t, pattern, exec, "ac")
	runMatch(t, pattern, exec, "ac\n", " 0: ac\\x0a")
}

func TestRE2ExtendedZero(t *testing.T) {
	notZero := "߀" // \u07c0

	exec := generateAndCompile(t, `^\d$`, syntax.RE2)
	runNoMatch(t, `^\d$`, exec, notZero)

	exec = generateAndCompile(t, `^\D$`, syntax.RE2)
	runMatch(t, `^\D$`, exec, notZero, " 0: \\xdf\\x80")
}

func TestRegularExtendedZero(t *testing.T) {
	notZero := "߀" // \u07c0

	exec := generateAndCompile(t, `^\d$`, 0)
	runMatch(t, `^\d$`, exec, notZero, " 0: \\xdf\\x80")

	exec = generateAndCompile(t, `^\D$`, 0)
	runNoMatch(t, `^\D$`, exec, notZero)
}

func TestRE2Word(t *testing.T) {
	exec := generateAndCompile(t, `\w`, syntax.RE2)
	runNoMatch(t, `\w`, exec, "å")

	exec = generateAndCompile(t, `\W`, syntax.RE2)
	runMatch(t, `\W`, exec, "å", " 0: \\xc3\\xa5")
}

func TestRegularWord(t *testing.T) {
	exec := generateAndCompile(t, `\w`, 0)
	runMatch(t, `\w`, exec, "å", " 0: \\xc3\\xa5")

	exec = generateAndCompile(t, `\W`, 0)
	runNoMatch(t, `\W`, exec, "å")
}

func TestRE2Space(t *testing.T) {
	exec := generateAndCompile(t, `\s`, syntax.RE2)
	runNoMatch(t, `\s`, exec, "\x0b")

	exec = generateAndCompile(t, `\S`, syntax.RE2)
	runMatch(t, `\S`, exec, "\x0b", " 0: \\x0b")
}

func TestRegularSpace(t *testing.T) {
	exec := generateAndCompile(t, `\s`, 0)
	runMatch(t, `\s`, exec, "\x0b", " 0: \\x0b")

	exec = generateAndCompile(t, `\S`, 0)
	runNoMatch(t, `\S`, exec, "\x0b")
}
