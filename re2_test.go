package main

import (
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
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

func TestRE2RequiredLandmarkChain(t *testing.T) {
	pattern := `(?P<name>[-\w\d\.]+?)(?:\s+at\s+|\s*@\s*|\s*(?:[\[\]@]){3}\s*)(?P<host>[-\w\d\.]*?)\s*(?:dot|\.|(?:[\[\]dot\.]){3,5})\s*(?P<domain>\w+)`
	tree, err := syntax.Parse(pattern, syntax.ParseOptions{RegexOptions: syntax.RE2, CodeGen: true})
	if err != nil {
		t.Fatal(err)
	}
	if got, want := tree.FindOptimizations.FindMode, syntax.RequiredLandmarkChain_LeftToRight; got != want {
		t.Fatalf("FindMode = %v, want %v", got, want)
	}

	exec := generateAndCompile(t, pattern, syntax.RE2)
	runMatch(t, pattern, exec, "contact user at example dot com", " 0: user at example dot com")
	runMatch(t, pattern, exec, "contact user@@@example...com", " 0: user@@@example...com")
	runNoMatch(t, pattern, exec, "contact user near example dash com")
	//t.Fail()
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
