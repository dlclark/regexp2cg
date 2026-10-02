package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestGeneratedECMAUnicode(t *testing.T) {
	// Keep the generated package in this module so it uses the same dependency
	// version and module settings as the generator and the rest of the tests.
	dir, err := os.MkdirTemp(".", ".ecmaharness-")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	source, err := os.ReadFile("testdata/ecma_unicode/ecma_test.go")
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "ecma_test.go"), source, 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("go", "run", ".", "-path", dir, "-test", "-o", filepath.Join(dir, "generated.go"))
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("generate ECMAScript engines: %v\n%s", err, out)
	}
	// Run each regression separately so a panic does not mask other failures.
	for _, name := range []string{"TestPropertyLoopBounds", "TestPropertyRangeBounds", "TestPropertyASCIIBoundary", "TestSurrogateProperty", "TestPropertyComposition", "TestEscapedGroupNames", "TestLegacyECMALookaheadQuantifier", "TestDuplicateNamesBoolean"} {
		t.Run(name, func(t *testing.T) {
			cmd := exec.Command("go", "test", "-run", "^"+name+"$", ".")
			cmd.Dir = dir
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("generated engine regression: %v\n%s", err, out)
			}
		})
	}
}

func TestECMAUnicodeCompileErrors(t *testing.T) {
	binary := filepath.Join(t.TempDir(), "regexp2cg")
	if out, err := exec.Command("go", "build", "-o", binary, ".").CombinedOutput(); err != nil {
		t.Fatalf("build generator: %v\n%s", err, out)
	}
	for _, tc := range []struct {
		pattern string
		options syntax.RegexOptions
		message string
	}{
		{`^*`, syntax.ECMAScript, "assertion cannot be quantified"},
		{`(?<=a)?`, syntax.ECMAScript, "assertion cannot be quantified"},
		{`(?=a)*`, syntax.ECMAScript | syntax.Unicode, "assertion cannot be quantified"},
		{`\p{Greek}`, syntax.ECMAScript | syntax.Unicode, "unknown unicode category"},
		{`\p{script=Greek}`, syntax.ECMAScript | syntax.Unicode, "unknown unicode category"},
		{`\p{ASCII=Yes}`, syntax.ECMAScript | syntax.Unicode, "unknown unicode category"},
		{`[\p{ASCII}-a]`, syntax.ECMAScript | syntax.Unicode, "cannot create range"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			out, err := exec.Command(binary, "-expr", tc.pattern, "-opt", strconv.Itoa(int(tc.options))).CombinedOutput()
			if err == nil || !strings.Contains(string(out), tc.message) {
				t.Fatalf("generate %q: %v\n%s\nwant error containing %q", tc.pattern, err, out, tc.message)
			}
		})
	}
}

func TestECMAScriptUnicodeCategoryAliases(t *testing.T) {
	pattern := `\p{digit}+`
	exec := generateAndCompile(t, pattern, syntax.ECMAScript|syntax.Unicode)

	runMatch(t, pattern, exec, "abc1", " 0: 1")
	runNoMatch(t, pattern, exec, "abc")
}

func TestECMAScriptUnicodeLongCategoryAlias(t *testing.T) {
	pattern := `\p{Letter}+`
	exec := generateAndCompile(t, pattern, syntax.ECMAScript|syntax.Unicode)

	runMatch(t, pattern, exec, "abc\\xc3\\xa9", " 0: abc\\xc3\\xa9")
	runNoMatch(t, pattern, exec, "123")
}

func TestECMAScriptNonUnicodeSlashPIsLiteral(t *testing.T) {
	pattern := `\p{L}`
	exec := generateAndCompile(t, pattern, syntax.ECMAScript)

	runMatch(t, pattern, exec, "p{L}", " 0: p{L}")
	runNoMatch(t, pattern, exec, "abc")
}

func TestECMAScriptIgnoreCaseComplementClass(t *testing.T) {
	pattern := `^\D$`
	exec := generateAndCompile(t, pattern, syntax.ECMAScript|syntax.IgnoreCase)
	runMatch(t, pattern, exec, "t", " 0: t")
	runNoMatch(t, pattern, exec, "1")
}

func TestGeneratedECMADuplicateNames(t *testing.T) {
	// Extends Test262's matching cases with quantified regexp2 regressions:
	// https://github.com/tc39/test262/blob/main/test/built-ins/RegExp/named-groups/duplicate-names-exec.js
	for _, tc := range []struct {
		pattern, input string
		groups         []string
	}{
		{`^(?:(?<x>a)|(?<x>b))+\k<x>$`, "abb", []string{"abb", "<unset>", "b"}},
		{`^(?:(?<x>a)|(?<x>b)?)+$`, "b", []string{"b", "<unset>", "b"}},
		{`^(?:(?<x>a)|(?<x>b)?)*?\k<x>$`, "bb", []string{"bb", "<unset>", "b"}},
		{`^(?:(?<x>a*)|(?<x>b*)){1,3}\k<x>$`, "baaaa", []string{"baaaa", "aa", "<unset>"}},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			binary := generateAndCompile(t, tc.pattern, syntax.ECMAScript)
			for i, group := range tc.groups {
				runMatch(t, tc.pattern, binary, tc.input, fmt.Sprintf("%2d: %s", i, group))
			}
		})
	}
}

func TestGeneratedECMADuplicateNamesEmptyIterationFailure(t *testing.T) {
	// regexp2 regression for ECMAScript 2025 §22.2.2.3.1 RepeatMatcher,
	// step 2.2: a lazy optional empty iteration fails and backtracks.
	pattern := `^(?:(?<x>a)|(?<x>b)?)*?\k<x>$`
	binary := generateAndCompile(t, pattern, syntax.ECMAScript)
	runNoMatch(t, pattern, binary, "b")
}
