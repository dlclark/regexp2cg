package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
	"github.com/pkg/errors"
)

func problem(t *testing.T, input string, args ...interface{}) {
	t.Helper()
	t.Errorf(input, args...)
}

func validateNoMatch(t *testing.T, pattern string, m string, toMatch string) {
	t.Helper()
	if len(m) == 0 || m == "No match\n" {
		return
	}

	problem(t, "Expected no match\npattern: %q\ninput:   %q\noutput:  %q", pattern, toMatch, m)
}

func validateMatch(t *testing.T, pattern string, m string, line, toMatch string) {
	t.Helper()
	if len(m) == 0 {
		// already error'd earlier up stream
		return
	}

	if m == "No match\n" {
		// we didn't match, but should have
		problem(t, "Expected match\npattern: %q\ninput:   %q\nwant:    %q\noutput:  %q", pattern, toMatch, line, m)
		return
	}

	// find our line in our output
	lines := strings.Split(m, "\n")
	if !slices.Contains(lines, line) {
		// we did not find our line in the input
		problem(t, "Missing expected match line\npattern: %q\ninput:   %q\nwant:    %q\noutput:  %q\nlines:   %#v", pattern, toMatch, line, m, lines)
	}
}

func TestLargeEmptyRepeatIsReduced(t *testing.T) {
	pattern := `a(?:){50000000}b`
	exec := generateAndCompile(t, pattern, 0)
	runMatch(t, pattern, exec, "ab", " 0: ab")
	runNoMatch(t, pattern, exec, "ac")
}

func TestIgnoreCaseAlternationMatchesWholeBranch(t *testing.T) {
	pattern := `(?i:'s|'t|'re)`
	exec := generateAndCompile(t, pattern, 0)
	runMatch(t, pattern, exec, "'RE", " 0: 'RE")
}

func TestGeneratedSyntaxExtensions(t *testing.T) {
	tests := []struct {
		name    string
		pattern string
		options syntax.RegexOptions
		input   string
		match   string
	}{
		{name: "literal quoting", pattern: `\A\Qfoo.bar[0]+\E\z`, input: `foo.bar[0]+`, match: ` 0: foo.bar[0]+`},
		{name: "unicode newline", pattern: `\A\R{2}\z`, input: "\r\n\n", match: ` 0: \x0d\x0a\x0a`},
		{name: "unicode newline right to left", pattern: `\R`, options: syntax.RightToLeft, input: "x\r\ny", match: ` 0: \x0d\x0a`},
		{name: "grapheme clusters", pattern: `\A\X{3}\z`, input: "a\u0301b\r\n", match: ` 0: a\xcc\x81b\x0d\x0a`},
		{name: "grapheme right to left", pattern: `\X`, options: syntax.RightToLeft, input: "a\u0301b", match: " 0: b"},
		{name: "unicode property aliases", pattern: `\A\p{InCB=Linker}\z`, input: "\u094D", match: ` 0: \xe0\xa5\x8d`},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			exec := generateAndCompile(t, tt.pattern, tt.options)
			runMatch(t, tt.pattern, exec, tt.input, tt.match)
		})
	}
}

func TestGraphemeIsAtomic(t *testing.T) {
	pattern := `\A\X\p{GCB=Extend}\z`
	exec := generateAndCompile(t, pattern, 0)
	runNoMatch(t, pattern, exec, "a\u0301")
}

func TestPossessiveQuantifierDoesNotBacktrack(t *testing.T) {
	pattern := `\Aa++a\z`
	exec := generateAndCompile(t, pattern, 0)
	runNoMatch(t, pattern, exec, "aa")
}

func TestGeneratedLeftContextWordBoundary(t *testing.T) {
	pattern := `\bfoo`
	exec := generateAndCompile(t, pattern, 0)
	runNoMatch(t, pattern, exec, "xxxfoo")
	runMatch(t, pattern, exec, "xxx foo", " 0: foo")
}

func TestGeneratedLeftContextLookbehind(t *testing.T) {
	pattern := `(?<=x)foo`
	exec := generateAndCompile(t, pattern, 0)
	runNoMatch(t, pattern, exec, strings.Repeat("z", 40)+"yfoo")
	runMatch(t, pattern, exec, strings.Repeat("z", 40)+"xfoo", " 0: foo")
}

func TestGeneratedLeadingStringsFindMode(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:1:1", "MyPattern", `(?:apple|tiger)\d+`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}
	got := buf.String()
	if !strings.Contains(got, "multiple strings that could begin the match") {
		t.Fatalf("expected leading-strings find mode, generated:\n%s", got)
	}

	exec := generateAndCompile(t, `(?:apple|tiger)\d+`, 0)
	runMatch(t, `(?:apple|tiger)\d+`, exec, strings.Repeat("z", 200)+"tiger9", " 0: tiger9")
}

func TestGeneratedNegatedClassIsNotLeadingStrings(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:1:1", "MyPattern", `a[^bc]d`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}
	got := buf.String()
	if strings.Contains(got, "multiple strings that could begin the match") {
		t.Fatalf("negated class was treated as leading strings:\n%s", got)
	}

	exec := generateAndCompile(t, `a[^bc]d`, 0)
	runMatch(t, `a[^bc]d`, exec, "aed", " 0: aed")
	runNoMatch(t, `a[^bc]d`, exec, "abd")
}

func TestGeneratedLeftContextMetadata(t *testing.T) {
	tests := []struct {
		pattern string
		want    int
	}{
		{pattern: `foo`, want: 0},
		{pattern: `\bfoo`, want: 1},
		{pattern: `(?m)^foo`, want: 1},
		{pattern: `\Afoo`, want: 1},
		{pattern: `(?<=x)foo`, want: -1},
		{pattern: `\Gfoo`, want: -1},
	}
	for _, tt := range tests {
		t.Run(tt.pattern, func(t *testing.T) {
			var buf bytes.Buffer
			c, err := newConverter(&buf, "main")
			if err != nil {
				t.Fatal(err)
			}
			if err := c.addRegexp("MyFile.go:1:1", "MyPattern", tt.pattern, 0, false, nil); err != nil {
				t.Fatal(err)
			}
			if err := c.addFooter(); err != nil {
				t.Fatal(err)
			}
			want := fmt.Sprintf("LeftContextRunes:%d", tt.want)
			got := strings.ReplaceAll(buf.String(), " ", "")
			if !strings.Contains(got, want) {
				t.Fatalf("generated engine missing LeftContextRunes: %d:\n%s", tt.want, buf.String())
			}
		})
	}
}

// returns the path to an executable for running tests against this pattern
func generateAndCompile(t *testing.T, pattern string, opts syntax.RegexOptions) string {
	t.Helper()
	genPattern, err := os.CreateTemp("", "*.go")
	if err != nil {
		panic("could not create tmp file: " + err.Error())
	}
	t.Logf("generated regex source\npattern: %q\noptions: %v\nsource:  %s", pattern, opts, genPattern.Name())
	c, err := newConverter(genPattern, "main")
	if err != nil {
		t.Error(errors.Wrap(err, "code generation error"))
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", pattern, opts, false, []string{getOptString(opts)}); err != nil {
		t.Error(errors.Wrap(err, "code generation error"))
	}
	if err := c.addFooter(); err != nil {
		t.Error(errors.Wrap(err, "code generation error"))
	}

	// compile our tmp file

	// get our output file name
	outFile, _ := os.CreateTemp("", "")

	// get go path
	goPath, _ := exec.LookPath("go")

	// customize the main file for this pattern
	mainFile, _ := os.CreateTemp("", "*.go")
	origMainFile, _ := filepath.Abs("_runtestmain.go")
	mainContent, _ := os.ReadFile(origMainFile)
	mainContent = bytes.Replace(mainContent, []byte("__PATTERN__"), []byte(fmt.Sprintf("%#v", pattern)), 1)
	mainContent = bytes.Replace(mainContent, []byte("__OPTIONS__"), []byte(fmt.Sprintf("%#v", opts)), 1)
	mainFile.Write(mainContent) // nolint:errcheck

	// build!
	cmd := exec.Command(goPath, "build", "-o", outFile.Name(), genPattern.Name(), mainFile.Name())
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Logf("generated regex build failed\npattern: %q\nsource:  %s\nmain:    %s\noutput:  %s", pattern, genPattern.Name(), mainFile.Name(), string(out))
		t.Errorf("build error for pattern %q", pattern)
		os.Remove(outFile.Name()) // nolint:errcheck
		return ""
	}

	// our executable!
	t.Logf("compiled regex test binary\npattern: %q\nsource:  %s\nmain:    %s\nbinary:  %s", pattern, genPattern.Name(), mainFile.Name(), outFile.Name())
	return outFile.Name()
}

func matchString(t *testing.T, pattern string, reExec string, toMatch string) string {
	t.Helper()
	if len(reExec) == 0 {
		t.Logf("skipping match because regex executable was not built\npattern: %q\ninput:   %q", pattern, toMatch)
		return ""
	}

	escp := ""
	var err error
	if toMatch != "\\" {
		escp = toMatch // unEscapeToMatch(toMatch)
	}
	//t.Logf("Testing: %v", escp)
	cmd := exec.Command(reExec, escp)
	out, err := cmd.CombinedOutput()
	if err != nil {
		problem(t, "Error running generated regex\npattern: %q\ninput:   %q\nbinary:  %s\nerror:   %v\noutput:  %q", pattern, toMatch, reExec, err, string(out))
	}
	//t.Logf("Result: %v", string(out))
	return string(out)
}
