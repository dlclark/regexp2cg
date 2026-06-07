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
