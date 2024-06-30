package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strconv"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/syntax"
	"github.com/pkg/errors"
)

// Process the file "testoutput1" from PCRE2 v10.21 (public domain)
var totalCount, failCount = 0, 0

func TestConversion(t *testing.T) {
	defer func() {
		if failCount > 0 {
			t.Logf("%v of %v patterns failed", failCount, totalCount)
		}
	}()
	// open our test patterns file and run through it
	// validating results as we go
	file, err := os.Open("testoutput1")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	// the high level structure of the file:
	//		#comments - ignore only outside of the pattern
	//		pattern (could be multi-line, could be surrounded by "" or //) after the / there are the options some we understand, some we don't
	//		    test case
	//		 0: success case
	//		\= Expect no match (ignored)
	//		    another test case
	//		No Match
	//
	//		another pattern ...etc

	scanner := bufio.NewScanner(file)
	// main pattern loop
	for scanner.Scan() {
		// reading the file a line at a time
		line := scanner.Text()

		if trim := strings.TrimSpace(line); trim == "" || strings.HasPrefix(trim, "#") {
			// skip blanks and comments
			continue
		}

		patternStart := line[0]
		if patternStart != '/' && patternStart != '"' {
			// an error!  expected a pattern but we didn't understand what was in the file
			t.Fatalf("Unknown file format, expected line to start with '/' or '\"', line in: %v", line)
		}

		// start building our pattern, handling multi-line patterns
		pattern := line
		totalCount++

		// keep appending the lines to our pattern string until we
		// find our closing tag, don't allow the first char to match on the
		// line start, but subsequent lines could end on the first char
		allowFirst := false
		for !containsEnder(line, patternStart, allowFirst) {
			if !scanner.Scan() {
				// an error!  expected more pattern, but got eof
				t.Fatalf("Unknown file format, expected more pattern text, but got EOF, pattern so far: %v", pattern)
			}
			line = scanner.Text()
			pattern += fmt.Sprintf("\n%s", line)
			allowFirst = true
		}

		//subtest
		t.Run(fmt.Sprintf("pcre %s", pattern), func(t *testing.T) {

			//t.Logf("Compile Pattern: %v", pattern)
			// we have our raw pattern! -- we need to convert this to a compiled regex
			reExec := compileRawPattern(t, pattern)
			//t.Logf("Program created: %v", reExec)
			var (
				capsIdx map[int]int
				m       string
				toMatch string
			)
			// now we need to parse the test cases if there are any
			// they start with 4 spaces -- if we don't get a 4-space start then
			// we're back out to our next pattern
			for scanner.Scan() {
				line = scanner.Text()

				// blank line is our separator for a new pattern
				if strings.TrimSpace(line) == "" {
					break
				}

				// could be either "    " or "\= Expect"
				if strings.HasPrefix(line, "\\= Expect") {
					continue
				} else if strings.HasPrefix(line, "    ") {
					// trim off leading spaces for our text to match
					toMatch = line[4:]
					// trim off trailing spaces too
					toMatch = strings.TrimRight(toMatch, " ")

					m = matchString(t, pattern, reExec, toMatch)

					capsIdx = make(map[int]int)
					continue
					//t.Fatalf("Expected match text to start with 4 spaces, instead got: '%v'", line)
				} else if strings.HasPrefix(line, "No match") {
					validateNoMatch(t, pattern, m, toMatch)
					// no match means we're done
					continue
				} else if subs := matchGroup.FindStringSubmatch(line); len(subs) == 3 {
					gIdx, _ := strconv.Atoi(subs[1])
					if _, ok := capsIdx[gIdx]; !ok {
						capsIdx[gIdx] = 0
					}
					validateMatch(t, pattern, m, line, toMatch)
					capsIdx[gIdx]++
					continue
				} else {
					// no match -- problem
					t.Fatalf("Unknown file format, expected match or match group but got '%v'", line)
				}
			}
		})
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

var matchGroup = regexp.MustCompile(`^\s*(\d+): (.*)`)

func problem(t *testing.T, input string, args ...interface{}) {
	failCount++
	t.Errorf(input, args...)
}

func validateNoMatch(t *testing.T, pattern string, m string, toMatch string) {
	if len(m) == 0 || m == "No match\n" {
		return
	}

	problem(t, "Expected no match for pattern '%v' with input '%v', but got '%v'", pattern, toMatch, m)
}

func validateMatch(t *testing.T, pattern string, m string, line, toMatch string) {
	if len(m) == 0 {
		// already error'd earlier up stream
		return
	}

	if m == "No match\n" {
		// we didn't match, but should have
		problem(t, "Expected match for pattern '%v' with input '%v', but got no match", pattern, toMatch)
		return
	}

	// find our line in our output
	lines := strings.Split(m, "\n")
	if !slices.Contains(lines, line) {
		// we did not find our line in the input
		problem(t, "Did not find expected line '%s' for pattern '%v' with input '%v'. Got '%s'", line, pattern, toMatch, m)
	}
}

// returns the path to an executable for running tests against this pattern
func compileRawPattern(t *testing.T, pattern string) string {
	// check our end for RegexOptions -trim them off
	index := strings.LastIndexAny(pattern, "/\"")
	//
	// Append "= Debug" to compare details between corefx and regexp2 on the PCRE test suite
	//
	var opts syntax.RegexOptions

	if index+1 < len(pattern) {
		textOptions := pattern[index+1:]
		pattern = pattern[:index+1]
		// there are lots of complex options here
		for _, textOpt := range strings.Split(textOptions, ",") {
			switch textOpt {
			case "dupnames":
				// we don't know how to handle this...
			default:
				if strings.Contains(textOpt, "i") {
					opts |= syntax.IgnoreCase
				}
				if strings.Contains(textOpt, "s") {
					opts |= syntax.Singleline
				}
				if strings.Contains(textOpt, "m") {
					opts |= syntax.Multiline
				}
				if strings.Contains(textOpt, "x") {
					opts |= syntax.IgnorePatternWhitespace
				}
			}
		}

	}

	// trim off first and last char
	pattern = pattern[1 : len(pattern)-1]

	defer func() {
		if rec := recover(); rec != nil {
			problem(t, "PANIC in compiling \"%v\": %v", pattern, rec)
		}
	}()

	return generateAndCompile(t, pattern, opts)
}

func generateAndCompile(t *testing.T, pattern string, opts syntax.RegexOptions) string {
	genPattern, err := os.CreateTemp("", "*.go")
	if err != nil {
		panic("could not create tmp file: " + err.Error())
	}
	c, err := newConverter(genPattern, "main")
	if err != nil {
		t.Error(errors.Wrap(err, "code generation error"))
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", pattern, opts); err != nil {
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
	mainFile.Write(mainContent)

	// build!
	cmd := exec.Command(goPath, "build", "-o", outFile.Name(), genPattern.Name(), mainFile.Name())
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Log(string(out))
		t.Errorf("build error for pattern %v", pattern)
		os.Remove(outFile.Name())
		return ""
	}

	// our executable!
	return outFile.Name()
}

func matchString(t *testing.T, pattern string, reExec string, toMatch string) string {
	if len(reExec) == 0 {
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
		problem(t, "Error matching \"%v\" in pattern \"%v\": %v", toMatch, pattern, err)
	}
	//t.Logf("Result: %v", string(out))
	return string(out)
}

func containsEnder(line string, ender byte, allowFirst bool) bool {
	index := strings.LastIndexByte(line, ender)
	if index > 0 {
		return true
	} else if index == 0 && allowFirst {
		return true
	}
	return false
}
