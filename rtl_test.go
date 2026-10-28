package main

import (
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestRightToLeft_UnicodeLiteralPrefix(t *testing.T) {
	pattern := `.éΣ`
	binary := generateAndCompile(t, pattern, syntax.RightToLeft)
	runMatch(t, pattern, binary, "??#éΣ", ` 0: #\xc3\xa9\xce\xa3`)
	runNoMatch(t, pattern, binary, "??#éX??")
}

func TestRightToLeft_LongLiteralPrefix(t *testing.T) {
	for _, literal := range []string{strings.Repeat("abcd", 32), strings.Repeat("éΣ𐐀z", 32)} {
		t.Run(literal, func(t *testing.T) {
			pattern := `.` + literal
			binary := generateAndCompile(t, pattern, syntax.RightToLeft)
			want := strings.NewReplacer("é", `\xc3\xa9`, "Σ", `\xce\xa3`, "𐐀", `\xf0\x90\x90\x80`).Replace("#" + literal)
			runMatch(t, pattern, binary, "??#"+literal, " 0: "+want)
			runNoMatch(t, pattern, binary, "??#"+literal[:len(literal)-1]+"!??")
		})
	}
}

func TestRightToLeft_Basic(t *testing.T) {
	pattern := `foo\d+`
	s := "0123456789foo4567890foo1foo  0987"
	exec := generateAndCompile(t, pattern, syntax.RightToLeft)

	runMatch(t, pattern, exec, s, " 0: foo1")
}

func TestRightToLeft_LiteralPrefix(t *testing.T) {
	pattern := `abc`
	exec := generateAndCompile(t, pattern, syntax.RightToLeft)

	runMatch(t, pattern, exec, "abc abc", " 0: abc")
}

func TestRightToLeft_StartAt(t *testing.T) {
	pattern := `\d`
	exec := generateAndCompile(t, pattern, syntax.RightToLeft)

	runMatch(t, pattern, exec, "0123", " 0: 3")
}

func TestGeneratedLookbehindPreservesBacktracking(t *testing.T) {
	for _, tc := range []struct {
		pattern, input string
	}{
		{`(?<=(?:a*(ba)){2})$`, "baba"},
		{`(?<=(?:a*\z){2})$`, "a"},
		{`(?<=aba*)`, "ab"},
	} {
		t.Run(tc.pattern, func(t *testing.T) {
			binary := generateAndCompile(t, tc.pattern, 0)
			runMatch(t, tc.pattern, binary, tc.input, " 0: ")
		})
	}
}
