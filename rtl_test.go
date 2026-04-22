package main

import (
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

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
