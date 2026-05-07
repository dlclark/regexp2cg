package main

import (
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

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
