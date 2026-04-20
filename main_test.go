package main

import (
	"go/parser"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestIsStaticCompileCall_V2CompileOptions(t *testing.T) {
	tests := []struct {
		name     string
		expr     string
		wantPat  string
		wantOpts syntax.RegexOptions
	}{
		{
			name:     "none",
			expr:     `regexp2.MustCompile("abc", regexp2.None)`,
			wantPat:  "abc",
			wantOpts: 0,
		},
		{
			name:     "variadic regex options",
			expr:     `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.Multiline)`,
			wantPat:  "abc",
			wantOpts: syntax.IgnoreCase | syntax.Multiline,
		},
		{
			name:     "explicit regex options conversion",
			expr:     `regexp2.MustCompile("abc", regexp2.RegexOptions(0))`,
			wantPat:  "abc",
			wantOpts: 0,
		},
		{
			name:     "mixed regex and skipped options",
			expr:     `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.OptionDisableCharClassASCIIBitmap())`,
			wantPat:  "abc",
			wantOpts: syntax.IgnoreCase,
		},
		{
			name:     "mixed regex and runtime optimization options",
			expr:     `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.OptionMaxCachedRuneBufferLength(64*1024), regexp2.OptionMaxCachedReplaceBufferLength(64*1024), regexp2.OptionMaxCachedReplacerDataEntries(8), regexp2.OptionMaxCachedReplacerDataBytes(1024))`,
			wantPat:  "abc",
			wantOpts: syntax.IgnoreCase,
		},
		{
			name:     "or expression mixed with runtime and trailing regex option",
			expr:     `regexp2.MustCompile("testpattern", regexp2.IgnoreCase|regexp2.RE2, regexp2.OptionMaxCachedReplacerDataEntries(10), regexp2.Multiline)`,
			wantPat:  "testpattern",
			wantOpts: syntax.IgnoreCase | syntax.RE2 | syntax.Multiline,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			node, err := parser.ParseExpr(tt.expr)
			if err != nil {
				t.Fatal(err)
			}

			ok, pattern, opts, _, err := isStaticCompileCall(node, "regexp2")
			if err != nil {
				t.Fatal(err)
			}
			if !ok {
				t.Fatalf("isStaticCompileCall(%s) = false", tt.expr)
			}
			if pattern != tt.wantPat {
				t.Fatalf("pattern = %q, want %q", pattern, tt.wantPat)
			}
			if syntax.RegexOptions(opts) != tt.wantOpts {
				t.Fatalf("opts = %v, want %v", syntax.RegexOptions(opts), tt.wantOpts)
			}
		})
	}
}

func TestIsStaticCompileCall_UnknownCompileOption(t *testing.T) {
	node, err := parser.ParseExpr(`regexp2.MustCompile("abc", opts)`)
	if err != nil {
		t.Fatal(err)
	}

	ok, _, _, _, err := isStaticCompileCall(node, "regexp2")
	if err != nil {
		t.Fatal(err)
	}
	if ok {
		t.Fatal("isStaticCompileCall returned true for unknown compile options")
	}
}

func TestIsStaticCompileCall_UnknownRegexp2OptionErrors(t *testing.T) {
	node, err := parser.ParseExpr(`regexp2.MustCompile("abc", regexp2.OptionFutureFastThing())`)
	if err != nil {
		t.Fatal(err)
	}

	ok, _, _, _, err := isStaticCompileCall(node, "regexp2")
	if err == nil {
		t.Fatal("isStaticCompileCall returned nil error for unknown regexp2 option")
	}
	if ok {
		t.Fatal("isStaticCompileCall returned true for unknown regexp2 option")
	}
}
