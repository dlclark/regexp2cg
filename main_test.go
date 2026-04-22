package main

import (
	"bytes"
	"go/parser"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2/syntax"
)

func TestIsStaticCompileCall_V2CompileOptions(t *testing.T) {
	tests := []struct {
		name                     string
		expr                     string
		wantPat                  string
		wantOpts                 syntax.RegexOptions
		wantCompileOptions       []string
		wantMaintainCaptureOrder bool
	}{
		{
			name:               "none",
			expr:               `regexp2.MustCompile("abc", regexp2.None)`,
			wantPat:            "abc",
			wantOpts:           0,
			wantCompileOptions: []string{"regexp2.None"},
		},
		{
			name:               "variadic regex options",
			expr:               `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.Multiline)`,
			wantPat:            "abc",
			wantOpts:           syntax.IgnoreCase | syntax.Multiline,
			wantCompileOptions: []string{"regexp2.IgnoreCase", "regexp2.Multiline"},
		},
		{
			name:               "explicit regex options conversion",
			expr:               `regexp2.MustCompile("abc", regexp2.RegexOptions(0))`,
			wantPat:            "abc",
			wantOpts:           0,
			wantCompileOptions: []string{"regexp2.RegexOptions(0)"},
		},
		{
			name:               "mixed regex and skipped options",
			expr:               `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.OptionDisableCharClassASCIIBitmap())`,
			wantPat:            "abc",
			wantOpts:           syntax.IgnoreCase,
			wantCompileOptions: []string{"regexp2.IgnoreCase", "regexp2.OptionDisableCharClassASCIIBitmap()"},
		},
		{
			name:               "mixed regex and runtime optimization options",
			expr:               `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.OptionMaxCachedRuneBufferLength(64*1024), regexp2.OptionMaxCachedReplaceBufferLength(64*1024), regexp2.OptionMaxCachedReplacerDataEntries(8), regexp2.OptionMaxCachedReplacerDataBytes(1024))`,
			wantPat:            "abc",
			wantOpts:           syntax.IgnoreCase,
			wantCompileOptions: []string{"regexp2.IgnoreCase", "regexp2.OptionMaxCachedRuneBufferLength(64 * 1024)", "regexp2.OptionMaxCachedReplaceBufferLength(64 * 1024)", "regexp2.OptionMaxCachedReplacerDataEntries(8)", "regexp2.OptionMaxCachedReplacerDataBytes(1024)"},
		},
		{
			name:               "or expression mixed with runtime and trailing regex option",
			expr:               `regexp2.MustCompile("testpattern", regexp2.IgnoreCase|regexp2.RE2, regexp2.OptionMaxCachedReplacerDataEntries(10), regexp2.Multiline)`,
			wantPat:            "testpattern",
			wantOpts:           syntax.IgnoreCase | syntax.RE2 | syntax.Multiline,
			wantCompileOptions: []string{"regexp2.IgnoreCase | regexp2.RE2", "regexp2.OptionMaxCachedReplacerDataEntries(10)", "regexp2.Multiline"},
		},
		{
			name:                     "maintain capture order",
			expr:                     `regexp2.MustCompile("(?<first>This) (is)", regexp2.OptionMaintainCaptureOrder())`,
			wantPat:                  "(?<first>This) (is)",
			wantCompileOptions:       []string{"regexp2.OptionMaintainCaptureOrder()"},
			wantMaintainCaptureOrder: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			node, err := parser.ParseExpr(tt.expr)
			if err != nil {
				t.Fatal(err)
			}

			ok, pattern, opts, compileOptions, maintainCaptureOrder, _, err := isStaticCompileCall(node, "regexp2")
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
			if maintainCaptureOrder != tt.wantMaintainCaptureOrder {
				t.Fatalf("maintainCaptureOrder = %v, want %v", maintainCaptureOrder, tt.wantMaintainCaptureOrder)
			}
			if len(compileOptions) != len(tt.wantCompileOptions) {
				t.Fatalf("compileOptions = %#v, want %#v", compileOptions, tt.wantCompileOptions)
			}
			for i := range compileOptions {
				if compileOptions[i] != tt.wantCompileOptions[i] {
					t.Fatalf("compileOptions = %#v, want %#v", compileOptions, tt.wantCompileOptions)
				}
			}
		})
	}
}

func TestGeneratedRegisterEngineUsesV2Signature(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}

	err = c.addRegexp(
		"MyFile.go:120:10",
		"MyPattern",
		"abc",
		syntax.IgnoreCase,
		true,
		[]string{"regexp2.IgnoreCase", "regexp2.OptionMaintainCaptureOrder()"},
	)
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	if !strings.Contains(got, `regexp2.RegisterEngine("abc", regexp2.RuntimeEngineData{`) {
		t.Fatalf("generated RegisterEngine call does not use RuntimeEngineData as second argument:\n%s", got)
	}
	if !strings.Contains(got, `FindFirstChar:`) || !strings.Contains(got, `MyPattern_FindFirstChar,`) {
		t.Fatalf("generated RegisterEngine call does not use generated FindFirstChar function:\n%s", got)
	}
	if !strings.Contains(got, `Execute:`) || !strings.Contains(got, `MyPattern_Execute,`) {
		t.Fatalf("generated RegisterEngine call does not use generated Execute function:\n%s", got)
	}
	if !strings.Contains(got, `StringPrefixFilter:`) || !strings.Contains(got, `MyPattern_StringPrefixFilter,`) {
		t.Fatalf("generated RegisterEngine call does not use generated StringPrefixFilter function:\n%s", got)
	}
	if !strings.Contains(got, `}, regexp2.IgnoreCase, regexp2.OptionMaintainCaptureOrder())`) {
		t.Fatalf("generated RegisterEngine call does not pass compile options after RuntimeEngineData:\n%s", got)
	}
}

func TestIsStaticCompileCall_UnknownCompileOption(t *testing.T) {
	node, err := parser.ParseExpr(`regexp2.MustCompile("abc", opts)`)
	if err != nil {
		t.Fatal(err)
	}

	ok, _, _, _, _, _, err := isStaticCompileCall(node, "regexp2")
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

	ok, _, _, _, _, _, err := isStaticCompileCall(node, "regexp2")
	if err == nil {
		t.Fatal("isStaticCompileCall returned nil error for unknown regexp2 option")
	}
	if ok {
		t.Fatal("isStaticCompileCall returned true for unknown regexp2 option")
	}
}
