package main

import (
	"bytes"
	"go/parser"
	"go/token"
	"strings"
	"testing"

	"github.com/dlclark/regexp2/v2"
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
			expr:               `regexp2.MustCompile("abc", regexp2.IgnoreCase, regexp2.OptionMaxBacktrackingStackSize(4096), regexp2.OptionMaxCachedRuneBufferLength(64*1024), regexp2.OptionMaxCachedReplaceBufferLength(64*1024), regexp2.OptionMaxCachedReplacerDataEntries(8), regexp2.OptionMaxCachedReplacerDataBytes(1024))`,
			wantPat:            "abc",
			wantOpts:           syntax.IgnoreCase,
			wantCompileOptions: []string{"regexp2.IgnoreCase", "regexp2.OptionMaxBacktrackingStackSize(4096)", "regexp2.OptionMaxCachedRuneBufferLength(64 * 1024)", "regexp2.OptionMaxCachedReplaceBufferLength(64 * 1024)", "regexp2.OptionMaxCachedReplacerDataEntries(8)", "regexp2.OptionMaxCachedReplacerDataBytes(1024)"},
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

func TestGeneratedLeadingSetStringPrefixFilter(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `[a-c]`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	if !strings.Contains(got, `StringPrefixFilter: MyPattern_StringPrefixFilter`) {
		t.Fatalf("generated engine does not register a leading-set string prefilter:\n%s", got)
	}
	if !strings.Contains(got, `input[i] >= 'a' && input[i] <= 'c'`) {
		t.Fatalf("generated leading-set prefilter does not scan the ASCII range:\n%s", got)
	}
}

func TestGeneratedLeadingSetStringPrefixFilterMatchesUTF8Input(t *testing.T) {
	pattern := `[a-c]`
	exec := generateAndCompile(t, pattern, 0)
	runMatch(t, pattern, exec, "ééb", " 0: b")
	runNoMatch(t, pattern, exec, "ééz")
}

func TestGeneratedStringPrefixFilterHandlesInvalidUTF8(t *testing.T) {
	for _, pattern := range []string{"�abc", "(?:�abc|xyz)"} {
		t.Run(pattern, func(t *testing.T) {
			exec := generateAndCompile(t, pattern, 0)
			runMatch(t, pattern, exec, "x\xffabc", " 0: \\xffabc")
		})
	}
}

func TestGeneratedCharacterClassUsesContains(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `[\p{Greek}x]`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	if strings.Contains(got, `.CharIn(`) {
		t.Fatalf("generated character-class checks still copy CharSet values:\n%s", got)
	}
	if !strings.Contains(got, `.Contains(`) {
		t.Fatalf("generated character-class checks do not use CharSet.Contains:\n%s", got)
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
	if !strings.Contains(got, `LeftContextKnown:`) || !strings.Contains(got, `LeftContextRunes:`) {
		t.Fatalf("generated RegisterEngine call does not publish left-context decode metadata:\n%s", got)
	}
}

func TestGeneratedQuickExecuteElidesUnusedCapture(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `(a+)b`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	if !strings.Contains(got, `ExecuteQuick:`) || !strings.Contains(got, `MyPattern_ExecuteQuick,`) {
		t.Fatalf("generated engine does not register a quick executor:\n%s", got)
	}
	if !strings.Contains(got, `func MyPattern_ExecuteQuick(r *regexp2.Runner) error`) {
		t.Fatalf("generated engine does not define its quick executor:\n%s", got)
	}
	quick := got[strings.Index(got, `func MyPattern_ExecuteQuick`):]
	if strings.Contains(quick, `r.Capture(1,`) {
		t.Fatalf("quick executor retains unused capture 1:\n%s", quick)
	}
	if !strings.Contains(quick, `r.Capture(0,`) {
		t.Fatalf("quick executor does not retain the match-success capture:\n%s", quick)
	}
}

func TestGeneratedQuickExecuteRetainsReferencedCapture(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `(a+)\1`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	if got := buf.String(); strings.Contains(got, `ExecuteQuick:`) {
		t.Fatalf("generated engine emitted a redundant quick executor when every capture is observable:\n%s", got)
	}
}

func TestGeneratedBacktrackingStackLimitCheck(t *testing.T) {
	for _, tt := range []struct {
		name       string
		option     string
		wantChecks bool
	}{
		{name: "bounded", option: "regexp2.OptionMaxBacktrackingStackSize(4096)", wantChecks: true},
		{name: "unbounded", option: "regexp2.OptionMaxBacktrackingStackSize(-1)", wantChecks: false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			c, err := newConverter(&buf, "main")
			if err != nil {
				t.Fatal(err)
			}
			if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `(a|b)*c`, 0, false, []string{tt.option}); err != nil {
				t.Fatal(err)
			}
			if err := c.addFooter(); err != nil {
				t.Fatal(err)
			}

			got := buf.String()
			hasChecks := strings.Contains(got, `r.StackDepth() >`) && strings.Contains(got, `return regexp2.ErrBacktrackingStackLimit`)
			if hasChecks != tt.wantChecks {
				t.Fatalf("generated stack checks = %v, want %v:\n%s", hasChecks, tt.wantChecks, got)
			}
		})
	}
}

func TestGetMaxBacktrackingStackSize(t *testing.T) {
	for _, tt := range []struct {
		name    string
		options []string
		want    int
	}{
		{name: "default", want: regexp2.DefaultOptimizationOptions.MaxBacktrackingStackSize},
		{name: "constant expression", options: []string{"regexp2.OptionMaxBacktrackingStackSize(64 * 1024)"}, want: 64 * 1024},
		{name: "unbounded", options: []string{"regexp2.OptionMaxBacktrackingStackSize(-1)"}, want: -1},
	} {
		t.Run(tt.name, func(t *testing.T) {
			got, err := getMaxBacktrackingStackSize(tt.options)
			if err != nil {
				t.Fatal(err)
			}
			if got != tt.want {
				t.Fatalf("getMaxBacktrackingStackSize() = %d, want %d", got, tt.want)
			}
		})
	}
}

func TestGeneratedBeginningEndAnchoredExactLengthCheck(t *testing.T) {
	var buf bytes.Buffer
	c, err := newConverter(&buf, "main")
	if err != nil {
		t.Fatal(err)
	}

	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", `\Aabc\z`, 0, false, nil); err != nil {
		t.Fatal(err)
	}
	if err := c.addFooter(); err != nil {
		t.Fatal(err)
	}

	got := buf.String()
	if !strings.Contains(got, `pos == 0 && len(r.Runtext) == 3`) {
		t.Fatalf("generated FindFirstChar does not include exact-length anchored fast path:\n%s", got)
	}
}

func TestCollectStaticCompileCalls_CompositeLiterals(t *testing.T) {
	tests := []struct {
		name       string
		expr       string
		wantPat    []string
		wantOpts   []int
		wantOption [][]string
	}{
		{
			name: "slice",
			expr: `[]*regexp2.Regexp{
				regexp2.MustCompile("^test1$", regexp2.Compiled),
				regexp2.MustCompile("^test2$", regexp2.Compiled),
			}`,
			wantPat:    []string{"^test1$", "^test2$"},
			wantOpts:   []int{1 << 3, 1 << 3},
			wantOption: [][]string{{"regexp2.Compiled"}, {"regexp2.Compiled"}},
		},
		{
			name: "struct pointer",
			expr: `&regexReplace{
				Regex:       regexp2.MustCompile("^foo$", regexp2.Compiled),
				Replacement: "bar",
			}`,
			wantPat:    []string{"^foo$"},
			wantOpts:   []int{1 << 3},
			wantOption: [][]string{{"regexp2.Compiled"}},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			node, err := parser.ParseExpr(tt.expr)
			if err != nil {
				t.Fatal(err)
			}

			matches, compileErr := collectStaticCompileCalls(node, "regexp2")
			if compileErr != nil {
				t.Fatal(compileErr)
			}
			if len(matches) != len(tt.wantPat) {
				t.Fatalf("collectStaticCompileCalls returned %d matches, want %d", len(matches), len(tt.wantPat))
			}
			for i, match := range matches {
				if match.Pattern != tt.wantPat[i] {
					t.Fatalf("match[%d].Pattern = %q, want %q", i, match.Pattern, tt.wantPat[i])
				}
				if match.Opts != tt.wantOpts[i] {
					t.Fatalf("match[%d].Opts = %v, want %v", i, match.Opts, tt.wantOpts[i])
				}
				if len(match.CompileOptions) != len(tt.wantOption[i]) {
					t.Fatalf("match[%d].CompileOptions = %#v, want %#v", i, match.CompileOptions, tt.wantOption[i])
				}
				for j := range match.CompileOptions {
					if match.CompileOptions[j] != tt.wantOption[i][j] {
						t.Fatalf("match[%d].CompileOptions = %#v, want %#v", i, match.CompileOptions, tt.wantOption[i])
					}
				}
			}
		})
	}
}

func TestDiscoverStaticCompileCalls_FullFileScan(t *testing.T) {
	const src = `package main

import "github.com/dlclark/regexp2/v2"

func registerRegex(*regexp2.Regexp) {}
func makeRegex(*regexp2.Regexp) *regexp2.Regexp { return nil }
func condition(*regexp2.Regexp) bool { return false }

func test(ch chan *regexp2.Regexp) *regexp2.Regexp {
	var direct = regexp2.MustCompile("^direct$", regexp2.Compiled)
	var inSlice = []*regexp2.Regexp{
		regexp2.MustCompile("^slice$", regexp2.Compiled),
	}
	assigned := makeRegex(regexp2.MustCompile("^assigned$", regexp2.Compiled))
	registerRegex(regexp2.MustCompile("^standalone$", regexp2.Compiled))
	defer registerRegex(regexp2.MustCompile("^defer$", regexp2.Compiled))
	go registerRegex(regexp2.MustCompile("^go$", regexp2.Compiled))
	if condition(regexp2.MustCompile("^if$", regexp2.Compiled)) {
		return regexp2.MustCompile("^return-if$", regexp2.Compiled)
	}
	for condition(regexp2.MustCompile("^for$", regexp2.Compiled)) {
		ch <- regexp2.MustCompile("^send$", regexp2.Compiled)
		break
	}
	_ = direct
	_ = inSlice
	return assigned
}`

	file, err := parser.ParseFile(token.NewFileSet(), "input.go", src, parser.SkipObjectResolution)
	if err != nil {
		t.Fatal(err)
	}

	matches, compileErr := discoverStaticCompileCalls(file, "regexp2")
	if compileErr != nil {
		t.Fatal(compileErr)
	}

	want := []struct {
		name    string
		pattern string
	}{
		{name: "direct", pattern: "^direct$"},
		{name: "inSlice", pattern: "^slice$"},
		{name: "assigned", pattern: "^assigned$"},
		{pattern: "^standalone$"},
		{pattern: "^defer$"},
		{pattern: "^go$"},
		{pattern: "^if$"},
		{pattern: "^return-if$"},
		{pattern: "^for$"},
		{pattern: "^send$"},
	}

	if len(matches) != len(want) {
		t.Fatalf("discoverStaticCompileCalls returned %d matches, want %d", len(matches), len(want))
	}
	for i, match := range matches {
		if match.Name != want[i].name {
			t.Fatalf("match[%d].Name = %q, want %q", i, match.Name, want[i].name)
		}
		if match.Pattern != want[i].pattern {
			t.Fatalf("match[%d].Pattern = %q, want %q", i, match.Pattern, want[i].pattern)
		}
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
