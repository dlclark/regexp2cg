package main

import (
	"bytes"
	"crypto/sha256"
	"go/format"
	"strconv"

	"fmt"
	"io"
	"reflect"
	"slices"
	"strings"
	"unicode"

	"github.com/dlclark/regexp2/syntax"
	"github.com/pkg/errors"
)

type converter struct {
	// buffer for our output
	buf *bytes.Buffer
	// writer from the consumer
	out io.Writer

	data []*regexpData
	// global helpers across the package
	requiredHelpers map[string]string

	convertedNames map[string]int

	err error
}

func newConverter(out io.Writer, packageName string) (*converter, error) {
	c := &converter{
		buf:             &bytes.Buffer{},
		out:             out,
		requiredHelpers: make(map[string]string),
		convertedNames:  make(map[string]int),
	}
	if err := c.addHeader(packageName); err != nil {
		return nil, err
	}

	return c, nil
}

func (c *converter) addHeader(packageName string) error {
	// TODO: this
	// add package and imports
	/*
		package regexp2codegen

		import (
			"github.com/dlclark/regexp2"
		)
	*/
	c.writeLineFmt("package %s", packageName)
	c.writeLine("import (")
	c.writeLine("  \"github.com/dlclark/regexp2\"")
	c.writeLine("  \"github.com/dlclark/regexp2/helpers\"")
	c.writeLine("  \"github.com/dlclark/regexp2/syntax\"")
	c.writeLine("  \"unicode\"")
	//c.writeLine("  \"fmt\"")
	c.writeLine(")")

	return c.err
}

func (c *converter) addFooter() error {
	/*
			func init() {
				regexp2.RegisterEngine("ABCD+", regexp2.ECMAScript, &MyPattern_Engine{})
		   }
	*/

	// emit helpers
	for _, val := range c.requiredHelpers {
		c.writeLine(val)
	}

	// emit init func
	c.writeLine("func init() {")
	for _, rm := range c.data {
		c.writeLineFmt("regexp2.RegisterEngine(%v, %v, &%s_Engine{})", getGoLiteral(rm.Pattern), getOptString(rm.Options), rm.GeneratedName)
	}
	// emit basic usage of imports so we don't have to deal with import re-writing
	c.writeLine("var _ = helpers.Min")
	c.writeLine("var _ = syntax.NewCharSetRuntime")
	c.writeLine("var _ = unicode.IsDigit")
	c.writeLine("}")

	//format the code
	origCode := c.buf.Bytes()
	fmtOut, err := format.Source(origCode)

	if err != nil {
		c.out.Write(origCode)
		return err
	}
	c.out.Write(fmtOut)

	return c.err
}

type regexpData struct {
	SourceLocation string
	GeneratedName  string
	Pattern        string
	Options        syntax.RegexOptions
	Tree           *syntax.RegexTree
	Analysis       *analysisResults

	// parsing state
	findEndsInAlwaysReturningTrue bool
	noMatchFoundLabelNeeded       bool

	// In some cases, we need to emit declarations at the beginning of the method, but we only discover we need them later.
	// To handle that, we build up a collection of all the declarations to include and switch the underlying writer to
	// another writer so we can merge at the end
	additionalDeclarations []string

	// state during emitExecute
	usedNames             map[string]int
	sliceSpan             string
	sliceStaticPos        int
	topLevelDoneLabel     string
	expressionHasCaptures bool
	doneLabel             string

	// track our labels since Go doesn't like unused labels, we need to find them and
	// remove them as a post-process step
	emittedLabels []string
	usedLabels    []string
	//TODO: timeout?
	//TODO: string vs rune vs byte?
}

func (rm *regexpData) unusedLabels() []string {
	var retval []string

	for _, s := range rm.emittedLabels {
		if !slices.Contains(rm.usedLabels, s) {
			retval = append(retval, s)
		}
	}

	return retval
}
func (rm *regexpData) addLocalDec(dec string) {
	// prevent dupes
	if slices.Contains(rm.additionalDeclarations, dec) {
		return
	}
	rm.additionalDeclarations = append(rm.additionalDeclarations, dec)
}

func (c *converter) addRegexp(sourceLocation, name string, txt string, opt syntax.RegexOptions) error {
	// check if already converted
	for _, data := range c.data {
		// match!  we're done here
		if data.Pattern == txt && data.Options == opt {
			return nil
		}
	}

	// parse pattern
	tree, err := syntax.Parse(txt, opt|syntax.Compiled)
	if err != nil {
		return errors.Wrap(err, "error parsing regexp")
	}
	if err := supportsCodeGen(tree); err != nil {
		return errors.Wrap(err, "code generation not supported")
	}

	// generate unique class name
	newName := name
	for {
		if _, ok := c.convertedNames[newName]; ok {
			// name already exists, increment the number on the base name and try again
			c.convertedNames[name]++
			val := c.convertedNames[name]
			newName = fmt.Sprint(name, "_", val)
		} else {
			break
		}
	}
	c.convertedNames[newName] = 1

	oldOut := c.buf
	buf := &bytes.Buffer{}
	c.buf = buf

	c.writeLineFmt("/*\n%s\n*/", tree.Dump())

	rm := &regexpData{
		SourceLocation: sourceLocation,
		GeneratedName:  newName,
		Pattern:        txt,
		Options:        opt,
		Tree:           tree,
		Analysis:       analyze(tree),
	}
	c.data = append(c.data, rm)

	c.emitRegexStart(rm)

	// we need to emit 2 functions: FindFirstChar() and Execute()
	// the C# version has a "scan" function above these that I've omitted here
	c.emitFindFirstChar(rm)
	c.emitExecute(rm)

	// get our string for final manipulation
	output := buf.String()
	c.buf = oldOut

	// finalize our code
	removeUnusedLabels(&output, rm)

	// write our temp out buffer into our saved buffer
	c.buf.Write([]byte(output))

	return c.err
}

func removeUnusedLabels(output *string, rm *regexpData) {
	unusedLabels := rm.unusedLabels()

	// find and remove the unused labels in the output
	for _, label := range unusedLabels {
		// the label is on its own line with a colon at the end
		*output = strings.ReplaceAll(*output, "\n"+label+":\n", "\n")
		// or the label could be on its own line with a semicolon at the end
		*output = strings.ReplaceAll(*output, "\n"+label+": ;\n", "\n")
	}
}

func (c *converter) emitRegexStart(rm *regexpData) {

	/*
		// From ABC.go:120:10
		// Pattern: [ABCD]+
		// Options: regexp2.ECMAScript
		type MyPattern0_Engine struct{}

		func (MyPattern0_Engine) Caps() map[int]int        { return map[int]int{} }
		func (MyPattern0_Engine) CapNames() map[string]int { return map[string]int{} }
		func (MyPattern0_Engine) CapsList() []string       { return []string{} }
		func (MyPattern0_Engine) CapSize() int             { return 1 }
	*/
	caps, capsize := getCaps(rm.Tree)
	rm.Tree.Caps = caps
	rm.Tree.Captop = capsize

	c.writeLineFmt("// From %s", rm.SourceLocation)
	c.writeLineFmt("// Pattern: %#v", rm.Pattern)
	c.writeLineFmt("// Options: %v", getOptString(rm.Options))
	c.writeLineFmt("type %s_Engine struct{}", rm.GeneratedName)
	c.writeLineFmt("func (%s_Engine) Caps() map[int]int { return %s }", rm.GeneratedName, getGoLiteral(caps))
	c.writeLineFmt("func (%s_Engine) CapNames() map[string]int { return %s }", rm.GeneratedName, getGoLiteral(rm.Tree.Capnames))
	c.writeLineFmt("func (%s_Engine) CapsList() []string { return %s }", rm.GeneratedName, getGoLiteral(rm.Tree.Caplist))
	c.writeLineFmt("func (%s_Engine) CapSize() int { return %v }", rm.GeneratedName, capsize)
	c.writeLine("")
}

var optNames = []string{
	"IgnoreCase",
	"Multiline",
	"ExplicitCapture",
	"Compiled",
	"Singleline",
	"IgnorePatternWhitespace",
	"RightToLeft",
	"Debug",
	"ECMAScript",
	"RE2",
	"Unicode",
}

func getOptString(opts syntax.RegexOptions) string {
	if opts == 0 {
		return "regexp2.None"
	}

	stringOpts := []string{}
	remain := int(opts)
	for i, v := range optNames {
		//bit := i + 1
		mask := 1 << i
		// check if this bit is enabled in opts
		if remain&mask != 0 {
			remain &= ^mask
			stringOpts = append(stringOpts, "regexp2."+v)
		}
		// once we're out of options, stop looping
		if remain == 0 {
			break
		}
	}
	if remain > 0 {
		stringOpts = append(stringOpts, strconv.Itoa(remain))
	}
	return strings.Join(stringOpts, "|")
}

func isNilish(val any) bool {
	if val == nil {
		return true
	}

	v := reflect.ValueOf(val)
	k := v.Kind()
	switch k {
	case reflect.Chan, reflect.Func, reflect.Map, reflect.Pointer,
		reflect.UnsafePointer, reflect.Interface, reflect.Slice:
		return v.IsNil()
	}

	return false
}

func getGoLiteral(in any) string {
	if isNilish(in) {
		return "nil"
	}
	switch in.(type) {
	case rune:
		return fmt.Sprintf("%q", in)
	}
	return fmt.Sprintf("%#v", in)
}

func getCaps(tree *syntax.RegexTree) (caps map[int]int, capSize int) {
	if tree.Capnumlist == nil || tree.Captop == len(tree.Capnumlist) {
		return nil, tree.Captop
	}

	capSize = len(tree.Capnumlist)
	caps = tree.Caps
	for i := 0; i < len(tree.Capnumlist); i++ {
		caps[tree.Capnumlist[i]] = i
	}

	return caps, capSize
}

func getRuneSliceLiteral[T []rune | string](in T) string {
	return fmt.Sprintf("[]rune(%#v)", string(in))
}

func getRuneLiteralParams(in []rune) string {
	if len(in) == 0 {
		return ""
	}

	buf := &bytes.Buffer{}
	sep := "'"
	for _, ch := range in {
		buf.WriteString(sep)
		buf.WriteRune(ch)
		sep = "', '"
	}
	buf.WriteRune('\'')
	return buf.String()
}

// Determines whether its ok to embed the string in the field name.
func isValidInFieldName(str string) bool {
	for _, c := range str {
		if unicode.IsLetter(c) || c == '_' || unicode.IsDigit(c) {
			continue
		}
		return false
	}
	return true
}

func getSHA256FieldName(toEncode string) string {
	sha := sha256.New()
	sha.Write([]byte(toEncode))
	bs := sha.Sum(nil)
	return fmt.Sprintf("%x", bs)
}

func shouldUseSearchValues(chars []rune) bool {
	//TODO: perf optimizations will be different for Go
	// these are from C#
	// IndexOfAny(SearchValues) is faster than a regular IndexOfAny("abcd") if:
	// - There are more than 5 characters in the needle, or
	// - There are only 4 or 5 characters in the needle and they're all ASCII.
	if len(chars) > 5 {
		return true
	}

	if len(chars) < 4 {
		return false
	}

	return isAscii(chars)
}

func isAscii(chars []rune) bool {
	for _, c := range chars {
		if c > unicode.MaxASCII {
			return false
		}
	}
	return true
}

func (c *converter) emitIndexOfChars(chars []rune, negate bool, spanName string) string {
	// We have a chars array, so we can use IndexOf{Any}{Except} to search for it. Choose the best overload.
	// 1, 2, 3 have dedicated optimized IndexOfAny overloads
	// 4, 5 have dedicated optimized IndexOfAny overloads accessible via the ReadOnlySpan<char> overload,
	// but can also be handled via SearchValues
	// > 5 can only be handled efficiently via SearchValues
	var indexOfAnyName = "IndexOfAny"
	if negate {
		indexOfAnyName = "IndexOfAnyExcept"
	}

	switch len(chars) {
	case 1:
		return fmt.Sprintf("helpers.%s1(%s, %q)", indexOfAnyName, spanName, chars[0])
	case 2:
		return fmt.Sprintf("helpers.%s2(%s, %q, %q)", indexOfAnyName, spanName, chars[0], chars[1])
	case 3:
		return fmt.Sprintf("helpers.%s3(%s, %q, %q, %q)", indexOfAnyName, spanName, chars[0], chars[1], chars[2])
	case 4, 5:
		if shouldUseSearchValues(chars) {
			return fmt.Sprintf("%s.%s(%s)", c.emitSearchValues(chars, ""), indexOfAnyName, spanName)
		} else {
			return fmt.Sprintf("helpers.%s(%s, %s)", indexOfAnyName, spanName, getRuneSliceLiteral(chars))
		}
	}
	return fmt.Sprintf("%s.%s(%s)", c.emitSearchValues(chars, ""), indexOfAnyName, spanName)
}

var emitSearchValueConstNames = map[string]string{
	"FFFFFFFF000000000000000000000080": "svAsciiControl",
	"000000000000FF030000000000000000": "svAsciiDigits",
	"0000000000000000FEFFFF07FEFFFF07": "svAsciiLetters",
	"000000000000FF03FEFFFF07FEFFFF07": "svAsciiLettersAndDigits",
	"000000000000FF037E0000007E000000": "svAsciiHexDigits",
	"000000000000FF03000000007E000000": "svAsciiHexDigitsLower",
	"000000000000FF037E00000000000000": "svAsciiHexDigitsUpper",
	"00000000EEF7008C010000B800000028": "svAsciiPunctuation",
	"00000000010000000000000000000000": "svAsciiSeparators",
	"00000000100800700000004001000050": "svAsciiSymbols",
	"003E0000010000000000000000000000": "svAsciiWhiteSpace",
	"000000000000FF03FEFFFF87FEFFFF07": "svAsciiWordChars",

	"00000000FFFFFFFFFFFFFFFFFFFFFF7F": "svAsciiExceptControl",
	"FFFFFFFFFFFF00FCFFFFFFFFFFFFFFFF": "svAsciiExceptDigits",
	"FFFFFFFFFFFFFFFF010000F8010000F8": "svAsciiExceptLetters",
	"FFFFFFFFFFFF00FC010000F8010000F8": "svAsciiExceptLettersAndDigits",
	"FFFFFFFFFFFFFFFFFFFFFFFF010000F8": "svAsciiExceptLower",
	"FFFFFFFF1108FF73FEFFFF47FFFFFFD7": "svAsciiExceptPunctuation",
	"FFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFF": "svAsciiExceptSeparators",
	"FFFFFFFFEFF7FF8FFFFFFFBFFEFFFFAF": "svAsciiExceptSymbols",
	"FFFFFFFFFFFFFFFF010000F8FFFFFFFF": "svAsciiExceptUpper",
	"FFC1FFFFFEFFFFFFFFFFFFFFFFFFFFFF": "svAsciiExceptWhiteSpace",
	"FFFFFFFFFFFF00FC01000078010000F8": "svAsciiExceptWordChars",
}

func (c *converter) emitSearchValues(chars []rune, fieldName string) string {
	slices.Sort(chars)
	asciiOnly := isAscii(chars)
	if len(fieldName) == 0 {
		if asciiOnly {
			// The set of ASCII characters can be represented as a 128-bit bitmap. Use the 16-byte hex string as the key.
			bitmap := make([]byte, 16)
			for _, c := range chars {
				bitmap[c>>3] |= (byte)(1 << (c & 7))
			}
			hexBitmap := fmt.Sprintf("%x", bitmap)
			var ok bool
			fieldName, ok = emitSearchValueConstNames[hexBitmap]
			if !ok {
				fieldName = "svAscii" + strings.TrimLeft(hexBitmap, "0")
			}
		} else {
			fieldName = "sNonAscii" + getSHA256FieldName(string(chars))
		}
	}

	if _, ok := c.requiredHelpers[fieldName]; !ok {
		if asciiOnly {
			c.requiredHelpers[fieldName] = fmt.Sprintf(`// Supports searching for the chars in or not in %#v
			var %v = helpers.NewAsciiSearchValues(%#v)`,
				string(chars), fieldName, string(chars))
		} else {
			c.requiredHelpers[fieldName] = fmt.Sprintf(`// Supports searching for the chars in or not in %#v
			var %v = helpers.NewRuneSearchValues(%#v)`,
				string(chars), fieldName, string(chars))
		}
	}

	return fieldName
}

func (c *converter) emitGoto(label string) {
	c.writeLineFmt("goto %s", label)
}

func (c *converter) emitLabel(label string) {
	c.writeLineFmt("%s:", label)
}

func (c *converter) write(data string) {
	_, err := fmt.Fprint(c.buf, data)
	if err != nil {
		c.err = err
	}
}
func (c *converter) writeLine(line string) {
	_, err := fmt.Fprintln(c.buf, line)
	if err != nil {
		c.err = err
	}
}

func (c *converter) writeLineFmt(format string, args ...any) {
	_, err := fmt.Fprintf(c.buf, format, args...)
	if err != nil {
		c.err = err
	}
	_, err = c.buf.Write([]byte{'\n'})
	if err != nil {
		c.err = err
	}
}

func supportsCodeGen(tree *syntax.RegexTree) error {
	//TODO: filter out invalid trees
	//https://github.com/dotnet/runtime/blob/main/src/libraries/System.Text.RegularExpressions/gen/RegexGenerator.cs#L296
	return nil
}

// helper to make ident names unique, add nums for dupes
func (rm *regexpData) reserveName(prefix string) string {
	num := rm.usedNames[prefix]
	rm.usedNames[prefix] = num + 1
	if num == 0 {
		return prefix
	}
	return fmt.Sprint(prefix, num)
}
