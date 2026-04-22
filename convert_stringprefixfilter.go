package main

import (
	"fmt"

	"github.com/dlclark/regexp2/v2/syntax"
)

func (c *converter) emitStringPrefixFilter(rm *regexpData) {
	opts := rm.Tree.FindOptimizations
	if opts == nil || rm.Options&syntax.RightToLeft != 0 {
		return
	}

	switch opts.FindMode {
	case syntax.LeadingString_LeftToRight:
		c.emitStringPrefixFilterForPrefix(rm, opts.LeadingPrefix, false)
	case syntax.LeadingString_OrdinalIgnoreCase_LeftToRight:
		c.emitStringPrefixFilterForPrefix(rm, opts.LeadingPrefix, true)
	case syntax.LeadingStrings_LeftToRight:
		c.emitStringPrefixFilterForPrefixes(rm, opts.LeadingPrefixes, false)
	case syntax.LeadingStrings_OrdinalIgnoreCase_LeftToRight:
		c.emitStringPrefixFilterForPrefixes(rm, opts.LeadingPrefixes, true)
	}
}

func (c *converter) emitStringPrefixFilterForPrefix(rm *regexpData, prefix string, ignoreCase bool) {
	if prefix == "" || (ignoreCase && !isASCIIString(prefix)) {
		return
	}
	if ignoreCase {
		c.ensureStringPrefixFilterASCIIIgnoreCaseHelper()
	}

	name := fmt.Sprintf("%s_StringPrefixFilter", rm.GeneratedName)
	rm.StringPrefixFilterName = name

	indexExpr := fmt.Sprintf("strings.Index(input[startAt:], %#[1]v)", prefix)
	if ignoreCase {
		indexExpr = fmt.Sprintf("regexp2cgIndexASCIIIgnoreCase(input[startAt:], %#[1]v)", prefix)
	}

	c.writeLineFmt(`func %[1]s(input string, startAt int) (int, bool) {
	if startAt < 0 || startAt > len(input) {
		return 0, false
	}
	if len(input)-startAt < %[2]d {
		return 0, false
	}
	offset := %[3]s
	if offset < 0 {
		return 0, false
	}
	return startAt + offset, true
}
`, name, rm.Tree.FindOptimizations.MinRequiredLength, indexExpr)
}

func (c *converter) emitStringPrefixFilterForPrefixes(rm *regexpData, prefixes []string, ignoreCase bool) {
	if len(prefixes) == 0 {
		return
	}
	if ignoreCase {
		for _, prefix := range prefixes {
			if !isASCIIString(prefix) {
				return
			}
		}
		c.ensureStringPrefixFilterASCIIIgnoreCaseHelper()
	}

	prefixesName := fmt.Sprintf("stringPrefixFilterPrefixes_%s", getSHA256FieldName(fmt.Sprint(prefixes, ignoreCase)))
	if _, ok := c.requiredHelpers[prefixesName]; !ok {
		c.requiredHelpers[prefixesName] = fmt.Sprintf("var %s = %s", prefixesName, getGoLiteral(prefixes))
	}

	name := fmt.Sprintf("%s_StringPrefixFilter", rm.GeneratedName)
	rm.StringPrefixFilterName = name

	c.writeLineFmt(`func %[1]s(input string, startAt int) (int, bool) {
	if startAt < 0 || startAt > len(input) {
		return 0, false
	}
	if len(input)-startAt < %[2]d {
		return 0, false
	}

	best := -1
	remaining := input[startAt:]
	for _, prefix := range %[3]s {
		var offset int
`, name, rm.Tree.FindOptimizations.MinRequiredLength, prefixesName)
	if ignoreCase {
		c.writeLine(`		offset = regexp2cgIndexASCIIIgnoreCase(remaining, prefix)`)
	} else {
		c.writeLine(`		offset = strings.Index(remaining, prefix)`)
	}
	c.writeLine(`		if offset >= 0 && (best < 0 || offset < best) {
			best = offset
		}
	}
	if best < 0 {
		return 0, false
	}
	return startAt + best, true
}
`)
}

func (c *converter) ensureStringPrefixFilterASCIIIgnoreCaseHelper() {
	const name = "regexp2cgIndexASCIIIgnoreCase"
	if _, ok := c.requiredHelpers[name]; ok {
		return
	}

	c.requiredHelpers[name] = `func regexp2cgIndexASCIIIgnoreCase(s, prefix string) int {
	if len(prefix) == 0 {
		return 0
	}
	end := len(s) - len(prefix)
	for i := 0; i <= end; i++ {
		if regexp2cgEqualASCIIIgnoreCase(s[i:i+len(prefix)], prefix) {
			return i
		}
	}
	return -1
}

func regexp2cgEqualASCIIIgnoreCase(s, prefix string) bool {
	for i := 0; i < len(prefix); i++ {
		if regexp2cgFoldASCII(s[i]) != regexp2cgFoldASCII(prefix[i]) {
			return false
		}
	}
	return true
}

func regexp2cgFoldASCII(c byte) byte {
	if 'A' <= c && c <= 'Z' {
		return c + ('a' - 'A')
	}
	return c
}`
}

func isASCIIString(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] >= 0x80 {
			return false
		}
	}
	return true
}
