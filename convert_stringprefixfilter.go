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
	case syntax.LeadingSet_LeftToRight:
		if len(opts.FixedDistanceSets) > 0 {
			c.emitStringPrefixFilterForLeadingSet(rm, opts.FixedDistanceSets[0])
		}
	}
}

func (c *converter) emitStringPrefixFilterForLeadingSet(rm *regexpData, set syntax.FixedDistanceSet) {
	// regexp2's string prefilter only handles positive ASCII ranges and small
	// enumerated ASCII sets. A leading set is always at distance zero, so its
	// byte position is also the candidate match's byte position.
	if set.Negated || set.Distance != 0 {
		return
	}

	indexExpr := ""
	if set.Range != nil {
		if set.Range.First < 0 || set.Range.Last >= 0x80 {
			return
		}
		if set.Range.First == set.Range.Last {
			indexExpr = fmt.Sprintf("strings.IndexByte(input[startAt:], %q)", byte(set.Range.First))
		}
	} else {
		if len(set.Chars) == 0 || len(set.Chars) > 5 {
			return
		}
		chars := make([]byte, len(set.Chars))
		for i, ch := range set.Chars {
			if ch < 0 || ch >= 0x80 {
				return
			}
			chars[i] = byte(ch)
		}
		if len(chars) == 1 {
			indexExpr = fmt.Sprintf("strings.IndexByte(input[startAt:], %q)", chars[0])
		} else {
			indexExpr = fmt.Sprintf("strings.IndexAny(input[startAt:], %q)", string(chars))
		}
	}

	name := fmt.Sprintf("%s_StringPrefixFilter", rm.GeneratedName)
	rm.StringPrefixFilterName = name
	if indexExpr != "" {
		c.writeLineFmt(`func %[1]s(input string, startAt int) (int, bool) {
	if startAt < 0 || startAt > len(input) || len(input)-startAt < %[2]d {
		return 0, false
	}
	offset := %[3]s
	if offset < 0 {
		return 0, false
	}
	return startAt + offset, true
}
`, name, rm.Tree.FindOptimizations.MinRequiredLength, indexExpr)
		return
	}

	// strings has no byte-range search helper, so emit the small scan directly.
	c.writeLineFmt(`func %[1]s(input string, startAt int) (int, bool) {
	if startAt < 0 || startAt > len(input) || len(input)-startAt < %[2]d {
		return 0, false
	}
	for i := startAt; i < len(input); i++ {
		if input[i] >= %[3]q && input[i] <= %[4]q {
			return i, true
		}
	}
	return 0, false
}
`, name, rm.Tree.FindOptimizations.MinRequiredLength, byte(set.Range.First), byte(set.Range.Last))
}

func (c *converter) emitStringPrefixFilterForPrefix(rm *regexpData, prefix string, ignoreCase bool) {
	if prefix == "" || (ignoreCase && !isASCIIString(prefix)) {
		return
	}

	name := fmt.Sprintf("%s_StringPrefixFilter", rm.GeneratedName)
	rm.StringPrefixFilterName = name

	indexExpr := fmt.Sprintf("strings.Index(input[startAt:], %#[1]v)", prefix)
	if ignoreCase {
		indexExpr = fmt.Sprintf("helpers.IndexStringIgnoreCaseASCII(input[startAt:], %#[1]v)", prefix)
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
		c.writeLine(`		offset = helpers.IndexStringIgnoreCaseASCII(remaining, prefix)`)
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

func isASCIIString(s string) bool {
	for i := 0; i < len(s); i++ {
		if s[i] >= 0x80 {
			return false
		}
	}
	return true
}
