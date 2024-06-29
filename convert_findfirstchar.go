package main

import (
	"bytes"
	"fmt"
	"math"
	"unicode"

	"github.com/dlclark/regexp2/syntax"
)

func (c *converter) emitFindFirstChar(rm *regexpData) {
	c.writeLineFmt("func (%s_Engine) FindFirstChar(r *regexp2.Runner) bool {", rm.GeneratedName)
	defer func() {
		c.writeLine("}\n")
	}()

	rtl := rm.Options&syntax.RightToLeft != 0
	root := rm.Tree.Root.Children[0]

	if root.T == syntax.NtEmpty {
		// we always match the current char since we match the empty string
		c.writeLine("return true")
		return
	}
	if root.T == syntax.NtNothing {
		// this never matches anything
		c.writeLine("return false")
		return
	}

	needPosVar := true
	oldOut := c.buf
	buf := &bytes.Buffer{}
	c.buf = buf
	defer func() {
		// lets clean this up at the end
		c.buf = oldOut

		if needPosVar {
			c.writeLine("pos := r.Runtextpos")
		}

		// write additionalDeclarations
		for _, l := range rm.additionalDeclarations {
			c.writeLine(l)
		}

		//reset
		rm.additionalDeclarations = []string{}

		// then write our temp out buffer into our saved buffer
		c.buf.Write(buf.Bytes())
	}()

	// Generate length check.  If the input isn't long enough to possibly match, fail quickly.
	// It's rare for min required length to be 0, so we don't bother special-casing the check,
	// especially since we want the "return false" code regardless.
	minRequiredLength := rm.Tree.FindOptimizations.MinRequiredLength
	endBlock := ""
	if minRequiredLength > 0 {
		if minRequiredLength == 1 {
			c.writeLine("// Empty matches aren't possible")
			if !rtl {
				c.writeLine("if pos < len(r.Runtext) {")
			} else {
				c.writeLine("if pos > 1 {")
			}
		} else {
			c.writeLineFmt("// Any possible match is at least %v characters", minRequiredLength)
			if !rtl {
				c.writeLineFmt("if pos <= len(r.Runtext) - %v {", minRequiredLength)
			} else {
				c.writeLineFmt("if pos >= %v {", minRequiredLength)
			}
		}
		endBlock = "}"
	}

	const NoMatchFound = "NoMatchFound"

	if !c.emitAnchors(rm) {
		// Either anchors weren't specified, or they don't completely root all matches to a specific location.

		// Emit the code for whatever find mode has been determined.
		switch rm.Tree.FindOptimizations.FindMode {
		case syntax.LeadingString_LeftToRight, syntax.LeadingString_OrdinalIgnoreCase_LeftToRight, syntax.FixedDistanceString_LeftToRight:
			c.emitIndexOfString_LeftToRight(rm)
		case syntax.LeadingString_RightToLeft:
			c.emitIndexOfString_RightToLeft(rm)
		case syntax.LeadingStrings_LeftToRight, syntax.LeadingStrings_OrdinalIgnoreCase_LeftToRight:
			c.emitIndexOfStrings_LeftToRight(rm)
		case syntax.LeadingSet_LeftToRight, syntax.FixedDistanceSets_LeftToRight:
			c.emitFixedSet_LeftToRight(rm)
		case syntax.LeadingSet_RightToLeft:
			c.emitFixedSet_RightToLeft(rm)
		case syntax.LiteralAfterLoop_LeftToRight:
			c.emitLiteralAfterAtomicLoop(rm)
		default:
			//there's a special case here where we haven't written anything
			// and we don't want to declare the "pos" var
			needPosVar = buf.Len() > 0
			c.writeLine("return true")
			rm.findEndsInAlwaysReturningTrue = true
		}
	}

	if endBlock != "" {
		c.writeLine(endBlock)
	}

	// If the main path is guaranteed to end in a "return true;" and nothing is going to
	// jump past it, we don't need a "return false;" path.
	if minRequiredLength > 0 || !rm.findEndsInAlwaysReturningTrue || rm.noMatchFoundLabelNeeded {
		c.writeLine("\n// No match found")
		if rm.noMatchFoundLabelNeeded {
			c.emitLabel(NoMatchFound)
		}
		var setPos string
		if !rtl {
			setPos = "len(r.Runtext)"
		} else {
			setPos = "0"
		}
		c.writeLineFmt("r.Runtextpos = %v", setPos)
		c.writeLine("return false")
	}
}

func (c *converter) emitAnchors(rm *regexpData) bool {
	regexTree := rm.Tree
	// Anchors that fully implement TryFindNextPossibleStartingPosition, with a check that leads to immediate success or failure determination.
	switch regexTree.FindOptimizations.FindMode {
	case syntax.LeadingAnchor_LeftToRight_Beginning:
		c.writeLine("// The pattern leads with a beginning (\\A) anchor.")
		// If we're at the beginning, we're at a possible match location.  Otherwise,
		// we'll never be, so fail immediately.
		c.writeLine(`if pos == 0 {
			return true
		}`)
		return true

	case syntax.LeadingAnchor_LeftToRight_Start:
	case syntax.LeadingAnchor_RightToLeft_Start:
		c.write("// The pattern leads with a start (\\G) anchor")
		if regexTree.FindOptimizations.FindMode == syntax.LeadingAnchor_RightToLeft_Start {
			c.write(" when processed right to left")
		}

		// For both left-to-right and right-to-left, if we're  currently at the start,
		// we're at a possible match location.  Otherwise, because we've already moved
		// beyond it, we'll never be, so fail immediately.
		c.writeLine(`
			if (pos == r.Runtextstart) {
				return true
			}
		`)
		return true

	case syntax.LeadingAnchor_LeftToRight_EndZ:
		// If we're not currently at the end (or a newline just before it), skip ahead
		// since nothing until then can possibly match.
		c.writeLine(`// The pattern leads with an end (\Z) anchor.
		if pos < len(r.Runtext) - 1 {
			r.Runtextpos = len(r.Runtext) - 1
		}
		return true
		`)
		rm.findEndsInAlwaysReturningTrue = true
		return true

	case syntax.LeadingAnchor_LeftToRight_End:
		// If we're not currently at the end (or a newline just before it), skip ahead
		// since nothing until then can possibly match.
		c.writeLine(`// The pattern leads with an end (\z) anchor.
		if pos < len(r.Runtext) {
			r.Runtextpos = len(r.Runtext)
		}
		return true
		`)
		rm.findEndsInAlwaysReturningTrue = true
		return true

	case syntax.LeadingAnchor_RightToLeft_Beginning:
		c.writeLine(`// The pattern leads with a beginning (\A) anchor when processed right to left.
		if pos != 0 {
			r.Runtextpos = 0
		}
		return true
		`)
		rm.findEndsInAlwaysReturningTrue = true
		return true

	case syntax.LeadingAnchor_RightToLeft_EndZ:
		// If we're currently at the end, we're at a valid position to try.  Otherwise,
		// we'll never be (we're iterating from end to beginning), so fail immediately.
		c.writeLine(`// The pattern leads with an end (\Z) anchor when processed right to left.
		if pos >= len(r.Runtext) - 1 && (pos >= len(r.Runtext) || r.Runtext[pos] == '\n') {
			return true
		}
		`)
		return true

	case syntax.LeadingAnchor_RightToLeft_End:
		// If we're currently at the end, we're at a valid position to try.  Otherwise,
		// we'll never be (we're iterating from end to beginning), so fail immediately.
		c.writeLine(`// The pattern leads with an end (\z) anchor when processed right to left.
		if pos >= len(r.Runtext) {
			return true
		}
		`)
		return true

	case syntax.TrailingAnchor_FixedLength_LeftToRight_EndZ:
		// Jump to the end, minus the min required length, which in this case is actually the fixed length, minus 1 (for a possible ending \n).
		c.writeLineFmt(`// The pattern has a trailing end (\Z) anchor, and any possible match is exactly %v characters.
		if pos < len(r.Runtext) - %v {
			r.Runtextpos = len(r.Runtext) - %[2]v
		}
		return true
		`, regexTree.FindOptimizations.MinRequiredLength, regexTree.FindOptimizations.MinRequiredLength+1)
		rm.findEndsInAlwaysReturningTrue = true
		return true

	case syntax.TrailingAnchor_FixedLength_LeftToRight_End:
		// Jump to the end, minus the min required length, which in this case is actually the fixed length.
		c.writeLineFmt(`// The pattern has a trailing end (\z) anchor, and any possible match is exactly %v characters.
		if pos < len(r.Runtext) - %[1]v {
			r.Runtextpos = len(r.Runtext) - %[1]v
		}
		return true
		`, regexTree.FindOptimizations.MinRequiredLength)
		rm.findEndsInAlwaysReturningTrue = true
		return true
	}

	// Now handle anchors that boost the position but may not determine immediate success or failure.

	if regexTree.FindOptimizations.LeadingAnchor == syntax.NtBol {
		str1 := ">"
		str2 := fmt.Sprint(" - ", regexTree.FindOptimizations.MinRequiredLength)
		if regexTree.FindOptimizations.MinRequiredLength == 0 {
			str2 = ""
		} else if regexTree.FindOptimizations.MinRequiredLength == 1 {
			str1 = ">="
			str2 = ""
		}
		// Optimize the handling of a Beginning-Of-Line (BOL) anchor.  BOL is special, in that unlike
		// other anchors like Beginning, there are potentially multiple places a BOL can match.  So unlike
		// the other anchors, which all skip all subsequent processing if found, with BOL we just use it
		// to boost our position to the next line, and then continue normally with any searches.
		c.writeLineFmt(`// The pattern has a leading beginning-of-line anchor.
			if pos > 0 && r.Runtext[pos-1] != '\n' {
				newlinePos := helpers.IndexOfAny1(r.Runtext[pos:], '\n')
				if newlinePos > len(r.Runtext) - pos - 1 {
					goto NoMatchFound
				}
				pos += newlinePos + 1

				if pos %v len(r.Runtext)%v {
					goto NoMatchFound
				}
			}
			`, str1, str2)
		rm.noMatchFoundLabelNeeded = true
	}

	// if we have a max len
	if regexTree.FindOptimizations.MaxPossibleLength > -1 {
		if regexTree.FindOptimizations.TrailingAnchor == syntax.NtEnd {
			c.writeLineFmt(`// The pattern has a trailing end (\z) anchor, and any possible match is no more than %v characters.
			if pos < len(r.Runtext) - %[1]v {
				pos = len(r.Runtext) - %[1]v
			}
			`, regexTree.FindOptimizations.MaxPossibleLength)
		} else if regexTree.FindOptimizations.TrailingAnchor == syntax.NtEndZ {
			c.writeLineFmt(`// The pattern has a trailing end (\Z) anchor, and any possible match is no more than %v characters.
			if pos < len(r.Runtext) - %[1]v {
				pos = len(r.Runtext) - %[1]v
			}
			`, regexTree.FindOptimizations.MaxPossibleLength+1)
		}

	}

	return false
}

// Emits a case-sensitive left-to-right search for a substring.
func (c *converter) emitIndexOfString_LeftToRight(rm *regexpData) {
	opts := rm.Tree.FindOptimizations

	substring, stringComparison, offset, offsetDescription := "", "", "", ""
	//ignoreCase := false
	switch opts.FindMode {
	case syntax.LeadingString_LeftToRight:
		substring = opts.LeadingPrefix
		offsetDescription = "at the beginning of the pattern"

	case syntax.LeadingString_OrdinalIgnoreCase_LeftToRight:
		substring = opts.LeadingPrefix
		stringComparison = "IgnoreCase"
		offsetDescription = " case-insensitive at the beginning of the pattern"
		//ignoreCase = true

	case syntax.FixedDistanceString_LeftToRight:
		substring = opts.FixedDistanceLiteral.S
		if opts.FixedDistanceLiteral.Distance > 0 {
			offset = fmt.Sprint(" + ", opts.FixedDistanceLiteral.Distance)
			offsetDescription = fmt.Sprint(" at index ", opts.FixedDistanceLiteral.Distance, " in the pattern")
		}
	}

	/*
		TODO: is this needed? not sure a stringsearch is going to add value here

		substringAndComparison := fmt.Sprint(substring, stringComparison)
		fieldName := "sv"
		if isValidInFieldName(substring) {
			fieldName += substringAndComparison
		} else {
			fieldName += getSHA256FieldName(substringAndComparison)
		}

		if _, ok := c.requiredHelpers[fieldName]; !ok {
			c.requiredHelpers[fieldName] = fmt.Sprintf(`// Supports searching for the string %#[1]v
				var %[2]v = helpers.NewStringSearchValues(%#[1]v, %#[3]v)`,
				[]rune(substring), fieldName, ignoreCase)
		}*/

	c.writeLineFmt(`// The pattern has the literal %#v %v. Find the next occurrence.
	// If it can't be found, there's no match
	if i := helpers.IndexOf%v(r.Runtext[pos%v:], %s); i >= 0 {
		r.Runtextpos = i
		return true
	}`, substring, offsetDescription, stringComparison, offset, getRuneSliceLiteral(substring))
}

// Emits a case-sensitive right-to-left search for a substring.
func (c *converter) emitIndexOfString_RightToLeft(rm *regexpData) {
	prefix := rm.Tree.FindOptimizations.LeadingPrefix

	c.writeLineFmt(`// The pattern begins with a literal %#[1]v. Find the next occurrence right-to-left.
	// If it can't be found, there's no match.
	pos = r.LastIndexOf(r.Runtext, pos, []rune(%#[1]v))
	if pos >= 0 {
		r.Runtextpos = pos + %[2]v
		return true
	}
	`, prefix, len(prefix))
}

func getRuneSliceSliceLiteral(vals []string) string {
	buf := &bytes.Buffer{}
	buf.WriteString("[][]rune{")
	sep := ""
	for i := 0; i < len(vals); i++ {
		buf.WriteString(sep)
		buf.WriteString(getRuneSliceLiteral(vals[i]))
		sep = ", "
	}
	buf.WriteString("}")
	return buf.String()
}

// Emits a case-sensitive left-to-right search for any one of multiple leading prefixes.
func (c *converter) emitIndexOfStrings_LeftToRight(rm *regexpData) {
	opts := rm.Tree.FindOptimizations

	prefixes := getRuneSliceSliceLiteral(opts.LeadingPrefixes)
	stringComparison := ""
	ignoreCase := false
	if opts.FindMode == syntax.LeadingStrings_OrdinalIgnoreCase_LeftToRight {
		stringComparison = "_IgnoreCase"
		ignoreCase = true
	}
	fieldName := fmt.Sprint("indexOfAnyStrings", stringComparison, "_", getSHA256FieldName(prefixes))

	if _, ok := c.requiredHelpers[fieldName]; !ok {
		// explicitly using an array in case prefixes is large
		c.requiredHelpers[fieldName] = fmt.Sprintf(`// Supports searching for the specified strings
		var %v = helpers.NewStringSearchValues(%s, %v)`,
			fieldName, prefixes, ignoreCase)
	}

	c.writeLineFmt(`// The pattern has multiple strings that could begin the match. Search for any of them.
	// If none can be found, there's no match
	if i := %v.IndexOfAny(r.Runtext[pos:]); i >= 0 {
		r.Runtextpos = i
		return true
	}`, fieldName)
}

func (c *converter) emitSetDefinition(set *syntax.CharSet) string {
	var hash []byte
	set.HashInto(hash)
	vals := string(hash)

	fieldName := fmt.Sprint("set_", getSHA256FieldName(vals))

	if _, ok := c.requiredHelpers[fieldName]; !ok {
		// explicitly using an array in case prefixes is large
		c.requiredHelpers[fieldName] = fmt.Sprintf(`// The set %v
		var %v = syntax.NewCharSetRuntime(%#v)`,
			set.String(), fieldName, vals)
	}

	return fieldName
}

// Emits a search for a set at a fixed position from the start of the pattern,
// and potentially other sets at other fixed positions in the pattern.
func (c *converter) emitFixedSet_LeftToRight(rm *regexpData) {
	sets := rm.Tree.FindOptimizations.FixedDistanceSets
	primarySet := sets[0]

	const MaxSets = 4
	setsToUse := len(sets)
	if setsToUse > MaxSets {
		setsToUse = MaxSets
	}

	if primarySet.Distance == 0 {
		c.writeLineFmt(`// The pattern begins with %v`, primarySet.Set)
	} else {
		c.writeLineFmt(`// The pattern matches %v at index %v`, primarySet.Set, primarySet.Distance)
	}
	c.writeLine("// Find the next occurrence. If it can't be found, there's no match.")

	// Use IndexOf{Any} to accelerate the skip loop via vectorization to match the first prefix.
	// But we avoid using it for the relatively common case of the starting set being '.', aka anything other than
	// a newline, as it's very rare to have long, uninterrupted sequences of newlines. And we avoid using it
	// for the case of the starting set being anything (e.g. '.' with SingleLine), as in that case it'll always match
	// the first char.
	setIndex := 0
	canUseIndexOf := !primarySet.Set.Equals(syntax.NotNewLineClass()) && !primarySet.Set.IsAnything()

	needLoop := !canUseIndexOf || setsToUse > 1

	endBlock := ""
	if needLoop {
		c.writeLine("span := r.Runtext[pos:]")
		upperBound := "len(span)"
		if setsToUse > 1 || primarySet.Distance != 0 {
			upperBound = fmt.Sprint(upperBound, " - ", rm.Tree.FindOptimizations.MinRequiredLength-1)
		}
		c.writeLineFmt(`for i := 0; i < %v; i++ {`, upperBound)
		endBlock = "}"
	}

	if canUseIndexOf {
		var span string
		if needLoop {
			if primarySet.Distance == 0 {
				span = "span[i:]"
			} else {
				span = fmt.Sprint("span[i+", primarySet.Distance, ":]")
			}
		} else {
			if primarySet.Distance == 0 {
				span = "r.Runtext[pos:]"
			} else {
				span = fmt.Sprint("r.Runtext[pos+", primarySet.Distance, ":]")
			}
		}

		// Get the IndexOf* expression to use to perform the search.
		var indexOf string

		if len(primarySet.Chars) > 0 {
			indexOf = c.emitIndexOfChars(primarySet.Chars, primarySet.Negated, span)

		} else if primarySet.Range != nil {
			// We have a range, so we can use IndexOfAny{Except}InRange to search for it.  In the corner case,
			// where we end up with a set of a single char, we can use IndexOf instead.
			if primarySet.Range.First == primarySet.Range.Last {
				if primarySet.Negated {
					indexOf = fmt.Sprintf("helpers.IndexOfAnyExcept(%v, %q)", span, primarySet.Range.First)
				} else {
					indexOf = fmt.Sprintf("helpers.IndexOfAny1(%v, %q)", span, primarySet.Range.First)
				}
			} else {
				if primarySet.Negated {
					indexOf = fmt.Sprintf("helpers.IndexOfAnyExceptInRange(%v, %q, %q)", span, primarySet.Range.First, primarySet.Range.Last)
				} else {
					indexOf = fmt.Sprintf("helpers.IndexOfAnyInRange(%v, %q, %q)", span, primarySet.Range.First, primarySet.Range.Last)
				}
			}
		} else if isSmall, setChars, negated, desc := primarySet.Set.IsUnicodeCategoryOfSmallCharCount(); isSmall {
			// We have a known set of characters, and we can use the supplied IndexOfAny{Except}(...).
			fName := "IndexOfAny"
			if negated {
				fName = "IndexOfAnyExcept"
			}
			if len(desc) > 0 {
				desc = "rsvSet" + desc
			}
			indexOf = fmt.Sprintf("%v.%v(%v)", c.emitSearchValues(setChars, desc), fName, span)
		} else {
			// We have an arbitrary set of characters that's really large or otherwise not enumerable.
			// We use a custom IndexOfAny helper that will perform the search as efficiently as possible.
			indexOf = c.emitIndexOfAnyCustomHelper(rm, primarySet.Set, negated, span)
		}

		if needLoop {
			c.writeLineFmt(`indexOfPos := %v
						if indexOfPos < 0 {
							goto NoMatchFound
						}
						i += indexOfPos
						`, indexOf)
			rm.noMatchFoundLabelNeeded = true

			if setsToUse > 1 {
				// Of the remaining sets we're going to check, find the maximum distance of any of them.
				// If it's further than the primary set we checked, we need a bounds check.
				maxDistance := sets[1].Distance
				for i := 2; i < setsToUse; i++ {
					if sets[i].Distance > maxDistance {
						maxDistance = sets[i].Distance
					}

					if maxDistance > primarySet.Distance {
						numRemainingSets := setsToUse - 1
						c.writeLineFmt(`// The primary set being searched for was found. %v more set(s) will be checked so as
								 // to minimize the number of places TryMatchAtCurrentPosition is run unnecessarily.
								 // Make sure everything fits in the remainder of the input.
								 if i+%v >= len(span) {
									goto NoMatchFound
								 }
								 `, numRemainingSets, maxDistance)
						rm.noMatchFoundLabelNeeded = true

					}
				}
			}
		} else {
			c.writeLineFmt(`i := %v
						if i >= 0 {
							r.Runtextpos = pos+i
							return true
						}
						`, indexOf)
		}

		setIndex = 1
	}

	if needLoop {
		endBlock2 := ""
		if setIndex < setsToUse {
			// if (CharInClass(textSpan[i + charClassIndex], prefix[0], "...") &&
			//     ...)

			start := setIndex
			for ; setIndex < setsToUse; setIndex++ {
				addOn := ""
				if sets[setIndex].Distance > 0 {
					addOn = fmt.Sprintf(" + %v", sets[setIndex].Distance)
				}
				spanIndex := fmt.Sprintf("span[i%v]", addOn)
				charInClassExpr := c.emitMatchCharacterClass(rm, sets[setIndex].Set, false, spanIndex)

				if setIndex == start {
					c.write("if ")
					c.write(charInClassExpr)
				} else {
					c.writeLine(" &&")
					c.write("    ")
					c.write(charInClassExpr)
				}

			}
			c.writeLine(` {`)
			endBlock2 = "}"
		}
		c.writeLine(`r.Runtextpos = pos+i
						return true`)
		c.writeLine(endBlock2)
	}

	c.writeLine(endBlock)

}

// Emits a right-to-left search for a set at a fixed position from the start of the pattern.
// (Currently that position will always be a distance of 0, meaning the start of the pattern itself.)
func (c *converter) emitFixedSet_RightToLeft(rm *regexpData) {
	set := rm.Tree.FindOptimizations.FixedDistanceSets[0]

	c.writeLineFmt(`// The pattern begins with %v
	// Find the next occurrence. If it can't be found, there's no match.`, set.Set.String())

	if len(set.Chars) == 1 {
		c.writeLineFmt(`pos = r.LastIndexOfRune(0, pos, %q)
		if pos >= 0 {
			r.Runtextpos = pos + 1
			return true
		}`, set.Chars[0])
	} else {
		c.writeLineFmt(`for pos--; pos < len(r.Runtext); pos-- {
			if %v {
				r.Runtextpos = pos + 1
				return true
			}
		}`, c.emitMatchCharacterClass(rm, set.Set, false, "r.Runtext[pos]"))
	}
}

// Emits a search for a literal following a leading atomic single-character loop.
func (c *converter) emitLiteralAfterAtomicLoop(rm *regexpData) {
	target := rm.Tree.FindOptimizations.LiteralAfterLoop

	targetComment := ""

	if len(target.String) > 0 {
		stringComparisonComment := ""
		if target.StringIgnoreCase {
			stringComparisonComment = "case-insensitive "
		}
		targetComment = "the " + stringComparisonComment + "string " + target.String
	} else if len(target.Chars) > 0 {
		targetComment = fmt.Sprintf("one of the characters %#v", string(target.Chars))
	} else {
		targetComment = fmt.Sprintf("the character %q", target.Char)
	}

	c.writeLineFmt(`// The pattern begins with an atomic loop for %v {DescribeSet(target.LoopNode.Str!)}, followed by %v
	// Search for the literal, and then walk backwards to the beginning of the loop.`,
		target.LoopNode.Set.String(), targetComment)

	endBlock := ""

	if target.LoopNode.M > 0 {
		// If there's no lower bound on the loop, then once we find the literal, we know we have a valid starting position to try.
		// If there is a lower bound, then we need a loop, as we could find the literal but it might not be prefixed with enough
		// appropriate characters to satisfy the minimum bound.
		c.writeLine("for {")
		endBlock = "}"
	}

	c.writeLine("slice := r.Runtext[pos:]\n")
	// Find the literal.  If we can't find it, we're done searching.
	if len(target.String) > 0 {
		// find string
		c.writeLineFmt("i := helpers.IndexOf(slice, %s)", getRuneSliceLiteral(target.String))
	} else if len(target.Chars) > 0 {
		// find char any
		c.writeLineFmt("i := %v", c.emitIndexOfChars(target.Chars, false, "slice"))
	} else {
		// find char any
		c.writeLineFmt("i := %v", c.emitIndexOfChars([]rune{target.Char}, false, "slice"))
	}

	endBlock2 := ""
	if target.LoopNode.M > 0 {
		c.writeLine(`if i < 0 {
				break
			}
			`)
	} else {
		c.writeLine(`if i >= 0 {`)
		endBlock2 = "}"
	}

	// We found the literal.  Walk backwards from it finding as many matches as we can against the loop.
	c.writeLineFmt(`prev := i - 1
		for prev < len(slice) && %v {
			prev--
		}
		`, c.emitMatchCharacterClass(rm, target.LoopNode.Set, false, "slice[prev]"))

	if target.LoopNode.M > 0 {
		// If we found fewer than needed, loop around to try again.  The loop doesn't overlap with the literal,
		// so we can start from after the last place the literal matched.
		c.writeLineFmt(`if (i - prev - 1) < %v {
				pos += i + 1
				continue
			}
			`, target.LoopNode.M)
	}

	// We have a winner.  The starting position is just after the last position that failed to match the loop.
	// We also store the position after the loop into runtrackpos (an extra, unused field on RegexRunner) in order
	// to communicate this position to the match algorithm such that it can skip the loop.
	c.writeLine(`r.Runtextpos = pos + prev + 1
	r.Runtrackpos = pos + i
	return true`)

	c.writeLine(endBlock2)
	c.writeLine(endBlock)
}

func getFuncCallIfEqual(set *syntax.CharSet, negate bool, setB *syntax.CharSet, negSetB *syntax.CharSet, funcName string, chExpr string) (string, bool) {
	// example
	// if set is a DigitClass, but it's negated then we need to match
	// NotDigit and we need to write !isDigit() code
	//
	// if set is a NotDigitClass, but it's negated then we need to match
	// Digit and write isDigit() code
	eq := false
	if set.Equals(setB) {
		eq = true
	} else if set.Equals(negSetB) {
		eq = true
		negate = !negate
	}
	if !eq {
		return "", false
	}
	if negate {
		return fmt.Sprint("!", funcName, "(", chExpr, ")"), true
	}
	return fmt.Sprint(funcName, "(", chExpr, ")"), true
}

// Determines whether the 'a' and 'b' values differ by only a single bit, setting that bit in 'mask'.
func differByOneBit(a, b rune) (rune, bool) {
	mask := a ^ b
	if mask == 0 {
		return 0, false
	}
	return mask, mask&(mask-1) == 0
}

func (c *converter) emitMatchCharacterClass(rm *regexpData, set *syntax.CharSet, negate bool, chExpr string) string {
	//this is in-line and produces an expression that resolves to a bool,
	//so anything that requires a new var must call a function

	// We need to perform the equivalent of calling RegexRunner.CharInClass(ch, charClass),
	// but that call is relatively expensive.  Before we fall back to it, we try to optimize
	// some common cases for which we can do much better, such as known character classes
	// for which we can call a dedicated method, or a fast-path for ASCII using a lookup table.
	// In some cases, multiple optimizations are possible for a given character class: the checks
	// in this method are generally ordered from fastest / simplest to slowest / most complex so
	// that we get the best optimization for a given char class.

	// First, see if the char class is a built-in one for which there's a better function
	// we can just call directly.

	if set.IsAnything() {
		// This assumes chExpr never has side effects.
		if negate {
			return "false"
		}
		return "true"
	}

	if val, eq := getFuncCallIfEqual(set, negate, syntax.DigitClass(), syntax.NotDigitClass(), "unicode.IsDigit", chExpr); eq {
		return val
	}
	if val, eq := getFuncCallIfEqual(set, negate, syntax.SpaceClass(), syntax.NotSpaceClass(), "unicode.IsSpace", chExpr); eq {
		return val
	}
	if val, eq := getFuncCallIfEqual(set, negate, syntax.WordClass(), syntax.NotWordClass(), "helpers.IsWordChar", chExpr); eq {
		return val
	}
	/*
		TODO: Lots more classes here we don't have right now
		if val, eq := getFuncCallIfEqual(set, negate, syntax.ControlClass(), syntax.NotControlClass(), "unicode.IsControl", chExpr); eq {
			return val
		}
		if val, eq := getFuncCallIfEqual(set, negate, syntax.LetterClass(), syntax.NotLetterClass(), "unicode.IsLetter", chExpr); eq {
			return val
		}
						if val, eq := getFuncCallIfEqual(set, negate, syntax.LetterOrDigitClass(), syntax.NotLetterOrDigitClass(), "syntax.IsLetterOrDigit", chExpr); eq {
							return val
						}

			                case RegexCharClass.LowerClass:
			                case RegexCharClass.NotLowerClass:
			                    negate ^= charClass == RegexCharClass.NotLowerClass;
			                    return $"{(negate ? "!" : "")}char.IsLower({chExpr})";

			                case RegexCharClass.UpperClass:
			                case RegexCharClass.NotUpperClass:
			                    negate ^= charClass == RegexCharClass.NotUpperClass;
			                    return $"{(negate ? "!" : "")}char.IsUpper({chExpr})";

			                case RegexCharClass.NumberClass:
			                case RegexCharClass.NotNumberClass:
			                    negate ^= charClass == RegexCharClass.NotNumberClass;
			                    return $"{(negate ? "!" : "")}char.IsNumber({chExpr})";

			                case RegexCharClass.PunctuationClass:
			                case RegexCharClass.NotPunctuationClass:
			                    negate ^= charClass == RegexCharClass.NotPunctuationClass;
			                    return $"{(negate ? "!" : "")}char.IsPunctuation({chExpr})";

			                case RegexCharClass.SeparatorClass:
			                case RegexCharClass.NotSeparatorClass:
			                    negate ^= charClass == RegexCharClass.NotSeparatorClass;
			                    return $"{(negate ? "!" : "")}char.IsSeparator({chExpr})";

			                case RegexCharClass.SymbolClass:
			                case RegexCharClass.NotSymbolClass:
			                    negate ^= charClass == RegexCharClass.NotSymbolClass;
			                    return $"{(negate ? "!" : "")}char.IsSymbol({chExpr})";

			                case RegexCharClass.AsciiLetterClass:
			                case RegexCharClass.NotAsciiLetterClass:
			                    negate ^= charClass == RegexCharClass.NotAsciiLetterClass;
			                    return $"{(negate ? "!" : "")}char.IsAsciiLetter({chExpr})";

			                case RegexCharClass.AsciiLetterOrDigitClass:
			                case RegexCharClass.NotAsciiLetterOrDigitClass:
			                    negate ^= charClass == RegexCharClass.NotAsciiLetterOrDigitClass;
			                    return $"{(negate ? "!" : "")}char.IsAsciiLetterOrDigit({chExpr})";

			                case RegexCharClass.HexDigitClass:
			                case RegexCharClass.NotHexDigitClass:
			                    negate ^= charClass == RegexCharClass.NotHexDigitClass;
			                    return $"{(negate ? "!" : "")}char.IsAsciiHexDigit({chExpr})";

			                case RegexCharClass.HexDigitLowerClass:
			                case RegexCharClass.NotHexDigitLowerClass:
			                    negate ^= charClass == RegexCharClass.NotHexDigitLowerClass;
			                    return $"{(negate ? "!" : "")}char.IsAsciiHexDigitLower({chExpr})";

			                case RegexCharClass.HexDigitUpperClass:
			                case RegexCharClass.NotHexDigitUpperClass:
			                    negate ^= charClass == RegexCharClass.NotHexDigitUpperClass;
			                    return $"{(negate ? "!" : "")}char.IsAsciiHexDigitUpper({chExpr})";
			            }*/

	// Next, handle simple sets of one range, e.g. [A-Z], [0-9], etc.  This includes some built-in classes, like ECMADigitClass.
	if rs := set.GetIfNRanges(1); len(rs) == 1 {
		r := rs[0]
		negate = (negate != set.IsNegated())
		if r.First == r.Last {
			// single char
			if negate {
				return fmt.Sprintf("(%v != %q)", chExpr, r.First)
			}
			return fmt.Sprintf("(%v == %q)", chExpr, r.First)
		}
		if negate {
			return fmt.Sprintf("!helpers.IsBetween(%s, %q, %q)", chExpr, r.First, r.Last)
		}
		return fmt.Sprintf("helpers.IsBetween(%s, %q, %q)", chExpr, r.First, r.Last)
	}

	// Next, if the character class contains nothing but Unicode categories, we can call char.GetUnicodeCategory and
	// compare against it.  It has a fast-lookup path for ASCII, so is as good or better than any lookup we'd generate (plus
	// we get smaller code), and it's what we'd do for the fallback (which we get to avoid generating) as part of CharInClass,
	// but without the optimizations the C# compiler will provide for switches.
	cats, neg := set.GetIfOnlyUnicodeCategories()
	if len(cats) > 0 {
		negate = (negate != neg)
		// convert cats to strings
		sb := &bytes.Buffer{}
		if negate {
			sb.WriteString("!")
		}
		sb.WriteString("unicode.In(")
		sb.WriteString(chExpr)
		for _, cat := range cats {
			sb.WriteString(", unicode.")
			sb.WriteString(cat.Cat)
		}
		sb.WriteString(")")
		return sb.String()
	}

	// Next, if there's only 2 or 3 chars in the set (fairly common due to the sets we create for prefixes),
	// it may be cheaper and smaller to compare against each than it is to use a lookup table.  We can also special-case
	// the very common case with case insensitivity of two characters next to each other being the upper and lowercase
	// ASCII variants of each other, in which case we can use bit manipulation to avoid a comparison.
	//setChars := make([]rune, 0, 3)
	setChars := set.GetSetChars(3)
	if len(setChars) == 2 {
		negate = (negate != set.IsNegated())
		eqStr := "=="
		bitJoin := "||"
		if negate {
			eqStr = "!="
			bitJoin = "&&"
		}
		if mask, ok := differByOneBit(setChars[0], setChars[1]); ok {
			return fmt.Sprintf("(%s|0x%x %v %q)", chExpr, mask, eqStr, setChars[1]|mask)
		}
		return fmt.Sprintf("(%s %s %q %s %[1]s %[2]s %[5]q)", chExpr, eqStr, setChars[0], bitJoin, setChars[1])
	} else if len(setChars) == 3 {
		negate = (negate != set.IsNegated())
		eqStr := "=="
		bitJoin := "||"
		if negate {
			eqStr = "!="
			bitJoin = "&&"
		}
		if mask, ok := differByOneBit(setChars[0], setChars[1]); ok {
			return fmt.Sprintf("((%s|0x%x %v %q) %s (%[1]s %[3]s %[6]q))", chExpr, mask, eqStr, setChars[1]|mask, bitJoin, setChars[2])
		}
		return fmt.Sprintf("(%s %s %q %s %[1]s %[2]s %[5]q %[4]s %[1]s %[2]s %[6]q)", chExpr, eqStr, setChars[0], bitJoin, setChars[1], setChars[2])
	}

	// Next, handle simple sets of two ASCII letter ranges that are cased versions of each other, e.g. [A-Za-z].
	// This can be implemented as if it were a single range, with an additional bitwise operation.
	// TODO: the original C# code assumed an order of ranges coming back
	// based on char order -- can we assume that here too? does [A-Za-z] and [a-zA-Z] work the same?
	if ranges := set.GetIfNRanges(2); len(ranges) == 2 {
		if ranges[1].First <= unicode.MaxASCII &&
			ranges[1].Last <= unicode.MaxASCII &&
			ranges[0].First|0x20 == ranges[1].First &&
			ranges[0].Last|0x20 == ranges[1].Last {

			negate = (negate != set.IsNegated())
			op := "<="
			if negate {
				op = ">"
			}
			return fmt.Sprintf("((%s|0x20 - %q) %s (%q - %q))", chExpr, ranges[1].First, op, ranges[1].Last, ranges[1].First)
		}
	}

	// Analyze the character set more to determine what code to generate.
	analysis := set.Analyze()

	// Next, handle sets where the high - low + 1 range is <= 32.  In that case, we can emit
	// a branchless lookup in a uint that does not rely on loading any objects (e.g. the string-based
	// lookup we use later).  This nicely handles common sets like [\t\r\n ].
	if analysis.OnlyRanges && (analysis.UpperBoundExclusiveIfOnlyRanges-analysis.LowerBoundInclusiveIfOnlyRanges) <= 32 {
		// Create the 32-bit value with 1s at indices corresponding to every character in the set,
		// where the bit is computed to be the char value minus the lower bound starting from
		// most significant bit downwards.
		negatedClass := set.IsNegated()
		bitmap := uint32(0)
		for i := analysis.LowerBoundInclusiveIfOnlyRanges; i < analysis.UpperBoundExclusiveIfOnlyRanges; i++ {
			if set.CharIn(i) != negatedClass {
				bitmap |= 1 << (31 - (i - analysis.LowerBoundInclusiveIfOnlyRanges))
			}
		}

		// To determine whether a character is in the set, we subtract the lowest char; this subtraction happens before the result is
		// zero-extended to uint, meaning that `charMinusLowUInt32` will always have upper 16 bits equal to 0.
		// We then left shift the constant with this offset, and apply a bitmask that has the highest
		// bit set (the sign bit) if and only if `chExpr` is in the [low, low + 32) range.
		// Then we only need to check whether this final result is less than 0: this will only be
		// the case if both `charMinusLowUInt32` was in fact the index of a set bit in the constant, and also
		// `chExpr` was in the allowed range (this ensures that false positive bit shifts are ignored).
		negate = (negate != negatedClass)
		negStr := ""
		if negate {
			negStr = "!"
		}
		return fmt.Sprintf("%shelpers.IsInMask32(%s-%q, 0x%x)", negStr, chExpr, analysis.LowerBoundInclusiveIfOnlyRanges, bitmap)
	}

	// Next, handle sets where the high - low + 1 range is <= 64.  As with the 32-bit case above, we can emit
	// a branchless lookup in a ulong that does not rely on loading any objects (e.g. the string-based lookup
	// we use later). Note that unlike RegexCompiler, the source generator doesn't know whether the code is going
	// to be run in a 32-bit or 64-bit process: in a 64-bit process, this is an optimization, but in a 32-bit process,
	// it's a deoptimization.  In general we optimize for 64-bit perf, so this code remains; it complicates the code
	// too much to try to include both this and a fallback for the check. This, however, is why we do the 32-bit
	// version and check first, as that variant performs equally well on both 32-bit and 64-bit systems.
	if analysis.OnlyRanges && (analysis.UpperBoundExclusiveIfOnlyRanges-analysis.LowerBoundInclusiveIfOnlyRanges) <= 64 {
		// Create the 64-bit value with 1s at indices corresponding to every character in the set,
		// where the bit is computed to be the char value minus the lower bound starting from
		// most significant bit downwards.
		negatedClass := set.IsNegated()
		bitmap := uint64(0)
		for i := analysis.LowerBoundInclusiveIfOnlyRanges; i < analysis.UpperBoundExclusiveIfOnlyRanges; i++ {
			if set.CharIn(i) != negatedClass {
				bitmap |= 1 << (63 - (i - analysis.LowerBoundInclusiveIfOnlyRanges))
			}
		}

		// To determine whether a character is in the set, we subtract the lowest char; this subtraction happens before
		// the result is zero-extended to uint, meaning that `charMinusLowUInt64` will always have upper 32 bits equal to 0.
		// We then left shift the constant with this offset, and apply a bitmask that has the highest bit set (the sign bit)
		// if and only if `chExpr` is in the [low, low + 64) range. Then we only need to check whether this final result is
		// less than 0: this will only be the case if both `charMinusLowUInt64` was in fact the index of a set bit in the constant,
		// and also `chExpr` was in the allowed range (this ensures that false positive bit shifts are ignored).
		negate = (negate != negatedClass)
		negStr := ""
		if negate {
			negStr = "!"
		}
		return fmt.Sprintf("%shelpers.IsInMask64(%s-%q, 0x%x)", negStr, chExpr, analysis.LowerBoundInclusiveIfOnlyRanges, bitmap)
	}

	// All options after this point require a ch local.
	// in the C# version this requires assignment statements, which Go doesn't have
	// so we just repeat chExpr and let the compiler handle temp var
	//rm.addLocalDec("var ch rune")

	// Next, handle simple sets of two ranges, e.g. [\p{IsGreek}\p{IsGreekExtended}].
	if ranges := set.GetIfNRanges(2); len(ranges) == 2 {
		negate = (negate != set.IsNegated())

		op := "||"
		if negate {
			op = "&&"
		}
		return fmt.Sprintf("%s %s %s",
			getRangeCheckClause(chExpr, ranges[0], negate),
			op,
			getRangeCheckClause(chExpr, ranges[1], negate))
	}

	if analysis.ContainsNoAscii {
		// We determined that the character class contains only non-ASCII,
		// for example if the class were [\u1000-\u2000\u3000-\u4000\u5000-\u6000].
		// (In the future, we could possibly extend the rm.Analysis to produce a known
		// lower-bound and compare against that rather than always using 128 as the
		// pivot point.)
		return c.emitContainsNoAscii(negate, chExpr, set)
	}
	if analysis.AllAsciiContained {
		// We determined that every ASCII character is in the class, for example
		// if the class were the negated example from case 1 above:
		// [^\p{IsGreek}\p{IsGreekExtended}].
		return c.emitAllAsciiContained(negate, chExpr, set)
	}

	// Now, our big hammer is to generate a lookup table that lets us quickly index by character into a yes/no
	// answer as to whether the character is in the target character class.  However, we don't want to store
	// a lookup table for every possible character for every character class in the regular expression; at one
	// bit for each of 65K characters, that would be an 8K bitmap per character class.  Instead, we handle the
	// common case of ASCII input via such a lookup table, which at one bit for each of 128 characters is only
	// 16 bytes per character class.  We of course still need to be able to handle inputs that aren't ASCII, so
	// we check the input against 128, and have a fallback if the input is >= to it.  Determining the right
	// fallback could itself be expensive.  For example, if it's possible that a value >= 128 could match the
	// character class, we output a call to RegexRunner.CharInClass, but we don't want to have to enumerate the
	// entire character class evaluating every character against it, just to determine whether it's a match.
	// Instead, we employ some quick heuristics that will always ensure we provide a correct answer even if
	// we could have sometimes generated better code to give that answer.

	// Generate the lookup table to store 128 answers as bits. We use a const string instead of a byte[] / static
	// data property because it lets IL emit handle all the details for us.
	// String length is 8 chars == 16 bytes == 128 bits.
	bitVector := make([]uint64, 2)

	for i := rune(0); i < unicode.MaxASCII; i++ {
		if set.CharIn(i) {
			bitVector[i/64] |= (1 << (i % 64))
		}
	}

	// There's a chance that the class contains either no ASCII characters or all of them,
	// and the analysis could not find it (for example if the class has a subtraction).
	// We optimize away the bit vector in these trivial cases.
	if bitVector[0] == 0 && bitVector[1] == 0 {
		// no ascii at all
		return c.emitContainsNoAscii(negate, chExpr, set)
	}
	if bitVector[0] == math.MaxUint64 && bitVector[1] == math.MaxUint64 {
		// all ascii is included
		return c.emitAllAsciiContained(negate, chExpr, set)
	}
	/*
	   // We determined that the character class may contain ASCII, so we
	   // output the lookup against the lookup table.

	   if (analysis.ContainsOnlyAscii)
	   {
	       // If all inputs that could match are ASCII, we only need the lookup table, guarded
	       // by a check for the upper bound (which serves both to limit for what characters
	       // we need to access the lookup table and to bounds check the lookup table access).
	       return negate ?
	           $"((ch = {chExpr}) >= {Literal((char)analysis.UpperBoundExclusiveIfOnlyRanges)} || ({Literal(bitVectorString)}[ch >> 4] & (1 << (ch & 0xF))) == 0)" :
	           $"((ch = {chExpr}) < {Literal((char)analysis.UpperBoundExclusiveIfOnlyRanges)} && ({Literal(bitVectorString)}[ch >> 4] & (1 << (ch & 0xF))) != 0)";
	   }

	   if (analysis.AllNonAsciiContained)
	   {
	       // If every non-ASCII value is considered a match, we can immediately succeed for any
	       // non-ASCII inputs, and access the lookup table for the rest.
	       return negate ?
	           $"((ch = {chExpr}) < 128 && ({Literal(bitVectorString)}[ch >> 4] & (1 << (ch & 0xF))) == 0)" :
	           $"((ch = {chExpr}) >= 128 || ({Literal(bitVectorString)}[ch >> 4] & (1 << (ch & 0xF))) != 0)";
	   }

	   // We know that the whole class wasn't ASCII, and we don't know anything about the non-ASCII
	   // characters other than that some might be included, for example if the character class
	   // were [\w\d], so if ch >= 128, we need to fall back to calling CharInClass. For ASCII, we
	   // can use a lookup table, but if it's a known set of ASCII characters we can also use a helper.
	   string asciiExpr = bitVectorString switch
	   {
	       "\0\0\0\u03ff\ufffe\u07ff\ufffe\u07ff" => $"{(negate ? "!" : "")}char.IsAsciiLetterOrDigit(ch)",

	       "\0\0\0\u03FF\0\0\0\0" => $"{(negate ? "!" : "")}char.IsAsciiDigit(ch)",

	       "\0\0\0\0\ufffe\u07FF\ufffe\u07ff" => $"{(negate ? "!" : "")}char.IsAsciiLetter(ch)",
	       "\0\0\0\0\0\0\ufffe\u07ff" => $"{(negate ? "!" : "")}char.IsAsciiLetterLower(ch)",
	       "\0\0\0\0\ufffe\u07FF\0\0" => $"{(negate ? "!" : "")}char.IsAsciiLetterUpper(ch)",

	       "\0\0\0\u03FF\u007E\0\u007E\0" => $"{(negate ? "!" : "")}char.IsAsciiHexDigit(ch)",
	       "\0\0\0\u03FF\0\0\u007E\0" => $"{(negate ? "!" : "")}char.IsAsciiHexDigitLower(ch)",
	       "\0\0\0\u03FF\u007E\0\0\0" => $"{(negate ? "!" : "")}char.IsAsciiHexDigitUpper(ch)",

	       _ => $"({Literal(bitVectorString)}[ch >> 4] & (1 << (ch & 0xF))) {(negate ? "=" : "!")}= 0",
	   };
	   return $"((ch = {chExpr}) < 128 ? {asciiExpr} : {(negate ? "!" : "")}RegexRunner.CharInClass((char)ch, {Literal(charClass)}))";
	*/

	// very base option, not optimized
	setField := c.emitSetDefinition(set)
	if negate {
		return fmt.Sprintf("!%s.CharIn(%s)", setField, chExpr)
	}
	return fmt.Sprintf("%s.CharIn(%s)", setField, chExpr)
}

func getRangeCheckClause(chExpr string, r syntax.SingleRange, negate bool) string {
	if negate {
		if r.First == r.Last {
			return fmt.Sprintf("%s != %q", chExpr, r.First)
		} else {
			return fmt.Sprintf("%s - %q > %v", chExpr, r.First, r.Last-r.First)
		}
	}
	if r.First == r.Last {
		return fmt.Sprintf("%s == %q", chExpr, r.First)
	}
	return fmt.Sprintf("%s - %q <= %v", chExpr, r.First, r.Last-r.First)
}

func (c *converter) emitIndexOfAnyCustomHelper(rm *regexpData, set *syntax.CharSet, negate bool, spanName string) string {
	//TODO: see if it's worth it to identify the set and
	//use a dedicated helper for this set

	// this is the most general form of the helper
	match := c.emitMatchCharacterClass(rm, set, negate, "ch")
	return fmt.Sprintf("helpers.IndexFunc(%s, func(ch rune) bool { return %s })", spanName, match)
}

func (c *converter) emitContainsNoAscii(negate bool, chExpr string, set *syntax.CharSet) string {
	setField := c.emitSetDefinition(set)
	if negate {
		return fmt.Sprintf("%s < 128 || !%s.CharIn(%[1]s)", chExpr, setField)
	}
	return fmt.Sprintf("%s >= 128 && %s.CharIn(%[1]s)", chExpr, setField)
}

func (c *converter) emitAllAsciiContained(negate bool, chExpr string, set *syntax.CharSet) string {
	setField := c.emitSetDefinition(set)
	if negate {
		return fmt.Sprintf("%s >= 128 && !%s.CharIn(%[1]s)", chExpr, setField)
	}
	return fmt.Sprintf("%s < 128 || %s.CharIn(%[1]s)", chExpr, setField)
}
