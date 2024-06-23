package main

import (
	"bytes"
	"fmt"
	"math"
	"strconv"

	"github.com/dlclark/regexp2/syntax"
)

// Arbitrary limit for unrolling vs creating a loop.  We want to balance size in the generated
// code with other costs, like the (small) overhead of slicing to create the temp span to iterate.
const MaxUnrollSize = 16

func (c *converter) emitExecute(rm *regexpData) {
	c.writeLineFmt("func (%s_Engine) Execute(r *regexp2.Runner) error {", rm.GeneratedName)
	defer func() {
		c.writeLine("}\n")
	}()

	rtl := rm.Options&syntax.RightToLeft != 0
	root := rm.Tree.Root.Children[0]

	if root.T == syntax.NtEmpty {
		// Emit a capture for the current position of length 0.  This is rare to see with a real-world pattern,
		// but it's very common as part of exploring the source generator, because it's what you get when you
		// start out with an empty pattern.
		c.writeLine("// The pattern matches the empty string")
		c.writeLine("var pos = r.Runtextpos")
		c.writeLine("r.Capture(0, pos, pos)")
		c.writeLine("return nil")
		return
	}

	if root.T == syntax.NtNothing {
		// Emit nothing.  This is rare in production and not something to we need optimize for, but as with
		// empty, it's helpful as a learning exposition tool.
		c.writeLine("return nil")
		return
	}

	if root.T == syntax.NtMulti || root.T == syntax.NtOne || root.T == syntax.NtNotone || root.T == syntax.NtSet {
		// If the whole expression is just one or more characters, we can rely on the FindOptimizations spitting out
		// an IndexOf that will find the exact sequence or not, and we don't need to do additional checking beyond that.

		op := "+"
		if rtl {
			op = "-"
		}

		jmp := 1
		if root.T == syntax.NtMulti {
			jmp = len(root.Str)
		}

		c.writeLine("// the search in findFirstChar did the entire match")
		c.writeLine("var start = r.Runtextpos")
		c.writeLineFmt("var end = r.Runtextpos %s %v", op, jmp)
		c.writeLine("r.Runtextpos = end")
		c.writeLine("r.Capture(0, start, end)")
		c.writeLine("return nil")
		return
	}

	// In .NET Framework and up through .NET Core 3.1, the code generated for RegexOptions.Compiled was effectively an unrolled
	// version of what RegexInterpreter would process.  The RegexNode tree would be turned into a series of opcodes via
	// RegexWriter; the interpreter would then sit in a loop processing those opcodes, and the RegexCompiler iterated through the
	// opcodes generating code for each equivalent to what the interpreter would do albeit with some decisions made at compile-time
	// rather than at run-time.  This approach, however, lead to complicated code that wasn't pay-for-play (e.g. a big backtracking
	// jump table that all compilations went through even if there was no backtracking), that didn't factor in the shape of the
	// tree (e.g. it's difficult to add optimizations based on interactions between nodes in the graph), and that didn't read well
	// when decompiled from IL to C# or when directly emitted as C# as part of a source generator.
	//
	// This implementation is instead based on directly walking the RegexNode tree and outputting code for each node in the graph.
	// A dedicated for each kind of RegexNode emits the code necessary to handle that node's processing, including recursively
	// calling the relevant function for any of its children nodes.  Backtracking is handled not via a giant jump table, but instead
	// by emitting direct jumps to each backtracking construct.  This is achieved by having all match failures jump to a "done"
	// label that can be changed by a previous emitter, e.g. before EmitLoop returns, it ensures that "doneLabel" is set to the
	// label that code should jump back to when backtracking.  That way, a subsequent EmitXx function doesn't need to know exactly
	// where to jump: it simply always jumps to "doneLabel" on match failure, and "doneLabel" is always configured to point to
	// the right location.  In an expression without backtracking, or before any backtracking constructs have been encountered,
	// "doneLabel" is simply the final return location from the TryMatchAtCurrentPosition method that will undo any captures and exit, signaling to
	// the calling scan loop that nothing was matched.

	regexTree := rm.Tree

	// Helper to define names.  Names start unadorned, but as soon as there's repetition,
	// they begin to have a numbered suffix.
	rm.usedNames = make(map[string]int)

	// Every RegexTree is rooted in the implicit Capture for the whole expression.
	// Skip the Capture node. We handle the implicit root capture specially.
	node := regexTree.Root
	node = node.Children[0]

	// In some cases, we need to emit declarations at the beginning of the method, but we only discover we need them later.
	// To handle that, we build up a collection of all the declarations to include, track where they should be inserted,
	// and then insert them at that position once everything else has been output.
	oldOut := c.out
	buf := &bytes.Buffer{}
	c.out = buf
	defer func() {
		// lets clean this up at the end
		c.out = oldOut

		// write additionalDeclarations
		for _, l := range rm.additionalDeclarations {
			c.writeLine(l)
		}

		//reset
		rm.additionalDeclarations = []string{}

		// then write our temp out buffer into our saved buffer
		c.out.Write(buf.Bytes())
	}()

	// Declare some locals.
	rm.sliceSpan = "slice"
	c.writeLine(`pos := base.runtextpos
			matchStart := pos
			`)

	// The implementation tries to use const indexes into the span wherever possible, which we can do
	// for all fixed-length constructs.  In such cases (e.g. single chars, repeaters, strings, etc.)
	// we know at any point in the regex exactly how far into it we are, and we can use that to index
	// into the span created at the beginning of the routine to begin at exactly where we're starting
	// in the input.  When we encounter a variable-length construct, we transfer the static value to
	// pos, slicing the inputSpan appropriately, and then zero out the static position.
	rm.sliceStaticPos = 0
	c.sliceInputSpan(rm, true)
	c.writeLine("")

	// doneLabel starts out as the top-level label for the whole expression failing to match.  However,
	// it may be changed by the processing of a node to point to whereever subsequent match failures
	// should jump to, in support of backtracking or other constructs.  For example, before emitting
	// the code for a branch N, an alternation will set the doneLabel to point to the label for
	// processing the next branch N+1: that way, any failures in the branch N's processing will
	// implicitly end up jumping to the right location without needing to know in what context it's used.
	rm.doneLabel = rm.reserveName("NoMatch")
	rm.topLevelDoneLabel = rm.doneLabel

	// Check whether there are captures anywhere in the expression. If there isn't, we can skip all
	// the boilerplate logic around uncapturing, as there won't be anything to uncapture.
	rm.expressionHasCaptures = rm.Analysis.MayContainCapture(node)

	// Emit the code for all nodes in the tree.
	c.emitExecuteNode(rm, node, nil, true)

	// If we fall through to this place in the code, we've successfully matched the expression.
	c.writeLine("\n// The input matched.")
	if rm.sliceStaticPos > 0 {
		// TransferSliceStaticPosToPos would also slice, which isn't needed here
		c.emitAddStmt("pos", rm.sliceStaticPos)
	}
	c.writeLine(`r.Runtextpos = pos
			r.Capture(0, matchStart, pos)
			return nil`)

	// We're done with the match.
}

// Emits the code for the node.
// subsequent = nil, emitLengthChecksIfRequired = True
func (c *converter) emitExecuteNode(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode, emitLengthChecksIfRequired bool) {
	// Before we handle general-purpose matching logic for nodes, handle any special-casing.
	if rm.Tree.FindOptimizations.FindMode == syntax.LiteralAfterLoop_LeftToRight &&
		rm.Tree.FindOptimizations.LiteralAfterLoop.LoopNode == node {
		// This is the set loop that's part of the literal-after-loop optimization: the end of the loop
		// is stored in runtrackpos, so we just need to transfer that to pos. The optimization is only
		// selected if the shape of the tree is amenable.
		c.writeLine(`// Skip loop already matched in TryFindNextPossibleStartingPosition.
		pos = r.Runtrackpos`)
		c.sliceInputSpan(rm, false)
		return
	}

	if node.Options&syntax.RightToLeft != 0 {
		// RightToLeft doesn't take advantage of static positions.  While RightToLeft won't update static
		// positions, a previous operation may have left us with a non-zero one.  Make sure it's zero'd out
		// such that pos and slice are up-to-date.  Note that RightToLeft also shouldn't use the slice span,
		// as it's not kept up-to-date; any RightToLeft implementation that wants to use it must first update
		// it from pos.
		c.transferSliceStaticPosToPos(rm, false)
	}

	// Separate out several node types that, for conciseness, don't need a header nor scope written into the source.
	// Effectively these either evaporate, are completely self-explanatory, or only exist for their children to be rendered.
	switch node.T {
	// Nothing is written for an empty.
	case syntax.NtEmpty:
		return

	// A single-line goto for a failure doesn't need a scope or comment.
	case syntax.NtNothing:
		c.emitExecuteGoto(rm, rm.doneLabel)
		return

	// Skip atomic nodes that wrap non-backtracking children; in such a case there's nothing to be made atomic.
	case syntax.NtAtomic:
		if !rm.Analysis.MayBacktrack(node.Children[0]) {
			c.emitExecuteNode(rm, node.Children[0], nil, true)
			return
		}

	// Concatenate is a simplification in the node tree so that a series of children can be represented as one.
	// We don't need its presence visible in the source.
	case syntax.NtConcatenate:
		c.emitExecuteConcatenation(rm, node, subsequent, emitLengthChecksIfRequired)
		return
	}

	// For everything else, output a comment about what the node is.
	c.writeLineFmt("// %s", describeNode(rm, node))

	// Separate out several node types that, for conciseness, don't need a scope written into the source as they're
	// always a single statement / block.
	switch node.T {
	case syntax.NtBeginning, syntax.NtStart, syntax.NtBol, syntax.NtEol, syntax.NtEnd, syntax.NtEndZ:
		c.emitExecuteAnchors(rm, node)
		return

	case syntax.NtBoundary, syntax.NtNonboundary, syntax.NtECMABoundary, syntax.NtNonECMABoundary:
		c.emitExecuteBoundary(rm, node)
		return

	case syntax.NtOne, syntax.NtNotone, syntax.NtSet:
		c.emitExecuteSingleChar(rm, node, emitLengthChecksIfRequired, nil, false)
		return

	case syntax.NtMulti:
		if (node.Options & syntax.RightToLeft) == 0 {
			c.emitExecuteMultiCharString(rm, node.Str, emitLengthChecksIfRequired, false, node.Options&syntax.RightToLeft != 0)
			return
		}

	case syntax.NtUpdateBumpalong:
		c.emitExecuteUpdateBumpalong(rm, node)
		return
	}

	// For everything else, put the node's code into its own scope, purely for readability. If the node contains labels
	// that may need to be visible outside of its scope, the scope is still emitted for clarity but is commented out.
	//using (EmitBlock(writer, null, faux: rm.Analysis.MayBacktrack(node)))
	//{
	switch node.T {
	case syntax.NtMulti:
		c.emitExecuteMultiCharString(rm, node.Str, emitLengthChecksIfRequired, false, node.Options&syntax.RightToLeft != 0)
		return

	case syntax.NtOneloop, syntax.NtNotoneloop, syntax.NtSetloop:
		c.emitExecuteSingleCharLoop(rm, node, subsequent, emitLengthChecksIfRequired)
		return

	case syntax.NtOnelazy, syntax.NtNotonelazy, syntax.NtSetlazy:
		c.emitExecuteSingleCharLazy(rm, node, subsequent, emitLengthChecksIfRequired)
		return

	/*case syntax.NtOneloopatomic, syntax.NtNotoneloopatomic, syntax.NtSetloopatomic:
	c.emitExecuteSingleCharAtomicLoop(node, emitLengthChecksIfRequired)
	return
	*/
	case syntax.NtLoop:
		c.emitExecuteLoop(rm, node)
		return

	case syntax.NtLazyloop:
		c.emitExecuteLazy(rm, node)
		return

	case syntax.NtAlternate:
		c.emitExecuteAlternation(rm, node)
		return

	case syntax.NtRef:
		c.emitExecuteBackreference(rm, node)
		return

	case syntax.NtBackRefCond:
		c.emitExecuteBackreferenceConditional(rm, node)
		return

	case syntax.NtExprCond:
		c.emitExecuteExpressionConditional(rm, node)
		return

	case syntax.NtAtomic:
		c.emitExecuteAtomic(rm, node, subsequent)
		return

	case syntax.NtCapture:
		c.emitExecuteCapture(rm, node, subsequent)
		return

	case syntax.NtPosLook:
		c.emitExecutePositiveLookaroundAssertion(rm, node)
		return

	case syntax.NtNegLook:
		c.emitExecuteNegativeLookaroundAssertion(rm, node)
		return
	}
	//}

	panic(fmt.Sprintf("unhandled node: %v", node))
}

// Emits the node for an atomic.
func (c *converter) emitExecuteAtomic(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode) {
	// Grab the current done label and the current backtracking position.  The purpose of the atomic node
	// is to ensure that nodes after it that might backtrack skip over the atomic, which means after
	// rendering the atomic's child, we need to reset the label so that subsequent backtracking doesn't
	// see any label left set by the atomic's child.  We also need to reset the backtracking stack position
	// so that the state on the stack remains consistent.
	originalDoneLabel := rm.doneLabel
	rm.additionalDeclarations = append(rm.additionalDeclarations, "stackpos := 0")
	startingStackpos := rm.reserveName("atomic_stackpos")
	c.writeLineFmt("%s = stackpos\n", startingStackpos)

	// Emit the child.
	c.emitExecuteNode(rm, node.Children[0], subsequent, true)

	// Reset the stack position and done label.
	c.writeLineFmt("\nstackpos = %s", startingStackpos)
	rm.doneLabel = originalDoneLabel
}

// Emits the code to handle updating base.runtextpos to pos in response to
// an UpdateBumpalong node.  This is used when we want to inform the scan loop that
// it should bump from this location rather than from the original location.
func (c *converter) emitExecuteUpdateBumpalong(rm *regexpData, node *syntax.RegexNode) {
	c.transferSliceStaticPosToPos(rm, false)
	c.writeLine(`if base.runtextpos < pos {
		r.Runtextpos = pos
	}`)
}

// Emits code for a concatenation
func (c *converter) emitExecuteConcatenation(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode, emitLengthChecksIfRequired bool) {
	// Emit the code for each child one after the other.
	var prevDescription *string

	for i := 0; i < len(node.Children); i++ {
		// If we can find a subsequence of fixed-length children, we can emit a length check once for that sequence
		// and then skip the individual length checks for each.  We can also discover case-insensitive sequences that
		// can be checked efficiently with methods like StartsWith. We also want to minimize the repetition of if blocks,
		// and so we try to emit a series of clauses all part of the same if block rather than one if block per child.
		var requiredLength, exclusiveEnd int
		if node.Options&syntax.RightToLeft == 0 &&
			emitLengthChecksIfRequired &&
			node.TryGetJoinableLengthCheckChildRange(i, &requiredLength, &exclusiveEnd) {
			wroteClauses := true

			writePrefix := func() {
				if wroteClauses {
					if prevDescription != nil {
						c.writeLineFmt(" || /* %s */ ", *prevDescription)
					} else {
						c.writeLine(" || ")
					}
				} else {
					c.writeLine("if ")
				}
			}

			c.write(fmt.Sprintf("if %s", spanLengthCheck(rm, requiredLength, nil)))

			for i < exclusiveEnd {
				for ; i < exclusiveEnd; i++ {
					child := node.Children[0]
					if ok, nodesConsumed, caseInsensitiveString := node.TryGetOrdinalCaseInsensitiveString(i, exclusiveEnd, false); ok {
						writePrefix()
						sourceSpan := rm.sliceSpan
						if rm.sliceStaticPos > 0 {
							sourceSpan = fmt.Sprintf("%s[%v:]", rm.sliceSpan, rm.sliceStaticPos)
						}
						c.write(fmt.Sprintf("!helpers.StartsWith(%s, %s, true)", sourceSpan, caseInsensitiveString))
						*prevDescription = fmt.Sprintf("Match the string %#v (ordinal case-insensitive)", caseInsensitiveString)
						wroteClauses = true

						rm.sliceStaticPos += len(caseInsensitiveString)
						i += nodesConsumed - 1
					} else if child.T == syntax.NtMulti {
						writePrefix()
						c.emitExecuteMultiCharString(rm, child.Str, false, true, false)
						*prevDescription = describeNode(rm, child)
						wroteClauses = true
					} else if (child.IsOneFamily() || child.IsNotoneFamily() || child.IsSetFamily()) &&
						child.M == child.N &&
						child.M <= MaxUnrollSize {

						repeatCount := child.M
						if child.T == syntax.NtOne || child.T == syntax.NtNotone || child.T == syntax.NtSet {
							repeatCount = 1
						}
						for x := 0; x < repeatCount; x++ {
							writePrefix()
							c.emitExecuteSingleChar(rm, child, false, nil, true)
							if x == 0 {
								*prevDescription = describeNode(rm, child)
							} else {
								prevDescription = nil
							}
							wroteClauses = true
						}
					} else {
						break
					}
				}

				if wroteClauses {
					if prevDescription != nil {
						c.writeLineFmt("/* %s */", *prevDescription)
					}
					c.writeLine(" {")
					c.emitExecuteGoto(rm, rm.doneLabel)
					c.writeLine("}")

					if i < len(node.Children) {
						c.writeLine("")
					}

					wroteClauses = false
					prevDescription = nil
				}

				if i < exclusiveEnd {
					c.emitExecuteNode(rm, node.Children[i], getSubsequentOrDefault(i, node, subsequent), false)
					if i < len(node.Children)-1 {
						c.writeLine("")
					}
					i++
				}
			}

			i--
			continue
		}

		c.emitExecuteNode(rm, node.Children[i], getSubsequentOrDefault(i, node, subsequent), emitLengthChecksIfRequired)
		if i < len(node.Children)-1 {
			c.writeLine("")
		}
	}
}

// Gets the node to treat as the subsequent one to node.Child(index)
func getSubsequentOrDefault(index int, node *syntax.RegexNode, defaultNode *syntax.RegexNode) *syntax.RegexNode {
	for i := index + 1; i < len(node.Children); i++ {
		next := node.Children[i]
		// skip node types that don't have a semantic impact
		if next.T != syntax.NtUpdateBumpalong {
			return next
		}
	}

	return defaultNode
}

func (c *converter) emitExecuteMultiCharString(rm *regexpData, str []rune, emitLengthCheck bool, clauseOnly bool, rightToLeft bool) {

	if rightToLeft {
		c.writeLineFmt("if pos - %v >= r.Runtextlen {", len(str))
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n")

		c.writeLineFmt(`for i:=0; i < %v; i++ {
						pos--
						if r.Runtext[pos] != %s[%[1]v - i] {`, len(str), getRuneSliceLiteral(str))
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n}")

		return
	}

	sourceSpan := rm.sliceSpan
	if rm.sliceStaticPos > 0 {
		sourceSpan = fmt.Sprintf("%s[%v]", rm.sliceSpan, rm.sliceStaticPos)
	}
	clause := fmt.Sprintf("!helpers.StartsWith(%s, %s)", sourceSpan, getRuneSliceLiteral(str))
	if clauseOnly {
		c.write(clause)
	} else {
		c.writeLineFmt("if %s {", clause)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
	}

	rm.sliceStaticPos += len(str)
}

// Emits the code to handle a single-character match.
// emitLengthCheck = true, offset = nil, clauseOnly = false
func (c *converter) emitExecuteSingleChar(rm *regexpData, node *syntax.RegexNode, emitLengthCheck bool, offset *string, clauseOnly bool) {
	rtl := node.Options&syntax.RightToLeft != 0

	expr := "r.Runtext[pos-1]"
	if !rtl {
		expr = fmt.Sprintf("%s[%s]", rm.sliceSpan, sum(rm.sliceStaticPos, offset))
	}

	if node.IsSetFamily() {
		expr = c.emitMatchCharacterClass(rm, node.Set, true, expr)
	} else if node.IsOneFamily() {
		expr = fmt.Sprintf("%s != %q", expr, node.Ch)
	} else {
		expr = fmt.Sprintf("%s == %q", expr, node.Ch)
	}

	if clauseOnly {
		c.write(expr)
	} else {
		var clause string
		if emitLengthCheck {
			clause = fmt.Sprintf("if %s {", expr)
		} else if !rtl {
			clause = fmt.Sprintf("if %s || %s {", spanLengthCheck(rm, 1, offset), expr)
		} else {
			clause = fmt.Sprintf("if pos - 1 >= r.Runtextend || %s", expr)
		}

		c.writeLine(clause)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
	}

	if !rtl {
		rm.sliceStaticPos++
	} else {
		c.writeLine("pos--")
	}
}

// emitLengthChecksIfRequired=true
func (c *converter) emitExecuteSingleCharLoop(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode, emitLengthChecksIfRequired bool) {
}

// emitLengthChecksIfRequired=true
func (c *converter) emitExecuteSingleCharLazy(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode, emitLengthChecksIfRequired bool) {
}
func (c *converter) emitExecuteAnchors(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteBoundary(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteLoop(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteLazy(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteAlternation(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteBackreference(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteBackreferenceConditional(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteExpressionConditional(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteCapture(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode) {
}
func (c *converter) emitExecutePositiveLookaroundAssertion(rm *regexpData, node *syntax.RegexNode) {
}
func (c *converter) emitExecuteNegativeLookaroundAssertion(rm *regexpData, node *syntax.RegexNode) {
}

// Gets whether calling Goto(label) will result in exiting the match method.
func gotoWillExitMatch(rm *regexpData, label string) bool {
	return label == rm.topLevelDoneLabel
}

// Emits a goto to jump to the specified label.  However, if the specified label is the top-level done label indicating
// that the entire match has failed, we instead emit our epilogue, uncapturing if necessary and returning out of TryMatchAtCurrentPosition.
func (c *converter) emitExecuteGoto(rm *regexpData, label string) {
	if gotoWillExitMatch(rm, label) {
		// We only get here in the code if the whole expression fails to match and jumps to
		// the original value of doneLabel.
		if rm.expressionHasCaptures {
			c.emitUncaptureUntil("0")
		}
		c.writeLine("return false // The input didn't match.")
	} else {
		c.writeLineFmt("goto %s", label)
	}
}

// Emits code to unwind the capture stack until the crawl position specified in the provided local.
func (c *converter) emitUncaptureUntil(capturepos string) {
	c.writeLineFmt("r.UncaptureUntil(%s)", capturepos)
}

// Emits the sum of a constant and a value from a local.
func sum(constant int, local *string) string {
	if local == nil {
		return strconv.Itoa(constant)
	}
	if constant == 0 {
		return *local
	}
	return fmt.Sprintf("%v + %s", constant, local)
}

func spanLengthCheck(rm *regexpData, requiredLength int, dynamicRequiredLength *string) string {
	if dynamicRequiredLength == nil && rm.sliceStaticPos+requiredLength == 1 {
		return fmt.Sprintf("len(%v) == 0", rm.sliceSpan)
	}
	return fmt.Sprintf("len(%v) < %s", rm.sliceSpan, sum(rm.sliceStaticPos+requiredLength, dynamicRequiredLength))
}

// Adds the value of sliceStaticPos into the pos local, slices slice by the corresponding amount,
// and zeros out sliceStaticPos.
// forceSliceReload = false
func (c *converter) transferSliceStaticPosToPos(rm *regexpData, forceSliceReload bool) {
	if rm.sliceStaticPos > 0 {
		c.emitAddStmt("pos", rm.sliceStaticPos)
		rm.sliceStaticPos = 0
		c.sliceInputSpan(rm, false)
	} else if forceSliceReload {
		c.sliceInputSpan(rm, false)
	}
}

func (c *converter) emitAddStmt(variable string, value int) {
	if value == 0 {
		return
	}
	if value == 1 {
		c.writeLineFmt("%s++", variable)
	}
	if value == -1 {
		c.writeLineFmt("%s--", variable)
	}
	if value > 0 {
		c.writeLineFmt("%s += %v", variable, value)
	}
	if value < 0 {
		c.writeLineFmt("%s -= %v", variable, -value)
	}
}

func (c *converter) sliceInputSpan(rm *regexpData, declare bool) {
	// Slices the inputSpan starting at pos until end and stores it into slice.
	if declare {
		c.write("var ")
	}
	c.writeLineFmt("%s = r.Runslice[pos:]", rm.sliceSpan)
}

func (c *converter) emitTimeoutCheck() {
	c.writeLine(`if err := r.CheckTimeout(); err != nil {
		return err
	}`)
}

// / <summary>Gets a textual description of the node fit for rendering in a comment in source.</summary>
func describeNode(rm *regexpData, node *syntax.RegexNode) string {
	rtl := node.Options&syntax.RightToLeft != 0

	direction := ""
	if rtl {
		direction = " right-to-left"
	}

	switch node.T {
	case syntax.NtAlternate:
		atomic := ""
		if rm.Analysis.IsAtomicByAncestor(node) {
			atomic = ", atomically"
		}
		return fmt.Sprintf(`Match with %v alternative expressions%s.`, len(node.Children), atomic)
	case syntax.NtAtomic:
		return `Atomic group.`
	case syntax.NtBeginning:
		return "Match if at the beginning of the string."
	case syntax.NtBol:
		return "Match if at the beginning of a line."
	case syntax.NtBoundary:
		return `Match if at a word boundary.`
	case syntax.NtCapture:
		if node.M == -1 && node.N != -1 {
			return fmt.Sprintf(`Non-capturing balancing group. Uncaptures the %s.`, describeCapture(rm, node.N))
		} else if node.N != -1 {
			return fmt.Sprintf(`Balancing group. Captures the %s and uncaptures the %s.`, describeCapture(rm, node.M), describeCapture(rm, node.N))
		} else if node.N == -1 {
			return describeCapture(rm, node.M)
		}

	case syntax.NtConcatenate:
		return "Match a sequence of expressions."
	case syntax.NtECMABoundary:
		return `Match if at a word boundary (according to ECMAScript rules).`
	case syntax.NtEmpty:
		return `Match an empty string.`
	case syntax.NtEnd:
		return "Match if at the end of the string."
	case syntax.NtEndZ:
		return "Match if at the end of the string or if before an ending newline."
	case syntax.NtEol:
		return "Match if at the end of a line."
	case syntax.NtLoop, syntax.NtLazyloop:
		if node.M == 0 && node.N == 1 {
			ty := "lazy"
			if node.T == syntax.NtLoop {
				ty = "greedy"
			}
			return fmt.Sprintf(`Optional (%s).`, ty)
		}
		return fmt.Sprintf(`Loop %s%s.`, describeLoop(rm, node), direction)
	case syntax.NtMulti:
		return fmt.Sprintf(`Match the string %#v%s.`, string(node.Str), direction)
	case syntax.NtNonboundary:
		return `Match if at anything other than a word boundary.`
	case syntax.NtNonECMABoundary:
		return `Match if at anything other than a word boundary (according to ECMAScript rules).`
	case syntax.NtNothing:
		return `Fail to match.`
	case syntax.NtNotone:
		return fmt.Sprintf(`Match any character other than %q%s.`, node.Ch, direction)
	case syntax.NtNotoneloop /*syntax.NtNotoneloopatomic,*/, syntax.NtNotonelazy:
		return fmt.Sprintf(`Match a character other than %q %s%s.`, node.Ch, describeLoop(rm, node), direction)
	case syntax.NtOne:
		return fmt.Sprintf(`Match %q%s.`, node.Ch, direction)
	case syntax.NtOneloop /*syntax.NtOneloopatomic,*/, syntax.NtOnelazy:
		return fmt.Sprintf(`Match %q %s%s.`, node.Ch, describeLoop(rm, node), direction)
	case syntax.NtNegLook:
		if rtl {
			return "Zero-width negative lookbehind"
		}
		return "Zero-width negative lookahead"
	case syntax.NtRef:
		return fmt.Sprintf(`Match the same text as matched by the %s%s.`, describeCapture(rm, node.M), direction)
	case syntax.NtPosLook:
		if rtl {
			return "Zero-width positive lookbehind"
		}
		return "Zero-width positive lookahead"
	case syntax.NtSet:
		return fmt.Sprintf(`Match %s%s.`, node.Set.String(), direction)
	case syntax.NtSetloop /*syntax.NtSetloopatomic,*/, syntax.NtSetlazy:
		return fmt.Sprintf(`Match %s %s%s.`, node.Set.String(), describeLoop(rm, node), direction)
	case syntax.NtStart:
		return "Match if at the start position."
	case syntax.NtExprCond:
		return `Conditionally match one of two expressions depending on whether an initial expression matches.`
	case syntax.NtBackRefCond:
		return fmt.Sprintf(`Conditionally match one of two expressions depending on whether the %s matched.`, describeCapture(rm, node.M))
	case syntax.NtUpdateBumpalong:
		return `Advance the next matching position.`
	}
	return fmt.Sprintf(`Unknown node type %v`, node.T)
}

// Gets an identifier to describe a capture group.
func describeCapture(rm *regexpData, capNum int) string {
	// If we can get a capture name from the captures collection and it's not just a numerical representation of the group, use it.
	name := groupNameFromNumber(rm.Tree, capNum)

	id, err := strconv.Atoi(name)
	if err == nil || id != capNum {
		return fmt.Sprintf("%#v capture group", name)
	}
	// Otherwise, create a numerical description of the capture group.
	tens := capNum % 10
	// Ends in 1, 2, 3 but not 11, 12, or 13
	if tens >= 1 && tens <= 3 && !(capNum == 11 || capNum == 12 || capNum == 13) {
		switch tens {
		case 1:
			return fmt.Sprint(capNum, "st capture group")
		case 2:
			return fmt.Sprint(capNum, "nd capture group")
		case 3:
			return fmt.Sprint(capNum, "rd capture group")
		}
	}
	return fmt.Sprint(capNum, "th capture group")

}

// Gets group name from its number.
// matches public version from regexp.go, but uses input caps data
func groupNameFromNumber(tree *syntax.RegexTree, i int) string {
	if len(tree.Caplist) == 0 {
		caplen := len(tree.Capnumlist)
		if tree.Capnumlist == nil {
			caplen = tree.Captop
		}
		if i >= 0 && i < caplen {
			return strconv.Itoa(i)
		}

		return ""
	}

	if tree.Caps != nil {
		var ok bool
		if i, ok = tree.Caps[i]; !ok {
			return ""
		}
	}

	if i >= 0 && i < len(tree.Caplist) {
		return tree.Caplist[i]
	}

	return ""
}

// / <summary>Gets a textual description of a loop's style and bounds.</summary>
func describeLoop(rm *regexpData, node *syntax.RegexNode) string {
	if node.M == node.N {
		return fmt.Sprintf("exactly %v times", node.M)
	}

	var style string
	if node.T == syntax.NtOneloop || node.T == syntax.NtNotoneloop || node.T == syntax.NtSetloop {
		style = "greedily"
	} else if node.T == syntax.NtOnelazy || node.T == syntax.NtNotonelazy || node.T == syntax.NtSetlazy {
		style = "lazily"
	} else if node.T == syntax.NtLoop {
		if rm.Analysis.IsAtomicByAncestor(node) {
			style = "greedily and atomically"
		} else {
			style = "greedily"
		}
	} else {
		if rm.Analysis.IsAtomicByAncestor(node) {
			style = "lazily and atomically"
		} else {
			style = "lazily"
		}
	}

	var bounds string
	if node.N == math.MaxInt32 {
		if node.M == 0 {
			bounds = " any number of times"
		} else if node.M == 1 {
			bounds = " at least once"
		} else if node.M == 2 {
			bounds = " at least twice"
		} else {
			bounds = fmt.Sprintf(" at least %v times", node.M)
		}
	} else if node.M == 0 {
		if node.N == 1 {
			bounds = ", optionally"
		} else {
			bounds = fmt.Sprintf(" at most %v times", node.N)
		}
	} else {
		bounds = fmt.Sprintf(" at least %v and at most %v times", node.M, node.N)
	}

	return style + bounds
}
