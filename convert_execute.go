package main

import (
	"bytes"
	"fmt"
	"math"
	"slices"
	"strconv"
	"strings"

	"github.com/dlclark/regexp2/syntax"
)

// Arbitrary limit for unrolling vs creating a loop.  We want to balance size in the generated
// code with other costs, like the (small) overhead of slicing to create the temp span to iterate.
const MaxUnrollSize = 16

func (c *converter) emitExecute(rm *regexpData) {
	c.writeLineFmt("func (%s_Engine) Execute(r *regexp2.Runner) error {", rm.GeneratedName)
	//c.writeLine(`fmt.Println("Execute")`)
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
	oldOut := c.buf
	buf := &bytes.Buffer{}
	c.buf = buf
	defer func() {
		// lets clean this up at the end
		c.buf = oldOut

		// write additionalDeclarations
		for _, l := range rm.additionalDeclarations {
			c.writeLine(l)
		}

		//reset
		rm.additionalDeclarations = []string{}

		// then write our temp out buffer into our saved buffer
		c.buf.Write(buf.Bytes())
	}()

	// Declare some locals.
	rm.sliceSpan = "slice"
	c.writeLine(`pos := r.Runtextpos
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
			// just to prevent an unused var error in certain regex's
			var _ = slice
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

	//TODO: debug
	c.writeLineFmt("// Node: %s", node.Description())

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
		if node.Options&syntax.RightToLeft == 0 {
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

	case syntax.NtOneloopatomic, syntax.NtNotoneloopatomic, syntax.NtSetloopatomic:
		c.emitExecuteSingleCharAtomicLoop(rm, node, emitLengthChecksIfRequired)
		return

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
	startingStackpos := rm.reserveName("atomic_stackpos")
	rm.addLocalDec(fmt.Sprint(startingStackpos, " := 0"))
	c.writeLineFmt("%s = r.Runstackpos\n", startingStackpos)

	// Emit the child.
	c.emitExecuteNode(rm, node.Children[0], subsequent, true)

	// Reset the stack position and done label.
	c.writeLineFmt("\nr.Runstackpos = %s", startingStackpos)
	rm.doneLabel = originalDoneLabel
}

// Emits the code to handle updating r.Runtextpos to pos in response to
// an UpdateBumpalong node.  This is used when we want to inform the scan loop that
// it should bump from this location rather than from the original location.
func (c *converter) emitExecuteUpdateBumpalong(rm *regexpData, node *syntax.RegexNode) {
	c.transferSliceStaticPosToPos(rm, false)
	c.writeLine(`if r.Runtextpos < pos {
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
					child := node.Children[i]
					if ok, nodesConsumed, caseInsensitiveString := node.TryGetOrdinalCaseInsensitiveString(i, exclusiveEnd, false); ok {
						writePrefix()
						sourceSpan := rm.sliceSpan
						if rm.sliceStaticPos > 0 {
							sourceSpan = fmt.Sprintf("%s[%v:]", rm.sliceSpan, rm.sliceStaticPos)
						}
						c.write(fmt.Sprintf("!helpers.StartsWithIgnoreCase(%s, %s)", sourceSpan, getRuneSliceLiteral(strings.ToLower(caseInsensitiveString))))
						desc := fmt.Sprintf("Match the string %#v (case-insensitive)", caseInsensitiveString)
						prevDescription = &desc
						wroteClauses = true

						rm.sliceStaticPos += len(caseInsensitiveString)
						i += nodesConsumed - 1
					} else if child.T == syntax.NtMulti {
						writePrefix()
						c.emitExecuteMultiCharString(rm, child.Str, false, true, false)
						desc := describeNode(rm, child)
						prevDescription = &desc
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
								desc := describeNode(rm, child)
								prevDescription = &desc
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
						c.writeLineFmt("/* %s */ {", *prevDescription)
					} else {
						c.writeLine(" {")
					}
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
		c.writeLineFmt("if lastIdx := pos - %v; lastIdx < 0 || lastIdx >= len(r.Runtext) {", len(str))
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n")

		c.writeLineFmt(`for i:=0; i < %v; i++ {
						pos--
						if r.Runtext[pos] != %s[%v - i] {`, len(str), getRuneSliceLiteral(str), len(str)-1)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n}")

		return
	}

	sourceSpan := rm.sliceSpan
	if rm.sliceStaticPos > 0 {
		sourceSpan = fmt.Sprintf("%s[%v:]", rm.sliceSpan, rm.sliceStaticPos)
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
		if !emitLengthCheck {
			clause = fmt.Sprintf("if %s {", expr)
		} else if !rtl {
			clause = fmt.Sprintf("if %s || %s {", spanLengthCheck(rm, 1, offset), expr)
		} else {
			clause = fmt.Sprintf("if newIdx := pos - 1; newIdx < 0 || newIdx >= len(r.Runtext) || %s {", expr)
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
	// If this is actually atomic based on its parent, emit it as atomic instead; no backtracking necessary.
	if rm.Analysis.IsAtomicByAncestor(node) {
		c.emitExecuteSingleCharAtomicLoop(rm, node, true)
		return
	}

	// If this is actually a repeater, emit that instead; no backtracking necessary.
	if node.M == node.N {
		c.emitExecuteSingleCharRepeater(rm, node, emitLengthChecksIfRequired)
		return
	}

	// Emit backtracking around an atomic single char loop.  We can then implement the backtracking
	// as an afterthought, since we know exactly how many characters are accepted by each iteration
	// of the wrapped loop (1) and that there's nothing captured by the loop.

	backtrackingLabel := rm.reserveName("CharLoopBacktrack")
	endLoop := rm.reserveName("CharLoopEnd")
	startingPos := rm.reserveName("charloop_starting_pos")
	endingPos := rm.reserveName("charloop_ending_pos")
	rm.addLocalDec(fmt.Sprintf("var %s, %s = 0, 0", startingPos, endingPos))
	rtl := node.Options&syntax.RightToLeft != 0
	isInLoop := rm.Analysis.IsInLoop(node)

	// We're about to enter a loop, so ensure our text position is 0.
	c.transferSliceStaticPosToPos(rm, false)

	// Grab the current position, then emit the loop as atomic, and then
	// grab the current position again.  Even though we emit the loop without
	// knowledge of backtracking, we can layer it on top by just walking back
	// through the individual characters (a benefit of the loop matching exactly
	// one character per iteration, no possible captures within the loop, etc.)
	c.writeLineFmt("%s = pos\n", startingPos)
	c.emitExecuteSingleCharAtomicLoop(rm, node, true)
	c.writeLine("")

	c.transferSliceStaticPosToPos(rm, false)
	c.writeLineFmt("%s = pos", endingPos)
	if !rtl {
		c.emitAddStmt(startingPos, node.M)
	} else {
		c.emitAddStmt(startingPos, -node.M)
	}
	c.emitExecuteGoto(rm, endLoop)
	c.writeLine("")

	// Backtracking section. Subsequent failures will jump to here, at which
	// point we decrement the matched count as long as it's above the minimum
	// required, and try again by flowing to everything that comes after this.
	c.emitMarkLabel(rm, backtrackingLabel, false)
	stackCookie := c.createStackCookie()
	var capturePos string
	if isInLoop {
		// This loop is inside of another loop, which means we persist state
		// on the backtracking stack rather than relying on locals to always
		// hold the right state (if we didn't do that, another iteration of the
		// outer loop could have resulted in the locals being overwritten).
		// Pop the relevant state from the stack.
		if rm.expressionHasCaptures {
			c.emitUncaptureUntil("r.StackPop()")
		}
		c.emitStackPop(stackCookie, endingPos, startingPos)
	} else if rm.expressionHasCaptures {
		// Since we're not in a loop, we're using a local to track the crawl position.
		// Unwind back to the position we were at prior to running the code after this loop.
		capturePos = rm.reserveName("charloop_capture_pos")
		rm.addLocalDec(fmt.Sprintf("%s := 0", capturePos))
		c.emitUncaptureUntil(capturePos)
	}
	c.writeLine("")

	// We're backtracking.  Check the timeout.
	c.emitTimeoutCheckIfNeeded(rm)

	var literalNode *syntax.RegexNode
	if subsequent != nil {
		literalNode = subsequent.FindStartingLiteralNode(true)
	}
	var literalLength int
	var indexOfExpr string

	if !rtl &&
		node.N > 1 && // no point in using IndexOf for small loops, in particular optionals
		literalNode != nil &&
		c.tryEmitExecuteIndexOf(rm, literalNode, fmt.Sprintf("r.Runtext[%s:%%s]", startingPos), true, false, &literalLength, &indexOfExpr) {
		//hack -- if the indexOfExpr comes back it'll be a format string (notice the double %)
		//for the final index into the slice so we can populate it here
		//e.g. r.Runtext[startingPos:%s]
		c.writeLineFmt(`if %s >= %s {`, startingPos, endingPos)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")

		if literalLength > 1 {
			indexOfExpr = fmt.Sprintf(indexOfExpr, fmt.Sprintf("helpers.Min(len(r.Runtext), %s+%v)", endingPos, literalLength-1))
		} else {
			indexOfExpr = fmt.Sprintf(indexOfExpr, endingPos)
		}
		c.writeLineFmt("%s = %s", endingPos, indexOfExpr)
		c.writeLineFmt("if %s < 0 { // miss", endingPos)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
		c.writeLineFmt(`%s += %s
			pos = %[1]s`, endingPos, startingPos)
	} else {
		op := ">="
		if rtl {
			op = "<="
		}
		c.writeLineFmt("if %s %s %s {", startingPos, op, endingPos)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
		if !rtl {
			c.writeLineFmt(`%s--
			pos = %[1]s`, endingPos)
		} else {
			c.writeLineFmt(`%s++
			pos = %[1]s`, endingPos)
		}
	}

	if !rtl {
		c.sliceInputSpan(rm, false)
	}
	c.writeLine("")

	c.emitMarkLabel(rm, endLoop, false)
	if isInLoop {
		// We're in a loop and thus can't rely on locals correctly holding the state we
		// need (the locals could be overwritten by a subsequent iteration).  Push the state
		// on to the backtracking stack.
		if rm.expressionHasCaptures {
			c.emitStackPush(stackCookie, startingPos, endingPos, "r.Crawlpos()")
		} else {
			c.emitStackPush(stackCookie, startingPos, endingPos)
		}
	} else if len(capturePos) > 0 {
		// We're not in a loop and so can trust our locals.  Store the current capture position
		// into the capture position local; we'll uncapture back to this when backtracking to
		// remove any captures from after this loop that we need to throw away.
		c.writeLineFmt("%s = r.Crawlpos()", capturePos)
	}

	rm.doneLabel = backtrackingLabel // leave set to the backtracking label for all subsequent nodes
}

func (c *converter) emitMarkLabel(rm *regexpData, label string, emitSemiColon bool) {
	rm.emittedLabels = append(rm.emittedLabels, label)
	if emitSemiColon {
		c.writeLineFmt("%s: ;", label)
	} else {
		c.writeLineFmt("%s:", label)
	}
}

// emitLengthChecksIfRequired=true
func (c *converter) emitExecuteSingleCharLazy(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode, emitLengthChecksIfRequired bool) {
	// Emit the min iterations as a repeater.  Any failures here don't necessitate backtracking,
	// as the lazy itself failed to match, and there's no backtracking possible by the individual
	// characters/iterations themselves.
	if node.M > 0 {
		c.emitExecuteSingleCharRepeater(rm, node, emitLengthChecksIfRequired)
	}

	// If the whole thing was actually that repeater, we're done. Similarly, if this is actually an atomic
	// lazy loop, nothing will ever backtrack into this node, so we never need to iterate more than the minimum.
	if node.M == node.N || rm.Analysis.IsAtomicByAncestor(node) {
		return
	}

	if node.M > 0 {
		// We emitted a repeater to handle the required iterations; add a newline after it.
		c.writeLine("")
	}

	// We now need to match one character at a time, each time allowing the remainder of the expression
	// to try to match, and only matching another character if the subsequent expression fails to match.

	// We're about to enter a loop, so ensure our text position is 0.
	c.transferSliceStaticPosToPos(rm, false)

	// If the loop isn't unbounded, track the number of iterations and the max number to allow.
	var iterationCount, maxIterations string
	if node.N != math.MaxInt32 {
		maxIterations = strconv.Itoa(node.N - node.M)
		iterationCount = rm.reserveName("lazyloop_iteration")
		rm.addLocalDec(fmt.Sprint(iterationCount, " := 0"))
		c.writeLineFmt("%s = 0", iterationCount)
	}

	// Track the current crawl position.  Upon backtracking, we'll unwind any captures beyond this point.
	var capturePos string
	if rm.expressionHasCaptures {
		capturePos = rm.reserveName("lazyloop_capturepos")
		rm.addLocalDec(fmt.Sprint(capturePos, " := 0"))
	}

	// Track the current pos.  Each time we backtrack, we'll reset to the stored position, which
	// is also incremented each time we match another character in the loop.
	startingPos := rm.reserveName("lazyloop_pos")
	rm.addLocalDec(fmt.Sprint(startingPos, " := 0"))
	c.writeLineFmt("%s = pos", startingPos)

	// Skip the backtracking section for the initial subsequent matching.  We've already matched the
	// minimum number of iterations, which means we can successfully match with zero additional iterations.
	endLoopLabel := rm.reserveName("LazyLoopEnd")
	c.emitExecuteGoto(rm, endLoopLabel)
	c.writeLine("")

	// Backtracking section. Subsequent failures will jump to here.
	backtrackingLabel := rm.reserveName("LazyLoopBacktrack")
	c.emitMarkLabel(rm, backtrackingLabel, false)

	// Uncapture any captures if the expression has any.  It's possible the captures it has
	// are before this node, in which case this is wasted effort, but still functionally correct.
	if len(capturePos) > 0 {
		c.emitUncaptureUntil(capturePos)
	}

	// If there's a max number of iterations, see if we've exceeded the maximum number of characters
	// to match.  If we haven't, increment the iteration count.
	if len(maxIterations) > 0 {
		c.writeLineFmt("if %s >= %s {", iterationCount, maxIterations)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLineFmt("}\n%s++", iterationCount)
	}

	// We're backtracking.  Check the timeout.
	c.emitTimeoutCheckIfNeeded(rm)

	// Now match the next item in the lazy loop.  We need to reset the pos to the position
	// just after the last character in this loop was matched, and we need to store the resulting position
	// for the next time we backtrack.
	c.writeLineFmt("pos = %s", startingPos)
	c.sliceInputSpan(rm, false)
	c.emitExecuteSingleChar(rm, node, true, nil, false)
	c.transferSliceStaticPosToPos(rm, false)

	// Now that we've appropriately advanced by one character and are set for what comes after the loop,
	// see if we can skip ahead more iterations by doing a search for a following literal.
	if (node.Options & syntax.RightToLeft) == 0 {
		var literal *syntax.StartingLiteral
		var literalNode *syntax.RegexNode
		var indexOfExpr string

		if subsequent != nil {
			literal = subsequent.FindStartingLiteral()
			literalNode = subsequent.FindStartingLiteralNode(true)
		}
		if len(iterationCount) == 0 && node.T == syntax.NtNotonelazy &&
			literal != nil &&
			!literal.Negated && // not negated; can't search for both the node.Ch and a negated subsequent char with an IndexOf* method
			(len(literal.String) > 0 ||
				len(literal.SetChars) > 0 ||
				literal.Range.First == literal.Range.Last ||
				(literal.Range.First <= node.Ch && node.Ch <= literal.Range.Last)) {
			// for ranges, only allow when the range overlaps with the target, since there's no accelerated way to search for the union

			// e.g. "<[^>]*?>"

			// Whether the not'd character matches the subsequent literal. This impacts whether we need to search
			// for both or just the literal, as well as what assumptions we can make once a match is found.
			var overlap bool

			// This lazy loop will consume all characters other than node.Ch until the subsequent literal.
			// We can implement it to search for either that char or the literal, whichever comes first.
			if len(literal.String) > 0 {
				// string literal
				overlap = (literal.String[0] == node.Ch)
				if overlap {
					c.writeLineFmt("%s = helpers.IndexOfAny1(%s, %q)", startingPos, rm.sliceSpan, node.Ch)
				} else {
					c.writeLineFmt("%s = helpers.IndexOfAny2(%s, %q, %q)", startingPos, rm.sliceSpan, node.Ch, literal.String[0])
				}
			} else if len(literal.SetChars) > 0 {
				// set literal
				overlap = slices.Contains(literal.SetChars, node.Ch)
				var chars []rune
				if !overlap {
					chars = []rune{node.Ch}
				}
				chars = append(chars, literal.SetChars...)
				c.writeLineFmt("%s = %s", startingPos, c.emitIndexOfChars(chars, false, rm.sliceSpan))
			} else if literal.Range.First == literal.Range.Last {
				// single char from a RegexNode.One
				overlap = (literal.Range.First == node.Ch)
				if overlap {
					c.writeLineFmt("%s = helpers.IndexOfAny1(%s, %q)", startingPos, rm.sliceSpan, node.Ch)
				} else {
					c.writeLineFmt("%s = helpers.IndexOfAny2(%s, %q, %q)", startingPos, rm.sliceSpan, node.Ch, literal.Range.First)
				}
			} else {
				// char range
				overlap = true
				c.writeLineFmt("%s = helpers.IndexOfAnyInRange(%s, %q, %q)", startingPos, rm.sliceSpan, literal.Range.First, literal.Range.Last)
			}

			// If the search didn't find anything, fail the match.  If it did find something, then we need to consider whether
			// that something is the loop character.  If it's not, we've successfully backtracked to the next lazy location
			// where we should evaluate the rest of the pattern.  If it does match, then we need to consider whether there's
			// overlap between the loop character and the literal.  If there is overlap, this is also a place to check.  But
			// if there's not overlap, and if the found character is the loop character, we also want to fail the match here
			// and now, as this means the loop ends before it gets to what needs to come after the loop, and thus the pattern
			// can't possibly match here.
			if overlap {
				c.writeLineFmt("if %s < 0 {", startingPos)
			} else {
				c.writeLineFmt("if %s >= len(%s) || %[2]s[%[1]s] == %[3]q {", startingPos, rm.sliceSpan, node.Ch)
			}
			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLineFmt(`}
						pos += %s`, startingPos)
			c.sliceInputSpan(rm, false)
		} else if len(iterationCount) == 0 &&
			node.T == syntax.NtSetlazy &&
			node.Set.IsAnything() &&
			literalNode != nil &&
			c.tryEmitExecuteIndexOf(rm, literalNode, rm.sliceSpan, false, false, new(int), &indexOfExpr) {
			// e.g. ".*?string" with RegexOptions.Singleline
			// This lazy loop will consume all characters until the subsequent literal. If the subsequent literal
			// isn't found, the loop fails. We can implement it to just search for that literal.
			c.writeLineFmt("%s = %s", startingPos, indexOfExpr)
			c.writeLineFmt("if %s < 0 {", startingPos)
			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLineFmt(`}
						pos += %s`, startingPos)
			c.sliceInputSpan(rm, false)
		}
	}

	// Store the position we've left off at in case we need to iterate again.
	c.writeLineFmt("%s = pos", startingPos)

	// Update the done label for everything that comes after this node.  This is done after we emit the single char
	// matching, as that failing indicates the loop itself has failed to match.
	rm.doneLabel = backtrackingLabel // leave set to the backtracking label for all subsequent nodes

	c.writeLine("")
	isInLoop := rm.Analysis.IsInLoop(node)

	c.emitMarkLabel(rm, endLoopLabel, !(len(capturePos) > 0 || isInLoop))
	if len(capturePos) != 0 {
		c.writeLineFmt("%s = r.Crawlpos()", capturePos)
	}

	// If this loop is itself not in another loop, nothing more needs to be done:
	// upon backtracking, locals being used by this loop will have retained their
	// values and be up-to-date.  But if this loop is inside another loop, multiple
	// iterations of this loop each need their own state, so we need to use the stack
	// to hold it, and we need a dedicated backtracking section to handle restoring
	// that state before jumping back into the loop itself.
	if isInLoop {
		c.writeLine("")
		stackCookie := c.createStackCookie()

		// Store the loop's state.
		args := []string{startingPos}
		if len(capturePos) > 0 {
			args = append(args, capturePos)
		}
		if len(iterationCount) > 0 {
			args = append(args, iterationCount)
		}
		c.emitStackPush(stackCookie, args...)

		// Skip past the backtracking section.
		end := rm.reserveName("LazyLoopSkipBacktrack")
		c.emitExecuteGoto(rm, end)
		c.writeLine("")

		// Emit a backtracking section that restores the loop's state and then jumps to the previous done label.
		backtrack := rm.reserveName("CharLazyBacktrack")
		c.emitMarkLabel(rm, backtrack, false)

		// Restore the loop's state.
		// pop in reverse order
		slices.Reverse(args)
		c.emitStackPop(stackCookie, args...)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("")
		rm.doneLabel = backtrack
		c.emitMarkLabel(rm, end, false)
	}
}

// Emits the code to handle a non-backtracking, variable-length loop around a single character comparison.
// emitLengthChecksIfRequired=true
func (c *converter) emitExecuteSingleCharAtomicLoop(rm *regexpData, node *syntax.RegexNode, emitLengthChecksIfRequired bool) {
	// If this is actually a repeater, emit that instead.
	if node.M == node.N {
		c.emitExecuteSingleCharRepeater(rm, node, emitLengthChecksIfRequired)
		return
	}

	// If this is actually an optional single char, emit that instead.
	if node.M == 0 && node.N == 1 {
		c.emitExecuteAtomicSingleCharZeroOrOne(rm, node)
		return
	}

	minIterations := node.M
	maxIterations := node.N
	rtl := (node.Options & syntax.RightToLeft) != 0
	iterationLocal := rm.reserveName("iteration")
	// needs to be function-wide in Go otherwise our Goto statements will fail because
	// Go doesn't support jumping over a variable decl
	rm.addLocalDec(fmt.Sprint(iterationLocal, " := 0"))
	var indexOfExpr string

	if rtl {
		c.transferSliceStaticPosToPos(rm, false) // we don't use static position for rtl

		if node.IsSetFamily() && maxIterations == math.MaxInt32 && node.Set.IsAnything() {
			// If this loop will consume the remainder of the input, just set the iteration variable
			// to pos directly rather than looping to get there.
			c.writeLineFmt("%s = pos", iterationLocal)
		} else {
			c.writeLineFmt("%s = 0", iterationLocal)

			expr := fmt.Sprintf("r.Runtext[pos - %s - 1]", iterationLocal)
			if node.IsSetFamily() {
				expr = c.emitMatchCharacterClass(rm, node.Set, false, expr)
			} else {
				op := "!="
				if node.IsOneFamily() {
					op = "=="
				}
				expr = fmt.Sprintf("%s %s %q", expr, op, node.Ch)
			}

			maxClause := ""
			if maxIterations != math.MaxInt32 {
				maxClause = fmt.Sprintf("%s && ", countIsLessThan(iterationLocal, maxIterations))
			}
			c.writeLineFmt(`for %spos > %s && %s {
					%[2]s++
				}
				`, maxClause, iterationLocal, expr)
		}
	} else if node.IsSetFamily() && maxIterations == math.MaxInt32 && node.Set.IsAnything() {
		// .* was used with RegexOptions.Singleline, which means it'll consume everything.  Just jump to the end.
		// The unbounded constraint is the same as in the Notone case above, done purely for simplicity.

		c.transferSliceStaticPosToPos(rm, false)
		c.writeLineFmt("%s = len(r.Runtext) - pos", iterationLocal)
	} else if c.tryEmitExecuteIndexOf(rm, node, "%s", false, true, new(int), &indexOfExpr) {
		// We can use an IndexOf method to perform the search. If the number of iterations is unbounded, we can just search the whole span.
		// If, however, it's bounded, we need to slice the span to the min(remainingSpan.Length, maxIterations) so that we don't
		// search more than is necessary.

		// If maxIterations is 0, the node should have been optimized away. If it's 1 and min is 0, it should
		// have been handled as an optional loop above, and if it's 1 and min is 1, it should have been transformed
		// into a single char match. So, we should only be here if maxIterations is greater than 1. And that's relevant,
		// because we wouldn't want to invest in an IndexOf call if we're only going to iterate once.
		c.transferSliceStaticPosToPos(rm, false)

		if maxIterations != math.MaxInt32 {
			indexOfExpr = fmt.Sprintf(indexOfExpr, fmt.Sprintf("%s[:helpers.Min(len(%[1]s), %v)]", rm.sliceSpan, maxIterations))
		} else {
			indexOfExpr = fmt.Sprintf(indexOfExpr, rm.sliceSpan)
		}
		c.writeLineFmt("%s = %s", iterationLocal, indexOfExpr)

		rhs := fmt.Sprintf("len(%s)", rm.sliceSpan)
		if maxIterations != math.MaxInt32 {
			rhs = fmt.Sprintf("helpers.Min(len(%s), %v)", rm.sliceSpan, maxIterations)
		}
		c.writeLineFmt(`if %s < 0 {
				%[1]s = %s
			}
			`, iterationLocal, rhs)
	} else {
		// For everything else, do a normal loop.
		expr := fmt.Sprintf("%s[%v]", rm.sliceSpan, iterationLocal)
		if node.IsSetFamily() {
			expr = c.emitMatchCharacterClass(rm, node.Set, false, expr)
		} else {
			op := "!="
			if node.IsOneFamily() {
				op = "=="
			}
			expr = fmt.Sprintf("%s %s %q", expr, op, node.Ch)
		}

		if minIterations != 0 || maxIterations != math.MaxInt32 {
			// For any loops other than * loops, transfer text pos to pos in
			// order to zero it out to be able to use the single iteration variable
			// for both iteration count and indexer.
			c.transferSliceStaticPosToPos(rm, false)
		}

		c.writeLineFmt("%s = %v", iterationLocal, rm.sliceStaticPos)
		rm.sliceStaticPos = 0

		maxClause := ""
		if maxIterations != math.MaxInt32 {
			maxClause = fmt.Sprintf("%s && ", countIsLessThan(iterationLocal, maxIterations))
		}
		c.writeLineFmt(`for %s%s < len(%s) && %s {
			   %[2]s++
		   }
		   `, maxClause, iterationLocal, rm.sliceSpan, expr)
	}

	// Check to ensure we've found at least min iterations.
	if minIterations > 0 {
		c.writeLineFmt("if %s {", countIsLessThan(iterationLocal, minIterations))
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n")
	}

	// Now that we've completed our optional iterations, advance the text span
	// and pos by the number of iterations completed.

	if !rtl {
		c.writeLineFmt(`%s = %[1]s[%s:]
			pos += %[2]s`, rm.sliceSpan, iterationLocal)
	} else {
		c.writeLineFmt("pos -= %s", iterationLocal)
	}
}

// Gets a comparison for whether the iteration count is less than the upper bound.
func countIsLessThan(count string, exclusiveUpper int) string {
	if exclusiveUpper == 1 {
		return count + " == 0"
	}
	return fmt.Sprintf("%s < %v", count, exclusiveUpper)
}

// Gets a comparison for whether the iteration count is greater than or equal to the upper bound
func countIsGreaterThanOrEqualTo(count string, exclusiveUpper int) string {
	if exclusiveUpper == 1 {
		return count + " != 0"
	}
	return fmt.Sprintf("%s >= %v", count, exclusiveUpper)
}

// emitLengthCheck=true
func (c *converter) emitExecuteSingleCharRepeater(rm *regexpData, node *syntax.RegexNode, emitLengthCheck bool) {
	iterations := node.M
	rtl := node.Options&syntax.RightToLeft != 0

	// No iterations, nothing to do.
	if iterations == 0 {
		return
	}
	// Just match the individual item
	if iterations == 1 {
		c.emitExecuteSingleChar(rm, node, emitLengthCheck, nil, false)
		return
	}
	if iterations < syntax.MultiVsRepeaterLimit && node.IsOneFamily() {
		// This is a repeated case-sensitive character; emit it as a multi in order to get all the optimizations
		// afforded to a multi, e.g. unrolling the loop with multi-char reads/comparisons at a time.
		c.emitExecuteMultiCharString(rm, []rune(strings.Repeat(string(node.Ch), iterations)), emitLengthCheck, false, rtl)
		return
	}

	if rtl {
		c.transferSliceStaticPosToPos(rm, false) // we don't use static position with rtl
		c.writeLineFmt("for i:=0; i < %v; i++ {", iterations)
		c.emitExecuteSingleChar(rm, node, true, nil, false)
		c.writeLine("}")
	} else if node.IsSetFamily() && node.Set.IsAnything() {
		// This is a repeater for anything, which means we only care about length and can jump past that length.
		if emitLengthCheck {
			c.emitSpanLengthCheck(rm, iterations, nil)
		}
		rm.sliceStaticPos += iterations
	} else if iterations <= MaxUnrollSize {
		// if ((uint)(sliceStaticPos + iterations - 1) >= (uint)slice.Length ||
		//     slice[sliceStaticPos] != c1 ||
		//     slice[sliceStaticPos + 1] != c2 ||
		//     ...)
		// {
		//     goto doneLabel;
		// }
		c.write("if ")
		if emitLengthCheck {
			c.write(spanLengthCheck(rm, iterations, nil))
			c.writeLine(" ||")
		}
		c.emitExecuteSingleChar(rm, node, false, nil, true)
		for i := 1; i < iterations; i++ {
			c.writeLine(" ||")
			c.emitExecuteSingleChar(rm, node, false, nil, true)
		}
		c.writeLine(" {")
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
	} else {
		// if ((uint)(sliceStaticPos + iterations - 1) >= (uint)slice.Length) goto doneLabel;
		if emitLengthCheck {
			c.emitSpanLengthCheck(rm, iterations, nil)
			c.writeLine("")
		}

		// If we're able to vectorize the search, do so. Otherwise, fall back to a loop.
		// For the loop, we're validating that each char matches the target node.
		// For IndexOf, we're looking for the first thing that _doesn't_ match the target node,
		// and thus similarly validating that everything does.
		sliceName := fmt.Sprintf("%s[%v:%v]", rm.sliceSpan, rm.sliceStaticPos, rm.sliceStaticPos+iterations)
		var indexOfExpr string
		if c.tryEmitExecuteIndexOf(rm, node, sliceName, false, true, new(int), &indexOfExpr) {
			c.writeLineFmt("if %s >= 0 {", indexOfExpr)
			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLine("}")
		} else {
			repeaterSpan := "repeaterSlice" // As this repeater doesn't wrap arbitrary node emits, this shouldn't conflict with anything
			c.writeLineFmt(`%s := %s
						 for i:=0; i < len(%[1]s); i++ {`, repeaterSpan, sliceName)
			tmpTextSpanLocal, tmpSliceStaticPos := rm.sliceSpan, rm.sliceStaticPos
			rm.sliceSpan = repeaterSpan
			rm.sliceStaticPos = 0
			i := "i"
			c.emitExecuteSingleChar(rm, node, false, &i, false)
			rm.sliceSpan = tmpTextSpanLocal
			rm.sliceStaticPos = tmpSliceStaticPos
		}

		rm.sliceStaticPos += iterations
	}
}

// Emits the code to handle a non-backtracking optional zero-or-one loop.
func (c *converter) emitExecuteAtomicSingleCharZeroOrOne(rm *regexpData, node *syntax.RegexNode) {
	rtl := (node.Options & syntax.RightToLeft) != 0
	if rtl {
		c.transferSliceStaticPosToPos(rm, false) // we don't use static pos for rtl
	}

	expr := fmt.Sprintf("%s[%v]", rm.sliceSpan, rm.sliceStaticPos)
	if rtl {
		expr = "r.Runtext[pos-1]"
	}

	if node.IsSetFamily() {
		expr = c.emitMatchCharacterClass(rm, node.Set, false, expr)
	} else {
		op := "!="
		if node.IsOneFamily() {
			op = "=="
		}
		expr = fmt.Sprintf("%s %s %q", expr, op, node.Ch)
	}

	var spaceAvailable string
	if rtl {
		spaceAvailable = "pos > 0"
	} else if rm.sliceStaticPos != 0 {
		spaceAvailable = fmt.Sprintf("len(%s) > %v", rm.sliceSpan, rm.sliceStaticPos)
	} else {
		spaceAvailable = fmt.Sprintf("len(%s) > 0", rm.sliceSpan)
	}

	c.writeLineFmt("if %s && %s {", spaceAvailable, expr)
	if !rtl {
		c.writeLineFmt(`%s = %[1]s[1:]
		pos++`, rm.sliceSpan)
	} else {
		c.writeLineFmt("pos--")
	}
	c.writeLine("}")
}

func (c *converter) emitTimeoutCheckIfNeeded(rm *regexpData) {
	// we just always write it for now
	c.emitTimeoutCheck()
}

// tries to create an indexof call for a node
func (c *converter) tryEmitExecuteIndexOf(rm *regexpData, node *syntax.RegexNode, spanName string, useLast bool, negate bool, literalLength *int, indexOfExpr *string) bool {
	last := ""
	if useLast {
		last = "Last"
	}

	if node.T == syntax.NtMulti {
		*indexOfExpr = fmt.Sprintf("helpers.%sIndexOf(%s, %s)", last, spanName, getRuneSliceLiteral(node.Str))
		*literalLength = len(node.Str)
		return true
	}

	if node.IsOneFamily() {
		var expr string
		if negate {
			expr = fmt.Sprintf("helpers.%sIndexOfAnyExcept1(%s, %q)", last, spanName, node.Ch)
		} else {
			expr = fmt.Sprintf("helpers.%sIndexOfAny1(%s, %q)", last, spanName, node.Ch)
		}
		*indexOfExpr = expr
		*literalLength = 1
		return true
	}

	if node.IsNotoneFamily() {
		var expr string
		if negate {
			expr = fmt.Sprintf("helpers.%sIndexOfAny1(%s, %q)", last, spanName, node.Ch)
		} else {
			expr = fmt.Sprintf("helpers.%sIndexOfAnyExcept1(%s, %q)", last, spanName, node.Ch)
		}
		*indexOfExpr = expr
		*literalLength = 1
		return true
	}

	if node.IsSetFamily() {
		negate = node.Set.IsNegated() != negate

		// Prefer IndexOfAnyInRange over IndexOfAny, except for tiny ranges (1 or 2 items) that IndexOfAny handles more efficiently
		if rs := node.Set.GetIfNRanges(1); len(rs) == 1 && rs[0].Last-rs[0].First > 1 {
			var expr string
			if negate {
				expr = fmt.Sprintf("helpers.%sIndexOfAnyExceptInRange(%s, %q, %q)", last, spanName, rs[0].First, rs[0].Last)
			} else {
				expr = fmt.Sprintf("helpers.%sIndexOfAnyInRange(%s, %q, %q)", last, spanName, rs[0].First, rs[0].Last)
			}
			*indexOfExpr = expr
			*literalLength = 1
			return true
		}

		setChars := node.Set.GetSetChars(128)
		if len(setChars) > 0 {
			expr := c.emitIndexOfChars(setChars, negate, spanName)
			*indexOfExpr = expr
			*literalLength = 1
			return true
		}
	}

	indexOfExpr = nil
	*literalLength = 0
	return false
}

func (c *converter) emitExecuteAnchors(rm *regexpData, node *syntax.RegexNode) {
	switch node.T {
	case syntax.NtBeginning, syntax.NtStart:
		if rm.sliceStaticPos > 0 {
			// If we statically know we've already matched part of the regex, there's no way we're at the
			// beginning or start, as we've already progressed past it.
			c.emitExecuteGoto(rm, rm.doneLabel)
		} else {
			if node.T == syntax.NtBeginning {
				c.writeLine("if pos != 0 {")
			} else {
				c.writeLine("if pos != r.Runtextstart {")
			}
			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLine("}")
		}

	case syntax.NtBol:
		if rm.sliceStaticPos > 0 {
			c.writeLineFmt("if %s[%v-1] != '\\n' {", rm.sliceSpan, rm.sliceStaticPos)
		} else {
			c.writeLine("if pos > 0 && r.Runtext[pos-1] != '\\n' {")
		}
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")

	case syntax.NtEnd:
		if rm.sliceStaticPos > 0 {
			c.writeLineFmt("if %v < len(%s) {", rm.sliceStaticPos, rm.sliceSpan)
		} else {
			c.writeLine("if pos < len(r.Runtext) {")
		}
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")

	case syntax.NtEndZ:
		if rm.sliceStaticPos > 0 {
			c.writeLineFmt("if len(%s) > %v || (len(%[1]s) > %[3]v && %[1]s[%[3]v] != '\\n') {", rm.sliceSpan, rm.sliceStaticPos+1, rm.sliceStaticPos)
		} else {
			c.writeLine("if (pos < len(r.Runtext) - 1) || (pos < len(r.Runtext) && r.Runtext[pos] != '\\n') {")
		}

		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")

	case syntax.NtEol:
		if rm.sliceStaticPos > 0 {
			c.writeLineFmt("if %v < len(%s) && %[2]s[%[1]v] != '\\n' {", rm.sliceStaticPos, rm.sliceSpan)
		} else {
			c.writeLine("if pos < len(r.Runtext) && r.Runtext[pos] != '\\n' {")
		}
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}")
	}
}

// Emits the code to handle a boundary check on a character.
func (c *converter) emitExecuteBoundary(rm *regexpData, node *syntax.RegexNode) {
	var call string
	switch node.T {
	case syntax.NtBoundary:
		call = "!r.IsBoundary"
	case syntax.NtNonboundary:
		call = "r.IsBoundary"
	case syntax.NtECMABoundary:
		call = "!r.IsECMABoundary"
	case syntax.NtNonECMABoundary:
		call = "r.IsECMABoundary"
	}

	end := ""
	if rm.sliceStaticPos > 0 {
		end = fmt.Sprint(" + ", rm.sliceStaticPos)
	}
	c.writeLineFmt("if %s(pos%s) {", call, end)
	c.emitExecuteGoto(rm, rm.doneLabel)
	c.writeLineFmt("}")
}

func (c *converter) emitExecuteNonBacktrackingRepeater(rm *regexpData, node *syntax.RegexNode) {
	// Ensure every iteration of the loop sees a consistent value.
	c.transferSliceStaticPosToPos(rm, false)

	// Loop M==N times to match the child exactly that numbers of times.
	i := rm.reserveName("loop_iteration")
	c.writeLineFmt("for %s := 0; %[1]s < %v; %[1]s++ {", i, node.M)
	c.emitExecuteNode(rm, node.Children[0], nil, true)
	c.transferSliceStaticPosToPos(rm, false) // make sure static the static position remains at 0 for subsequent constructs
	c.writeLine("}")
}

func (c *converter) emitExecuteLoop(rm *regexpData, node *syntax.RegexNode) {
	child := node.Children[0]

	minIterations := node.M
	maxIterations := node.N
	stackCookie := c.createStackCookie()

	// Special-case some repeaters.
	if minIterations == maxIterations {
		if minIterations == 0 {
			// No iteration. Nop.
			return
		}
		if minIterations == 1 {
			// One iteration.  Just emit the child without any loop ceremony.
			c.emitExecuteNode(rm, child, nil, true)
			return
		}
		if minIterations > 1 && !rm.Analysis.MayBacktrack(child) {
			// The child doesn't backtrack.  Emit it as a non-backtracking repeater.
			// (If the child backtracks, we need to fall through to the more general logic
			// that supports unwinding iterations.)
			c.emitExecuteNonBacktrackingRepeater(rm, node)
			return
		}
	}

	// We might loop any number of times.  In order to ensure this loop and subsequent code sees sliceStaticPos
	// the same regardless, we always need it to contain the same value, and the easiest such value is 0.
	// So, we transfer sliceStaticPos to pos, and ensure that any path out of here has sliceStaticPos as 0.
	c.transferSliceStaticPosToPos(rm, false)

	isAtomic := rm.Analysis.IsAtomicByAncestor(node)
	var startingStackpos string
	usedStartingStackpos := false
	if isAtomic || minIterations > 1 {
		// If the loop is atomic, constructs will need to backtrack around it, and as such any backtracking
		// state pushed by the loop should be removed prior to exiting the loop.  Similarly, if the loop has
		// a minimum iteration count greater than 1, we might end up with at least one successful iteration
		// only to find we can't iterate further, and will need to clear any pushed state from the backtracking
		// stack.  For both cases, we need to store the starting stack index so it can be reset to that position.

		// but if the child backtracks then we won't use this...
		// which is a no-no in Go
		startingStackpos = rm.reserveName("startingStackpos")
		oldOut := c.buf
		newOut := &bytes.Buffer{}
		c.buf = newOut
		defer func() {
			// swap out buffers
			c.buf = oldOut

			// write new var if we need
			if usedStartingStackpos {
				rm.addLocalDec(fmt.Sprintf("%s := 0", startingStackpos))
				c.writeLineFmt("%s = r.Runstackpos", startingStackpos)
			}

			// copy new buffer into old one
			c.buf.Write(newOut.Bytes())
		}()

	}

	originalDoneLabel := rm.doneLabel
	body := rm.reserveName("LoopBody")
	endLoop := rm.reserveName("LoopEnd")
	iterationCount := rm.reserveName("loop_iteration")

	// Loops that match empty iterations need additional checks in place to prevent infinitely matching (since
	// you could end up looping an infinite number of times at the same location).  We can avoid those
	// additional checks if we can prove that the loop can never match empty, which we can do by computing
	// the minimum length of the child; only if it's 0 might iterations be empty.
	iterationMayBeEmpty := child.ComputeMinLength() == 0
	var startingPos string
	if iterationMayBeEmpty {
		startingPos = rm.reserveName("loop_starting_pos")
		rm.addLocalDec(fmt.Sprintf("%s, %s := 0, 0", iterationCount, startingPos))
		c.writeLineFmt("%s = pos", startingPos)
	} else {
		rm.addLocalDec(fmt.Sprintf("%s := 0", iterationCount))
	}
	c.writeLineFmt("%s = 0\n", iterationCount)

	// Iteration body
	c.emitMarkLabel(rm, body, isAtomic)

	// We need to store the starting pos and crawl position so that it may be backtracked through later.
	// This needs to be the starting position from the iteration we're leaving, so it's pushed before updating
	// it to pos. Note that unlike some other constructs that only need to push state on to the stack if
	// they're inside of a loop (because if they're not inside of a loop, nothing would overwrite the locals),
	// here we still need the stack, because each iteration of _this_ loop may have its own state, e.g. we
	// need to know where each iteration began so when backtracking we can jump back to that location.  This is
	// true even if the loop is atomic, as we might need to backtrack within the loop in order to match the
	// minimum iteration count.
	if rm.expressionHasCaptures && iterationMayBeEmpty {
		c.emitStackPush(stackCookie, "r.Crawlpos()", startingPos, "pos")
	} else if rm.expressionHasCaptures {
		c.emitStackPush(stackCookie, "r.Crawlpos()", "pos")
	} else if iterationMayBeEmpty {
		c.emitStackPush(stackCookie, startingPos, "pos")
	} else {
		c.emitStackPush(stackCookie, "pos")
	}
	c.writeLine("")

	// Save off some state.  We need to store the current pos so we can compare it against
	// pos after the iteration, in order to determine whether the iteration was empty. Empty
	// iterations are allowed as part of min matches, but once we've met the min quote, empty matches
	// are considered match failures.
	if iterationMayBeEmpty {
		c.writeLineFmt("%s = pos", startingPos)
	}

	// Proactively increase the number of iterations.  We do this prior to the match rather than once
	// we know it's successful, because we need to decrement it as part of a failed match when
	// backtracking; it's thus simpler to just always decrement it as part of a failed match, even
	// when initially greedily matching the loop, which then requires we increment it before trying.
	c.writeLineFmt("%s++\n", iterationCount)

	// Last but not least, we need to set the doneLabel that a failed match of the body will jump to.
	// Such an iteration match failure may or may not fail the whole operation, depending on whether
	// we've already matched the minimum required iterations, so we need to jump to a location that
	// will make that determination.
	iterationFailedLabel := rm.reserveName("LoopIterationNoMatch")
	rm.doneLabel = iterationFailedLabel

	// Finally, emit the child.
	c.emitExecuteNode(rm, child, nil, true)
	c.writeLine("")
	c.transferSliceStaticPosToPos(rm, false) // ensure sliceStaticPos remains 0
	childBacktracks := rm.doneLabel != iterationFailedLabel

	// Loop condition.  Continue iterating greedily if we've not yet reached the maximum.  We also need to stop
	// iterating if the iteration matched empty and we already hit the minimum number of iterations.
	c.writeLine("")
	if maxIterations == math.MaxInt32 && !iterationMayBeEmpty {
		// The loop has no upper bound and iterations can't be empty; this is a greedy loop, so regardless of whether
		// there's a min iterations required, we need to loop again.
		c.writeLine("// The loop has no upper bound. Continue iterating greedily.")
		c.emitExecuteGoto(rm, body)
	} else {

		if !iterationMayBeEmpty {
			// Iterations won't be empty, but there is an upper bound. Whether or not there's a min iterations required, we need to keep
			// iterating until we're at the maximum, and since the min is never more than the max, we don't need to check the min.
			c.writeLineFmt("// The loop has an upper bound of %v. Continue iterating greedily if it hasn't yet been reached.", maxIterations)
			c.writeLineFmt("if %s {", countIsLessThan(iterationCount, maxIterations))
		} else if minIterations > 0 && maxIterations == math.MaxInt32 {
			// Iterations may be empty, and there's a minimum iteration count required (but no maximum), so loop if either
			// the iteration isn't empty or we still need more iterations to meet the minimum.
			c.writeLineFmt(`// The loop has a lower bound of %v but no upper bound. Continue iterating greedily
						// if the last iteration wasn't empty (or if it was, if the lower bound hasn't yet been reached).
						if pos != %s || %s {
						`, minIterations, startingPos, countIsLessThan(iterationCount, minIterations))
		} else if minIterations > 0 {
			// Iterations may be empty and there's both a lower and upper bound on the loop.
			c.writeLineFmt(`// The loop has a lower bound of %v and an upper bound of %v. Continue iterating
						// greedily if the upper bound hasn't yet been reached and either the last iteration was non-empty or the
						// lower bound hasn't yet been reached.
						if (pos != %s || %s) && %s {`, minIterations, maxIterations, startingPos, countIsLessThan(iterationCount, minIterations), countIsLessThan(iterationCount, maxIterations))
		} else if maxIterations == math.MaxInt32 {
			// Iterations may be empty and there's no lower or upper bound.
			c.writeLineFmt(`// The loop is unbounded. Continue iterating greedily as long as the last iteration wasn't empty.
						if pos != %s {`, startingPos)
		} else {
			// Iterations may be empty, there's no lower bound, but there is an upper bound.
			c.writeLineFmt(`// The loop has an upper bound of %v. Continue iterating greedily if the upper bound hasn't
						// yet been reached (as long as the last iteration wasn't empty).
						if pos != %s && %s {`, maxIterations, startingPos, countIsLessThan(iterationCount, maxIterations))
		}

		c.emitExecuteGoto(rm, body)
		c.writeLine("}")

		c.emitExecuteGoto(rm, endLoop)
	}

	// We've matched as many iterations as we can with this configuration.  Jump to what comes after the loop.
	c.writeLine("")

	// Now handle what happens when an iteration fails, which could be an initial failure or it
	// could be while backtracking.  We need to reset state to what it was before just that iteration
	// started.  That includes resetting pos and clearing out any captures from that iteration.
	c.writeLine("// The loop iteration failed. Put state back to the way it was before the iteration.")
	c.emitMarkLabel(rm, iterationFailedLabel, false)

	// If the loop has a lower bound of 0, then we may try to match what comes after the loop
	// having matched 0 iterations.  If that fails, it'll then backtrack here, and the iteration
	// count will become negative, indicating the loop has exhausted its choices.
	c.writeLineFmt(`%s--
				if %[1]s < 0 {
					// Unable to match the remainder of the expression after exhausting the loop.`, iterationCount)
	c.emitExecuteGoto(rm, originalDoneLabel)
	c.writeLine("}")

	if iterationMayBeEmpty {
		c.emitStackPop(0, "pos", startingPos) // stack cookie handled is explicitly 0 to handle it below
	} else {
		c.emitStackPop(0, "pos")
	}

	if rm.expressionHasCaptures {
		c.emitUncaptureUntil("r.StackPop()")
	}

	c.emitStackCookieValidate(stackCookie)
	c.sliceInputSpan(rm, false)

	// If there's a required minimum iteration count, validate now that we've processed enough iterations.
	if minIterations > 0 {
		if childBacktracks {
			// The child backtracks.  If we don't have any iterations, there's nothing to backtrack into,
			// and at least one iteration is required, so fail the loop.
			c.writeLineFmt("if %s == 0 {", iterationCount)
			c.writeLine("// No iterations have been matched to backtrack into. Fail the loop.")
			c.emitExecuteGoto(rm, originalDoneLabel)
			c.writeLine("}\n")

			// We have at least one iteration; if that's insufficient to meet the minimum, backtrack
			// into the previous iteration.  We only need to do this check if the min iteration requirement
			// is more than one, since the above check already handles the case where the min count is 1,
			// since the only value that wouldn't meet that is 0.
			if minIterations > 1 {
				c.writeLineFmt(`if %s { 
							// All possible iterations have matched, but it's below the required minimum of %v.
							// Backtrack into the prior iteration.`, countIsLessThan(iterationCount, minIterations), minIterations)
				c.emitExecuteGoto(rm, rm.doneLabel)
				c.writeLine("}\n")
			}
		} else {
			// The child doesn't backtrack, which means there's no other way the matched iterations could
			// match differently, so if we haven't already greedily processed enough iterations, fail the loop.
			c.writeLineFmt(`if %s {
							// All possible iterations have matched, but it's below the required minimum of %v. Fail the loop.	
							`, countIsLessThan(iterationCount, minIterations), minIterations)

			// If the minimum iterations is 1, then since we're only here if there are fewer, there must be 0
			// iterations, in which case there's nothing to reset.  If, however, the minimum iteration count is
			// greater than 1, we need to check if there was at least one successful iteration, in which case
			// any backtracking state still set needs to be reset; otherwise, constructs earlier in the sequence
			// trying to pop their own state will erroneously pop this state instead.
			if minIterations > 1 {
				c.writeLineFmt(`if %s != 0 {
								// Ensure any stale backtracking state is removed.
								r.Runstackpos = %s
							}`, iterationCount, startingStackpos)
				usedStartingStackpos = true
			}

			c.emitExecuteGoto(rm, originalDoneLabel)
			c.writeLine("}\n")
		}
	}

	if isAtomic {
		rm.doneLabel = originalDoneLabel
		c.emitMarkLabel(rm, endLoop, len(startingStackpos) == 0)

		// The loop is atomic, which means any backtracking will go around this loop.  That also means we can't leave
		// stack polluted with state from successful iterations, so we need to remove all such state; such state will
		// only have been pushed if minIterations > 0.
		if len(startingStackpos) > 0 {
			c.writeLineFmt("r.Runstackpos = %s // Ensure any remaining backtracking state is removed.", startingStackpos)
			usedStartingStackpos = true
		}
	} else {
		if childBacktracks {
			c.emitExecuteGoto(rm, endLoop)
			c.writeLine("")

			backtrack := rm.reserveName("LoopBacktrack")
			c.emitMarkLabel(rm, backtrack, false)

			// We're backtracking.  Check the timeout.
			c.emitTimeoutCheckIfNeeded(rm)

			c.writeLineFmt(`if %s == 0 {
							// No iterations of the loop remain to backtrack into. Fail the loop.`, iterationCount)
			c.emitExecuteGoto(rm, originalDoneLabel)
			c.writeLine("}")

			c.emitExecuteGoto(rm, rm.doneLabel)
			rm.doneLabel = backtrack
		}

		isInLoop := rm.Analysis.IsInLoop(node)
		c.emitMarkLabel(rm, endLoop, !isInLoop)

		// If this loop is itself not in another loop, nothing more needs to be done:
		// upon backtracking, locals being used by this loop will have retained their
		// values and be up-to-date.  But if this loop is inside another loop, multiple
		// iterations of this loop each need their own state, so we need to use the stack
		// to hold it, and we need a dedicated backtracking section to handle restoring
		// that state before jumping back into the loop itself.
		if isInLoop {
			c.writeLine("")

			// Store the loop's state
			stackCookie = c.createStackCookie()
			if len(startingPos) > 0 && len(startingStackpos) > 0 {
				c.emitStackPush(stackCookie, startingPos, startingStackpos, iterationCount)
			} else if len(startingPos) > 0 {
				c.emitStackPush(stackCookie, startingPos, iterationCount)
			} else if len(startingStackpos) > 0 {
				c.emitStackPush(stackCookie, startingStackpos, iterationCount)
			} else {
				c.emitStackPush(stackCookie, iterationCount)
			}
			usedStartingStackpos = true

			// Skip past the backtracking section
			end := rm.reserveName("LoopSkipBacktrack")
			c.emitExecuteGoto(rm, end)
			c.writeLine("")

			// Emit a backtracking section that restores the loop's state and then jumps to the previous done label
			backtrack := rm.reserveName("LoopBacktrack")
			c.emitMarkLabel(rm, backtrack, false)

			if len(startingPos) > 0 && len(startingStackpos) > 0 {
				c.emitStackPop(stackCookie, iterationCount, startingPos, startingPos)
			} else if len(startingPos) > 0 {
				c.emitStackPop(stackCookie, iterationCount, startingPos)
			} else if len(startingStackpos) > 0 {
				c.emitStackPop(stackCookie, iterationCount, startingStackpos)
			} else {
				c.emitStackPop(stackCookie, iterationCount)
			}

			// We're backtracking.  Check the timeout.
			c.emitTimeoutCheckIfNeeded(rm)

			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLine("")

			rm.doneLabel = backtrack
			c.emitMarkLabel(rm, end, true)
		}
	}
}

func (c *converter) emitExecuteLazy(rm *regexpData, node *syntax.RegexNode) {
	child := node.Children[0]
	minIterations := node.M
	maxIterations := node.N
	originalDoneLabel := rm.doneLabel

	// If this is actually a repeater, reuse the loop implementation, as a loop and a lazy loop
	// both need to greedily consume up to their min iteration count and are identical in
	// behavior when min == max.
	if minIterations == maxIterations {
		c.emitExecuteLoop(rm, node)
		return
	}

	// We should only be here if the lazy loop isn't atomic due to an ancestor, as the optimizer should
	// in such a case have lowered the loop's upper bound to its lower bound, at which point it would
	// have been handled by the above delegation to EmitLoop.  However, if the optimizer missed doing so,
	// this loop could still be considered atomic by ancestor by its parent nodes, in which case we want
	// to make sure the code emitted here conforms (e.g. doesn't leave any state erroneously on the stack).
	// So, we assert it's not atomic, but still handle that case.
	isAtomic := rm.Analysis.IsAtomicByAncestor(node)

	// We might loop any number of times.  In order to ensure this loop and subsequent code sees sliceStaticPos
	// the same regardless, we always need it to contain the same value, and the easiest such value is 0.
	// So, we transfer sliceStaticPos to pos, and ensure that any path out of here has sliceStaticPos as 0.
	c.transferSliceStaticPosToPos(rm, false)

	body := rm.reserveName("LazyLoopBody")
	endLoop := rm.reserveName("LazyLoopEnd")

	iterationCount := rm.reserveName("lazyloop_iteration")
	rm.addLocalDec(fmt.Sprint(iterationCount, " := 0"))
	c.writeLineFmt("%s = 0", iterationCount)

	// Loops that match empty iterations need additional checks in place to prevent infinitely matching (since
	// you could end up looping an infinite number of times at the same location).  We can avoid those
	// additional checks if we can prove that the loop can never match empty, which we can do by computing
	// the minimum length of the child; only if it's 0 might iterations be empty.
	iterationMayBeEmpty := child.ComputeMinLength() == 0
	var startingPos, sawEmpty string
	if iterationMayBeEmpty {
		startingPos = rm.reserveName("lazyloop_starting_pos")
		sawEmpty = rm.reserveName("lazyloop_empty_seen")
		c.writeLineFmt("%s, %s := pos, 0 // the lazy loop may match empty iterations", startingPos, sawEmpty)
	}

	// If the min count is 0, start out by jumping right to what's after the loop.  Backtracking
	// will then bring us back in to do further iterations.
	if minIterations == 0 {
		c.emitExecuteGoto(rm, endLoop)
	}
	c.writeLine("")

	// Iteration body
	c.emitMarkLabel(rm, body, isAtomic)

	// In case iterations are backtracked through and unwound, we need to store the current position (so that
	// matching can resume from that location), the current crawl position if captures are possible (so that
	// we can uncapture back to that position), and both the starting position from the iteration we're leaving
	// and whether we've seen an empty iteration (if iterations may be empty).  Since there can be multiple
	// iterations, this state needs to be stored on to the backtracking stack.
	if isAtomic {
		return
	}
	stackCookie := c.createStackCookie()
	entriesPerIteration := 1 //pos
	if iterationMayBeEmpty {
		entriesPerIteration += 2 //startingPos+sawEmpty
	}
	if rm.expressionHasCaptures {
		entriesPerIteration += 1 //Crawlpos
	}
	if stackCookie != 0 {
		entriesPerIteration += 1
	}
	args := []string{"pos"}
	if iterationMayBeEmpty {
		args = append(args, startingPos, sawEmpty)
	}
	if rm.expressionHasCaptures {
		args = append(args, "r.Crawlpos()")
	}
	c.emitStackPush(stackCookie, args...)

	if iterationMayBeEmpty {
		// We need to store the current pos so we can compare it against pos after the iteration, in order to
		// determine whether the iteration was empty.
		c.writeLineFmt("%s = pos", startingPos)
	}

	// Proactively increase the number of iterations.  We do this prior to the match rather than once
	// we know it's successful, because we need to decrement it as part of a failed match when
	// backtracking; it's thus simpler to just always decrement it as part of a failed match, even
	// when initially greedily matching the loop, which then requires we increment it before trying.
	c.writeLineFmt("%s++", iterationCount)

	// Last but not least, we need to set the doneLabel that a failed match of the body will jump to.
	// Such an iteration match failure may or may not fail the whole operation, depending on whether
	// we've already matched the minimum required iterations, so we need to jump to a location that
	// will make that determination.
	iterationFailedLabel := rm.reserveName("LazyLoopIterationNoMatch")
	rm.doneLabel = iterationFailedLabel

	// Finally, emit the child.
	c.writeLine("")
	c.emitExecuteNode(rm, child, nil, true)
	c.writeLine("")
	c.transferSliceStaticPosToPos(rm, false) // ensure sliceStaticPos remains 0
	if rm.doneLabel == iterationFailedLabel {
		rm.doneLabel = originalDoneLabel
	}

	// Loop condition.  Continue iterating if we've not yet reached the minimum.  We just successfully
	// matched an iteration, so the only reason we'd need to forcefully loop around again is if the
	// minimum were at least 2.
	if minIterations >= 2 {
		c.writeLineFmt("// The lazy loop requires a minimum of %v iterations. If that many haven't yet matched, loop now.", minIterations)
		c.writeLineFmt("if %s {", countIsLessThan(iterationCount, minIterations))
		c.emitExecuteGoto(rm, body)
		c.writeLine("}")
	}

	if iterationMayBeEmpty {
		// If the last iteration was empty, we need to prevent further iteration from this point
		// unless we backtrack out of this iteration.
		c.writeLineFmt(`// If the iteration successfully matched zero-length input, record that an empty iteration was seen.
						if pos == %s {
							%s = 1 // true
						}
						`, startingPos, sawEmpty)
	}

	// We matched the next iteration.  Jump to the subsequent code.
	c.emitExecuteGoto(rm, endLoop)
	c.writeLine("")

	// Now handle what happens when an iteration fails (and since a lazy loop only executes an iteration
	// when it's required to satisfy the loop by definition of being lazy, the loop is failing).  We need
	// to reset state to what it was before just that iteration started.  That includes resetting pos and
	// clearing out any captures from that iteration.
	c.writeLine("// The lazy loop iteration failed to match.")
	c.emitMarkLabel(rm, iterationFailedLabel, false)

	// we don't need to back anything out if we're about to exit TryMatchAtCurrentPosition anyway.
	if rm.doneLabel != originalDoneLabel || !gotoWillExitMatch(rm, originalDoneLabel) {
		// Fail this loop iteration, including popping state off the backtracking stack that was pushed
		// on as part of the failing iteration.
		c.writeLineFmt("%s--", iterationCount)
		if rm.expressionHasCaptures {
			c.emitUncaptureUntil("r.StackPop()")
		}
		args := []string{"pos"}
		if iterationMayBeEmpty {
			args = append(args, sawEmpty, startingPos)
		}

		c.emitStackPop(stackCookie, args...)
		c.sliceInputSpan(rm, false)

		// If the loop's child doesn't backtrack, then this loop has failed.
		// If the loop's child does backtrack, we need to backtrack back into the previous iteration if there was one.
		if rm.doneLabel == originalDoneLabel {
			// Since the only reason we'd end up revisiting previous iterations of the lazy loop is if the child had backtracking constructs
			// we'd backtrack into, and the child doesn't, the whole loop is failed and done. If we successfully processed any iterations,
			// we thus need to pop all of the state we pushed onto the stack for those iterations, as we're exiting out to the parent who
			// will expect the stack to be cleared of any child state.
			// TODO: our runtime expects addition for stack pops -- for now compat with legacy
			if entriesPerIteration > 1 {
				c.writeLineFmt("r.Runstackpos += %s * %v", iterationCount, entriesPerIteration)
			} else {
				c.writeLineFmt("r.Rtackpos += %s", iterationCount)
			}
		} else {
			// The child has backtracking constructs.  If we have no successful iterations previously processed, just bail.
			// If we do have successful iterations previously processed, however, we need to backtrack back into the last one.
			c.writeLineFmt("if %s > 0 {", iterationCount)
			c.writeLine("// The lazy loop matched at least one iteration; backtrack into the last one.")
			if iterationMayBeEmpty {
				// If we saw empty, it must have been in the most recent iteration, as we wouldn't have
				// allowed additional iterations after one that was empty.  Thus, we reset it back to
				// false prior to backtracking / undoing that iteration.
				c.writeLineFmt("%s = 0 // false", sawEmpty)
			}
			c.emitExecuteGoto(rm, rm.doneLabel)
			c.writeLine("}\n")
		}
	}
	c.emitExecuteGoto(rm, originalDoneLabel)
	c.writeLine("")

	c.emitMarkLabel(rm, endLoop, false)

	// If the lazy loop is not atomic, then subsequent code may backtrack back into this lazy loop, either
	// causing it to add additional iterations, or backtracking into existing iterations and potentially
	// unwinding them.  We need to do a timeout check, and then determine whether to branch back to add more
	// iterations (if we haven't hit the loop's maximum iteration count and haven't seen an empty iteration)
	// or unwind by branching back to the last backtracking location.  Either way, we need a dedicated
	// backtracking section that a subsequent construct will see as its backtracking target.

	// We need to ensure that some state (e.g. iteration count) is persisted if we're backtracked to.
	// We also need to push the current position, so that subsequent iterations pick up at the right
	// point (and subsequent expressions are almost certain to have changed the current pos). However,
	// if we're not inside of a loop, the other local's used for this construct are sufficient, as nothing
	// else will overwrite them between now and when backtracking occurs.  If, however, we are inside
	// of another loop, then any number of iterations might have such state that needs to be stored,
	// and thus it needs to be pushed on to the backtracking stack.
	isInLoop := rm.Analysis.IsInLoop(node)
	stackCookie = c.createStackCookie()
	args = []string{"pos"}
	if isInLoop {
		args = append(args, iterationCount)
		if iterationMayBeEmpty {
			args = append(args, startingPos, sawEmpty)
		}
	}
	if rm.expressionHasCaptures {
		args = append(args, "r.Crawlpos()")
	}
	c.emitStackPush(stackCookie, args...)

	skipBacktrack := rm.reserveName("LazyLoopSkipBacktrack")
	c.emitExecuteGoto(rm, skipBacktrack)
	c.writeLine("")

	// Emit a backtracking section that checks the timeout, restores the loop's state, and jumps to
	// the appropriate label.
	backtrack := rm.reserveName("LazyLoopBacktrack")
	c.emitMarkLabel(rm, backtrack, false)

	// We're backtracking.  Check the timeout.
	c.emitTimeoutCheckIfNeeded(rm)

	if rm.expressionHasCaptures {
		c.emitUncaptureUntil("r.StackPop()")
	}
	if !isInLoop {
		args = []string{"pos"}
	} else if iterationMayBeEmpty {
		args = []string{sawEmpty, startingPos, iterationCount, "pos"}
	} else {
		args = []string{iterationCount, "pos"}
	}
	c.emitStackPop(stackCookie, args...)
	c.sliceInputSpan(rm, false)

	// Determine where to branch, either back to the lazy loop body to add an additional iteration,
	// or to the last backtracking label.
	if maxIterations != math.MaxInt32 || iterationMayBeEmpty {
		c.writeLine("")
		if maxIterations == math.MaxInt32 {
			// If the last iteration matched empty, backtrack.
			c.writeLineFmt(`// If the last iteration matched empty, don't continue lazily iterating. Instead, backtrack.
							if %s != 0 {`, sawEmpty)
		} else if iterationMayBeEmpty {
			// If the last iteration matched empty or if we've reached our upper bound, backtrack.
			c.writeLineFmt(`// If the upper bound %v has already been reached, or if the last
							// iteration matched empty, don't continue lazily iterating. Instead, backtrack.
							if %s || %s != 0 {`, maxIterations, countIsGreaterThanOrEqualTo(iterationCount, maxIterations), sawEmpty)
		} else {
			// If we've reached our upper bound, backtrack.
			c.writeLineFmt(`// If the upper bound %v has already been reached,
							// don't continue lazily iterating. Instead, backtrack.
							if %s {`, maxIterations, countIsGreaterThanOrEqualTo(iterationCount, maxIterations))
		}

		// We're backtracking, which could either be to something prior to the lazy loop or to something
		// inside of the lazy loop.  If it's to something inside of the lazy loop, then either the loop
		// will eventually succeed or we'll eventually end up unwinding back through the iterations all
		// the way back to the loop not matching at all, in which case the state we first pushed on at the
		// beginning of the !isAtomic section will get popped off. But if here we're instead going to jump
		// to something prior to the lazy loop, then we need to pop off that state here.
		if rm.doneLabel == originalDoneLabel {
			//TODO: our stack is backwards from C# version
			c.emitAddStmt("r.Runstackpos", entriesPerIteration)
		}

		if iterationMayBeEmpty {
			// If we saw empty, it must have been in the most recent iteration, as we wouldn't have
			// allowed additional iterations after one that was empty.  Thus, we reset it back to
			// false prior to backtracking / undoing that iteration.
			c.writeLineFmt("%s = 0 // false", sawEmpty)
		}

		c.emitExecuteGoto(rm, rm.doneLabel)

		c.writeLine("}")
	}

	// Otherwise, try to match another iteration.
	c.emitExecuteGoto(rm, body)
	c.writeLine("")

	rm.doneLabel = backtrack
	c.emitMarkLabel(rm, skipBacktrack, true)
}

// arbitrary limit; we want it to be large enough to handle ignore-case of common sets, like hex, the latin alphabet, etc.
const SetCharsSize = 64

func (c *converter) emitExecuteAlternation(rm *regexpData, node *syntax.RegexNode) {
	originalDoneLabel := rm.doneLabel

	// Both atomic and non-atomic are supported.  While a parent RegexNode.Atomic node will itself
	// successfully prevent backtracking into this child node, we can emit better / cheaper code
	// for an Alternate when it is atomic, so we still take it into account here.
	isAtomic := rm.Analysis.IsAtomicByAncestor(node)

	// If no child branch overlaps with another child branch, we can emit more streamlined code
	// that avoids checking unnecessary branches, e.g. with abc|def|ghi if the next character in
	// the input is 'a', we needn't try the def or ghi branches.  A simple, relatively common case
	// of this is if every branch begins with a specific, unique character, in which case
	// the whole alternation can be treated as a simple switch, so we special-case that. However,
	// we can't goto _into_ switch cases, which means we can't use this approach if there's any
	// possibility of backtracking into the alternation.
	useSwitchedBranches := false
	if node.Options&syntax.RightToLeft == 0 {
		useSwitchedBranches = isAtomic
		if !useSwitchedBranches {
			useSwitchedBranches = true
			for i := 0; i < len(node.Children); i++ {
				if rm.Analysis.MayBacktrack(node.Children[i]) {
					useSwitchedBranches = false
					break
				}
			}
		}
	}

	// Detect whether every branch begins with one or more unique characters.
	//setChars := make([]rune, 0, SetCharsSize)
	if useSwitchedBranches {
		// Iterate through every branch, seeing if we can easily find a starting One, Multi, or small Set.
		// If we can, extract its starting char (or multiple in the case of a set), validate that all such
		// starting characters are unique relative to all the branches.
		seenChars := make(map[rune]struct{})
		for i := 0; i < len(node.Children) && useSwitchedBranches; i++ {
			// Look for the guaranteed starting node that's a one, multi, set,
			// or loop of one of those with at least one minimum iteration. We need to exclude notones.
			startingLiteralNode := node.Children[i].FindStartingLiteralNode(false)
			if startingLiteralNode == nil || startingLiteralNode.IsNotoneFamily() {
				useSwitchedBranches = false
				break
			}

			// If it's a One or a Multi, get the first character and add it to the set.
			// If it was already in the set, we can't apply this optimization.
			if startingLiteralNode.IsOneFamily() || startingLiteralNode.T == syntax.NtMulti {
				st := startingLiteralNode.FirstCharOfOneOrMulti()
				if _, ok := seenChars[st]; ok {
					useSwitchedBranches = false
					break
				}
				seenChars[st] = struct{}{}
			} else {
				// The branch begins with a set.  Make sure it's a set of only a few characters
				// and get them.  If we can't, we can't apply this optimization.
				setChars := startingLiteralNode.Set.GetSetChars(SetCharsSize)
				if startingLiteralNode.Set.IsNegated() || len(setChars) == 0 {
					useSwitchedBranches = false
					break
				}

				// Check to make sure each of the chars is unique relative to all other branches examined.
				for _, c := range setChars {
					if _, ok := seenChars[c]; ok {
						useSwitchedBranches = false
						break
					}
					seenChars[c] = struct{}{}
				}
			}
		}
	}

	if useSwitchedBranches {
		// Note: This optimization does not exist with RegexOptions.Compiled.  Here we rely on the
		// C# compiler to lower the C# switch statement with appropriate optimizations. In some
		// cases there are enough branches that the compiler will emit a jump table.  In others
		// it'll optimize the order of checks in order to minimize the total number in the worst
		// case.  In any case, we get easier to read and reason about C#.
		//c.emitExecuteSwitchedBranches()
		// We need at least 1 remaining character in the span, for the char to switch on.
		c.emitSpanLengthCheck(rm, 1, nil)
		c.writeLine("")

		// Emit a switch statement on the first char of each branch.
		c.writeLineFmt("switch %s[%v] {", rm.sliceSpan, rm.sliceStaticPos)

		startingSliceStaticPos := rm.sliceStaticPos

		// Emit a case for each branch.
		for i := 0; i < len(node.Children); i++ {
			rm.sliceStaticPos = startingSliceStaticPos

			// We know we're only in this code if every branch has a valid starting literal node. Get it.
			// We also get the immediate child. Ideally they're the same, in which case we might be able to
			// use the switch as the processing of that node, e.g. if the node is a One, then by matching the
			// literal via the switch, we've fully processed it. But there may be other cases in which it's not
			// sufficient, e.g. if that one was wrapped in a Capture, we still want to emit the capture code,
			// and for simplicity, we still end up emitting the re-evaluation of that character. It's still much
			// cheaper to do this than to emit the full alternation code.
			child := node.Children[i]
			startingLiteralNode := child.FindStartingLiteralNode(false)

			// Emit the case for this branch to match on the first character.
			if startingLiteralNode.IsSetFamily() {
				setChars := startingLiteralNode.Set.GetSetChars(SetCharsSize)
				c.writeLineFmt("case %s:", getRuneLiteralParams(setChars))
			} else {
				c.writeLineFmt("case %q:", startingLiteralNode.FirstCharOfOneOrMulti())
			}

			// Emit the code for the branch, without the first character that was already matched in the switch.
			var remainder *syntax.RegexNode
		HandleChild:
			switch child.T {
			case syntax.NtOne, syntax.NtSet:
				// The character was handled entirely by the switch. No additional matching is needed.
				rm.sliceStaticPos++

			case syntax.NtMulti:
				// First character was handled by the switch. Emit matching code for the remainder of the multi string.
				rm.sliceStaticPos++
				if len(child.Str) == 2 {
					c.emitExecuteNode(rm, &syntax.RegexNode{T: syntax.NtOne, Options: child.Options, Ch: child.Str[1]}, nil, true)
				} else {
					c.emitExecuteNode(rm, &syntax.RegexNode{T: syntax.NtMulti, Options: child.Options, Str: child.Str[1:]}, nil, true)
				}
				c.writeLine("")

			case syntax.NtConcatenate:
				if child.Children[0] == startingLiteralNode &&
					(startingLiteralNode.T == syntax.NtOne || startingLiteralNode.T == syntax.NtSet || startingLiteralNode.T == syntax.NtMulti) {
					// This is a concatenation where its first node is the starting literal we found and that starting literal
					// is one of the nodes above that we know how to handle completely. This is a common
					// enough case that we want to special-case it to avoid duplicating the processing for that character
					// unnecessarily. So, we'll shave off that first node from the concatenation and then handle the remainder.
					// Note that it's critical startingLiteralNode is something we can fully handle above: if it's not,
					// we'll end up losing some of the pattern due to overwriting `remainder`.
					remainder = child
					child = child.Children[0]
					remainder.ReplaceChild(0, &syntax.RegexNode{T: syntax.NtEmpty, Options: remainder.Options})
					goto HandleChild // reprocess just the first node that was saved; the remainder will then be processed below
				}
				fallthrough
			default:
				remainder = child
			}

			if remainder != nil {
				// Emit a full match for whatever part of the child we haven't yet handled.
				c.emitExecuteNode(rm, remainder, nil, true)
				c.writeLine("")
			}

			// This is only ever used for atomic alternations, so we can simply reset the doneLabel
			// after emitting the child, as nothing will backtrack here (and we need to reset it
			// so that all branches see the original).
			rm.doneLabel = originalDoneLabel

			// If we get here in the generated code, the branch completed successfully.
			// Before jumping to the end, we need to zero out sliceStaticPos, so that no
			// matter what the value is after the branch, whatever follows the alternate
			// will see the same sliceStaticPos.
			c.transferSliceStaticPosToPos(rm, false)
			c.writeLine("")
		}

		// Default branch if the character didn't match the start of any branches.
		c.emitCaseGoto(rm, "default:", rm.doneLabel)
		c.writeLine("}")

	} else {
		//c.emitExecuteAllBranches(rm)
		// Label to jump to when any branch completes successfully.
		matchLabel := rm.reserveName("AlternationMatch")

		// Save off pos.  We'll need to reset this each time a branch fails.
		startingPos := rm.reserveName("alternation_starting_pos")

		// the Go compiler doen't allow us to Goto across declared vars
		// so we just declare them up top
		canUseLocalsForAllState := !isAtomic && !rm.Analysis.IsInLoop(node)

		rm.addLocalDec(fmt.Sprintf("%s := 0", startingPos))
		c.writeLineFmt("%s = pos", startingPos)

		startingSliceStaticPos := rm.sliceStaticPos

		// We need to be able to undo captures in two situations:
		// - If a branch of the alternation itself contains captures, then if that branch
		//   fails to match, any captures from that branch until that failure point need to
		//   be uncaptured prior to jumping to the next branch.
		// - If the expression after the alternation contains captures, then failures
		//   to match in those expressions could trigger backtracking back into the
		//   alternation, and thus we need uncapture any of them.
		// As such, if the alternation contains captures or if it's not atomic, we need
		// to grab the current crawl position so we can unwind back to it when necessary.
		// We can do all of the uncapturing as part of falling through to the next branch.
		// If we fail in a branch, then such uncapturing will unwind back to the position
		// at the start of the alternation.  If we fail after the alternation, and the
		// matched branch didn't contain any backtracking, then the failure will end up
		// jumping to the next branch, which will unwind the captures.  And if we fail after
		// the alternation and the matched branch did contain backtracking, that backtracking
		// construct is responsible for unwinding back to its starting crawl position. If
		// it eventually ends up failing, that failure will result in jumping to the next branch
		// of the alternation, which will again dutifully unwind the remaining captures until
		// what they were at the start of the alternation.  Of course, if there are no captures
		// anywhere in the regex, we don't have to do any of that.
		var startingCapturePos string
		if rm.expressionHasCaptures && (rm.Analysis.MayContainCapture(node) || !isAtomic) {
			startingCapturePos = rm.reserveName("alternation_starting_capturepos")
			rm.addLocalDec(fmt.Sprintf("%s := 0", startingCapturePos))
			c.writeLineFmt("%s = r.Crawlpos()", startingCapturePos)
		}
		c.writeLine("")

		// After executing the alternation, subsequent matching may fail, at which point execution
		// will need to backtrack to the alternation.  We emit a branching table at the end of the
		// alternation, with a label that will be left as the "doneLabel" upon exiting emitting the
		// alternation.  The branch table is populated with an entry for each branch of the alternation,
		// containing either the label for the last backtracking construct in the branch if such a construct
		// existed (in which case the doneLabel upon emitting that node will be different from before it)
		// or the label for the next branch.
		labelMap := make([]string, len(node.Children))
		backtrackLabel := rm.reserveName("AlternationBacktrack")

		// We're not atomic, so we'll have to handle backtracking, but we're not inside of a loop,
		// so we can store the current branch in a local rather than pushing it on to the backtracking
		// stack (if we were in a loop, such a local couldn't be used as it could be overwritten by
		// a subsequent iteration of that outer loop).
		var currentBranch string
		if canUseLocalsForAllState {
			currentBranch = rm.reserveName("alternation_branch")
			rm.addLocalDec(fmt.Sprintf("%s := 0", currentBranch))
		}

		stackCookie := c.createStackCookie()
		for i := 0; i < len(node.Children); i++ {
			// If the alternation isn't atomic, backtracking may require our jump table jumping back
			// into these branches, so we can't use actual scopes, as that would hide the labels.
			c.writeLineFmt("// Branch %v", i)
			isLastBranch := i == len(node.Children)-1

			var nextBranch string
			if !isLastBranch {
				// Failure to match any branch other than the last one should result
				// in jumping to process the next branch.
				nextBranch = rm.reserveName("AlternationBranch")
				rm.doneLabel = nextBranch
			} else {
				// Failure to match the last branch is equivalent to failing to match
				// the whole alternation, which means those failures should jump to
				// what "doneLabel" was defined as when starting the alternation.
				rm.doneLabel = originalDoneLabel
			}

			// Emit the code for each branch.
			c.emitExecuteNode(rm, node.Children[i], nil, true)
			c.writeLine("")

			// Add this branch to the backtracking table.  At this point, either the child
			// had backtracking constructs, in which case doneLabel points to the last one
			// and that's where we'll want to jump to, or it doesn't, in which case doneLabel
			// still points to the nextBranch, which similarly is where we'll want to jump to.
			if !isAtomic {
				// If we're inside of a loop, push the state we need to preserve on to the
				// the backtracking stack.  If we're not inside of a loop, simply ensure all
				// the relevant state is stored in our locals.
				if len(currentBranch) == 0 {
					if len(startingCapturePos) != 0 {
						c.emitStackPush(stackCookie+i, strconv.Itoa(i), startingPos, startingCapturePos)
					} else {
						c.emitStackPush(stackCookie+i, strconv.Itoa(i), startingPos)
					}
				} else {
					c.writeLineFmt("%s = %v", currentBranch, i)
				}
			}
			labelMap[i] = rm.doneLabel

			// If we get here in the generated code, the branch completed successfully.
			// Before jumping to the end, we need to zero out sliceStaticPos, so that no
			// matter what the value is after the branch, whatever follows the alternate
			// will see the same sliceStaticPos.
			c.transferSliceStaticPosToPos(rm, false)
			if !isLastBranch || !isAtomic {
				// If this isn't the last branch, we're about to output a reset section,
				// and if this isn't atomic, there will be a backtracking section before
				// the end of the method.  In both of those cases, we've successfully
				// matched and need to skip over that code.  If, however, this is the
				// last branch and this is an atomic alternation, we can just fall
				// through to the successfully matched location.
				c.emitExecuteGoto(rm, matchLabel)
			}

			// Reset state for next branch and loop around to generate it.  This includes
			// setting pos back to what it was at the beginning of the alternation,
			// updating slice to be the full length it was, and if there's a capture that
			// needs to be reset, uncapturing it.
			if !isLastBranch {
				c.writeLine("")
				c.emitMarkLabel(rm, nextBranch, false)
				c.writeLineFmt("pos = %s", startingPos)
				c.sliceInputSpan(rm, false)
				rm.sliceStaticPos = startingSliceStaticPos
				if len(startingCapturePos) != 0 {
					c.emitUncaptureUntil(startingCapturePos)
				}
			}

			c.writeLine("")
		}

		// We should never fall through to this location in the generated code.  Either
		// a branch succeeded in matching and jumped to the end, or a branch failed in
		// matching and jumped to the next branch location.  We only get to this code
		// if backtracking occurs and the code explicitly jumps here based on our setting
		// "doneLabel" to the label for this section.  Thus, we only need to emit it if
		// something can backtrack to us, which can't happen if we're inside of an atomic
		// node. Thus, emit the backtracking section only if we're non-atomic.
		if isAtomic {
			rm.doneLabel = originalDoneLabel
		} else {
			rm.doneLabel = backtrackLabel
			c.emitMarkLabel(rm, backtrackLabel, false)

			// We're backtracking.  Check the timeout.
			c.emitTimeoutCheckIfNeeded(rm)

			var switchClause string
			if len(currentBranch) == 0 {
				// We're in a loop, so we use the backtracking stack to persist our state.
				// Pop it off and validate the stack position.
				if len(startingCapturePos) != 0 {
					c.emitStackPop(0, startingCapturePos, startingPos)
				} else {
					c.emitStackPop(0, startingPos)
				}

				switchClause = c.validateStackCookieWithAdditionAndReturnPoppedStack(stackCookie)
			} else {
				// We're not in a loop, so our locals already store the state we need.
				switchClause = currentBranch
			}
			c.writeLineFmt("switch %s {", switchClause)
			for i := 0; i < len(labelMap); i++ {
				c.emitCaseGoto(rm, fmt.Sprintf("case %v:", i), labelMap[i])
			}
			c.writeLine("}\n")
		}

		// Successfully completed the alternate.
		c.emitMarkLabel(rm, matchLabel, true)
	}
}

func (c *converter) emitExecuteBackreference(rm *regexpData, node *syntax.RegexNode) {
	capnum := mapCaptureNumber(node.M, rm.Tree.Caps)

	if rm.sliceStaticPos > 0 {
		c.transferSliceStaticPosToPos(rm, false)
		c.writeLine("")
	}

	// If the specified capture hasn't yet captured anything, fail to match... except when using RegexOptions.ECMAScript,
	// in which case per ECMA 262 section 21.2.2.9 the backreference should succeed.
	if (node.Options & syntax.ECMAScript) != 0 {
		c.writeLineFmt(`// If the %s hasn't matched, the backreference matches with RegexOptions.ECMAScript rules.
					if r.IsMatched(%v) {`, describeCapture(rm, node.M), capnum)
		c.emitBackrefWhenHasCapture(rm, node, capnum)
		c.writeLine("}")
	} else {
		c.writeLineFmt(`// If the %s hasn't matched, the backreference doesn't match.
					if !r.IsMatched(%v) {`, describeCapture(rm, node.M), capnum)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n")
		c.emitBackrefWhenHasCapture(rm, node, capnum)
	}
}

func (c *converter) emitBackrefWhenHasCapture(rm *regexpData, node *syntax.RegexNode, capnum int) {
	c.writeLine("// Get the captured text.  If it doesn't match at the current position, the backreference doesn't match.")
	rm.addLocalDec("matchLength := 0")
	c.writeLineFmt("matchLength = r.MatchLength(%v)", capnum)

	ignoreCase := ""
	if node.Options&syntax.IgnoreCase != 0 {
		ignoreCase = "IgnoreCase"
	}

	// Validate that the remaining length of the slice is sufficient
	// to possibly match, and then do a SequenceEqual against the matched text.
	if (node.Options & syntax.RightToLeft) == 0 {
		c.writeLineFmt("if len(%s) < matchLength || !helpers.Equals%s(r.Runtext, r.MatchIndex(%v), matchLength, %[1]s[:matchLength]) {",
			rm.sliceSpan, ignoreCase, capnum)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\npos += matchLength")
	} else {
		c.writeLineFmt("if pos < matchLength || !helpers.Equals%s(r.Runtext, r.MatchIndex(%v), matchLength, r.Runtext[pos-matchLength:pos]) {",
			ignoreCase, capnum)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\npos -= matchLength")
	}
	c.sliceInputSpan(rm, false)
}

// Emits the code for an if(backreference)-then-else conditional.
func (c *converter) emitExecuteBackreferenceConditional(rm *regexpData, node *syntax.RegexNode) {
	// We're branching in a complicated fashion.  Make sure sliceStaticPos is 0.
	c.transferSliceStaticPosToPos(rm, false)
	stackCookie := c.createStackCookie()

	// Get the capture number to test.
	capnum := mapCaptureNumber(node.M, rm.Tree.Caps)

	// Get the "yes" branch and the "no" branch.  The "no" branch is optional in syntax and is thus
	// somewhat likely to be Empty.
	yesBranch := node.Children[0]
	var noBranch *syntax.RegexNode
	if len(node.Children) > 1 && node.Children[1].T != syntax.NtEmpty {
		noBranch = node.Children[1]
	}
	originalDoneLabel := rm.doneLabel

	// If the child branches might backtrack, we can't emit the branches inside constructs that
	// require braces, e.g. if/else, even though that would yield more idiomatic output.
	// But if we know for certain they won't backtrack, we can output the nicer code.
	if rm.Analysis.IsAtomicByAncestor(node) || (!rm.Analysis.MayBacktrack(yesBranch) && (noBranch == nil || !rm.Analysis.MayBacktrack(noBranch))) {
		c.writeLineFmt(`if r.IsMatched(%v) {
		// The %s captured a value.  Match the first branch.`, capnum, describeCapture(rm, node.M))
		c.emitExecuteNode(rm, yesBranch, nil, true)
		c.writeLine("")
		c.transferSliceStaticPosToPos(rm, false) // make sure sliceStaticPos is 0 after each branch

		if noBranch != nil {
			c.writeLine("} else {")
			c.writeLine("// Otherwise, match the second branch.")
			c.emitExecuteNode(rm, noBranch, nil, true)
			c.writeLine("")
			c.transferSliceStaticPosToPos(rm, false)
		}
		c.writeLine("}")

		rm.doneLabel = originalDoneLabel // atomicity
		return
	}

	refNotMatched := rm.reserveName("ConditionalBackreferenceNotMatched")
	endConditional := rm.reserveName("ConditionalBackreferenceEnd")

	// As with alternations, we have potentially multiple branches, each of which may contain
	// backtracking constructs, but the expression after the conditional needs a single target
	// to backtrack to.  So, we expose a single Backtrack label and track which branch was
	// followed in this resumeAt local.
	resumeAt := rm.reserveName("conditionalbackreference_branch")
	isInLoop := rm.Analysis.IsInLoop(node)
	if isInLoop {
		c.writeLineFmt("var %s int", resumeAt)
	} else {
		rm.addLocalDec(fmt.Sprint(resumeAt, " := 0"))
	}

	// While it would be nicely readable to use an if/else block, if the branches contain
	// anything that triggers backtracking, labels will end up being defined, and if they're
	// inside the scope block for the if or else, that will prevent jumping to them from
	// elsewhere.  So we implement the if/else with labels and gotos manually.
	// Check to see if the specified capture number was captured.
	c.writeLineFmt("if !r.IsMatched(%v) {", capnum)
	c.emitExecuteGoto(rm, refNotMatched)
	c.writeLine("}\n")

	// The specified capture was captured.  Run the "yes" branch.
	// If it successfully matches, jump to the end.
	c.emitExecuteNode(rm, yesBranch, nil, false)
	c.writeLine("")
	c.transferSliceStaticPosToPos(rm, false) // make sure sliceStaticPos is 0 after each branch
	postYesDoneLabel := rm.doneLabel
	if postYesDoneLabel != originalDoneLabel || isInLoop {
		c.writeLineFmt("%s = 0", resumeAt)
	}

	needsEndConditional := postYesDoneLabel != originalDoneLabel || noBranch != nil
	if needsEndConditional {
		c.emitExecuteGoto(rm, endConditional)
		c.writeLine("")
	}

	c.emitMarkLabel(rm, refNotMatched, true)
	postNoDoneLabel := originalDoneLabel
	if noBranch != nil {
		// Output the no branch.
		rm.doneLabel = originalDoneLabel
		c.emitExecuteNode(rm, noBranch, nil, true)
		c.writeLine("")
		c.transferSliceStaticPosToPos(rm, false) // make sure sliceStaticPos is 0 after each branch
		postNoDoneLabel = rm.doneLabel
		if postNoDoneLabel != originalDoneLabel || isInLoop {
			c.writeLineFmt("%s = 1", resumeAt)
		}
	} else {
		// There's only a yes branch.  If it's going to cause us to output a backtracking
		// label but code may not end up taking the yes branch path, we need to emit a resumeAt
		// that will cause the backtracking to immediately pass through this node.
		if postYesDoneLabel != originalDoneLabel || isInLoop {
			c.writeLineFmt("%s = 2", resumeAt)
		}
	}

	// If either the yes branch or the no branch contained backtracking, subsequent expressions
	// might try to backtrack to here, so output a backtracking map based on resumeAt.
	hasBacktracking := postYesDoneLabel != originalDoneLabel || postNoDoneLabel != originalDoneLabel
	if hasBacktracking {
		// Skip the backtracking section.
		c.emitExecuteGoto(rm, endConditional)
		c.writeLine("")

		// Backtrack section
		backtrack := rm.reserveName("ConditionalBackreferenceBacktrack")
		rm.doneLabel = backtrack
		c.emitMarkLabel(rm, backtrack, false)

		// Pop from the stack the branch that was used and jump back to its backtracking location.
		// If we're not in a loop, though, we won't have pushed it on to the stack as nothing will
		// have been able to overwrite it in the interim, so we can just trust the value already in
		// the local.
		if isInLoop {
			c.emitStackPop(stackCookie, resumeAt)
		}
		c.writeLineFmt("switch %s {", resumeAt)
		if postYesDoneLabel != originalDoneLabel {
			c.emitCaseGoto(rm, "case 0:", postYesDoneLabel)
		}
		if postNoDoneLabel != originalDoneLabel {
			c.emitCaseGoto(rm, "case 1:", postNoDoneLabel)
		}

		c.emitCaseGoto(rm, "default:", originalDoneLabel)
		c.writeLine("}")
	}

	if needsEndConditional {
		c.emitMarkLabel(rm, endConditional, true)
	}

	if hasBacktracking && isInLoop {
		// We're not atomic and at least one of the yes or no branches contained backtracking constructs,
		// so finish outputting our backtracking logic, which involves pushing onto the stack which
		// branch to backtrack into.  If we're not in a loop, though, nothing else can overwrite this local
		// in the interim, so we can avoid pushing it.
		c.emitStackPush(stackCookie, resumeAt)
	}
}

// Emits the code for an if(expression)-then-else conditional.
func (c *converter) emitExecuteExpressionConditional(rm *regexpData, node *syntax.RegexNode) {
	isAtomic := rm.Analysis.IsAtomicByAncestor(node)

	// We're branching in a complicated fashion.  Make sure sliceStaticPos is 0.
	c.transferSliceStaticPosToPos(rm, false)

	// The first child node is the condition expression.  If this matches, then we branch to the "yes" branch.
	// If it doesn't match, then we branch to the optional "no" branch if it exists, or simply skip the "yes"
	// branch, otherwise. The condition is treated as a positive lookaround.
	condition := node.Children[0]

	// Get the "yes" branch and the "no" branch.  The "no" branch is optional in syntax and is thus
	// somewhat likely to be Empty.
	yesBranch := node.Children[1]
	var noBranch *syntax.RegexNode
	if len(node.Children) > 2 && node.Children[2].T != syntax.NtEmpty {
		noBranch = node.Children[2]
	}
	originalDoneLabel := rm.doneLabel

	expressionNotMatched := rm.reserveName("ConditionalExpressionNotMatched")
	endConditional := rm.reserveName("ConditionalExpressionEnd")

	// As with alternations, we have potentially multiple branches, each of which may contain
	// backtracking constructs, but the expression after the condition needs a single target
	// to backtrack to.  So, we expose a single Backtrack label and track which branch was
	// followed in this resumeAt local.
	isInLoop := false
	resumeAt := rm.reserveName("conditionalexpression_branch")
	if !isAtomic {
		isInLoop = rm.Analysis.IsInLoop(node)
		if isInLoop {
			c.writeLineFmt("%v = 0", resumeAt)
		}
		rm.addLocalDec(fmt.Sprint(resumeAt, " := 0"))
		c.writeLineFmt("_ = %s", resumeAt)
	}

	// If the condition expression has captures, we'll need to uncapture them in the case of no match.
	var startingCapturePos string
	if rm.Analysis.MayContainCapture(condition) {
		startingCapturePos = rm.reserveName("conditionalexpression_starting_capturepos")
		c.writeLineFmt("%v := r.Crawlpos()", startingCapturePos)
	}

	// Emit the condition expression.  Route any failures to after the yes branch.  This code is almost
	// the same as for a positive lookaround; however, a positive lookaround only needs to reset the position
	// on a successful match, as a failed match fails the whole expression; here, we need to reset the
	// position on completion, regardless of whether the match is successful or not.
	rm.doneLabel = expressionNotMatched

	// Save off pos.  We'll need to reset this upon successful completion of the lookaround.
	startingPos := rm.reserveName("conditionalexpression_starting_pos")
	c.writeLineFmt("%s := pos\n", startingPos)
	startingSliceStaticPos := rm.sliceStaticPos

	// Emit the condition. The condition expression is a zero-width assertion, which is atomic,
	// so prevent backtracking into it.
	c.writeLine("// Condition:")
	if rm.Analysis.MayBacktrack(condition) {
		// Condition expressions are treated like positive lookarounds and thus are implicitly atomic,
		// so we need to emit the node as atomic if it might backtrack.
		c.emitExecuteAtomic(rm, node, nil)
	} else {
		c.emitExecuteNode(rm, condition, nil, true)
	}
	c.writeLine("")
	rm.doneLabel = originalDoneLabel

	// After the condition completes successfully, reset the text positions.
	// Do not reset captures, which persist beyond the lookaround.
	c.writeLine("// Condition matched:")
	c.writeLineFmt("pos = %s", startingPos)
	c.sliceInputSpan(rm, false)
	rm.sliceStaticPos = startingSliceStaticPos
	c.writeLine("")

	// The expression matched.  Run the "yes" branch. If it successfully matches, jump to the end.
	c.emitExecuteNode(rm, yesBranch, nil, true)
	c.writeLine("")
	c.transferSliceStaticPosToPos(rm, false) // make sure sliceStaticPos is 0 after each branch
	postYesDoneLabel := rm.doneLabel
	if !isAtomic && postYesDoneLabel != originalDoneLabel {
		c.writeLineFmt("%s = 0", resumeAt)
	}
	c.emitExecuteGoto(rm, endConditional)
	c.writeLine("")

	// After the condition completes unsuccessfully, reset the text positions
	// _and_ reset captures, which should not persist when the whole expression failed.
	c.writeLine("// Condition did not match:")
	c.emitMarkLabel(rm, expressionNotMatched, false)
	c.writeLineFmt("pos = %s", startingPos)
	c.sliceInputSpan(rm, false)
	rm.sliceStaticPos = startingSliceStaticPos
	if len(startingCapturePos) > 0 {
		c.emitUncaptureUntil(startingCapturePos)
	}
	c.writeLine("")

	postNoDoneLabel := originalDoneLabel
	if noBranch != nil {
		// Output the no branch.
		rm.doneLabel = originalDoneLabel
		c.emitExecuteNode(rm, noBranch, nil, true)
		c.writeLine("")
		c.transferSliceStaticPosToPos(rm, false) // make sure sliceStaticPos is 0 after each branch
		postNoDoneLabel = rm.doneLabel
		if !isAtomic && postNoDoneLabel != originalDoneLabel {
			c.writeLineFmt("%s = 1", resumeAt)
		}
	} else {
		// There's only a yes branch.  If it's going to cause us to output a backtracking
		// label but code may not end up taking the yes branch path, we need to emit a resumeAt
		// that will cause the backtracking to immediately pass through this node.
		if !isAtomic && postYesDoneLabel != originalDoneLabel {
			c.writeLineFmt("%s = 2", resumeAt)
		}
	}

	// If either the yes branch or the no branch contained backtracking, subsequent expressions
	// might try to backtrack to here, so output a backtracking map based on resumeAt.
	if isAtomic || (postYesDoneLabel == originalDoneLabel && postNoDoneLabel == originalDoneLabel) {
		rm.doneLabel = originalDoneLabel
		c.emitMarkLabel(rm, endConditional, true)
	} else {
		// Skip the backtracking section.
		c.emitExecuteGoto(rm, endConditional)
		c.writeLine("")

		backtrack := rm.reserveName("ConditionalExpressionBacktrack")
		rm.doneLabel = backtrack
		c.emitMarkLabel(rm, backtrack, false)

		stackCookie := c.createStackCookie()

		if isInLoop {
			// If we're not in a loop, the local will maintain its value until backtracking occurs.
			// If we are in a loop, multiple iterations need their own value, so we need to use the stack.
			c.emitStackPop(stackCookie, resumeAt)
		}

		c.writeLineFmt("switch %s {", resumeAt)
		if postYesDoneLabel != originalDoneLabel {
			c.emitCaseGoto(rm, "case 0:", postYesDoneLabel)
		}

		if postNoDoneLabel != originalDoneLabel {
			c.emitCaseGoto(rm, "case 1:", postNoDoneLabel)
		}

		c.emitCaseGoto(rm, "default:", originalDoneLabel)
		c.writeLine("}")

		c.emitMarkLabel(rm, endConditional, !isInLoop)
		if isInLoop {
			c.emitStackPush(stackCookie, resumeAt)
		}
	}
}

func mapCaptureNumber(capNum int, caps map[int]int) int {
	if capNum == -1 {
		return -1
	}
	if caps != nil {
		return caps[capNum]
	}
	return capNum
}
func (c *converter) emitExecuteCapture(rm *regexpData, node *syntax.RegexNode, subsequent *syntax.RegexNode) {
	capnum := mapCaptureNumber(node.M, rm.Tree.Caps)
	uncapnum := mapCaptureNumber(node.N, rm.Tree.Caps)
	isAtomic := rm.Analysis.IsAtomicByAncestor(node)
	isInLoop := rm.Analysis.IsInLoop(node)

	c.transferSliceStaticPosToPos(rm, false)
	startingPos := rm.reserveName("capture_starting_pos")
	// in go we have to declare vars at the top so we can use Goto freely
	rm.addLocalDec(fmt.Sprint(startingPos, " := 0"))
	c.writeLineFmt("%s = pos", startingPos)
	c.writeLine("")

	child := node.Children[0]

	if uncapnum != -1 {
		c.writeLineFmt("if r.IsMatched(%v) {", uncapnum)
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("}\n")
	}

	// Emit child node.
	originalDoneLabel := rm.doneLabel
	c.emitExecuteNode(rm, child, subsequent, true)
	childBacktracks := rm.doneLabel != originalDoneLabel

	c.writeLine("")
	c.transferSliceStaticPosToPos(rm, false)
	if uncapnum == -1 {
		//TODO: showing 0 instead of 1?
		c.writeLineFmt("r.Capture(%v, %s, pos)", capnum, startingPos)
	} else {
		c.writeLineFmt("r.TransferCapture(%v, %v, %s, pos)", capnum, uncapnum, startingPos)
	}

	if isAtomic || !childBacktracks {
		// If the capture is atomic and nothing can backtrack into it, we're done.
		// Similarly, even if the capture isn't atomic, if the captured expression
		// doesn't do any backtracking, we're done.
		rm.doneLabel = originalDoneLabel
	} else {
		// We're not atomic and the child node backtracks.  When it does, we need
		// to ensure that the starting position for the capture is appropriately
		// reset to what it was initially (it could have changed as part of being
		// in a loop or similar).  So, we emit a backtracking section that
		// pushes/pops the starting position before falling through.
		c.writeLine("")

		stackCookie := c.createStackCookie()
		if isInLoop {
			// If we're in a loop, different iterations of the loop need their own
			// starting position, so push it on to the stack.  If we're not in a loop,
			// the local will maintain its value and will suffice.
			c.emitStackPush(stackCookie, startingPos)
		}

		// Skip past the backtracking section
		end := rm.reserveName("CaptureSkipBacktrack")
		c.emitExecuteGoto(rm, end)
		c.writeLine("")

		// Emit a backtracking section that restores the capture's state and then jumps to the previous done label
		backtrack := rm.reserveName("CaptureBacktrack")
		c.emitMarkLabel(rm, backtrack, false)
		if isInLoop {
			c.emitStackPop(stackCookie, startingPos)
		}
		c.emitExecuteGoto(rm, rm.doneLabel)
		c.writeLine("")

		rm.doneLabel = backtrack
		c.emitMarkLabel(rm, end, true)
	}
}

// Emits the code to handle a positive lookaround assertion. This is a positive lookahead
// for left-to-right and a positive lookbehind for right-to-left.
func (c *converter) emitExecutePositiveLookaroundAssertion(rm *regexpData, node *syntax.RegexNode) {

	if rm.Analysis.HasRightToLeft() {
		// Lookarounds are the only places in the node tree where we might change direction,
		// i.e. where we might go from RegexOptions.None to RegexOptions.RightToLeft, or vice
		// versa.  This is because lookbehinds are implemented by making the whole subgraph be
		// RegexOptions.RightToLeft and reversed.  Since we use static position to optimize left-to-right
		// and don't use it in support of right-to-left, we need to resync the static position
		// to the current position when entering a lookaround, just in case we're changing direction.
		c.transferSliceStaticPosToPos(rm, true)
	}

	// Save off pos.  We'll need to reset this upon successful completion of the lookaround.
	name := "positivelookahead_starting_pos"
	if node.Options&syntax.RightToLeft != 0 {
		name = "positivelookbehind_starting_pos"
	}
	startingPos := rm.reserveName(name)
	rm.addLocalDec(fmt.Sprint(startingPos, " := 0"))
	c.writeLineFmt("%s = pos\n", startingPos)

	startingSliceStaticPos := rm.sliceStaticPos

	// Check for timeout. Lookarounds result in re-processing the same input, so while not
	// technically backtracking, it's appropriate to have a timeout check.
	c.emitTimeoutCheckIfNeeded(rm)

	// Emit the child.
	child := node.Children[0]
	if rm.Analysis.MayBacktrack(child) {
		// Lookarounds are implicitly atomic, so we need to emit the node as atomic if it might backtrack.
		c.emitExecuteAtomic(rm, node, nil)
	} else {
		c.emitExecuteNode(rm, child, nil, true)
	}

	// After the child completes successfully, reset the text positions.
	// Do not reset captures, which persist beyond the lookaround.
	c.writeLineFmt("\npos = %s", startingPos)
	c.sliceInputSpan(rm, false)

	rm.sliceStaticPos = startingSliceStaticPos
}

// Emits the code to handle a negative lookaround assertion. This is a negative lookahead
// for left-to-right and a negative lookbehind for right-to-left.
func (c *converter) emitExecuteNegativeLookaroundAssertion(rm *regexpData, node *syntax.RegexNode) {
	if rm.Analysis.HasRightToLeft() {
		// Lookarounds are the only places in the node tree where we might change direction,
		// i.e. where we might go from RegexOptions.None to RegexOptions.RightToLeft, or vice
		// versa.  This is because lookbehinds are implemented by making the whole subgraph be
		// RegexOptions.RightToLeft and reversed.  Since we use static position to optimize left-to-right
		// and don't use it in support of right-to-left, we need to resync the static position
		// to the current position when entering a lookaround, just in case we're changing direction.
		c.transferSliceStaticPosToPos(rm, true)
	}

	originalDoneLabel := rm.doneLabel

	// Save off pos.  We'll need to reset this upon successful completion of the lookaround.
	variablePrefix := "negativelookahead_"
	if node.Options&syntax.RightToLeft != 0 {
		variablePrefix = "negativelookbehind_"
	}
	startingPos := rm.reserveName(fmt.Sprint(variablePrefix, "starting_pos"))
	rm.addLocalDec(fmt.Sprint(startingPos, " := 0"))
	c.writeLineFmt("%s = pos\n", startingPos)
	startingSliceStaticPos := rm.sliceStaticPos

	negativeLookaroundDoneLabel := rm.reserveName("NegativeLookaroundMatch")
	rm.doneLabel = negativeLookaroundDoneLabel

	// Check for timeout. Lookarounds result in re-processing the same input, so while not
	// technically backtracking, it's appropriate to have a timeout check.
	c.emitTimeoutCheckIfNeeded(rm)

	child := node.Children[0]

	// Ensure we're able to uncapture anything captured by the child.
	stackCookie := c.createStackCookie()
	isInLoop := false
	var capturePos string
	hasCaptures := rm.Analysis.MayContainCapture(child)
	if hasCaptures {
		// If we're inside a loop, push the current crawl position onto the stack,
		// so that each iteration tracks its own value. Otherwise, store it into a local.
		isInLoop = rm.Analysis.IsInLoop(node)
		if isInLoop {
			c.emitStackPush(stackCookie, "r.Crawlpos()")
		} else {
			capturePos = rm.reserveName(fmt.Sprint(variablePrefix, "capture_pos"))
			rm.addLocalDec(fmt.Sprint(capturePos, " := 0"))
			c.writeLineFmt("%s = r.Crawlpos()", capturePos)
		}
	}

	// Emit the child.
	if rm.Analysis.MayBacktrack(child) {
		// Lookarounds are implicitly atomic, so we need to emit the node as atomic if it might backtrack.
		c.emitExecuteAtomic(rm, node, nil)
	} else {
		c.emitExecuteNode(rm, child, nil, true)
	}

	// If the generated code ends up here, it matched the lookaround, which actually
	// means failure for a _negative_ lookaround, so we need to jump to the original done.
	c.writeLine("")
	if hasCaptures && isInLoop {
		// Pop the crawl position from the stack.
		//TODO: our stack is backwards
		c.writeLine("r.Runstackpos++")
		c.emitStackCookieValidate(stackCookie)
	}
	c.emitExecuteGoto(rm, originalDoneLabel)
	c.writeLine("")

	// Failures (success for a negative lookaround) jump here.
	c.emitMarkLabel(rm, negativeLookaroundDoneLabel, false)

	// After the child completes in failure (success for negative lookaround), reset the text positions.
	c.writeLineFmt("pos = %s", startingPos)
	c.sliceInputSpan(rm, false)
	rm.sliceStaticPos = startingSliceStaticPos

	// And uncapture anything if necessary. Negative lookaround captures don't persist beyond the lookaround.
	if hasCaptures {
		if isInLoop {
			c.emitUncaptureUntil("r.StackPop()")
			c.emitStackCookieValidate(stackCookie)
		} else {
			c.emitUncaptureUntil(capturePos)
		}
	}

	rm.doneLabel = originalDoneLabel
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
		c.writeLine("return nil // The input didn't match.")
	} else {
		rm.usedLabels = append(rm.usedLabels, label)
		c.writeLineFmt("goto %s", label)
	}
}

func (c *converter) emitCaseGoto(rm *regexpData, clause string, label string) {
	c.writeLine(clause)
	c.emitExecuteGoto(rm, label)
}

// Emits code to unwind the capture stack until the crawl position specified in the provided local.
func (c *converter) emitUncaptureUntil(capturepos string) {
	c.writeLineFmt("r.UncaptureUntil(%s)", capturepos)
}

func (c *converter) emitStackPop(stackCookie int, args ...string) {
	for _, arg := range args {
		c.writeLineFmt("%v = r.StackPop()", arg)
	}
}

func (c *converter) createStackCookie() int {
	//TODO: this is a debug function should consider setting it up
	return 0
}

func (c *converter) emitStackCookieValidate(stackCookie int) {
	//TODO: this
}

// / <summary>
// / Returns an expression that:
// / In debug, pops item 1 from the backtracking stack, pops item 2 and validates it against the cookie, then evaluates to item1.
// / In release, pops and evaluates to an item from the backtracking stack.
// / </summary>
func (c *converter) validateStackCookieWithAdditionAndReturnPoppedStack(stackCookie int) string {
	// non-debug behavior
	return "r.StackPop()"

	/*#if DEBUG
	                const string MethodName = "ValidateStackCookieWithAdditionAndReturnPoppedStack";
	                if (!requiredHelpers.ContainsKey(MethodName))
	                {
	                    requiredHelpers.Add(MethodName,
	                    [
	                        $"/// <summary>Validates that a stack cookie popped off the backtracking stack holds the expected value. Debug only.</summary>",
	                        $"internal static int {MethodName}(int poppedStack, int expectedCookie, int actualCookie)",
	                        $"{{",
	                        $"    expectedCookie += poppedStack;",
	                        $"    if (expectedCookie != actualCookie)",
	                        $"    {{",
	                        $"          throw new Exception($\"Backtracking stack imbalance detected. Expected {{expectedCookie}}. Actual {{actualCookie}}.\");",
	                        $"    }}",
	                        $"    return poppedStack;",
	                        $"}}",
	                    ]);
	                }

	                return $"{HelpersTypeName}.{MethodName}({StackPop()}, {stackCookie}, {StackPop()})";
	#else*/

}

/*
#if DEBUG
            /// <summary>Returns an expression that validates and returns a debug stack cookie.</summary>
            string StackCookieValidate(int stackCookie)
            {
                const string MethodName = "ValidateStackCookie";
                if (!requiredHelpers.ContainsKey(MethodName))
                {
                    requiredHelpers.Add(MethodName,
                    [
                        $"/// <summary>Validates that a stack cookie popped off the backtracking stack holds the expected value. Debug only.</summary>",
                        $"internal static int {MethodName}(int expected, int actual)",
                        $"{{",
                        $"    if (expected != actual)",
                        $"    {{",
                        $"        throw new Exception($\"Backtracking stack imbalance detected. Expected {{expected}}. Actual {{actual}}.\");",
                        $"    }}",
                        $"    return actual;",
                        $"}}",
                    ]);
                }

                return $"{HelpersTypeName}.{MethodName}({stackCookie}, {StackPop()})";
            }
#endif
*/

func (c *converter) emitStackPush(stackCookie int, args ...string) {
	switch len(args) {
	case 1:
		c.writeLineFmt("r.StackPush(%s)", args[0])
	case 2:
		c.writeLineFmt("r.StackPush2(%s, %s)", args[0], args[1])
	case 3:
		c.writeLineFmt("r.StackPush3(%s, %s, %s)", args[0], args[1], args[2])
	default:
		c.writeLineFmt("r.StackPushN(%s)", strings.Join(args, ", "))
	}

	//TODO: stack cookie debugging
}

// Emits the sum of a constant and a value from a local.
func sum(constant int, local *string) string {
	if local == nil {
		return strconv.Itoa(constant)
	}
	if constant == 0 {
		return *local
	}
	return fmt.Sprintf("%v + %s", constant, *local)
}

// Emits a check that the span is large enough at the currently known static position to handle the required additional length.
func (c *converter) emitSpanLengthCheck(rm *regexpData, requiredLength int, dynamicRequiredLength *string) {
	c.writeLineFmt("if %s {", spanLengthCheck(rm, requiredLength, dynamicRequiredLength))
	c.emitExecuteGoto(rm, rm.doneLabel)
	c.writeLine("}")
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
	} else if value == -1 {
		c.writeLineFmt("%s--", variable)
	} else if value > 0 {
		c.writeLineFmt("%s += %v", variable, value)
	} else if value < 0 {
		c.writeLineFmt("%s -= %v", variable, -value)
	}
}

func (c *converter) sliceInputSpan(rm *regexpData, declare bool) {
	// Slices the inputSpan starting at pos until end and stores it into slice.
	if declare {
		c.write("var ")
	}
	c.writeLineFmt("%s = r.Runtext[pos:]", rm.sliceSpan)
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
	case syntax.NtNotoneloop, syntax.NtNotoneloopatomic, syntax.NtNotonelazy:
		return fmt.Sprintf(`Match a character other than %q %s%s.`, node.Ch, describeLoop(rm, node), direction)
	case syntax.NtOne:
		return fmt.Sprintf(`Match %q%s.`, node.Ch, direction)
	case syntax.NtOneloop, syntax.NtOneloopatomic, syntax.NtOnelazy:
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
	case syntax.NtSetloop, syntax.NtSetloopatomic, syntax.NtSetlazy:
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
	if node.IsAtomicloopFamily() {
		style = "atomically"
	} else if node.T == syntax.NtOneloop || node.T == syntax.NtNotoneloop || node.T == syntax.NtSetloop {
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
