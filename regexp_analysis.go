package main

import "github.com/dlclark/regexp2/syntax"

type analysisResults struct {
	// true if the whole tree successfully processed, otherwise false
	// if false we assume the worst and "unknown" but this should be rare
	complete bool

	//Set of nodes that are considered to be atomic based on themselves or their ancestry.
	isAtomicByAncestor map[*syntax.RegexNode]struct{}

	//Set of nodes that directly or indirectly contain capture groups.
	containsCapture map[*syntax.RegexNode]struct{}

	//Set of nodes that directly or indirectly contain backtracking constructs that aren't hidden internaly by atomic constructs.
	mayBacktrack map[*syntax.RegexNode]struct{}

	//Set of nodes contained inside loops.
	inLoops map[*syntax.RegexNode]struct{}

	hasIgnoreCase  bool
	hasRightToLeft bool
}

func analyze(tree *syntax.RegexTree) *analysisResults {
	var results = &analysisResults{
		isAtomicByAncestor: make(map[*syntax.RegexNode]struct{}),
		containsCapture:    make(map[*syntax.RegexNode]struct{}),
	}

	results.complete = tryAnalyze(tree.Root, results, true, false)

	return results
}

func tryAnalyze(node *syntax.RegexNode, results *analysisResults, isAtomicByAncestor bool, isInLoop bool) bool {
	//TODO: stack-depth check?

	// Track whether we've seen any nodes with various options set.
	results.hasIgnoreCase = results.hasIgnoreCase || (node.Options&syntax.IgnoreCase) != 0
	results.hasRightToLeft = results.hasRightToLeft || (node.Options&syntax.RightToLeft) != 0

	// Track whether this node is inside of a loop.
	if isInLoop {
		if results.inLoops == nil {
			results.inLoops = make(map[*syntax.RegexNode]struct{})
		}
		results.inLoops[node] = struct{}{}
	}

	if isAtomicByAncestor {
		// We've been told by our parent that we should be considered atomic, so add ourselves
		// to the atomic collection.
		results.isAtomicByAncestor[node] = struct{}{}
	} else {
		// Certain kinds of nodes incur backtracking logic themselves: add them to the backtracking collection.
		// We may later find that a node contains another that has backtracking; we'll add nodes based on that
		// after examining the children.
		if node.T == syntax.NtAlternate ||
			(node.M != node.N &&
				(node.T == syntax.NtLoop || node.T == syntax.NtLazyloop ||
					node.T == syntax.NtOneloop || node.T == syntax.NtNotoneloop || node.T == syntax.NtSetloop ||
					node.T == syntax.NtOnelazy || node.T == syntax.NtNotonelazy || node.T == syntax.NtSetlazy)) {

			results.addMayBacktrack(node)
		}
	}

	// Update state for certain node types.
	var isAtomicBySelf = false
	switch node.T {
	case syntax.NtAtomic, syntax.NtNegLook, syntax.NtPosLook:
		isAtomicBySelf = true
	case syntax.NtCapture:
		results.containsCapture[node] = struct{}{}
	case syntax.NtLoop, syntax.NtLazyloop:
		isInLoop = true
	}

	childCount := len(node.Children)
	for i := 0; i < childCount; i++ {
		child := node.Children[i]

		// Determine whether the child should be treated as atomic (whether anything
		// can backtrack into it), which is influenced by whether this node (the child's
		// parent) is considered atomic by itself or by its parent.
		treatChildAsAtomic := isAtomicByAncestor || isAtomicBySelf

		// If the parent is atomic, so is the child.  That's the whole purpose
		// of the Atomic node, and lookarounds are also implicitly atomic.
		if !(node.T == syntax.NtAtomic || node.T == syntax.NtNegLook || node.T == syntax.NtPosLook ||
			node.T == syntax.NtAlternate || node.T == syntax.NtBackRefCond || node.T == syntax.NtExprCond ||
			node.T == syntax.NtCapture ||
			(node.T == syntax.NtConcatenate && i == childCount-1) ||
			((node.T == syntax.NtLoop || node.T == syntax.NtLazyloop) && node.N == 1)) {
			// if these conditions aren't met then we're not atomic
			treatChildAsAtomic = false
		}

		//analyze the child
		if !tryAnalyze(child, results, treatChildAsAtomic, isInLoop) {
			return false
		}

		// If the child contains captures, so too does this parent.
		if _, ok := results.containsCapture[child]; ok {
			results.containsCapture[node] = struct{}{}
		}

		// If the child might require backtracking into it, so too might the parent,
		// unless the parent is itself considered atomic.  Here we don't consider parental
		// atomicity, as we need to surface upwards to the parent whether any backtracking
		// will be visible from this node to it.

		if !isAtomicBySelf && results.mayBacktrack != nil {
			_, ok := results.mayBacktrack[child]
			if ok {
				results.addMayBacktrack(node)
			}
		}
	}

	return true
}

func (a *analysisResults) IsAtomicByAncestor(node *syntax.RegexNode) bool {
	_, ok := a.isAtomicByAncestor[node]
	return ok
}

func (a *analysisResults) MayContainCapture(node *syntax.RegexNode) bool {
	if !a.complete {
		return true
	}
	_, ok := a.containsCapture[node]
	return ok
}

func (a *analysisResults) MayBacktrack(node *syntax.RegexNode) bool {
	if !a.complete {
		return true
	}
	if a.mayBacktrack == nil {
		return false
	}
	_, ok := a.mayBacktrack[node]
	return ok
}

func (a *analysisResults) addMayBacktrack(node *syntax.RegexNode) {
	if a.mayBacktrack == nil {
		a.mayBacktrack = make(map[*syntax.RegexNode]struct{})
	}
	a.mayBacktrack[node] = struct{}{}
}

func (a *analysisResults) IsInLoop(node *syntax.RegexNode) bool {
	if !a.complete {
		return true
	}
	if a.inLoops == nil {
		return false
	}
	_, ok := a.inLoops[node]
	return ok
}

func (a *analysisResults) HasIgnoreCase() bool {
	return !a.complete || a.hasIgnoreCase
}

func (a *analysisResults) HasRightToLeft() bool {
	return !a.complete || a.hasRightToLeft
}
