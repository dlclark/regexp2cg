package main

import (
	"github.com/dlclark/regexp2"
	"github.com/dlclark/regexp2/helpers"
	"github.com/dlclark/regexp2/syntax"
	"unicode"
)

/*
Capture-I(index = 0, unindex = -1)
 Concatenate
  Loop(Min = 0, Max = 1)
   Capture(index = 1, unindex = -1)
    Concatenate
     One(Ch = >)
     Notoneloop(Ch = \n)(Min = 1, Max = inf)
  One(Ch = \n)
*/
// From ../regex-redux/main.go:57:30
// Pattern: "(>[^\n]+)?\n"
// Options: regexp2.IgnoreCase
type re_Engine struct{}

func (re_Engine) Caps() map[int]int        { return nil }
func (re_Engine) CapNames() map[string]int { return nil }
func (re_Engine) CapsList() []string       { return nil }
func (re_Engine) CapSize() int             { return 2 }

func (re_Engine) FindFirstChar(r *regexp2.Runner) bool {
	pos := r.Runtextpos
	// Empty matches aren't possible
	if pos < len(r.Runtext) {
		// The pattern begins with [\n>]
		// Find the next occurrence. If it can't be found, there's no match.
		i := helpers.IndexOfAny2(r.Runtext[pos:], '\n', '>')
		if i >= 0 {
			r.Runtextpos = pos + i
			return true
		}

	}

	// No match found
	r.Runtextpos = len(r.Runtext)
	return false
}

func (re_Engine) Execute(r *regexp2.Runner) error {
	loop_iteration := 0
	capture_starting_pos := 0
	var charloop_starting_pos, charloop_ending_pos = 0, 0
	iteration := 0
	pos := r.Runtextpos
	matchStart := pos

	var slice = r.Runtext[pos:]

	// Node: Concatenate
	// Node: Loop(Min = 0, Max = 1)
	// Optional (greedy).
	loop_iteration = 0

LoopBody:
	r.StackPush2(r.Crawlpos(), pos)

	loop_iteration++

	// Node: Capture(index = 1, unindex = -1)
	// "1" capture group
	capture_starting_pos = pos

	// Node: Concatenate
	// Node: One(Ch = >)
	// Match '>'.
	if len(slice) == 0 || slice[0] != '>' {
		goto LoopIterationNoMatch
	}

	// Node: Notoneloop(Ch = \n)(Min = 1, Max = inf)
	// Match a character other than '\n' greedily at least once.
	pos++
	slice = r.Runtext[pos:]
	charloop_starting_pos = pos

	iteration = helpers.IndexOfAny1(slice, '\n')
	if iteration < 0 {
		iteration = len(slice)
	}

	if iteration == 0 {
		goto LoopIterationNoMatch
	}

	slice = slice[iteration:]
	pos += iteration

	charloop_ending_pos = pos
	charloop_starting_pos++
	goto CharLoopEnd

CharLoopBacktrack:
	r.UncaptureUntil(r.StackPop())
	charloop_ending_pos = r.StackPop()
	charloop_starting_pos = r.StackPop()

	if err := r.CheckTimeout(); err != nil {
		return err
	}
	if charloop_starting_pos >= charloop_ending_pos {
		goto LoopIterationNoMatch
	}
	charloop_ending_pos--
	pos = charloop_ending_pos
	slice = r.Runtext[pos:]

CharLoopEnd:
	r.StackPush3(charloop_starting_pos, charloop_ending_pos, r.Crawlpos())

	r.Capture(1, capture_starting_pos, pos)

	r.StackPush(capture_starting_pos)
	goto CaptureSkipBacktrack

CaptureBacktrack:
	capture_starting_pos = r.StackPop()
	goto CharLoopBacktrack

CaptureSkipBacktrack:
	;

	// The loop has an upper bound of 1. Continue iterating greedily if it hasn't yet been reached.
	if loop_iteration == 0 {
		goto LoopBody
	}
	goto LoopEnd

	// The loop iteration failed. Put state back to the way it was before the iteration.
LoopIterationNoMatch:
	loop_iteration--
	if loop_iteration < 0 {
		// Unable to match the remainder of the expression after exhausting the loop.
		r.UncaptureUntil(0)
		return nil // The input didn't match.
	}
	pos = r.StackPop()
	r.UncaptureUntil(r.StackPop())
	slice = r.Runtext[pos:]
	goto LoopEnd

LoopBacktrack:
	if err := r.CheckTimeout(); err != nil {
		return err
	}
	if loop_iteration == 0 {
		// No iterations of the loop remain to backtrack into. Fail the loop.
		r.UncaptureUntil(0)
		return nil // The input didn't match.
	}
	goto CaptureBacktrack
LoopEnd:
	;

	// Node: One(Ch = \n)
	// Match '\n'.
	if len(slice) == 0 || slice[0] != '\n' {
		goto LoopBacktrack
	}

	// The input matched.
	pos++
	r.Runtextpos = pos
	r.Capture(0, matchStart, pos)
	// just to prevent an unused var error in certain regex's
	var _ = slice
	return nil
}

/*
Capture(index = 0, unindex = -1)
 Concatenate
  Loop(Min = 0, Max = 1)
   Capture(index = 1, unindex = -1)
    Concatenate
     One(Ch = >)
     Notoneloop(Ch = \n)(Min = 1, Max = inf)
  One(Ch = \n)
*/
// From ../regex-redux/main.go:71:28
// Pattern: "(>[^\n]+)?\n"
// Options: regexp2.Debug|regexp2.RE2
type re_2_Engine struct{}

func (re_2_Engine) Caps() map[int]int        { return nil }
func (re_2_Engine) CapNames() map[string]int { return nil }
func (re_2_Engine) CapsList() []string       { return nil }
func (re_2_Engine) CapSize() int             { return 2 }

func (re_2_Engine) FindFirstChar(r *regexp2.Runner) bool {
	pos := r.Runtextpos
	// Empty matches aren't possible
	if pos < len(r.Runtext) {
		// The pattern begins with [\n>]
		// Find the next occurrence. If it can't be found, there's no match.
		i := helpers.IndexOfAny2(r.Runtext[pos:], '\n', '>')
		if i >= 0 {
			r.Runtextpos = pos + i
			return true
		}

	}

	// No match found
	r.Runtextpos = len(r.Runtext)
	return false
}

func (re_2_Engine) Execute(r *regexp2.Runner) error {
	loop_iteration := 0
	capture_starting_pos := 0
	var charloop_starting_pos, charloop_ending_pos = 0, 0
	iteration := 0
	pos := r.Runtextpos
	matchStart := pos

	var slice = r.Runtext[pos:]

	// Node: Concatenate
	// Node: Loop(Min = 0, Max = 1)
	// Optional (greedy).
	loop_iteration = 0

LoopBody:
	r.StackPush2(r.Crawlpos(), pos)

	loop_iteration++

	// Node: Capture(index = 1, unindex = -1)
	// "1" capture group
	capture_starting_pos = pos

	// Node: Concatenate
	// Node: One(Ch = >)
	// Match '>'.
	if len(slice) == 0 || slice[0] != '>' {
		goto LoopIterationNoMatch
	}

	// Node: Notoneloop(Ch = \n)(Min = 1, Max = inf)
	// Match a character other than '\n' greedily at least once.
	pos++
	slice = r.Runtext[pos:]
	charloop_starting_pos = pos

	iteration = helpers.IndexOfAny1(slice, '\n')
	if iteration < 0 {
		iteration = len(slice)
	}

	if iteration == 0 {
		goto LoopIterationNoMatch
	}

	slice = slice[iteration:]
	pos += iteration

	charloop_ending_pos = pos
	charloop_starting_pos++
	goto CharLoopEnd

CharLoopBacktrack:
	r.UncaptureUntil(r.StackPop())
	charloop_ending_pos = r.StackPop()
	charloop_starting_pos = r.StackPop()

	if err := r.CheckTimeout(); err != nil {
		return err
	}
	if charloop_starting_pos >= charloop_ending_pos {
		goto LoopIterationNoMatch
	}
	charloop_ending_pos--
	pos = charloop_ending_pos
	slice = r.Runtext[pos:]

CharLoopEnd:
	r.StackPush3(charloop_starting_pos, charloop_ending_pos, r.Crawlpos())

	r.Capture(1, capture_starting_pos, pos)

	r.StackPush(capture_starting_pos)
	goto CaptureSkipBacktrack

CaptureBacktrack:
	capture_starting_pos = r.StackPop()
	goto CharLoopBacktrack

CaptureSkipBacktrack:
	;

	// The loop has an upper bound of 1. Continue iterating greedily if it hasn't yet been reached.
	if loop_iteration == 0 {
		goto LoopBody
	}
	goto LoopEnd

	// The loop iteration failed. Put state back to the way it was before the iteration.
LoopIterationNoMatch:
	loop_iteration--
	if loop_iteration < 0 {
		// Unable to match the remainder of the expression after exhausting the loop.
		r.UncaptureUntil(0)
		return nil // The input didn't match.
	}
	pos = r.StackPop()
	r.UncaptureUntil(r.StackPop())
	slice = r.Runtext[pos:]
	goto LoopEnd

LoopBacktrack:
	if err := r.CheckTimeout(); err != nil {
		return err
	}
	if loop_iteration == 0 {
		// No iterations of the loop remain to backtrack into. Fail the loop.
		r.UncaptureUntil(0)
		return nil // The input didn't match.
	}
	goto CaptureBacktrack
LoopEnd:
	;

	// Node: One(Ch = \n)
	// Match '\n'.
	if len(slice) == 0 || slice[0] != '\n' {
		goto LoopBacktrack
	}

	// The input matched.
	pos++
	r.Runtextpos = pos
	r.Capture(0, matchStart, pos)
	// just to prevent an unused var error in certain regex's
	var _ = slice
	return nil
}

func init() {
	regexp2.RegisterEngine("(>[^\n]+)?\n", regexp2.IgnoreCase, &re_Engine{})
	regexp2.RegisterEngine("(>[^\n]+)?\n", regexp2.Debug|regexp2.RE2, &re_2_Engine{})
	var _ = helpers.Min
	var _ = syntax.NewCharSetRuntime
	var _ = unicode.IsDigit
}
