package main

import (
	"bytes"
	"fmt"
	"os"
	"strings"

	"github.com/dlclark/regexp2"
)

// our file that runs the given regex and options against the args
// and outputs the results in a known way for comparison

func main() {
	re := regexp2.MustCompile(__PATTERN__, __OPTIONS__)

	if len(os.Args) > 2 {
		//debug mode
		fmt.Printf("Pattern: %s\n", re.String())
		//fmt.Printf("Options: %v\n", re.Options)
	}

	m, err := re.FindStringMatch(unEscapeToMatch(os.Args[1]))
	if err != nil {
		fmt.Printf("ERROR: %v", err)
	}
	if m == nil {
		fmt.Println("No match")
		return
	}

	g := m.Groups()
	for i := 0; i < len(g); i++ {
		val := "<unset>"
		if len(g[i].Captures) > 0 {
			val = unEscapeGroup(g[i].String())
		}
		fmt.Printf("%2v: %s\n", i, val)
	}

}

func unEscapeGroup(val string) string {
	// use hex for chars 0x00-0x1f, 0x7f-0xff
	buf := &bytes.Buffer{}

	for i := 0; i < len(val); i++ {
		ch := val[i]
		if ch <= 0x1f || ch >= 0x7f {
			//write it as a \x00
			fmt.Fprintf(buf, "\\x%.2x", ch)
		} else {
			// write as-is
			buf.WriteByte(ch)
		}
	}

	return buf.String()
}

func unEscapeToMatch(line string) string {
	idx := strings.IndexRune(line, '\\')
	// no slashes means no unescape needed
	if idx == -1 {
		return line
	}

	buf := bytes.NewBufferString(line[:idx])
	// get the runes for the rest of the string -- we're going full parser scan on this

	inEscape := false
	// take any \'s and convert them
	for i := idx; i < len(line); i++ {
		ch := line[i]
		if ch == '\\' {
			if inEscape {
				buf.WriteByte(ch)
			}
			inEscape = !inEscape
			continue
		}
		if inEscape {
			switch ch {
			case 'x':
				buf.WriteByte(scanHex(line, &i))
			case 'a':
				buf.WriteByte(0x07)
			case 'b':
				buf.WriteByte('\b')
			case 'e':
				buf.WriteByte(0x1b)
			case 'f':
				buf.WriteByte('\f')
			case 'n':
				buf.WriteByte('\n')
			case 'r':
				buf.WriteByte('\r')
			case 't':
				buf.WriteByte('\t')
			case 'v':
				buf.WriteByte(0x0b)
			default:
				if ch >= '0' && ch <= '7' {
					buf.WriteByte(scanOctal(line, &i))
				} else {
					buf.WriteByte(ch)
					//panic(fmt.Sprintf("unexpected escape '%v' in %v", string(ch), line))
				}
			}
			inEscape = false
		} else {
			buf.WriteByte(ch)
		}
	}

	return buf.String()
}

func scanHex(line string, idx *int) byte {
	if *idx >= len(line)-2 {
		panic(fmt.Sprintf("not enough hex chars in %v at %v", line, *idx))
	}
	(*idx)++
	d1 := hexDigit(line[*idx])
	(*idx)++
	d2 := hexDigit(line[*idx])
	if d1 < 0 || d2 < 0 {
		panic("bad hex chars")
	}

	return byte(d1*0x10 + d2)
}

// Returns n <= 0xF for a hex digit.
func hexDigit(ch byte) int {

	if d := uint(ch - '0'); d <= 9 {
		return int(d)
	}

	if d := uint(ch - 'a'); d <= 5 {
		return int(d + 0xa)
	}

	if d := uint(ch - 'A'); d <= 5 {
		return int(d + 0xa)
	}

	return -1
}

// Scans up to three octal digits (stops before exceeding 0377).
func scanOctal(line string, idx *int) byte {
	// Consume octal chars only up to 3 digits and value 0377

	// octals can be 3,2, or 1 digit
	c := 3

	if diff := len(line) - *idx; c > diff {
		c = diff
	}

	i := 0
	d := int(line[*idx] - '0')
	for c > 0 && d <= 7 {
		i *= 8
		i += d

		c--
		(*idx)++
		if *idx < len(line) {
			d = int(line[*idx] - '0')
		}
	}
	(*idx)--

	// Octal codes only go up to 255.  Any larger and the behavior that Perl follows
	// is simply to truncate the high bits.
	i &= 0xFF

	return byte(i)
}
