package main

import (
	"flag"
	"log"
	"os"

	"github.com/pkg/errors"

	"github.com/dlclark/regexp2/syntax"
)

var expr = flag.String("expr", "", "the regexp to convert to Go code")
var opt = flag.Int("opt", 0, "bitwise options to use when compiling the regexp")

//TODO: timeout
//TODO: eventually this should scan the given folder for regexp2.MustCompile() calls and convert them into
// pre-compiled regexp's.  For now, it recieves the input on the command line for simplicity

func main() {
	flag.Parse()

	if expr == nil {
		flag.PrintDefaults()
		os.Exit(-1)
	}

	options := syntax.RegexOptions(0)
	if opt != nil {
		options = syntax.RegexOptions(*opt)
	}
	txt := *expr

	c, err := newConverter(os.Stdout, "regexp2codegen")
	if err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	// TODO: loop through each regexp
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", txt, options); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	if err := c.addFooter(); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
}
