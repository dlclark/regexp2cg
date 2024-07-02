package main

import (
	"flag"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"slices"
	"strconv"

	"github.com/pkg/errors"

	"github.com/dlclark/regexp2/syntax"
)

// single regex options - if expr is set then use that, opt, and package
var expr = flag.String("expr", "", "the regexp to convert to Go code to output file")
var opt = flag.Int("opt", 0, "bitwise options to use when compiling the regexp")
var pkg = flag.String("package", "regexp2codegen", "package to use when converting a single regexp")

// if not single regex then scan the path and convert all regex's we find, optionally including test files
var path = flag.String("path", ".", "file path to scan and generate regexp's for")
var tests = flag.Bool("test", false, "true if go tests should be scanned as well")

// universal options
var out = flag.String("o", "", "output file to write generated regexp code into, if the file exists overwrites it. defaults to stdout")

func main() {
	flag.Parse()

	var outStream io.Writer
	if out == nil || len(*out) == 0 {
		outStream = os.Stdout
	} else {
		outStream, err := os.Create(*out)
		if err != nil {
			log.Fatalf("error creating out file: %v", err)
		}
		defer func() {
			outStream.Close()
		}()
	}

	if expr != nil && len(*expr) > 0 {
		options := syntax.RegexOptions(0)
		if opt != nil {
			options = syntax.RegexOptions(*opt)
		}
		convertSingle(*expr, options, *pkg, outStream)
		return
	}

	convPath, _ := os.Getwd()
	if path != nil && len(*path) > 0 {
		convPath, _ = filepath.Abs(*path)
	}

	convertPath(convPath, *tests, outStream)
}

func convertSingle(expr string, opts syntax.RegexOptions, pkg string, out io.Writer) {
	c, err := newConverter(out, pkg)
	if err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	if err := c.addRegexp("MyFile.go:120:10", "MyPattern", expr, opts); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	if err := c.addFooter(); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
}

func convertPath(path string, includeTest bool, out io.Writer) {
	log.Printf("Create regexp for path %s, include tests=%v", path, includeTest)
	//Create a FileSet to work with
	fset := token.NewFileSet()

	pkgs, err := parser.ParseDir(fset, path, func(fi fs.FileInfo) bool {
		// if we disallow tests and match the test pattern then skip
		if !includeTest {
			if ok, _ := filepath.Match("*_test.go", fi.Name()); ok {
				return false
			}
		}
		return true
	}, parser.ParseComments|parser.SkipObjectResolution)

	if err != nil {
		log.Fatalf("unable to parse go file: %v", err)
	}

	var alias string
	var c *converter
	for p, pkg := range pkgs {
		for f, file := range pkg.Files {
			if !importsRegexp2(file, &alias) {
				// we don't import the regexp engine in this file
				// so we won't find anything here
				continue
			}
			if alias == "" {
				alias = "regexp2"
			}
			log.Printf("file %v imports regexp2", f)
			ast.Inspect(file, func(n ast.Node) bool {
				// Find asignment statements
				// a := regexp2.MustCompile("pattern", 0)
				// var a = regexp2....
				if varDec, ok := n.(*ast.ValueSpec); ok {
					// var dec
					for i, val := range varDec.Values {
						ok, pat, opt, pos := isStaticCompileCall(val, alias)
						if ok {
							// first find inits a converter
							if c == nil {
								c, err = newConverter(out, p)
								if err != nil {
									log.Fatal(errors.Wrap(err, "code generation error"))
								}
							}

							if err := c.addRegexp(fset.Position(pos).String(), getName(varDec.Names[i]), pat, syntax.RegexOptions(opt)); err != nil {
								log.Fatal(errors.Wrap(err, "code generation error"))
							}
						}
					}
				} else if assign, ok := n.(*ast.AssignStmt); ok {
					for i, exp := range assign.Rhs {
						ok, pat, opt, pos := isStaticCompileCall(exp, alias)
						if ok {
							// first find inits a converter
							if c == nil {
								c, err = newConverter(out, p)
								if err != nil {
									log.Fatal(errors.Wrap(err, "code generation error"))
								}
							}

							if err := c.addRegexp(fset.Position(pos).String(), getName(assign.Lhs[i]), pat, syntax.RegexOptions(opt)); err != nil {
								log.Fatal(errors.Wrap(err, "code generation error"))
							}
						}
					}
				}

				return true
			})
		}
	}
	if c != nil {
		if err := c.addFooter(); err != nil {
			log.Fatal(errors.Wrap(err, "code generation error"))
		}
	}
}

func getName(lhs ast.Node) string {
	// should be an ident
	if ident, ok := lhs.(*ast.Ident); ok {
		return ident.Name
	}
	return ""
}

func isStaticCompileCall(n ast.Node, importAlias string) (ok bool, pattern string, opts int, patternPos token.Pos) {

	if funcCall, ok := n.(*ast.CallExpr); ok {
		//wrong number of args, can't be us
		if len(funcCall.Args) != 2 {
			return false, "", 0, 0
		}
		if ok, _ := isSelector(funcCall.Fun, importAlias, "MustCompile", "Compile"); ok {
			// get our pattern, the options, if they're both literals (or known constants)
			// then we're good
			pat, ok := funcCall.Args[0].(*ast.BasicLit)
			if !ok {
				return false, "", 0, 0
			}
			// our pattern
			pattern, _ = strconv.Unquote(pat.Value)

			// handle options as int constant
			opts, ok = getOpts(funcCall.Args[1], importAlias)
			if !ok {
				return false, "", 0, 0
			}

			// it parsed!
			return true, pattern, opts, funcCall.Args[0].Pos()
		}
	}

	return false, "", 0, 0
}

func getOpts(node ast.Node, importAlias string) (int, bool) {
	if op, ok := node.(*ast.BasicLit); ok {
		// string version of an int, convert to int
		opts, err := strconv.Atoi(op.Value)
		if err != nil {
			log.Printf("unknown constant for options: %s", op.Value)
			return 0, false
		}
		return opts, true
	} else if ok, name := isSelector(node, importAlias, optNames...); ok {
		//selector, convert to int
		return convertOptsNameToInt(name)
	} else if bin, ok := node.(*ast.BinaryExpr); ok {
		// binary expression, gonna need to split and then do the operator
		// converting to a constant
		x, ok := getOpts(bin.X, importAlias)
		if !ok {
			return 0, false
		}
		y, ok := getOpts(bin.Y, importAlias)
		if !ok {
			return 0, false
		}
		switch bin.Op {
		case token.ADD:
			return x + y, true
		case token.AND:
			return x & y, true
		case token.OR:
			return x | y, true
		case token.XOR:
			return x ^ y, true
		case token.SHL:
			return x << y, true
		case token.SHR:
			return x >> y, true
		case token.AND_NOT:
			return x &^ y, true
		default:
			log.Printf("unknown operator for options: %s", bin.Op.String())
		}
	} else {
		log.Printf("unknown ast node type for options: %T %+[1]v", node)
	}

	return 0, false
}

func convertOptsNameToInt(name string) (int, bool) {
	idx := slices.Index(optNames, name)
	if idx == -1 {
		log.Printf("unknown pattern option: %s", name)
		return 0, false
	}

	return 1 << idx, true
}

func isSelector(node ast.Node, pkg string, name ...string) (bool, string) {
	if sel, ok := node.(*ast.SelectorExpr); ok {
		if nm, ok := sel.X.(*ast.Ident); ok && nm.Name == pkg {
			if slices.Contains(name, sel.Sel.Name) {
				return true, sel.Sel.Name
			}
		}
	}
	return false, ""
}

func importsRegexp2(file *ast.File, alias *string) bool {
	*alias = ""
	for _, i := range file.Imports {
		if i.Path.Value == "\"github.com/dlclark/regexp2\"" {
			if i.Name != nil {
				*alias = i.Name.Name
			}

			return true
		}
	}
	return false
}
