package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"io"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	"github.com/dlclark/regexp2/v2/syntax"
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

	if expr != nil && len(*expr) > 0 {
		options := syntax.RegexOptions(0)
		if opt != nil {
			options = syntax.RegexOptions(*opt)
		}
		convertSingle(*expr, options, *pkg)
		return
	}

	convPath, _ := os.Getwd()
	if path != nil && len(*path) > 0 {
		convPath, _ = filepath.Abs(*path)
	}

	convertPath(convPath, *tests)
}

func getOutStream() (io.Writer, string) {
	outPath := ""
	if out == nil || len(*out) == 0 {
		return os.Stdout, ""
	}

	outPath, _ = filepath.Abs(*out)
	file, err := os.Create(outPath)
	if err != nil {
		log.Fatalf("error creating out file: %v", err)
	}

	return file, outPath
}

func convertSingle(expr string, opts syntax.RegexOptions, pkg string) {
	stream, _ := getOutStream()
	if stream == nil {
		log.Fatalf("unable to open output")
	}
	c, err := newConverter(stream, pkg)
	if err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	if err := c.addRegexp("command line", "MyPattern", expr, opts, false, []string{getOptString(opts)}); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
	if err := c.addFooter(); err != nil {
		log.Fatal(errors.Wrap(err, "code generation error"))
	}
}

func convertPath(path string, includeTest bool) {
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
	var stream io.Writer
	var outFile string
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
				// a := regexp2.MustCompile("pattern", regexp2.None)
				// var a = regexp2....
				if varDec, ok := n.(*ast.ValueSpec); ok {
					// var dec
					for i, val := range varDec.Values {
						ok, pat, opt, compileOptions, maintainCaptureOrder, pos, err := isStaticCompileCall(val, alias)
						if err != nil {
							log.Fatal(errors.Wrapf(err, "%s: unsupported regexp2 compile option", fset.Position(pos)))
						}
						if ok {
							log.Printf("%s: adding pattern %#v options %v", fset.Position(pos), pat, opt)
							// first find inits a converter
							if c == nil {
								stream, outFile = getOutStream()
								if stream == nil {
									log.Fatalf("unable to open output")
								}
								c, err = newConverter(stream, p)
								if err != nil {
									log.Fatal(errors.Wrap(err, "code generation error"))
								}
							}

							if err := c.addRegexp(getLocation(fset, pos, outFile), getName(varDec.Names[i]), pat, syntax.RegexOptions(opt), maintainCaptureOrder, compileOptions); err != nil {
								log.Fatal(errors.Wrap(err, "code generation error"))
							}
						}
					}
				} else if assign, ok := n.(*ast.AssignStmt); ok {
					for i, exp := range assign.Rhs {
						ok, pat, opt, compileOptions, maintainCaptureOrder, pos, err := isStaticCompileCall(exp, alias)
						if err != nil {
							log.Fatal(errors.Wrapf(err, "%s: unsupported regexp2 compile option", fset.Position(pos)))
						}
						if ok {
							log.Printf("%s: adding pattern %#v options %v", fset.Position(pos), pat, opt)
							// first find inits a converter
							if c == nil {
								stream, outFile = getOutStream()
								if stream == nil {
									log.Fatalf("unable to open output")
								}
								c, err = newConverter(stream, p)
								if err != nil {
									log.Fatal(errors.Wrap(err, "code generation error"))
								}
							}

							if err := c.addRegexp(getLocation(fset, pos, outFile), getName(assign.Lhs[i]), pat, syntax.RegexOptions(opt), maintainCaptureOrder, compileOptions); err != nil {
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

// returns a location in the fileset relative to the output path given
// or pwd if output path is blank
func getLocation(fset *token.FileSet, pos token.Pos, outPath string) string {
	fullPos := fset.Position(pos)

	//make filename relative to our pwd
	if outPath == "" {
		outPath, _ = os.Getwd()
	} else {
		outPath = filepath.Dir(outPath)
	}

	file, _ := filepath.Rel(outPath, fullPos.Filename)
	return fmt.Sprint(file, ":", fullPos.Line, ":", fullPos.Column)
}

func getName(lhs ast.Node) string {
	// should be an ident
	if ident, ok := lhs.(*ast.Ident); ok {
		return ident.Name
	}
	return ""
}

func isStaticCompileCall(n ast.Node, importAlias string) (ok bool, pattern string, opts int, compileOptions []string, maintainCaptureOrder bool, patternPos token.Pos, err error) {
	funcCall, ok := n.(*ast.CallExpr)
	if !ok {
		return false, "", 0, nil, false, 0, nil
	}

	if len(funcCall.Args) < 1 {
		return false, "", 0, nil, false, 0, nil
	}

	if match, _ := isSelector(funcCall.Fun, importAlias, "MustCompile", "Compile"); match {
		pattern, ok = extractPattern(funcCall.Args[0])
		if !ok {
			return false, "", 0, nil, false, 0, nil
		}

		opts = 0 // Default options
		for _, arg := range funcCall.Args[1:] {
			compileOption, err := getCompileOptionExpr(arg, importAlias)
			if err != nil {
				return false, "", 0, nil, false, arg.Pos(), err
			}

			tmpOpts, ok := getOpts(arg, importAlias)
			if ok {
				opts |= tmpOpts
				compileOptions = append(compileOptions, compileOption)
				continue
			}

			knownCompileOption, isMaintainCaptureOrder, err := isKnownNonRegexCompileOption(arg, importAlias)
			if err != nil {
				return false, "", 0, nil, false, arg.Pos(), err
			}
			if knownCompileOption {
				compileOptions = append(compileOptions, compileOption)
				maintainCaptureOrder = maintainCaptureOrder || isMaintainCaptureOrder
				continue
			}

			return false, "", 0, nil, false, 0, nil
		}

		return true, pattern, opts, compileOptions, maintainCaptureOrder, funcCall.Args[0].Pos(), nil
	}

	return false, "", 0, nil, false, 0, nil
}

func getCompileOptionExpr(node ast.Node, importAlias string) (string, error) {
	var buf bytes.Buffer
	if err := format.Node(&buf, token.NewFileSet(), node); err != nil {
		return "", err
	}

	expr := buf.String()
	if importAlias != "" && importAlias != "regexp2" {
		expr = strings.ReplaceAll(expr, importAlias+".", "regexp2.")
	}
	return expr, nil
}

func extractPattern(arg ast.Expr) (pattern string, ok bool) {
	switch v := arg.(type) {
	case *ast.BasicLit: // Direct string literal
		if v.Kind == token.STRING {
			pattern, _ = strconv.Unquote(v.Value) // Extract string
			return pattern, true
		}
	case *ast.BinaryExpr: // Concatenated strings
		// it does work here but the code for loading the pre-compiled regexp doesn't work with it, left it for future
		left, ok1 := extractPattern(v.X)
		right, ok2 := extractPattern(v.Y)
		if ok1 && ok2 {
			return left + right, true
		}
	case *ast.Ident: // Constant or variable
		// Example: const myPattern = "pattern"
		if v.Obj != nil && v.Obj.Kind == ast.Con {
			if valueSpec, ok := v.Obj.Decl.(*ast.ValueSpec); ok {
				if len(valueSpec.Values) > 0 {
					return extractPattern(valueSpec.Values[0])
				}
			}
		}
	}
	return "", false // Unsupported type
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
	} else if ok, _ := isSelector(node, importAlias, "None"); ok {
		return 0, true
	} else if ok, name := isSelector(node, importAlias, optNames...); ok {
		//selector, convert to int
		return convertOptsNameToInt(name)
	} else if call, ok := node.(*ast.CallExpr); ok {
		if ok, _ := isSelector(call.Fun, importAlias, "RegexOptions"); ok && len(call.Args) == 1 {
			return getOpts(call.Args[0], importAlias)
		}
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

func isKnownNonRegexCompileOption(node ast.Node, importAlias string) (bool, bool, error) {
	call, ok := node.(*ast.CallExpr)
	if !ok {
		return false, false, nil
	}

	if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
		if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == importAlias {
			if sel.Sel.Name == "OptionMaintainCaptureOrder" {
				return true, true, nil
			}
			if slices.Contains(runtimeCompileOptionNames, sel.Sel.Name) || slices.Contains(skippedCompileOptionNames, sel.Sel.Name) {
				return true, false, nil
			}
			if strings.HasPrefix(sel.Sel.Name, "Option") {
				return false, false, fmt.Errorf("unknown regexp2 compile option %s", sel.Sel.Name)
			}
		}
	}

	return false, false, nil
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
		if i.Path.Value == "\"github.com/dlclark/regexp2/v2\"" {
			if i.Name != nil {
				*alias = i.Name.Name
			}

			return true
		}
	}
	return false
}
