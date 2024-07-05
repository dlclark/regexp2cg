# What is it?
`regexp2cg` will convert `regexp2` patterns that typically run as interpreted state machines into Go code that can be compiled and optimized by the Go compiler. This can have a dramatic runtime performance improvement--typically ~300%, but it can be 10x depending on the pattern and workload. The tradeoff is an increase in Go program compile time due to larger code size. For hot-path regexp patterns this tradeoff is often worth it.

# Usage

## Get the code...
```bash
go install github.com/dlclark/regexp2cg
``` 

This will download `regexp2cg` from github, compile, and install it. 

**Since `regexp2cg` is currently experimental, to use the pre-compiled regex's your projects will need a specific `code_gen` branch of the `regexp2` library**:

```bash
go get github.com/dlclark/regexp2@code_gen
```

Eventually these changes will be merged into `regexp2` proper, but since there are a large number of changes I want to roll this in slowly.

## Run it...

```bash
regexp2cg -o regexp2_codegen.go
```

By default regexp2cg will search code files (excluding tests) in the current working directory for these patterns:
`regexp2.MustCompile("Pattern", options)` and `regexp2.Compile("Pattern", options)`

If it finds any instances of this pattern in the code it will make a new file (specified via `-o`, I recommend `regexp2_codegen.go`) that contains state machines for each pattern+options combination found. During `init` it will register these state machines with `regexp2` so the MustCompile method knows to return our code generated, compiled state machine instance instead of an regexp2 interpreter.

The original code is not changed in any way. A state machine replacement is registered with `regexp2` for that pattern and options and that's it. If you want to "undo" the change, delete the new file created by `regexp2cg` and the original regexp will once again be interpreted instead of compiled.

You can also convert a single, given pattern via the command line options `-expr ["my pattern"]` and `-opt [options as int]` and by default it'll output the converted code to STDOUT.

For future runs you may want to add a [`//go:generate` comment](https://go.dev/blog/generate) with the `regexp2cg` command to one of your files.

# Notes
* `regexp2cg` uses an AST parser to find the MustCompile and Compile methods, so the code needs to be in a compiling state for the patterns to be detected.
* The pattern and options specified cannot be dynamic -- if the pattern comes from a function call or is pieced together via string concatenation (e.g. `"pattern" + var + "more pattern"`) then it will not be converted. The concept only works for fully known-at-compile-time patterns and options.
* If specified, the output file is overwritten entirely
* The directory searching for code isn't recursive, you'll need to run `regexp2cg` in each directory you want to generate pre-compiled patterns for.

# Original code
C# 11 added a compile-time regex generator: https://github.com/dotnet/runtime/tree/main/src/libraries/System.Text.RegularExpressions/gen

This is a pure Go port of that generator (v9.0 preview) using the [regexp2](githug.com/dlclark/regexp2) engine as the base.

# Not supported patterns
Per the C# implementation patterns that contain the following cannot be dynamically generated:
* Case insensitive back-references  (I may have fixed this in the port) 
* RegexNode Tree depth of 40 or larger. This makes incredibly large code files that can impact compile performance. The value 40 is inherited from the C# compiler limitations. Will need to play with Go compiler to see what a reasonable value is.

# Reporting issues
This utility is new and likely has errors. If you think you found a bug please confirm the pattern works as expected on https://regex101.com using the .NET Flavor. Please include a short Go Test that uses the pattern, options, match text, and expected results.

# Future plans ...
It might be nice to be able to exclude a pattern from the directory processing. Maybe add a command line option `--exclude "Pattern"` or you add support for an exclude comment above the `regexp2.MustCompile` line: `// regexpcg: exclude`.

