# Original code
C# 11 added a compile-time regex generator.

https://github.com/dotnet/runtime/tree/main/src/libraries/System.Text.RegularExpressions/gen


# Not supported cases
Patterns that contain the following cannot be dynamically generated:
* Case insensitive back-references
* RegexNode Tree depth of 40 or larger. This makes incredibly large code files that can impact compile performance. The value 40 is inherited from the C# compiler limitations. Will need to play with Go compiler to see what a reasonable value is.

# Usage
regexp2cg will search your code for this pattern:
`regexp2.MustCompile("Pattern", options)`

If it finds any instances of this code pattern it will make a new file, `regexp2_codegen.go` that contains state machines for each pattern+options combination found. During `init` it will register these state machines with `regexp2` so the MustCompile method knows to return our code generated, compiled state machine instance instead of an regexp2 interpreter.

To exclude a pattern from this process you can add a command line option `--exclude "Pattern"` or you can add this comment above the `regexp2.MustCompile` line: `// regexpcg: exclude`.

