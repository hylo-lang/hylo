# Command line interface

## Usage

```bash
hc [--modules] [--import-builtin] [--no-std] [--typecheck] [--trace-inference <file:line>] [--emit <output-type>] [-o <file>] [--verbose] [<inputs> ...]
```

Compiles `<inputs>` and produces an output `<file>` of the specified `<output-type>`.

## Options

### `--modules`

**Not implemented**

Compile `<inputs>` as separate modules.

**Example:**
```swift
// In `Sources/Hello/Hello.hylo`
import Greetings
public fun main() {
  greet("World")
}
```
```swift
// In `Sources/Greetings/Greetings.hylo`
public fun greet(_ name: String) {
  print("Hello, ${name}!")
}
```

```bash
hc --modules Sources/Hello Sources/Greet -o hello
```

Running this command will:
1. Build the source files in the `Sources/Hello` directory into a module named `Hello`
2. Build the source files in the `Sources/Greetings` directory into a module named `Greetings`
3. Link the `Hello` and `Greetings` modules as an executable named `hello`.

### `--import-builtin`

Import the built-in module.  Allows using Hylo's built-in types and functions (e.g., `Builtin.terminate()`) in Hylo code.

### `--no-std`

Do not include the standard library module.

### `--typecheck`

Type-check the input file(s).  Exits the compilation pipeline after type-checking and does not produce an output file.

### `--trace-inference <file:line>`

Enable tracing of type inference requests at the given line.

**Example:**

```bash
hc --typecheck --trace-inference main.hylo:16 main.hylo
```

Running this command will show a trace of the type constraint solver for all root expressions at line 16 of `main.hylo`.

### `--emit <output-type>`

Emit the specified output type (default: `binary`).  Each type represents a stage of the compilation pipeline.

| `<output-type>` | Description |
|--|--|
| `raw-ast`   | AST before type checking |
| `raw-ir`    | Hylo IR before mandatory transformations |
| `ir`        | Hylo IR |
| `llvm`      | LLVM IR |
| `intel-asm` | Intel Assembly |
| `binary`    | Executable binary |

**Example:**
```bash
hc --emit raw-ast -o main.json main.hylo
```
Running this command will parse `main.hylo`, write the untyped AST in `main.json`, and exit the compilation pipeline.

### `-o <file>`

Write output to `<file>`.

### `-v`, `--verbose`

Use verbose output.
