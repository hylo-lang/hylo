# Val

Val is a research programming language to explore the concepts of [mutable value semantics](http://www.jot.fm/issues/issue_2022_02/article2.pdf) and [generic programming](https://www.fm2gp.com) for high-level systems programming.

This repository contains the sources of the reference implementation of Val.
Please visit our [website](https://val-lang.dev) to get more information about the language itself.

## Installation

This project is written in [Swift](https://www.swift.org) and distributed in the form of a package, built with [Swift Package Manager](https://swift.org/package-manager/).
You will need Swift 5.6 or higher to build the compiler from sources.

You may compile Val's compiler with the following commands:

```bash
swift build -c release
```

That command will create an executable named `valc` in `.build/release`.
That's Val compiler!

## Implementation status

This project is under active development; expect things to break and APIs to change.

The compiler pipeline is organized as below.
Incidentally, early stages of this pipeline are more stable than later ones.
*(Note: completion percentages are very rough estimations.)*
1. Parsing (100%)
2. Type checking (50%)
3. IR lowering (30%)
4. IR analysis and transformations (30%)
5. Machine code generation (20%)

You can select how deep the compiler should go through the pipeline with the following options:
- `--emit raw-ast`: Only parse the input files and output an untyped AST as a JSON file.
- `--typecheck`: Run the type checker on the input.
- `--emit raw-ir`: Lower the typed AST into Val IR and output the result in a file.
- `--emit ir`: Run mandatory IR passes and output the result in a file.
- `--emit cpp`: Produce a C++ source file.
- `--emit binary` (default): Produce an executable.

For example, `valc --emit raw-ast -o main.json main.val` will parse `main.val`, write the untyped AST in `main.json`, and exit the pipeline.

A more detailed description of the current implementation status is available on our [roadmap page](https://www.val-lang.dev/pages/implementation-status.html).

## Contributing

We welcome contributions to Val.
Please read through [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.

## License

Val is distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE) for details.
