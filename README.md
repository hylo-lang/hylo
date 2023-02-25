# Val

Val is a research programming language to explore the concepts of [mutable value semantics](http://jot.fm/issues/issue_2022_02/article2.pdf) and [generic programming](https://fm2gp.com) for high-level systems programming.

This repository contains the sources of the reference implementation of Val.
Please visit our [website](https://val-lang.dev) to get more information about the language itself.

## Installation

This project is written in [Swift](https://swift.org) and distributed in the form of a package, built with [Swift Package Manager](https://swift.org/package-manager/).
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
- `--emit binary` (default): Produce an executable 
  - Note: by default, C++ files will be compiled with `Clang`. Use `--cc {CXX compiler}` to use another compiler.
  - Note: You can specify parameters for the CXX compiler to use. like `--cc-flags O3`.
    - Note: Don't add an extraneous `-`, please use `O3` instead of `-O3`. 
    - You can also add more than one such parameter.
    - If you want to specify output file name or something like that, please use the Val command line argument instead of `--cc-flags`.
  - Note to MSVC users: be sure to use Visual Studio Developer Command Prompt or Visual Studio Developer PowerShell.

For example, `valc --emit raw-ast -o main.json main.val` will parse `main.val`, write the untyped AST in `main.json`, and exit the pipeline.

A more detailed description of the current implementation status is available on our [roadmap page](https://www.val-lang.dev/pages/implementation-status.html).

## Related video and audio

[Lightning Talk: An Object Model for Safety and Efficiency by Definition - Dave Abrahams CppNorth 22](https://www.youtube.com/watch?v=KGL02mSaplE)

[Keynote: A Future of Value Semantics and Generic Programming Part 1 - Dave Abrahams - CppNow 2022](https://www.youtube.com/watch?v=4Ri8bly-dJs)

[Keynote: A Future of Value Semantics and Generic Programming Part 2 - Dave Abrahams & Dimi Racordon - CppNow 2022](https://www.youtube.com/watch?v=GsxYnEAZoNI)

[Value Semantics: Safety, Independence, Projection, & Future of Programming - Dave Abrahams CppCon 22](https://www.youtube.com/watch?v=QthAU-t3PQ4)

[Val and Mutable Value Semantics - Dimi Racordon](https://cppcast.com/val-and-mutable-value-semantics) 

## Contributing

We welcome contributions to Val.
Please read through [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.

## License

Val is distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE) for details.
