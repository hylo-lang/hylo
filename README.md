# Val

Val is a research programming language to explore the concepts of [mutable value semantics](https://www.jot.fm/issues/issue_2022_02/article2.pdf) and [generic programming](https://fm2gp.com) for high-level systems programming.

This repository contains the sources of the reference implementation of Val.
Please visit our [website](https://val-lang.dev) to get more information about the language itself.

## Installation

This project is written in [Swift](https://swift.org) and distributed in the form of a package, built with [Swift Package Manager](https://swift.org/package-manager/).
You will need Swift 5.7 or higher to build the compiler from sources.

*Note to Windows users: although this project is **not** Unix-specific, Windows support is not guaranteed due to the instability of continuous integration (see https://github.com/val-lang/val/issues/252).*

### Prerequisites

You can skip this step if you're doing development exlusively in a [devcontainer](#building-val-devcontainer-with-vscode).  Otherwise:

1. Install LLVM 15 or later on your system. Then, in this project's root directory.
2. `swift package resolve` to get the tool for step 3.
3. `.build/checkouts/Swifty-LLVM/Tools/make-pkgconfig.sh llvm.pc` to generate LLVM's library description.
4. Either
   a. `sudo mv llvm.pc /usr/local/lib/pkgconfig` (if you want to use Xcode), or
   b. `export PKG_CONFIG_PATH=$PWD`
   
### Building the compiler

You may compile Val's compiler with the following commands:

```bash
swift build -c release
```

That command will create an executable named `valc` in `.build/release`.
That's Val compiler!

### Running the tests

To test your compiler, 

```bash
swift test -c release --parallel
```

### Building Val Devcontainer with VSCode

While Val supports Linux natively, it also provides a [Devcontainer](https://containers.dev/) specification to develop for Linux on other platforms through a Docker container. Our [Linux CI](.github/workflows/build-and-test.yml) uses this specification; this makes it possible to run Linux CI locally on other operating systems like macOS. While this specification should work for any IDE that supports devcontainers, keep in mind this team only uses VSCode. 

When opening the Val project in VSCode for the first time, you should be prompted to install the extension `recommendations` in `.vscode/extensions.json`. If you are not prompted, manually install the extensions by searching for the extension identifiers in the Extensions Marketplace.

Then, build the Devcontainer with the VSCode command: `> Dev Containers: Rebuild and Reopen in Container`.

Finally, open a new integrated terminal in VSCode and confirm that the shell user is `vscode`. You can run `whoami` to check this.

That integrated terminal is connected to the Devcontainer, as if by ssh. You can now run `swift test -c release` to build and test for Linux. 

The Val repository files are mounted into the container, so any changes made locally (in VSCode or in other editors) will be automatically propagated into the Devcontainer. However, if you need to modifiy any of the files in the `.devcontainer` directory, you will need to rebuild the container with `> Dev Containers: Rebuild and Reopen in Container`.

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
- `--emit llvm`: Transpile the program to LLVM and output LLVM IR.
- `--emit binary` (default): Produce an executable.

For example, `valc --emit raw-ast -o main.json main.val` will parse `main.val`, write the untyped AST in `main.json`, and exit the pipeline.

A more detailed description of the current implementation status is available on our [roadmap page](https://www.val-lang.dev/pages/implementation-status.html).

## Related video and audio

[Lightning Talk: An Object Model for Safety and Efficiency by Definition - Dave Abrahams CppNorth 22](https://www.youtube.com/watch?v=KGL02mSaplE)

[Keynote: A Future of Value Semantics and Generic Programming Part 1 - Dave Abrahams - CppNow 2022](https://www.youtube.com/watch?v=4Ri8bly-dJs)

[Keynote: A Future of Value Semantics and Generic Programming Part 2 - Dave Abrahams & Dimi Racordon - CppNow 2022](https://www.youtube.com/watch?v=GsxYnEAZoNI)

[Value Semantics: Safety, Independence, Projection, & Future of Programming - Dave Abrahams CppCon 22](https://www.youtube.com/watch?v=QthAU-t3PQ4)

[Val and Mutable Value Semantics - Dimi Racordon](https://cppcast.com/val-and-mutable-value-semantics) 

[Val: A Safe Language to Interoperate with C++ - Dimi Racordon - CppCon 2022](https://www.youtube.com/watch?v=ws-Z8xKbP4w)

## Contributing

We welcome contributions to Val.
Please read through [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.

## License

Val is distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE) for details.
