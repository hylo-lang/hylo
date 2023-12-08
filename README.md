# Hylo (formerly Val)

Hylo is a programming language that leverages [mutable value semantics](https://www.jot.fm/issues/issue_2022_02/article2.pdf) and [generic programming](https://fm2gp.com) for high-level systems programming.

This repository contains the sources of the reference implementation of Hylo.
Please visit our [website](https://hylo-lang.org) to get more information about the language itself.

## Installation

This project is written in [Swift](https://swift.org) and distributed in the form of a package, built with [Swift Package Manager](https://swift.org/package-manager/).
You will need Swift 5.7 or higher to build the compiler from sources.
The repository contains [submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules), so when you clone it, be sure you've got them.

**Windows users:** 
1. although this project is **not** Unix-specific, Windows support is not guaranteed due to the instability of continuous integration (see [issue 252](https://github.com/hylo-lang/hylo/issues/252) and [issue 805](https://github.com/hylo-lang/hylo/issues/805)).
2. This repository contains symbolic links, so you'll need to [enable support](https://stackoverflow.com/questions/5917249/git-symbolic-links-in-windows/59761201#59761201) for them before checking it out.

### Prerequisites

You can skip directly to step 3 if you're doing development exclusively in a [devcontainer](#building-a-hylo-devcontainer-with-vscode).
Otherwise:

1. Install LLVM 15 or later on your system (e.g. `brew install llvm`) 
2. Have the above installation's `llvm-config` in your `PATH` (homebrew doesn't do that automatically; you'd need `export PATH="$(brew --prefix --installed llvm)/bin:$PATH"`).
3. In this project's root directory.
    1. `swift package resolve` to get the `make-pkgconfig` tool.
    2. `.build/checkouts/Swifty-LLVM/Tools/make-pkgconfig.sh llvm.pc` to generate LLVM's library description 
    3. Either
        1. `sudo mkdir -p /usr/local/lib/pkgconfig && sudo mv llvm.pc /usr/local/lib/pkgconfig/` (if you want to use Xcode)
        2. or, `export PKG_CONFIG_PATH=$PWD` in any shell where you want to work on this project
   
### Building the compiler

You may compile Hylo's compiler with the following commands:

```bash
swift build -c release
```

That command will create an executable named `hc` in `.build/release`.
That's Hylo's compiler!

### Running the tests

To test your compiler, 

```bash
swift test -c release --parallel
```

### Building a Hylo Devcontainer with VSCode

While Hylo supports Linux natively, it also provides a [Devcontainer](https://containers.dev/) specification to develop for Linux on other platforms through a Docker container. Our [Linux CI](.github/workflows/build-and-test.yml) uses this specification; this makes it possible to run Linux CI locally on other operating systems like macOS. While this specification should work for any IDE that supports devcontainers, keep in mind this team only uses VSCode. 

When opening the Hylo project in VSCode for the first time, you should be prompted to install the extension `recommendations` in `.vscode/extensions.json`. If you are not prompted, manually install the extensions by searching for the extension identifiers in the Extensions Marketplace.

Then, build the Devcontainer with the VSCode command: `> Dev Containers: Rebuild and Reopen in Container`.

Finally, open a new integrated terminal in VSCode and confirm that the shell user is `vscode`. You can run `whoami` to check this.

That integrated terminal is connected to the Devcontainer, as if by ssh.
Use the `make-pkgconfig` tool to configure LLVM's library description (see steps 3 in [prerequisites](#prerequisites)).
You can now run `swift test -c release` to build and test for Linux.

The Hylo repository files are mounted into the container, so any changes made locally (in VSCode or in other editors) will be automatically propagated into the Devcontainer. However, if you need to modify any of the files in the `.devcontainer` directory, you will need to rebuild the container with `> Dev Containers: Rebuild and Reopen in Container`.

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
- `--emit raw-ir`: Lower the typed AST into Hylo IR and output the result in a file.
- `--emit ir`: Run mandatory IR passes and output the result in a file.
- `--emit llvm`: Transpile the program to LLVM and output LLVM IR.
- `--emit intel-asm`: Output Intel assembly for all user module(s).
- `--emit binary` (default): Produce an executable.

For example, `hc --emit raw-ast -o main.json main.hylo` will parse `main.hylo`, write the untyped AST in `main.json`, and exit the pipeline.

A more detailed description of the current implementation status is available on our [roadmap page](https://www.hylo-lang.org/pages/implementation-status.html).

## Related video and audio

### Conference Talks

| Conference |  Year   |            Speaker            |                                                           Title                                                           |
| :--------: | :-----: | :---------------------------: | :-----------------------------------------------------------------------------------------------------------------------: |
|   C++Now   | 2022-05 |         Dave Abrahams         |    [Keynote: A Future of Value Semantics and Generic Programming Part 1](https://www.youtube.com/watch?v=4Ri8bly-dJs)     |
|   C++Now   | 2022-05 | Dave Abrahams & Dimi Racordon |    [Keynote: A Future of Value Semantics and Generic Programming Part 2](https://www.youtube.com/watch?v=GsxYnEAZoNI)     |
|  CppNorth  | 2022-07 |         Dave Abrahams         |  [Lightning Talk: An Object Model for Safety and Efficiency by Definition](https://www.youtube.com/watch?v=KGL02mSaplE)   |
|   CppCon   | 2022-09 |         Dave Abrahams         | [Value Semantics: Safety, Independence, Projection, & Future of Programming](https://www.youtube.com/watch?v=QthAU-t3PQ4) |
|   CppCon   | 2022-09 |         Dimi Racordon         |               [Val: A Safe Language to Interoperate with C++](https://www.youtube.com/watch?v=ws-Z8xKbP4w)                |
|   ACCU     | 2023-03 |     Lucian Radu Teodorescu    |             [Concurrency Approaches: Past, Present, and Future](https://www.youtube.com/watch?v=uSG240pJGPM)              |

### Podcasts

| Podcast | Episode |    Date    |     Guest     |                                          Title                                          |
| :-----: | :-----: | :--------: | :-----------: | :-------------------------------------------------------------------------------------: |
| CppCast |   352   | 2023-01-20 | Dimi Racordon | [Val and Mutable Value Semantics](https://cppcast.com/val-and-mutable-value-semantics)  |
|  ADSP   |   137   | 2023-07-07 |  Sean Parent  | [Sean Parent on Hylo (vs Rust)!](https://adspthepodcast.com/2023/07/07/Episode-137.html) |
|  ADSP   |   138   | 2023-07-14 |  Sean Parent  | [Sean Parent on Hylo! (Part 2)](https://adspthepodcast.com/2023/07/14/Episode-138.html) |

## Contributing

We welcome contributions to Hylo.
Please read through [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.

You can also get in touch with the community by joining our [Slack](https://join.slack.com/t/val-qs97696/shared_invite/zt-1z3dsblrq-y4qXfEE6wr6uMEJSN9uFyg) or one of our [Teams Meetings](https://teams.microsoft.com/l/meetup-join/19%3ameeting_YjZmOTJiMjUtNDZhNy00MTcxLWJjY2YtMTQ0ZGEzY2RkY2E4%40thread.v2/0?context=%7b%22Tid%22%3a%22fa7b1b5a-7b34-4387-94ae-d2c178decee1%22%2c%22Oid%22%3a%22a102b458-98db-4c5e-acad-cfa08a096ae3%22%7d) (ID: 298 158 296 273, Passcode: D2beKF) on Tuesdays and Thursdays, 12:30-1:00 Pacific time.

## License

Hylo is distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE) for details.
