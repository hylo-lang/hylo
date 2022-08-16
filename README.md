# Val

Val is a research programming language to explore the concepts of [mutable value semantics](http://www.jot.fm/issues/issue_2022_02/article2.pdf) and [generic programming](https://www.fm2gp.com) for high-level systems programming.

This repository contains the sources of the reference implementation of Val.
Please visit our [website](https://val-lang.dev) to get more information about the language itself.

## Supported platforms

The compiler is currently developed for Unix-like operating systems.
Nonetheless, it should be compilable on Windows as well.
Please, fill in an issue if you are experiencing problems on Windows.
 
## Installation

| Dependency | Version |
|------------|---------|
| Swift      | >5.6    |
| LLVM       | =11.0   |


This project is written in [Swift](https://www.swift.org) and distributed in the form of a package, built with [Swift Package Manager](https://swift.org/package-manager/).

You will need Swift 5.6 or higher to build the compiler from source.

Our implementation depends on LLVM 11.0, via [LLVMSwift](https://github.com/llvm-swift/LLVMSwift).
### macOS

We recommend using [Homebrew](https://brew.sh/) to install LLVM.

```bash
$ brew install llvm@11
$ PATH="$PATH:`brew --prefix llvm@11`/bin"
# Or, set PATH in your ~/.zshrc, ~/.bashrc, etc.
```

Then, install Val's dependencies:

```bash
$ swift package resolve
```

Next, you need to create a `pkgconfig` file for your specific installation. Fortunately, the maintainers of [LLVMSwift](https://github.com/llvm-swift/LLVMSwift) were kind enough to provide a script:

```
swift .build/checkouts/LLVMSwift/utils/make-pkgconfig.swift
```

### Ubuntu

Installation on Ubuntu requires manually installing Swift and its dependencies. See [our Github Actions Workflow](.github/workflows/build-and-test.yml) for the most up-to-date list of packages, as verified by our Continuous Integration.

Note: the packages listed in the Ubuntu installation may be redundant, as packages like `git` are installed on Github Actions virtual environments by default. These have been left in place for consistency with [swift's installation instructions](https://www.swift.org/download/).
### Windows

Windows is not officially supported yet.
## Building

Once LLVM is installed and configured, you may compile Val's compiler with the following command:

```bash
swift build -c release
```

That command will create an executable named `valc` in `.build/release`.
That's the Val compiler!

## Testing

To test Val, we recommend building a debug configuration. Executing the tests is as simple as invoking `swift test`:

```
$ swift build -c debug
$ swift test -c debug
```

## Contributing

We welcome contributions to Val.
Please read through [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.

## License

Val is distributed under the terms of the Apache-2.0 license.
See [LICENSE](LICENSE) for details.