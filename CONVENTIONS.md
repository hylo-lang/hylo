# Project Conventions

This document describes some of the conventions and used in this project.
It is intended for developers who wish to contribute to the Hylo compiler or simply read its sources.

## Code formatting and conventions

The Swift source code in this repository follows the [Swift API Design guidelines](https://www.swift.org/documentation/api-design-guidelines/).
In addition, we maintain a set of code formatting conventions checked with `swift-format`.
There is a CI job that checks the formatting; this job will not auto-correct the formatting, it will just ensure that the formatting is correct.

Contributors are asked to format all the code they submit to the repository.
We use `swift-format` tagged with the commit [0bc2f03](https://github.com/apple/swift-format/commit/0bc2f0381c72d66a949254af22208a81377cf717).
The best way to get this tool is to obtain it from its [official repository](https://github.com/apple/swift-format) and compile it manually.

**Warning:** please make sure to get the version specified above.
Different versions of `swift-format` may produce different results.

The script `Tools/run-swift-format.sh` can be used to check and format code.
It will search for `swift-format` in your path `PATH` or at `/usr/local/bin/swift-format`.

To check the style for the Swift code in the repository, run the following command from the root directory:

```bash
Tools/run-swift-format.sh lint
```

To automatically format the code, run the following command:

```bash
Tools/run-swift-format.sh fix
```

Please note that `swift-format` might not be able to fix all issues raised by the `lint` command.
Hence, you may have to manually format your code to comply, even after using the `fix` command.

### Configuring your IDE with `swift-format``

#### VisualStudio Code

The simplest way to configure `swift-format` with VSCode is to install the [`apple-swift-format`](https://marketplace.visualstudio.com/items?itemName=vknabel.vscode-apple-swift-format) extension.
Once it's installed and configured, you can run the `Format Document` or `Format Selection` commands to format your code.
The default keyboard shortcut for running the formatter on the current document is `Alt+Shift+F`.

#### XCode

As of this writing, there isn't any XCode extension to integrate `swift-format`.
One workaround is to configure formatting as a post-build action in your project:
- Go to `Product` → `Scheme` → `Edit Scheme...` in the XCode menu
- Under `Build`, select `Post-build actions`
- Click on the `+` button and select `New Run Script Action`
- In the box corresponding to the script, paste the following:

```bash
${WORKSPACE_PATH}/../../../Tools/run-swift-format.sh fix
```

### File names

All Swift source files end with the extension `.swift` and all Hylo source files end with the extension `.hylo`.

In general, a file is named after the main entity that it defines.
A file that extends an existing type with a protocol (Swift) or trait (Hylo) conformance is named with a combination of the type name and the protocol or trait name, joined with a plus (`+`) sign.
For more complex situations, exercise your best judgment.

For example:
- A Swift file defining a type named `MyType` is named `MyType.swift`.
- A Swift file defining how `MyType` conforms to `Equatable` is named `MyType+Equatable.swift`.

Avoid defining multiple types, protocols, or traits in the same file unless they are scoped by a main entity or meant to be used only in that file.
Usually, conformances are small and are defined in the same file as the type to which they apply.

### Traps

#### Unimplemented features

It may be sometimes beneficial to stub the API of a feature before it is implemented.
In those cases,  file an issue with a reproducer for the missing feature and call `Utils.UNIMPLEMENTED("description of feature #issue-number")` to mark the unimplemented code path.
Many uses of `UNIMPLEMENTED()` predate this policy and don't include the string argument; if you hit one, please file the issue and submit a PR that adds the appropriate string.

#### Unreachable code paths

Call `unreachable()` (defined in `Utils`) on any code path that indicates a bug in the Hylo compiler if ever reached. 

## Reporting bugs

Please attach a commit hash to issues reporting a bug.
Please try to include a minimal Hylo reproducer.
