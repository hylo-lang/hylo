# Code Formatting

This document describes some guidelines to write code for this project.
The purpose of these conventions is to keep the style of the codebase consistent.
Contributors are kindly asked to follow these guidelines and format their code accordingly before submitting new pull requests.

As all set of conventions, many of the rules described in this document are arbitrary and based on personal preferences or simply follow common wisdom.
For code written in Swift, most of the syntactic conventions are simply inherited from the style currently applied by `swift-format` configured with `.swift-format.json` at the root of this repository.
Other rules are heavily inspired by [Google's Swift Style Guide](https://google.github.io/swift/).

## `swift-format`

We currently use the version `swift-format` tagged with the commit [0bc2f03](https://github.com/apple/swift-format/commit/0bc2f0381c72d66a949254af22208a81377cf717).
The best way to get `swift-format` is to obtain it from its [official repository](https://github.com/apple/swift-format) and compile it manually.

**Warning:** please make sure to get the version specified above.
Different versions of `swift-format` may produce different results.

### Running `swift-format`

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

Please note that it's possible some of your code may still require some hand formatting after running the above command.

### Configuring your IDE

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

## Source files

### File encoding

All source files are encoded in UTF-8.

### File names

All Swift source files end with the extension `.swift` and all Val source files end with the extension `.val`.

In general, a file is named after the main entity that it defines.
A file that extends an existing type with a protocol (Swift) or trait (Val) conformance is named with a combination of the type name and the protocol or trait name, joined with a plus (`+`) sign.
For more complex situations, exercise your best judgment.

For example:
- A Swift file defining a type named `MyType` is named `MyType.swift`.
- A Swift file defining how `MyType` conforms to `Equatable` is named `MyType+Equatable.swift`.

Avoid defining multiple types, protocols, or traits in the same file unless they are scoped by a main entity or meant to be used only in that file.

### Whitespace characters and indentation

Aside from the line terminator, the Unicode horizontal space character (`U+0020`) is the only whitespace character that appears anywhere in a source file.
The implications are:

- All other whitespace characters in string and character literals are represented by their corresponding escape sequence.
- Tab characters are not used for indentation.

Indentation is set at two horizontal spaces.

## General formatting

### Column limit

The column limit is 100 characters.
Any line that would exceed this limit must be line-wrapped.

## Formatting type declarations

Exactly one empty line is added before the first member of a type, protocol, or trait declaration.
Exactly one empty line is added after each member of a type, protocol, or trait declaration.
For example:

```swift
struct Foo {

  let bar: Int
  
  let ham: Int

}
```

Nested types are declared first, then static members, then non-static members.
Properties are declared before other members.
Initializers are declared before deinitializers and member functions.
Deinitializers are declared before member functions.
For example:

```swift
struct Foo {

  struct Index {

    let value: Int

  }

  static let default = Foo(bar: 0)

  var bar: Int

  init(bar: Int) {
    self.bar = bar
  }

  mutating func hammify() {
    self.bar += 1
  }

}
```