# Project Conventions

This document describes some of the conventions and used in this project.
It is intended for developers who wish to contribute to the Hylo compiler or simply read its sources.

## Code formatting and conventions

The Swift source code in this repository follows the [Swift API Design guidelines](https://www.swift.org/documentation/api-design-guidelines/).
In addition, we maintain a set of code formatting conventions checked with `swift-format`.
There is a CI job that checks the formatting; this job will not auto-correct the formatting, it will just ensure that the formatting is correct.
See [Docs/CodeFormatting.md](Docs/CodeFormatting.md) for information about setting up your environment.

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
In those cases,  file an issue with a reproducer for the missing feature and call `UNIMPLEMENTED("description of feature #issue-number")` (defined in `Utils`) to mark the unimplemented code path.
Many uses of `UNIMPLEMENTED()` predate this policy and don't include the string argument; if you hit one, please file the issue and submit a PR that adds the appropriate string.

#### Unreachable code paths

Call `unreachable()` (defined in `Utils`) on any code path that indicates a bug in the Hylo compiler if ever reached. 

## Reporting bugs

Please attach a commit hash to issues reporting a bug.
Please try to include a minimal Hylo reproducer.
