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
/// A 2-dimensional vector.
public struct Vector2 {

  /// The length of the vector along the x-axis.
  public var x: Double = 0.0
  
  /// The length of the vector along the y-axis.
  public var y: Double = 0.0

}
```

Prefer declaring core type aliases first, followed by stored properties or enum cases, followed by core initializers, followed other members.
Prefer declaring conformances separately unless they are synthesizable (e.g., `Equatable`).
For example:

```swift
/// A 2-dimensional matrix.
struct Matrix2: Equatable {

  /// A column in a 2-dimensional matrix.
  public typealias Column = Vector2

  /// The elements of the matrix.
  private var elements: (Column, Column)

  // Creates a matrix filled with zeros.
  public init() { ... }

  /// Creates scaling matrix with given scale `factors`.
  public init(scalingBy factors: Vector2) { ... }

  /// The determinant of `self`.
  public var determinant: Double { ... }

  /// Returns a transposed copy of `self`.
  public func transposed() -> Self { ... }

  /// The identity matrix.
  public static var identity: Matrix2 = ...

}

extension Matrix2: CustomStringConvertible {

  public var description: String { ... }

}
```

Conformances requiring custom implementations should be declared separately unless it would break encapsulation.
For example:

```swift
/// The figure formed by two rays sharing a common endpoint.
struct Angle {

  /// The value of the angle in radians.
  public var radians: Double

  /// Creates an instance with given value in `radians`.
  public init(radians) {
    self.radians = radians
  }

  /// `self` wrapped within the interval `0 ..< 2π`.
  public var wrapped: Double {
    let x = radians.truncatingRemainder(dividingBy: 2 * .pi)
    return x < 0 ? Angle(radians: x + 360) : Angle(radians: x)
  }

}

extension Angle: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    lhs.wrapped.radians == rhs.wrapped.radians
  }

}

extension Angle: Hashable {

  public func hash(into hasher: inout Hasher) { hasher.combine(wrapped) }

}
```

Use `self` when referring to stored properties in an initializer, even if the names of these properties doesn't clash with any of the initializer's parameters.
For example:

```swift
/// A figure whose boundary consists of points equidistant from a fixed center.
public struct Circle {

  /// The center of the circle.
  var center: Vector2

  /// The distance from `center` to any point in the circle's boundary.
  var radius: Double

  /// Creates a vector with given `radius` centered at the origin.
  init(radius: Double) {
    self.center = .init()
    self.radious = radius
  }

}
```

In Swift, declare operators as static members rather than free functions.
For example:

```swift
/// A 2-dimensional vector.
extension Vector2 {

  /// Returns the component-wise addition of `l` with `r`.
  static func + (l: Self, r: Self) -> Self { ... }

}
```

## Formatting function declarations

Omit needless return statements.

In Swift, use parentheses to declare closure parameters and only use `$0` for simple closures that fit a single line.
For example:

```swift
/// Returns `polygon` translated by `d`.
func translate(polygon: [Vector2], by d: Vector2) -> [Vector2] {
  polygon.map({ $0 + d })
}

/// Returns the edges of `polygon` clipped in `area`.
func clamp(_ polygon: [Vector2], to area: Circle) -> [Vector2] {
  polygon.map { (p) in
    if area.contains(p) {
      return p
    } else {
      let v = (p - area.center).magnitude
      return area.center + v * area.radius
    }
  }
}
```
