import Compiler

/// A test annotation.
///
/// Test annotations are parsed from single-line comments in the source. They are composed of a
/// command with, optionally, an argument and/or line offset.
///
///     //! <offset> <command> <argument>
///
/// The first occurrence of the character sequence `//!` is recognized as an annotation opener,
/// unless it is already part of a comment. The characters following an opener form an annotation
/// fragment. Two fragments are joined if they are extracted from consecutive lines in the source
/// and the opener of the second line is not prefixed by any non-space character. Joined fragments
/// form annotation bodies. For example:
///
///     //! @+2 one-annotation
///     //! and its argument on a different line
///     foo() //! another-anotation
///
/// An annotation body corresponds to a single annotation. When present, the line offset is parsed
/// as a natural number prefixed by either `@+` or `@-`. The next sequence of non-whitespace
/// characters is parsed as the command. Preceeding whitespaces are ignored; following whitespaces
/// are ignored only if they are on the same line as the command. The remainder of the annotation
/// body is parsed as the argument.
///
/// If the argument starts on a different line than the command, the number of spaces after the
/// corresponding annotation opener and before the first non-space character on the same line
/// define the indentation pattern of the argument and are not part of the argument's value. Each
/// line of an argument must be prefixed by the its indentation pattern. This prefix is not part
/// of the argument's value. For exmaple:
///
///     //! cpp
///     //! struct Foo {
///     //!   int bar;
///     //! };
///
/// The command of the annotation is `cpp` and the argument is a C++ struct declaration with no
/// indentation on the first and last line.
struct TestAnnotation {

  /// An error that occured while parsing a test annotation.
  struct ParseError: Error {

    /// The description of the error.
    var message: String

  }

  /// The line at which start this annotation.
  var line: Int

  /// The command.
  var command: String

  /// The argument, if any.
  var argument: String?

  /// Parses new annotation from `source`.
  init<S: Collection>(at line: Int, parsing source: S) throws where S.Element == Character {
    var s = source.drop(while: { $0.isWhitespace })

    // Parse the line offset, if any.
    self.line = line
    if s.starts(with: "@") {
      let offset = s.prefix(while: { !$0.isWhitespace })
      s = s.dropFirst(offset.count)
      self.line += Int(String(offset))!
    }

    // Parse the command.
    s = s.drop(while: { $0.isWhitespace })
    command = String(s.prefix(while: { !$0.isWhitespace }))
    s = s.dropFirst(command.count)

    // Skip the whitespaces after the command.
    while let head = s.first, head.isWhitespace {
      _ = s.removeFirst()
      if head.isNewline { break }
    }

    // Determine the indentation pattern.
    let indentation = s.prefix(while: { $0.isWhitespace && !$0.isNewline })

    // Parse the argument.
    let lines = s.split(separator: "\n")
    if lines.isEmpty {
      argument = nil
    } else {
      var argument = ""
      for i in 0 ..< lines.count {
        if i != 0 { argument.append("\n") }
        if !lines[i].starts(with: indentation) {
          throw ParseError(message: "inconsistent indentation")
        }
        argument.append(contentsOf: lines[i].dropFirst(indentation.count))
      }

      self.argument = argument
    }
  }

  /// Parses and returns the test annotations in `input`.
  static func parseAll(from input: SourceFile) throws -> [TestAnnotation] {
    var annotations: [TestAnnotation] = []
    var stream = input.contents.suffix(from: input.contents.startIndex)
    var line = 1

    while let head = stream.first {
      // Count new lines.
      if head.isNewline {
        _ = stream.popFirst()
        line += 1
        continue
      }

      // Look for the start of the next annotation.
      if !stream.starts(with: "//!") {
        _ = stream.popFirst()
        continue
      }

      let annotationLine = line

      // Identify the fragments of the source that relate a the text of an annotation.
      var fragments: [Substring] = []
      repeat {
        // Add the new fragment.
        let fragmentStart = stream.index(stream.startIndex, offsetBy: 3)
        if let j = stream.firstIndex(where: { $0.isNewline }) {
          fragments.append(stream[fragmentStart ... j])
          line += 1

          let k = stream.suffix(from: stream.index(after: j))
            .firstIndex(where: { $0.isNewline || !$0.isWhitespace })
          stream = stream.suffix(from: k ?? stream.endIndex)
        } else {
          // We're done if we reached the end of the file.
          fragments.append(stream[fragmentStart ..< stream.endIndex])
          break
        }
      } while stream.starts(with: "//!")

      // Parse the annotation if we could identify its text.
      if !fragments.isEmpty {
        try annotations.append(TestAnnotation(at: annotationLine, parsing: fragments.joined()))
      }
    }

    return annotations
  }

}
