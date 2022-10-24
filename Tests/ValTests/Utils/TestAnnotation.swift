import Compiler

/// A test annotation in a source file.
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

  /// The line location of this annotation.
  var location: LineLocation

  /// The command.
  var command: String

  /// The argument, if any.
  var argument: String?

  /// Parses a new annotation from `body`.
  ///
  /// - Parameters:
  ///   - location: The line location of the annotation.
  ///   - body: A collection of characters representing an annotation body.
  init<S: Collection>(at location: LineLocation, parsing body: S)
    where S.Element == Character
  {
    self.location = location

    var s = body.drop(while: { $0.isWhitespace })

    // Parse the line offset, if any.
    if s.starts(with: "@") {
      let offset = s.prefix(while: { !$0.isWhitespace })
      s = s.dropFirst(offset.count)
      self.location.line += Int(String(offset))!
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
        argument.append(contentsOf: lines[i].dropFirst(indentation.count))
      }

      self.argument = argument
    }
  }

  /// Parses and returns the test annotations in `source`.
  static func parseAll(from source: SourceFile) -> [TestAnnotation] {
    /// The annotations parsed from the input.
    var annotations: [TestAnnotation] = []
    /// The input stream.
    let stream = source.contents
    /// The current position in the stream.
    var index = stream.startIndex
    /// The line at the current position.
    var line = 1
    /// The number of opened block comments at the current position.
    var openedBlockComments = 0

    while index < stream.endIndex {
      // Count new lines.
      if stream[index].isNewline {
        index = stream.index(after: index)
        line += 1
        continue
      }

      // Skip block comments.
      if stream[index...].starts(with: "/*") {
        openedBlockComments += 1
        index = stream.index(index, offsetBy: 2)
        continue
      }

      if stream[index...].starts(with: "*/") {
        openedBlockComments = max(0, openedBlockComments - 1)
        index = stream.index(index, offsetBy: 2)
        continue
      }

      if openedBlockComments > 0 {
        index = stream.index(after: index)
        continue
      }

      // Look for the start of the next annotation, ignoring comment blocks.
      if stream[index...].starts(with: "//") {
        // We recognized '//'; now check if the next character is '!' or skip the line.
        // If we're parsing a single-line comment, skip the entire line.
        var i = stream.index(index, offsetBy: 2)
        if i == stream.endIndex || stream[i] != "!" {
          while (i < stream.endIndex) && !stream[i].isNewline {
            i = stream.index(after: i)
          }
          index = i
          continue
        }
      } else {
        // Advance to the next character.
        index = stream.index(after: index)
        continue
      }

      let annotationLine = line

      // Identify the fragments of the source that relate a the text of an annotation.
      var fragments: [Substring] = []
      repeat {
        // Add the new fragment.
        let fragmentStart = stream.index(index, offsetBy: 3)
        if let fragmentEnd = stream[fragmentStart...].firstIndex(where: { $0.isNewline }) {
          fragments.append(stream[fragmentStart ... fragmentEnd])
          index = stream.index(after: fragmentEnd)
          line += 1
        } else {
          // We're done if we reached the end of the file.
          fragments.append(stream[fragmentStart ..< stream.endIndex])
          break
        }
      } while stream[index...].starts(with: "//!")

      // Parse the annotation if we could identify its text.
      if !fragments.isEmpty {
        let location = LineLocation(url: source.url, line: annotationLine)
        annotations.append(TestAnnotation(at: location, parsing: fragments.joined()))
      }
    }

    return annotations
  }

}
