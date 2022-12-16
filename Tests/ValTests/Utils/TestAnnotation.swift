import FrontEnd
import Core

/// A test annotation in a source file.
///
/// Test annotations are parsed from comments in the source. They are composed of a command with,
/// optionally, an argument and/or line offset.
///
///     //! <offset> <command> <argument>
///
/// The character sequence `//!` is recognized as an annotation line opener, unless it is already
/// part of a comment. The characters between `//!` and the next new line form an annotation body.
/// For example:
///
///     //! @+1 line-annotation a b c
///     foo()
///
/// The line offset of the annotation is `+1`, its command is `line-annotation` and its argument is
/// the string "`a b c`" (without quotes).
///
/// The character sequence `/*!` is recognized as an annotation block opener, unless it is already
/// part of a comment. The block terminates immediately after a matching multiline comment closing
/// delimiter (i.e., `*/`). The sequence of characters that starts immediately after an annotation
/// block opener and the corresponding comment closing delimiter form an annotation body.
///
/// An annotation body corresponds to a single annotation. When present, the line offset is parsed
/// as a natural number prefixed by either `@+` or `@-`. The next sequence of non-whitespace
/// characters is parsed as the command. Preceeding whitespaces are ignored; following whitespaces
/// are ignored only if they are on the same line as the command. The remainder of the annotation
/// body is parsed as the argument.
///
/// If the argument starts on a different line than the command, the spaces before the first
/// non-space first non-space character of the argument defines the indentation pattern of the
/// argument and are not part of its value. Each line of an argument must be prefixed by the its
/// indentation pattern.
///
///     /*! cpp
///     struct Foo {
///       int bar;
///     };
///     */
///
/// The command of the annotation is `cpp` and the argument is a C++ struct declaration with no
/// indentation on the first and last line, and two spaces before the field declaration..
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
  where S.Element == Character {
    self.location = location

    var s = body.drop(while: { $0.isWhitespace })

    // Parse the line offset, if any.
    if s.starts(with: "@") {
      let offset = s.dropFirst().prefix(while: { !$0.isWhitespace })
      s = s.dropFirst(offset.count + 1)
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
    /// The index after the annotation block being parsed, if any.
    var indexAfterAnnotationBlockOpener: String.Index? = nil

    while index < stream.endIndex {
      // Count new lines.
      if stream[index].isNewline {
        index = stream.index(after: index)
        line += 1
        continue
      }

      // Look for the opening delimiter of a multiline comment.
      if stream[index...].starts(with: "/*") {
        openedBlockComments += 1
        index = stream.index(index, offsetBy: 2)

        // We recognized '/*'; ignore if we're already parsing a block comment.
        if openedBlockComments > 1 {
          continue
        }

        // Otherwise, check if the next character is `!` or interpret as a regular block comment.
        assert(indexAfterAnnotationBlockOpener == nil)
        if (index != stream.endIndex) && (stream[index] == "!") {
          indexAfterAnnotationBlockOpener = stream.index(after: index)
        }
        continue
      }

      // Look for the closing delimiter of a multiline comment.
      if stream[index...].starts(with: "*/") {
        switch openedBlockComments {
        case 0:
          break

        case 1:
          openedBlockComments = 0
          if let start = indexAfterAnnotationBlockOpener {
            annotations.append(
              TestAnnotation(
                at: LineLocation(url: source.url, line: line),
                parsing: stream[start ..< index]))
            indexAfterAnnotationBlockOpener = nil
          }

        default:
          openedBlockComments -= 1
        }

        index = stream.index(index, offsetBy: 2)
        continue
      }

      // Skip the characters in block comments.
      if openedBlockComments > 0 {
        index = stream.index(after: index)
        continue
      }

      // Look for single line comments.
      if stream[index...].starts(with: "//") {
        index = stream.index(index, offsetBy: 2)
        let start: String.Index? =
          (index != stream.endIndex) && (stream[index] == "!")
          ? stream.index(after: index)
          : nil

        while (index < stream.endIndex) && !stream[index].isNewline {
          index = stream.index(after: index)
        }

        if let start = start {
          annotations.append(
            TestAnnotation(
              at: LineLocation(url: source.url, line: line),
              parsing: stream[start ..< index]))
        }

        continue
      }

      // Advance to the next character.
      index = stream.index(after: index)
    }

    return annotations
  }

}
