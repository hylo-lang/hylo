import Basic

/// A collection of top-level declarations, abstracting over the concept of a "file".
///
/// Each file unit delimits a declaration space, which serves as a boundary for the visibility of
/// imported symbols and file protected declarations. Howrver, its does not function the same way
/// as other kinds of declaration spaces with respect to name lookup. Top-level entities declared
/// in a unit are merged with those from all of its siblings and should be looked up from the
/// enclosing module.
public class FileUnit: DeclSpace {

  fileprivate init() {}

  /// The top-level declarations of the unit.
  public var decls: [Decl] = []

  /// The module in which the unit resides.
  public var parentDeclSpace: DeclSpace?

  /// Overrides the default lookup mechanism.
  public func lookup(qualified name: String) -> LookupResult {
    return LookupResult()
  }

}

/// A unit containing the compiler's built-in declarations.
public final class BuiltinUnit: FileUnit {

  public override init() {}

}

/// A source file containing Val code.
public final class SourceUnit: FileUnit {

  public init(source: SourceFile) {
    self.source = source
  }

  /// A handle to the contents of the file in the context's file manager.
  public let source: SourceFile

}
