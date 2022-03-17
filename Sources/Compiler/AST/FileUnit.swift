import Foundation

/// A collection of top-level declarations, abstracting over the concept of a "file".
///
/// Each file unit delimits a declaration space, which serves as a boundary for the visibility of
/// imported symbols and file protected declarations. Howrver, its does not function the same way
/// as other kinds of declaration spaces with respect to name lookup. Top-level entities declared
/// in a unit are merged with those from all of its siblings and should be looked up from the
/// enclosing module.
public final class FileUnit: IterableDeclSpace {

  /// The module in which the unit resides.
  public var parentDeclSpace: DeclSpace?

  /// The top-level declarations of the unit.
  public var decls: [Decl] = []

  /// The source file containing this unit's contents, or `nil` if this is the built-in module.
  public let source: SourceFile?

  public init(source: SourceFile?) {
    self.source = source
  }

  /// Overrides the default lookup mechanism.
  public func lookup(qualified name: String) -> LookupResult {
    return LookupResult()
  }

}
