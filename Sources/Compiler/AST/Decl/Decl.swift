/// A declaration.
public protocol Decl {

  /// The source range of the declaration's textual representation, if any.
  var range: SourceRange? { get }

}
