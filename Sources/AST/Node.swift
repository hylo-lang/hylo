import Basic

/// A node in an abstract syntax tree (AST).
///
/// Val's AST is a collection of module declarations. Most nodes implement one or more umbrella
/// protocols denoting their function(s) (e.g., `Decl`, `Expr`).
///
/// All nodes are mutable and expose most of their internals publicly. This lets client code modify
/// the AST structure and/or properties. Nonetheless, the compiler assumes a number of invariants
/// at different stages, which the client code should maintain.
public protocol Node: AnyObject {

  /// The source range of this node's textual representation.
  var range: SourceRange { get }

  /// Accepts the given node transformer.
  ///
  /// - Parameter visitor: A node visitor.
  func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor

}
