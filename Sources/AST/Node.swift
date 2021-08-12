import Basic

/// A node in an abstract syntax tree (AST).
///
/// Val's ASTs are collections of module declarations, without any particular root. All nodes are
/// mutable and expose most of their internals publicly. This lets client code modify the AST
/// structure and/or properties. Nonetheless, the compiler assumes a number of invariants at
/// different stages, which the client code should maintain.
///
/// Do not declare new conformances to `Node` directly. Instead, use one of the umbrella protocols
/// describing a node's role (e.g., `Decl` or `Expr`).
public protocol Node: AnyObject {

  /// The source range of this node's textual representation.
  var range: SourceRange? { get }

}

extension Node {

  /// Dumps a textual representation of the node into the standard output.
  ///
  /// - Parameter context: The AST context in which the node was created.
  public func dump(context: Context) {
    var stream = StandardOutput()
    dump(context: context, to: &stream)
  }

  /// Dumps a textual representation of the node into the given stream.
  ///
  /// - Parameters:
  ///   - context: The AST context in which the node was created.
  ///   - stream: A text output stream.
  public func dump<S>(context: Context, to stream: inout S) where S: TextOutputStream {
    stream.write(NodePrinter(context: context).visit(any: self))
  }

}
