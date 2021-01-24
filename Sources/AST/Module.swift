import Basic

/// A module.
///
/// A module, or compilation unit, is a collection of types, variables and function declarations
/// declared in one or several source files.
public final class Module: DeclScope {

  public init(id: String, context: Context) {
    self.id = id

    type = context.unresolvedType
    type = ModuleType(context: context, module: self).kind
  }

  /// The module's identifier (typically its name).
  public let id: String

  /// The top-level statements of the module.
  public var statements: [Node] = []

  public private(set) var type: ValType

  public var parentDeclScope: DeclScope? {
    get { nil }
    set { precondition(newValue == nil) }
  }

}

extension Module: TypeDecl {

  public var name: String { id }

  public var range: SourceRange { .invalid }

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

extension Module: CustomStringConvertible {

  public var description: String {
    return "Module(\(id))"
  }

}
