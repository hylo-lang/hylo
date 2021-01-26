import Basic

/// A module.
///
/// A module, or compilation unit, is a collection of types, variables and function declarations
/// declared in one or several source files.
public final class Module: DeclSpace {

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

  public var parentDeclSpace: DeclSpace? {
    get { nil }
    set { precondition(newValue == nil) }
  }

  /// Returns the extensions of the given type declaration.
  public func extensions(of decl: AbstractNominalTypeDecl) -> [TypeExtDecl] {
    // Build the qualified name of `decl`.
    var names = [decl.name]
    var space = decl.parentDeclSpace
    while space !== self {
      guard let s = space as? AbstractNominalTypeDecl else {
        // Types nested in a "local" declaration space (e.g., a function) cannot be extended.
        return []
      }
      names.append(s.name)
      space = s.parentDeclSpace
    }

    // Search for extensions defined for the computed qualified name.
    var matches: [TypeExtDecl] = []
    stmt:for case let ext as TypeExtDecl in statements {
      let components = ext.extendedIdent.components
      guard components.count == names.count else { continue }
      for i in 0 ..< components.count {
        guard components[i].name == names[names.count - i - 1] else { continue stmt }
      }
      matches.append(ext)
    }

    return matches
  }

}

extension Module: TypeDecl {

  public var name: String { id }

  public var fullyQualName: [String] { [name] }

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
