import Basic

/// A module.
///
/// A module, or compilation unit, is a collection of types, variables and function declarations
/// declared in one or several source files.
public final class Module: IterableDeclSpace {

  public init(id: String, context: Context) {
    self.id = id

    type = context.unresolvedType
    type = ModuleType(context: context, module: self).kind
  }

  /// The module's identifier (typically its name).
  public let id: String

  /// The top-level declarations of the module.
  public var decls: [Decl] = []

  /// The type of the module.
  ///
  /// This is set directly within the module's constructor.
  public private(set) var type: ValType

  public var parentDeclSpace: DeclSpace? {
    get { nil }
    set { precondition(newValue == nil) }
  }

  /// Returns the extensions of the given type declaration.
  public func extensions(of decl: NominalTypeDecl) -> [TypeExtDecl] {
    var matches: [TypeExtDecl] = []

    // Loop through all extensions in the module, (partially) binding them if necessary.
    stmt:for case let ext as TypeExtDecl in decls {
      switch ext.state {
      case .bound(let d) where d === decl:
        // Simplest case. The extension was already bound to `decl`!
        matches.append(ext)

      case .parsed:
        // The extension was not bound yet, so we need to realize to resolve its identifier.
        if ext.extendedIdent is UnqualTypeRepr {
          if ext.extendedDecl === decl {
            matches.append(ext)
          }
          continue stmt
        }

        // If the identifier is a `CompoundTypeRepr`, we can't realize the full signature at once,
        // as we may risk to trigger infinite recursion of name lookups if the signature points
        // within `decl`. Instead, we must realize it lazily and abort if we detect that we're
        // about to start a lookup from `decl`.
        let compound = ext.extendedIdent as! CompoundTypeRepr
        let baseType = compound.components[0].realize(unqualifiedFrom: self)
        guard var baseDecl = (baseType as? NominalType)?.decl else { continue stmt }

        for i in 1 ..< compound.components.count {
          // The signature points within `decl`; we have to give up.
          guard baseDecl !== decl else { continue stmt }

          // Realize the next component.
          let nextType = compound.components[i].realize(qualifiedFrom: baseDecl)
          guard let nextDecl = (nextType as? NominalType)?.decl else { continue stmt }
          baseDecl = nextDecl
        }

        // Check if the declaration to which the signature resolves is `decl`.
        if baseDecl === decl {
          matches.append(ext)
        }

      default:
        // The extension is bound to another declaration, or it is invalid.
        continue
      }
    }

    return matches
  }

}

extension Module: TypeDecl {

  public var name: String { id }

  public var fullyQualName: [String] { [name] }

  public var range: SourceRange { .invalid }

  public var isOverloadable: Bool { false }

  public var isInvalid: Bool {
    return decls.contains(where: { $0.isInvalid })
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

extension Module: CustomStringConvertible {

  public var description: String {
    return "Module(\(id))"
  }

}
