/// A sequence with the requirements of a trait declaration.
public struct RequirementSequence: IteratorProtocol, Sequence {

  public typealias Element = AnyDeclID

  /// The AST containing `base`.
  private let ast: AST

  /// The declaration of the trait defining the requirements in this sequence.
  private let base: TraitDecl.ID

  /// An index in the members of `base`.
  private var member: Int = 0

  /// An index in the variants of `ast[base].members[member]` if it's a bundle.
  private var variant: Int = 0

  /// Creates an instance listing the requirements of `base`, which is in `ast`.
  public init(_ base: TraitDecl.ID, in ast: AST) {
    self.ast = ast
    self.base = base
  }

  /// Advances to the next requirement and returns it, or `nil` if no next requirement exists.
  public mutating func next() -> AnyDeclID? {
    while member != ast[base].members.count {
      let m = ast[base].members[member]
      switch m.kind {
      case AssociatedTypeDecl.self, AssociatedValueDecl.self, FunctionDecl.self,
        InitializerDecl.self:
        member += 1
        return AnyDeclID(m)

      case MethodDecl.self:
        if let d = next(in: MethodDecl.ID(m)!) {
          return d
        }

      case SubscriptDecl.self:
        if let d = next(in: SubscriptDecl.ID(m)!) {
          return d
        }

      default:
        break
      }

      member += 1
      variant = 0
    }

    // All requirements have been returned.
    return nil
  }

  /// Advances to the next requirement variant in `bundle` and returns it, or `nil` if no next
  /// variant exists.
  private mutating func next<T: BundleDecl>(in bundle: T.ID) -> AnyDeclID? {
    if variant != ast[bundle].impls.count {
      defer { variant += 1 }
      return AnyDeclID(ast[bundle].impls[variant])
    } else {
      return nil
    }
  }

}
