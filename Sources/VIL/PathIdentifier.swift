import AST

/// A value that identifies a path to a mutable location.
enum PathIdentifier: Hashable {

  case binding(decl: ValueDecl)

  indirect case member(base: PathIdentifier, decl: ValueDecl)

  func hash(into hasher: inout Hasher) {
    switch self {
    case .binding(let decl):
      hasher.combine(ObjectIdentifier(decl))

    case .member(let base, let decl):
      hasher.combine(base)
      hasher.combine(ObjectIdentifier(decl))
    }
  }

  static func == (lhs: PathIdentifier, rhs: PathIdentifier) -> Bool {
    switch (lhs, rhs) {
    case (.binding(let lDecl), .binding(let rDecl)):
      return lDecl === rDecl

    case (.member(let lBase, let lDecl), .member(let rBase, let rDecl)):
      return (lBase == rBase) && (lDecl === rDecl)

    default:
      return false
    }
  }

}
