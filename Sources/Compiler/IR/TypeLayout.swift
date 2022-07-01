/// A helper object that provides information about a type layout.
public struct TypeLayout {

  /// The type for which the layout is being queried.
  public let type: Type

  public init(_ type: Type) {
    self.type = type
  }

  /// Returns the offset of `property`.
  public func offset(of property: NodeID<VarDecl>, ast: AST) -> Int {
    switch type {
    case .product(let type):
      var result = 0
      for memberID in ast[type.decl].members {
        guard let binding = NodeID<BindingDecl>(converting: memberID) else { continue }
        for name in ast[binding].pattern.names(ast: ast) {
          if ast[name].decl == property {
            return result
          } else {
            result += 1
          }
        }
      }

    default:
      break
    }

    preconditionFailure("property is not member of the type")
  }

}
