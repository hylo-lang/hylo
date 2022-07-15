/// A helper object that provides information about a type layout.
public struct TypeLayout {

  /// The type for which the layout is being queried.
  public let type: Type

  /// Given `type` has a record layout, the indices of the stored properties.
  public let storedPropertiesIndices: [String: Int]

  /// Given `type` has a record layout, the type the member at the specified stored property index.
  public let storedPropertiesTypes: [Type]

  /// Creates the layout of `type` defined in `program`.
  public init(_ type: Type, in program: TypedProgram) {
    self.type = type

    let indicesAndTypes = program.storedPropertiesIndicesAndTypes(of: type)
    self.storedPropertiesIndices = indicesAndTypes.indices
    self.storedPropertiesTypes = indicesAndTypes.types
  }

}

extension TypedProgram {

  fileprivate func storedPropertiesIndicesAndTypes(
    of type: Type
  ) -> (indices: [String: Int], types: [Type]) {
    var indices: [String: Int] = [:]
    var types: [Type] = []

    switch type {
    case .product(let type):
      for m in ast[type.decl].members {
        guard let binding = NodeID<BindingDecl>(converting: m) else { continue }
        for (_, name) in ast.names(in: ast[binding].pattern) {
          let decl = ast[name].decl
          indices[ast[decl].name] = types.count
          types.append(declTypes[decl] ?? .error(ErrorType()))
        }
      }

    default:
      break
    }

    return (indices, types)
  }

}
