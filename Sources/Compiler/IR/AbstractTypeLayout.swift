/// A helper object that provides information the abstract layout of a type.
public struct AbstractTypeLayout {

  /// The type for which the layout is being queried.
  public let type: Type

  /// Given `type` has a record layout, the indices of the stored properties.
  public let storedPropertiesIndices: [String: Int]

  /// Given `type` has a record layout, the type the member at the specified stored property index.
  public let storedPropertiesTypes: [Type]

  fileprivate init(
    type: Type,
    storedPropertiesIndices: [String: Int],
    storedPropertiesTypes: [Type]
  ) {
    self.type = type
    self.storedPropertiesIndices = storedPropertiesIndices
    self.storedPropertiesTypes = storedPropertiesTypes
  }

}

extension TypedProgram {

  /// Returns the abstract layout of an instance of `type`, or sub-object thereof.
  ///
  /// If `path` is empty, the method returns the layout of an instance of `type`. Otherwise, each
  /// component is interpreter as the abstract offset of a stored property, leading to a sub-object
  /// of an instance of `type`.
  public func abstractLayout(of type: Type, at path: [Int] = []) -> AbstractTypeLayout {
    let indicesAndTypes = storedPropertiesIndicesAndTypes(of: type)
    var layout = AbstractTypeLayout(
      type: type,
      storedPropertiesIndices: indicesAndTypes.indices,
      storedPropertiesTypes: indicesAndTypes.types)

    for offset in path {
      layout = abstractLayout(of: layout.storedPropertiesTypes[offset])
    }
    return layout
  }

  private func storedPropertiesIndicesAndTypes(
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
