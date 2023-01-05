import Core

/// A helper object that provides information the abstract layout of a type.
public struct AbstractTypeLayout {

  /// The type for which the layout is being queried.
  public let type: AnyType

  /// Given `type` has a record layout, the indices of the stored properties.
  public let storedPropertiesIndices: [String: Int]

  /// Given `type` has a record layout, the type the member at the specified stored property index.
  public let storedPropertiesTypes: [AnyType]

  fileprivate init(
    type: AnyType,
    storedPropertiesIndices: [String: Int],
    storedPropertiesTypes: [AnyType]
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
  public func abstractLayout(of type: AnyType, at path: [Int] = []) -> AbstractTypeLayout {
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
    of type: AnyType
  ) -> (indices: [String: Int], types: [AnyType]) {
    var indices: [String: Int] = [:]
    var types: [AnyType] = []

    switch type.base {
    case let type as ProductType:
      let decl = self[type.decl]
      for m in decl.members where m.kind == BindingDecl.self {
        let binding = BindingDecl.Typed(m)!
        for (_, name) in binding.pattern.names {
          indices[name.decl.name] = types.count
          types.append(name.decl.type)
        }
      }

    default:
      break
    }

    return (indices, types)
  }

}
