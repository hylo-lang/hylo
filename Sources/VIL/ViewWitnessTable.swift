import AST

/// A view witness table, describing how a type conforms to a view.
public struct ViewWitnessTable: Identifiable {

  /// The conforming type.
  public let type: NominalType

  /// The conformed view.
  public let view: ViewType

  /// The table's entries.
  ///
  /// Each entry maps a view requirement to the VIL function which implements it.
  public let entries: [(decl: BaseFunDecl, impl: Function)]

  public init(
    type: NominalType,
    view: ViewType,
    entries: [(decl: BaseFunDecl, impl: Function)])
  {
    self.type = type
    self.view = view
    self.entries = entries
  }

  public var id: ID { ID(type: type, view: view) }

  /// The identifier of a witness table.
  public struct ID: Hashable {

    let rawValue: (Int, Int)

    public init(type: NominalType, view: ViewType) {
      self.rawValue = (
        Int(bitPattern: ObjectIdentifier(type)),
        Int(bitPattern: ObjectIdentifier(view)))
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(rawValue.0)
      hasher.combine(rawValue.1)
    }

    public static func == (lhs: ID, rhs: ID) -> Bool {
      return lhs.rawValue == rhs.rawValue
    }

  }

}
