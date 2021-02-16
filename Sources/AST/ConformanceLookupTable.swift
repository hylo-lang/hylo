import Basic

/// A lookup table that keeps track of the views to which a nominal type conforms.
public struct ConformanceLookupTable {

  public typealias Values = Dictionary<ObjectIdentifier, ViewConformance>.Values

  /// Creates an empty conformance table.
  public init() {}

  /// The conformances in the table, indexed by view declaration.
  private var conformances: [ObjectIdentifier: ViewConformance] = [:]

  /// Accesses the description of a particular view conformance, if such conformance exists.
  ///
  /// - Parameter viewType: The conformed view.
  public subscript(viewType: ViewType) -> ViewConformance? {
    get { conformances[ObjectIdentifier(viewType)] }
    set { conformances[ObjectIdentifier(viewType)] = newValue }
  }

  /// The set of view conformance relations in the table.
  public var values: Values {
    return conformances.values
  }

}
