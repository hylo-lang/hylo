import Basic

/// A lookup table that keeps track of the views to which a nominal type conforms.
public struct ConformanceLookupTable {

  public init() {
  }

  var conformances: [ViewConformance] = []

  @discardableResult
  public mutating func insert(
    _ newViewType: ViewType,
    range: SourceRange? = nil
  ) -> (inserted: Bool, conformance: ViewConformance) {
    if let conformance = self[newViewType] {
      return (false, conformance)
    }

    let conformance = ViewConformance(viewDecl: newViewType.decl as! ViewTypeDecl, range: range)
    conformances.append(conformance)
    return (true, conformance)
  }

  public subscript(viewType: ViewType) -> ViewConformance? {
    return conformances.first(where: { $0.viewDecl === viewType.decl })
  }

}

/// A data structure describing a particular cnformance to a given view.
public struct ViewConformance {

  /// The view being conformed to.
  public unowned let viewDecl: ViewTypeDecl

  /// The source range of this conformance's declaration.
  public let range: SourceRange?

}
