import Basic

/// A lookup table that keeps track of the views to which a nominal type conforms.
public struct ConformanceLookupTable {

  public init() {
  }

  private var conformances: [ViewConformance] = []

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

extension ConformanceLookupTable: Collection {

  public typealias Element = ViewConformance

  public var startIndex: Int {
    return conformances.startIndex
  }

  public var endIndex: Int {
    return conformances.endIndex
  }

  public func index(after i: Int) -> Int {
    return i + 1
  }

  public subscript(position: Int) -> ViewConformance {
    return conformances[position]
  }

}

/// A data structure describing a particular cnformance to a given view.
public struct ViewConformance {

  /// The view being conformed to.
  public unowned let viewDecl: ViewTypeDecl

  /// The source range of this conformance's declaration.
  public let range: SourceRange?

}
