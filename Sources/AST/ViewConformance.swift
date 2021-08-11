import Basic

/// A data structure describing the metadata of a particular view conformance relation.
public struct ViewConformance {

  /// The state of a conformance relation, as it goes through type checking.
  public enum State {

    /// The conformance has been realized from an explicit or implicit declaration, but has not
    /// been type checked yet.
    case realized

    /// The conformance has been type checked; it is semantically well-formed.
    ///
    /// A conformance is checked if the type checker was able to verify that the conforming type
    /// satisfies all the view's requirements, filling the conformance's entries accordingly.
    case checked

    /// The conformance has been found to be ill-formed; it should be ignored by all subsequent
    /// semantic analysis phases, without producing any further diagnostic.
    case invalid

  }

  /// The declaration of the view being conformed to.
  public unowned let viewDecl: ViewTypeDecl

  /// The view being conformed to.
  public var viewType: ViewType { viewDecl.instanceType as! ViewType }

  /// The source range from which the conformance originates.
  public let range: SourceRange?

  /// A mapping describing how each of the view's requirement is satisfied.
  public var entries: [(req: TypeOrValueDecl, impl: TypeOrValueDecl)] = []

  /// The (semantic) state of the conformance relation.
  public var state = State.realized

  public init(viewDecl: ViewTypeDecl, range: SourceRange?) {
    self.viewDecl = viewDecl
    self.range = range
  }

  /// Returns the declaration that witnesses the given requirement.
  ///
  /// - Parameter requirement: A value requirement, defined in a conformed view.
  public func witness(for requirement: ValueDecl) -> ValueDecl? {
    guard let entry = entries.first(where: { $0.req === requirement }) else { return nil }
    return entry.impl as? ValueDecl
  }

  /// Returns the declaration that witnesses the given requirement.
  ///
  /// - Parameter requirement: A type requirement, defined in a conformed view.
  public func witness(for requirement: TypeDecl) -> TypeDecl? {
    guard let entry = entries.first(where: { $0.req === requirement }) else { return nil }
    return entry.impl as? TypeDecl
  }

}
