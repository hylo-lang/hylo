/// The declaration of a type's conformance to one or multiple traits.
public protocol ConformanceSource: ExposableDecl, Sendable {

  /// The names of traits to which the type conforms.
  var conformances: [NameExpr.ID] { get }

}
