import Core

/// A reference to a method or subscript bundle.
public struct BundleReference<T: BundleDecl>: Hashable {

  /// The ID of the referred bundle.
  public let bundle: T.ID

  /// If `bundle` is generic, the arguments to its generic parameter.
  public let arguments: GenericArguments

  /// Creates a reference to `s` parameterized by `a`.
  public init(to s: T.ID, specializedBy a: GenericArguments) {
    self.bundle = s
    self.arguments = a
  }

}

extension BundleReference: CustomStringConvertible {

  public var description: String {
    if arguments.isEmpty {
      return "@\(bundle)"
    } else {
      return "@\(bundle)<\(list: arguments.values)>"
    }
  }

}
