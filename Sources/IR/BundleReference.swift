import Core

/// A reference to a method or subscript bundle.
public struct BundleReference<T: BundleDecl>: Hashable {

  /// The ID of the referred bundle.
  public let bundle: T.ID

  /// If `bundle` is generic, the arguments to its generic parameter.
  public let arguments: GenericArguments

  /// `true` if the reference is marked for mutation.
  public let isMutating: Bool

  /// Creates a reference to `s` parameterized by `a`.
  public init(to s: T.ID, usedMutably m: Bool, specializedBy a: GenericArguments) {
    self.bundle = s
    self.arguments = a
    self.isMutating = m
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
