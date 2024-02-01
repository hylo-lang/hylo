import Core

/// A reference to a method or subscript bundle.
public struct BundleReference<T: BundleDecl>: Hashable {

  /// The ID of the referred bundle.
  public let bundle: T.ID

  /// If `bundle` is generic, the arguments to its generic parameter.
  public let arguments: GenericArguments

  /// The capabilities requested on the bundle receiver.
  public let capabilities: AccessEffectSet

  /// Creates a reference to `d` specialized by `a`, requesting capabilities `k ` on the receiver.
  public init(to d: T.ID, specializedBy a: GenericArguments, requesting k: AccessEffectSet) {
    self.bundle = d
    self.arguments = a
    self.capabilities = k
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
