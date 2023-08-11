import Core

/// A Hylo IR reference to a user subscript bundle.
public struct SubscriptBundleReference: Hashable {

  /// The ID of the referred IR subscript bundle.
  public let bundle: SubscriptDecl.ID

  /// If `function` is generic, the arguments to its generic parameter.
  public let arguments: GenericArguments

  /// Creates a reference to `s` parameterized by `a`.
  public init(to s: SubscriptDecl.ID, parameterizedBy a: GenericArguments) {
    self.bundle = s
    self.arguments = a
  }

}

extension SubscriptBundleReference: CustomStringConvertible {

  public var description: String {
    if arguments.isEmpty {
      return "@\(bundle)"
    } else {
      return "@\(bundle)<\(list: arguments.values)>"
    }
  }

}
