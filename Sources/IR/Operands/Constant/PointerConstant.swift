import Core

/// A pointer to a global value in Hylo IR.
public struct PointerConstant: Constant, Hashable {

  /// The module in which the global is defined.
  public let container: ModuleDecl.ID

  /// The ID of the global in `container`.
  public let id: Module.GlobalID

  /// Creates a pointer to the global identified by `id` in `container`.
  public init(_ container: ModuleDecl.ID, _ id: Module.GlobalID) {
    self.container = container
    self.id = id
  }

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(BuiltinType.ptr) }

}

extension PointerConstant: CustomStringConvertible {

  public var description: String {
    "@\(container).\(id)"
  }

}
