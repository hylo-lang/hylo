import Core

/// The address of some statically allocated storage.
public struct StaticStorage: Constant, Hashable {

  /// The type for which storage is allocated.
  public let pointee: AnyType

  /// The function that is used to initialize the allocated storage on first access.
  public let initializer: Function.ID

  /// Creates a static allocation for an object of type `t`, initialized by calling `initializer`
  /// the first time it is accessed.
  public init(_ t: AnyType, initializedWith initializer: Function.ID) {
    self.pointee = t
    self.initializer = initializer
  }

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` {
    .address(pointee)
  }

}
