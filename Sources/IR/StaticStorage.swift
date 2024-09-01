import FrontEnd

/// The address of some statically allocated storage.
public struct StaticStorage {

  /// The identifier of the storage.
  public let id: AnyDeclID

  /// The type for which storage is allocated.
  public let pointee: AnyType

  /// The function that is used to initialize the allocated storage on first access.
  public let initializer: Function.ID

  /// Creates a static allocation identified by `id` for an object of type `t`, initialized by
  /// calling `initializer` the first time it is accessed.
  public init(
    _ t: AnyType, identifiedBy id: AnyDeclID, initializedWith initializer: Function.ID
  ) {
    precondition(t.isCanonical)
    self.id = id
    self.pointee = t
    self.initializer = initializer
  }

}

extension StaticStorage: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.id == r.id
  }

}

extension StaticStorage: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

}
