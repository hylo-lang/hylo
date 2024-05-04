/// The type of a buffer.
public struct BufferType: TypeProtocol {

  /// The type of the buffer's elements.
  public let element: AnyType

  /// The number of elements in the buffer.
  public let count: CompileTimeValue

  /// The structural properties of the type.
  public let flags: TypeFlags

  /// Creates an instance with the given properties.
  public init(_ element: AnyType, _ count: CompileTimeValue) {
    self.element = element
    self.count = count
    self.flags = element.flags
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    BufferType(element.transform(mutating: &m, transformer), count)
  }

}

extension BufferType: CustomStringConvertible {

  public var description: String {
    "\(element)[\(count)]"
  }

}
