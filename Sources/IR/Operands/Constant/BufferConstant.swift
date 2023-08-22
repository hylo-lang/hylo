import Core
import Foundation

/// A constant buffer of bytes in Hylo IR.
public struct BufferConstant: Constant, Hashable {

  /// The contents of this instance.
  public let contents: Data

  /// Creates an instance with given `contents`.
  public init(_ contents: Data) {
    self.contents = contents
  }

  /// The Hylo IR type of this instance.
  public var type: IR.`Type` { .object(BuiltinType.ptr) }

}

extension BufferConstant: CustomStringConvertible {

  public var description: String {
    contents.reduce(into: "\"", { (s, b) in s.append("\\x" + String(b, radix: 16)) }) + "\""
  }

}
