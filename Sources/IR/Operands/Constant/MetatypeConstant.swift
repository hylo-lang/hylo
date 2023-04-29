import Core
import Foundation

/// A constant metatype value in Val IR.
public struct MetatypeConstant: Constant, Hashable {

  /// The value of the constant.
  public let value: MetatypeType

  /// Creates an instance representing `t`.
  ///
  /// - Requires: `t` is canonical.
  public init(_ t: MetatypeType) {
    self.value = t
  }

  /// The Val IR type of this instance.
  public var type: LoweredType { .object(MetatypeType(of: value)) }

}

extension MetatypeConstant: CustomStringConvertible {

  public var description: String { .init(describing: value) }

}
