import FrontEnd
import Utils

/// The possibly synthetic declaration of a parameter to a Hylo IR function.
public struct Parameter: Sendable {

  /// The declaration of the parameter, if any.
  ///
  /// This property denotes either a parameter or a capture.
  public let decl: AnyDeclID?

  /// The type of the parameter.
  public let type: ParameterType

  /// Creates an instance with given properties.
  init(decl: AnyDeclID?, type: ParameterType) {
    self.decl = decl
    self.type = type
  }

  /// Creates an instance representing `d`, which is a capture with type `t`.
  init(_ d: AnyDeclID, capturedAs t: AnyType) {
    switch t.base {
    case let u as ParameterType:
      self.init(decl: d, type: u)
    case let u as RemoteType:
      self.init(decl: d, type: ParameterType(u))
    default:
      self.init(decl: d, type: ParameterType(.inout, t))
    }
  }

}

extension Parameter: CustomStringConvertible {

  public var description: String { String(describing: type) }

}
