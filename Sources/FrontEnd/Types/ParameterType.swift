import Utils

/// The type of a parameter in an arrow, method, or subscript type.
public struct ParameterType: TypeProtocol {

  /// The passing convention of the parameter.
  public let access: AccessEffect

  /// The type of the parameter's value.
  public let bareType: AnyType

  /// `true` iff the parameter is an autoclosure.
  public let isAutoclosure: Bool

  public let flags: ValueFlags

  /// Creates an instance with the given properties.
  public init(_ access: AccessEffect, _ bareType: AnyType, isAutoclosure: Bool = false) {
    assert(!(bareType.base is ParameterType), "bad type")
    self.access = access
    self.bareType = bareType
    self.flags = bareType.flags
    self.isAutoclosure = isAutoclosure
  }

  /// Creates an instance converting `t`.
  public init(_ t: RemoteType) {
    self.init(t.access, t.bareType)
  }

  /// `true` iff `self` is creates an access for initialization or mutation.
  public var isSetOrInout: Bool {
    (access == .set) || (access == .inout)
  }

  public func transformParts<M>(
    mutating m: inout M, _ transformer: (inout M, AnyType) -> TypeTransformAction
  ) -> Self {
    ParameterType(
      access, bareType.transform(mutating: &m, transformer), isAutoclosure: isAutoclosure)
  }
}

extension ParameterType: CustomStringConvertible {

  public var description: String {
    let r = "\(access) \(bareType)"
    return isAutoclosure ? "@autoclosure \(r)" : r
  }

}
