/// An implicit capture in a function or subscript declaration.
public struct ImplicitCapture {

  /// The name of the capture.
  public let name: Name

  /// The type of the capture.
  public let type: RemoteType

  /// The declaration of the capture.
  public let decl: AnyDeclID

  /// Creates an instance having the given properties.
  public init(name: Name, type: RemoteType, decl: AnyDeclID) {
    self.name = name
    self.type = type
    self.decl = decl
  }

  /// Returns a copy of `self` where `type` is `inout`.
  public func mutable() -> Self {
    ImplicitCapture(name: name, type: .init(.inout, type.bareType), decl: decl)
  }

}
