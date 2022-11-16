/// An implicit capture in a function or subscript declaration.
public struct ImplicitCapture {

  /// The name of the capture.
  let name: Name

  /// The type of the capture.
  let type: RemoteType

  /// The declaration of the capture.
  let decl: AnyDeclID

}
