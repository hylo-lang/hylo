/// The attributes of a function surfaced for its API.
public struct FunctionAttributes: Hashable {

  /// The external name of the function.
  public let externalName: String?

  /// The foreign name of the function.
  public let foreignName: String?

  init(externalName: String?, foreignName: String?) {
    self.externalName = externalName
    self.foreignName = foreignName
  }

}
