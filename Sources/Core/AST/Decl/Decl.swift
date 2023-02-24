/// A declaration.
public protocol Decl: Node {}

extension Decl {

  /// Indicates whether `self` denotes an overloadable declaration.
  public static var isOverloadable: Bool {
    switch kind {
    case FunctionDecl.self, InitializerDecl.self, MethodDecl.self, SubscriptDecl.self:
      return true
    default:
      return false
    }
  }

}
