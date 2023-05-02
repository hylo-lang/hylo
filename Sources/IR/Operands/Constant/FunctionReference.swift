import Core

/// A Val IR reference to a user function.
public struct FunctionRef: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: LoweredType

  /// Creates an instance with the given properties.
  public init(to function: Function.ID, type: LoweredType) {
    self.function = function
    self.type = type
  }

  /// Creates a reference to the lowered form of `d` in `module`.
  public init(to d: FunctionDecl.Typed, in module: inout Module) {
    self.function = module.getOrCreateFunction(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
  }

  /// Creates a reference to the lowered form of `d` in `module`.
  public init(to d: InitializerDecl.Typed, in module: inout Module) {
    self.function = module.initializerDeclaration(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
  }

}

extension FunctionRef: CustomStringConvertible {

  public var description: String { "@\(function)" }

}
