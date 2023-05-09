import Core

/// A Val IR reference to a user function.
public struct FunctionReference: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: LoweredType

  /// Creates a reference to `f`, which is in `module`.
  public init(to f: Function.ID, in module: Module) {
    self.function = f
    let v = module[f]
    let t = LambdaType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    self.type = .address(t)
  }

  /// Creates a reference to the lowered form of `d` in `module`.
  public init(to d: FunctionDecl.Typed, in module: inout Module) {
    self.function = module.demandFunctionDeclaration(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
  }

  /// Creates a reference to the lowered form of `d` in `module`.
  public init(to d: InitializerDecl.Typed, in module: inout Module) {
    self.function = module.demandInitializerDeclaration(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
  }

}

extension FunctionReference: CustomStringConvertible {

  public var description: String { "@\(function)" }

}
