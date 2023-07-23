import Core

/// A Val IR reference to a user function.
public struct FunctionReference: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: IR.`Type`

  /// If `function` is generic, actual arguments corresponding to its generic parameters.
  public let genericArguments: GenericArguments

  /// Creates a reference to `f`, which is in `module`.
  public init(
    to f: Function.ID, parameterizedBy a: GenericArguments = [:],
    in module: Module
  ) {
    let arguments = module.program.relations.canonical(a)
    let v = module[f]
    let t = module.program.monomorphize(
      ^LambdaType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output),
      for: arguments)

    self.function = f
    self.type = .address(t)
    self.genericArguments = arguments
  }

  /// Creates in `module` a reference to the lowered form of `d`, which is parameterized by `a`.
  public init(
    to d: FunctionDecl.ID, parameterizedBy a: GenericArguments = [:],
    in module: inout Module
  ) {
    let arguments = module.program.relations.canonical(a)
    let t = module.program.relations.canonical(
      module.program.monomorphize(module.program[d].type, for: arguments))

    self.function = module.demandFunctionDeclaration(lowering: d)
    self.type = .address(LambdaType(t)!.lifted)
    self.genericArguments = arguments
  }

  /// Creates in `module` a reference to the lowered form of `d`.
  public init(
    to d: InitializerDecl.ID, parameterizedBy a: GenericArguments = [:],
    in module: inout Module
  ) {
    let arguments = module.program.relations.canonical(a)
    let t = module.program.relations.canonical(
      module.program.monomorphize(module.program[d].type, for: arguments))

    self.function = module.demandInitializerDeclaration(lowering: d)
    self.type = .address(LambdaType(t)!.lifted)
    self.genericArguments = arguments
  }

}

extension FunctionReference: CustomStringConvertible {

  public var description: String {
    if genericArguments.isEmpty {
      return "@\(function)"
    } else {
      return "@\(function)<\(list: genericArguments.values)>"
    }
  }

}
