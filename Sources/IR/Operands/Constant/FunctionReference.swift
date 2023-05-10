import Core

/// A Val IR reference to a user function.
public struct FunctionReference: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: LoweredType

  /// The scope in Val sources from which the function is being referred to.
  public let useScope: AnyScopeID

  /// If `function` is generic, the arguments to its generic parameter.
  public let arguments: GenericArguments

  /// Creates a reference to `f`, which is in `module`, used in `s`.
  public init(to f: Function.ID, usedIn s: AnyScopeID, in module: Module) {
    self.function = f
    let v = module[f]
    let t = LambdaType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    self.type = .address(t)
    self.useScope = s
    self.arguments = [:]
  }

  /// Creates in `module` a reference to the lowered form of `d`, which is used in `s` and
  /// parameterized by `a`.
  public init(
    to d: FunctionDecl.Typed,
    usedIn s: AnyScopeID,
    parameterizedBy a: GenericArguments = [:],
    in module: inout Module
  ) {
    self.function = module.demandFunctionDeclaration(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
    self.useScope = s
    self.arguments = a
  }

  /// Creates in `module` a reference to the lowered form of `d`, which is used in `s`.
  public init(
    to d: InitializerDecl.Typed,
    usedIn s: AnyScopeID,
    in module: inout Module
  ) {
    self.function = module.demandInitializerDeclaration(lowering: d)
    self.type = .address(LambdaType(d.type)!.lifted)
    self.useScope = s
    self.arguments = [:]
  }

}

extension FunctionReference: CustomStringConvertible {

  public var description: String {
    if arguments.isEmpty {
      return "@\(function)"
    } else {
      return "@\(function)<\(list: arguments.values)>"
    }
  }

}
