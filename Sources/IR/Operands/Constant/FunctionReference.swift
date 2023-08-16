import Core

/// A Hylo IR reference to a user function.
public struct FunctionReference: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: IR.`Type`

  /// If `function` is generic, arguments corresponding to its generic parameters.
  public let specialization: GenericArguments

  /// Creates a reference to `f`, which is in `module`, without specialization.
  public init(to f: Function.ID, in module: Module) {
    let v = module[f]
    let t = LambdaType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    assert(t[.isCanonical])

    self.function = f
    self.type = .address(t)
    self.specialization = [:]
  }

  /// Creates a reference to `f`, which is in `module`, specialized by `specialization` in
  /// `scopeOfUse`.
  public init(
    to f: Function.ID, in module: Module,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) {
    let v = module[f]
    let t = LambdaType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    let a = module.program.canonical(specialization, in: scopeOfUse)
    let u = module.program.specialize(^t, for: a, in: scopeOfUse)
    assert(t[.isCanonical])

    self.function = f
    self.type = .address(u)
    self.specialization = a
  }

  /// Creates a reference to the lowered form of `d` in `module`, without specialization.
  public init(to d: FunctionDecl.ID, in module: inout Module) {
    let f = module.demandFunctionDeclaration(lowering: d)
    self.init(to: f, in: module)
  }

  /// Creates a reference to the lowered form of `d` in `module`, specialized by `specialization`
  /// in `scopeOfUse`.
  public init(
    to d: FunctionDecl.ID, in module: inout Module,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) {
    let f = module.demandFunctionDeclaration(lowering: d)
    self.init(to: f, in: module, specializedBy: specialization, in: scopeOfUse)
  }

  /// Creates a reference to the lowered form of `d` in `module`, without specialization.
  public init(to d: InitializerDecl.ID, in module: inout Module) {
    let f = module.demandInitializerDeclaration(lowering: d)
    self.init(to: f, in: module)
  }

  /// Creates a reference to the lowered form of `d` in `module`, specialized by `specialization`
  /// in `scopeOfUse`.
  public init(
    to d: InitializerDecl.ID, in module: inout Module,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) {
    let f = module.demandInitializerDeclaration(lowering: d)
    self.init(to: f, in: module, specializedBy: specialization, in: scopeOfUse)
  }

}

extension FunctionReference: CustomStringConvertible {

  public var description: String {
    if specialization.isEmpty {
      return "@\(function)"
    } else {
      return "@\(function)<\(list: specialization.values)>"
    }
  }

}
