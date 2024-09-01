import FrontEnd
import Utils

/// A Hylo IR reference to a user function.
public struct FunctionReference: Constant, Hashable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: IR.`Type`

  /// If `function` is generic, arguments corresponding to its generic parameters.
  public let specialization: GenericArguments

  /// Creates a reference to `f`, which is in `module`, without specialization.
  ///
  /// - Requires: `f` accepts no generic parameters.
  public init(to f: Function.ID, in module: Module) {
    precondition(module[f].genericParameters.isEmpty, "underspecialized function reference")

    let v = module[f]
    let t = ArrowType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    assert(t.isCanonical)

    self.function = f
    self.type = .address(t)
    self.specialization = .empty
  }

  /// Creates a reference to `f`, which is in `module`, specialized by `specialization` in
  /// `scopeOfUse`.
  ///
  /// - Requires: `specialization` supplies arguments for all generic parameters of `f`.
  public init(
    to f: Function.ID, in module: Module,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) {
    var a = GenericArguments()
    for p in module[f].genericParameters {
      let v = specialization[p] ?? preconditionFailure("underspecialized function reference")
      a[p] = module.program.canonical(v, in: scopeOfUse)
    }

    let v = module[f]
    let t = ArrowType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    let u = module.program.canonical(
      module.program.specialize(^t, for: a, in: scopeOfUse), in: scopeOfUse)

    self.function = f
    self.type = .address(u)
    self.specialization = a
  }

  /// Creates a reference to the lowered form of `d` in `module`, without specialization.
  ///
  /// - Requires: `d` can be lowered to an IR function.
  public init(to d: AnyDeclID, in module: inout Module) {
    let f = module.demandDeclaration(lowering: d)!
    self.init(to: f, in: module)
  }

  /// Creates a reference to the lowered form of `d` in `module`, specialized by `specialization`
  /// in `scopeOfUse`.
  ///
  /// - Requires: `d` can be lowered to an IR function.
  public init(
    to d: AnyDeclID, in module: inout Module,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) {
    let f = module.demandDeclaration(lowering: d)!
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
