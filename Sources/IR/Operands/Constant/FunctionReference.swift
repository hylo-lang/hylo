import FrontEnd
import Utils

/// A Hylo IR reference to a user function.
public struct FunctionReference: Constant, Hashable, Sendable {

  /// The ID of the referred IR function.
  public let function: Function.ID

  /// The type of the referred IR function.
  public let type: IR.`Type`

  /// If `function` is generic, arguments corresponding to its generic parameters.
  public let specialization: GenericArguments

  /// Creates a reference to `f`, which is in `module`, without specialization.
  ///
  /// - Requires: `f` accepts no generic parameters.
  fileprivate init(to f: Function.ID, in module: Module) {
    precondition(module[f].genericParameters.isEmpty, "underspecialized function reference")

    let v = module[f]
    let t = ArrowType(inputs: v.inputs.map({ .init(type: ^$0.type) }), output: v.output)
    assert(t.isCanonical)

    self.function = f
    self.type = .place(t)
    self.specialization = .empty
  }

  /// Creates a reference to `f`, which is in `module`, specialized by `specialization` in
  /// `scopeOfUse`.
  ///
  /// - Requires: `specialization` supplies arguments for all generic parameters of `f`.
  fileprivate init(
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
    self.type = .place(u)
    self.specialization = a
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

extension Module {

  /// Creates a reference to `functionID` without specialization.
  ///
  /// - Requires: `functionID` accepts no generic parameters.
  func makeReference(to functionID: Function.ID) -> FunctionReference {
    .init(to: functionID, in: self)
  }

  /// Creates a reference to `functionID` specialized by `specialization` in `scopeOfUse`.
  ///
  /// - Requires: `specialization` supplies arguments for all generic parameters of `functionID`.
  func makeReference(
    to functionID: Function.ID,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> FunctionReference {
    .init(to: functionID, in: self, specializedBy: specialization, in: scopeOfUse)
  }

  /// Creates a reference to the lowered form of `declID` specialized by `specialization` in
  /// `scopeOfUse`.
  ///
  /// - Requires: `declID` can be lowered to an IR function.
  mutating func makeReference(
    lowering declID: AnyDeclID,
    specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> FunctionReference {
    let f = demandDeclaration(lowering: declID)!
    return .init(to: f, in: self, specializedBy: specialization, in: scopeOfUse)
  }
}
