import Foundation
import FrontEnd
import OrderedCollections
import Utils

/// A module lowered to Hylo IR.
///
/// A lowered module is a collection of IR functions and a collection of constant IR values, which
/// represent nominal types, traits, and global bindings. These entities may not necessarily have
/// a definition. When they don't, they denote a declaration known to be defined in another module.
public struct Module: Sendable {

  /// Unique identifier of a module within a program.
  public typealias ID = ModuleDecl.ID

  /// The program defining the functions in `self`.
  public let program: TypedProgram

  /// The module's identifier.
  public let id: ID

  /// The nominal product types defined in the module.
  public private(set) var productTypes: [ProductType] = []

  /// The traits defined in the module.
  public private(set) var traits: [TraitType] = []

  /// The static allocations defined in the module.
  public private(set) var allocations: [StaticStorage] = []

  /// The functions in the module.
  public private(set) var functions: [Function.ID: Function] = [:]

  /// The skeletons of all the projections in the module.
  internal var projectionSkeletons: [Function.ID: ProjectionSkeleton]

  /// The module's entry function, if any.
  ///
  /// An entry function is the lowered form of a program's entry point, that is the `main` function
  /// of a Hylo program. A module with an entry function is called an entry module. There can be
  /// only one entry module in a program.
  public private(set) var entryFunction: Function.ID?

  /// Creates an instance lowering `m` in `p`, reporting errors and warnings to `log`.
  ///
  /// - Requires: `m` is a valid ID in `p`.
  /// - Throws: `Diagnostics` if lowering fails.
  public init(
    lowering m: ModuleDecl.ID, in p: TypedProgram,
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws {
    self.program = p
    self.id = m
    self.projectionSkeletons = [:]

    Emitter.withInstance(insertingIn: &self, reportingDiagnosticsTo: &log) { (e) in
      e.incorporateTopLevelDeclarations()
    }
    try log.throwOnError()
  }

  /// The module's name.
  public var name: String {
    program.ast[id].baseName
  }

  /// Accesses the given function.
  public subscript(f: Function.ID) -> Function {
    _read { yield functions[f]! }
    _modify { yield &functions[f]! }
  }

  /// Accesses the given block.
  public subscript(b: Block.ID, in f: Function.ID) -> Block {
    _read { yield functions[f]![b] }
    _modify { yield &functions[f]![b] }
  }

  /// Accesses the given instruction.
  public subscript(i: InstructionID, in f: Function.ID) -> Instruction {
    _read { yield functions[f]![i] }
    _modify { yield &functions[f]![i] }
  }

  /// Accesses the instruction denoted by `o` if it is `.register`; returns `nil` otherwise.
  public subscript(o: Operand, in f: Function.ID) -> Instruction? {
    if case .register(let i) = o {
      return self[i, in: f]
    } else {
      return nil
    }
  }

  /// Returns `true` if `i` is a deinitializer.
  public func isDeinit(_ i: Function.ID) -> Bool {
    switch i.value {
    case .lowered(let d):
      return FunctionDecl.ID(d).map({ (n) in program.ast[n].isDeinit }) ?? false
    case .existentialized(let j):
      return isDeinit(j)
    case .monomorphized(let j, arguments: _):
      return isDeinit(j)
    case .synthesized(let d):
      return d.kind == .deinitialize
    case .projectionRamp(_), .projectionSlide(_):
      return false
    }
  }

  /// Returns whether the IR in `self` is well-formed.
  ///
  /// Use this method as a sanity check to verify the module's invariants.
  public func isWellFormed() -> Bool {
    for f in functions.keys {
      if !isWellFormed(function: f) { return false }
    }
    return true
  }

  /// Returns whether `f` is well-formed.
  ///
  /// Use this method as a sanity check to verify the function's invariants.
  public func isWellFormed(function f: Function.ID) -> Bool {
    return true
  }

  /// Applies all mandatory passes in this module, accumulating diagnostics in `log` and throwing
  /// if a pass reports an error.
  public mutating func applyMandatoryPasses(
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws {
    // We only go over user implementations. Synthesized functions are assumed well-formed.
    let work = functions.compactMap({ (f, i) in !(f.isSynthesized || i.entry == nil) ? f : nil })
    func run(_ pass: (Function.ID) -> Void) throws {
      for f in work { pass(f) }
      try log.throwOnError()
    }

    try run({ removeDeadCode(in: $0, diagnostics: &log) })
    try run({ reifyCallsToBundles(in: $0, diagnostics: &log) })
    try run({ reifyAccesses(in: $0, diagnostics: &log) })
    try run({ simplify($0) })
    try run({ closeBorrows(in: $0, diagnostics: &log) })
    try run({ normalizeObjectStates(in: $0, diagnostics: &log) })
    try run({ ensureExclusivity(in: $0, diagnostics: &log) })
    try run({ checkYieldCoherence($0, reportingDiagnosticsTo: &log) })

    try generateSyntheticImplementations(reportingDiagnosticsTo: &log)
  }

  /// Inserts the IR for the synthesized declarations defined in this module, reporting diagnostics
  /// to `log` and throwing if a an error occurred.
  private mutating func generateSyntheticImplementations(
    reportingDiagnosticsTo log: inout DiagnosticSet
  ) throws {
    Emitter.withInstance(insertingIn: &self, reportingDiagnosticsTo: &log) { (e) in
      e.incorporateSyntheticDeclarations()
    }
    try log.throwOnError()
  }

  /// Adds `t` to the set of nominal product types defined in `self`.
  mutating func addProductType(_ t: ProductType) {
    productTypes.append(t)
  }

  /// Adds `t` to the set of traits defined in `self`.
  mutating func addTrait(_ t: TraitType) {
    traits.append(t)
  }

  /// Adds `d` to the set of static allocations in `self`.
  mutating func addStaticStorage(_ s: StaticStorage) {
    allocations.append(s)
  }

  /// Assigns `identity` to `value` in `self`.
  ///
  /// - Requires: `identity` is not already assigned.
  mutating func addFunction(_ value: Function, for identity: Function.ID) {
    modify(&functions[identity]) { (f) in
      precondition(f == nil)
      f = value
    }
  }

  /// Returns the identity of the IR function corresponding to `i`.
  mutating func demandDeclaration(lowering i: FrontEnd.Conformance.Implementation) -> Function.ID {
    switch i {
    case .explicit(let d):
      return demandDeclaration(lowering: d)!
    case .synthetic(let d):
      return demandDeclaration(lowering: d)
    }
  }

  /// Returns the identity of the IR function corresponding to `d`, or `nil` if `d` can't be
  /// lowered to an IR function.
  mutating func demandDeclaration(lowering d: AnyDeclID) -> Function.ID? {
    switch d.kind {
    case FunctionDecl.self:
      return demandDeclaration(lowering: FunctionDecl.ID(d)!)
    case MethodImpl.self:
      return demandDeclaration(lowering: MethodImpl.ID(d)!)
    case InitializerDecl.self:
      return demandDeclaration(lowering: InitializerDecl.ID(d)!)
    case SubscriptImpl.self:
      return demandDeclaration(lowering: SubscriptImpl.ID(d)!)
    default:
      return nil
    }
  }

  /// Returns the identity of the IR function corresponding to `d`.
  mutating func demandDeclaration(lowering d: FunctionDecl.ID) -> Function.ID {
    let f = Function.ID(d)
    if functions[f] != nil { return f }

    let parameters = program.accumulatedGenericParameters(in: d)
    let output = program.canonical(
      (program[d].type.base as! CallableType).output, in: program[d].scope)
    let inputs = loweredParameters(of: d)

    // External functions have external linkage at the IR level.
    let linkage: Linkage = (program.isExported(d) || program[d].isExternal) ? .external : .module

    let entity = Function(
      isSubscript: false,
      site: program.ast[d].site,
      linkage: linkage,
      genericParameters: Array(parameters),
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: f)

    // Determine if the new function is the module's entry.
    if program.isModuleEntry(d) {
      assert(entryFunction == nil)
      entryFunction = f
    }

    return f
  }

  /// Returns the identity of the IR function corresponding to `d`.
  mutating func demandDeclaration(lowering d: MethodImpl.ID) -> Function.ID {
    let f = Function.ID(d)
    if functions[f] != nil { return f }

    let parameters = program.accumulatedGenericParameters(in: d)
    let output = program.canonical(
      (program[d].type.base as! CallableType).output, in: program[d].scope)
    let inputs = loweredParameters(of: d)

    let entity = Function(
      isSubscript: false,
      site: program.ast[d].site,
      linkage: program.isExported(d) ? .external : .module,
      genericParameters: Array(parameters),
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: f)
    return f
  }

  /// Returns the identity of the IR function corresponding to `d`.
  mutating func demandDeclaration(lowering d: SubscriptImpl.ID) -> Function.ID {
    let f = Function.ID(d)
    if functions[f] != nil { return f }

    let parameters = program.accumulatedGenericParameters(in: d)
    let output = program.canonical(
      SubscriptImplType(program[d].type)!.output, in: program[d].scope)
    let inputs = loweredParameters(of: d)

    let entity = Function(
      isSubscript: true,
      site: program.ast[d].site,
      linkage: program.isExported(d) ? .external : .module,
      genericParameters: Array(parameters),
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: f)
    return f
  }

  /// Returns the identifier of the IR initializer corresponding to `d`.
  mutating func demandDeclaration(lowering d: InitializerDecl.ID) -> Function.ID {
    precondition(!program.ast[d].isMemberwise)

    let f = Function.ID(d)
    if functions[f] != nil { return f }

    let parameters = program.accumulatedGenericParameters(in: d)
    let inputs = loweredParameters(of: d)

    let entity = Function(
      isSubscript: false,
      site: program.ast[d].introducer.site,
      linkage: program.isExported(d) ? .external : .module,
      genericParameters: Array(parameters),
      inputs: inputs,
      output: .void,
      blocks: [])
    addFunction(entity, for: f)
    return f
  }

  /// Returns the identity of the IR function corresponding to `d`.
  mutating func demandDeclaration(lowering d: SynthesizedFunctionDecl) -> Function.ID {
    let f = Function.ID(d)
    if functions[f] != nil { return f }

    let output = program.canonical(d.type.output, in: d.scope)
    var inputs: [Parameter] = []
    appendCaptures(
      d.type.captures, passed: d.type.receiverEffect, to: &inputs, canonicalizedIn: d.scope)
    appendParameters(d.type.inputs, to: &inputs, canonicalizedIn: d.scope)

    let entity = Function(
      isSubscript: false,
      site: .empty(at: program.ast[id].site.start),
      linkage: .external,
      genericParameters: d.genericParameters,
      inputs: inputs,
      output: output,
      blocks: [])
    addFunction(entity, for: f)

    // Determine if the new function is defined in this module.
    if program.module(containing: d.scope) == id {
      var log = DiagnosticSet()
      Emitter.withInstance(insertingIn: &self, reportingDiagnosticsTo: &log) { (emitter) in
        emitter.lower(synthetic: d)
      }
      assert(log.isEmpty, "unexpected diagnostic in synthesized declaration")
    }

    return f
  }

  /// Returns the implementation of the requirement named `r` in `witness`.
  ///
  /// - Requires: `r` identifies a function or subscript requirement in the trait for which
  ///   `witness` has been established.
  mutating func demandImplementation<T: Decl>(
    of r: T.ID, for witness: FrontEnd.Conformance
  ) -> Function.ID {
    demandDeclaration(lowering: witness.implementations[r]!)
  }

  /// Returns the IR function implementing the deinitializer defined in `c`.
  mutating func demandDeinitDeclaration(
    from c: FrontEnd.Conformance
  ) -> Function.ID {
    let d = program.ast.core.deinitializable.deinitialize
    return demandDeclaration(lowering: c.implementations[d]!)
  }

  /// Returns the IR function implementing the `k` variant move-operation defined by
  /// `conformanceToMovable`.
  ///
  /// - Parameters:
  ///   - k: The semantics of a move operation. It must be either `.set` or `.inout`.
  ///   - conformanceToMovable: A conformance to `Movable`.
  mutating func demandTakeValueDeclaration(
    _ k: AccessEffect, definedBy conformanceToMovable: FrontEnd.Conformance
  ) -> Function.ID {
    switch k {
    case .set:
      let d = program.ast.core.movable.moveInitialize
      return demandDeclaration(lowering: conformanceToMovable.implementations[d]!)
    case .inout:
      let d = program.ast.core.movable.moveAssign
      return demandDeclaration(lowering: conformanceToMovable.implementations[d]!)
    default:
      preconditionFailure()
    }
  }

  /// Returns the IR function implementing the copy operation defined in `conformanceToCopyable`.
  ///
  /// - Parameter conformanceToCopyable: A conformance to `Copyable`.
  mutating func demandCopyDeclaration(
    definedBy conformanceToCopyable: FrontEnd.Conformance
  ) -> Function.ID {
    let d = program.ast.core.copyable.copy
    return demandDeclaration(lowering: conformanceToCopyable.implementations[d]!)
  }

  /// Returns the IR function implementing the operator defined in `conformanceToEquatable`.
  ///
  /// - Parameter conformanceToEquatable: A conformance to `Equatable`.
  mutating func demandEqualDeclaration(
    definedBy conformanceToEquatable: FrontEnd.Conformance
  ) -> Function.ID {
    let d = program.ast.core.equatable.equal
    return demandDeclaration(lowering: conformanceToEquatable.implementations[d]!)
  }

  /// Returns a function reference to the implementation of the requirement `r` in `witness`.
  ///
  /// - Requires: `r` identifies a function or subscript requirement in the trait for which
  ///   `witness` has been established.
  mutating func reference<T: Decl>(
    toImplementationOf r: T.ID, for witness: FrontEnd.Conformance
  ) -> FunctionReference {
    let d = demandImplementation(of: r, for: witness)
    return reference(to: d, implementedFor: witness)
  }

  /// Returns a function reference to `d`, which is an implementation that's part of `witness`.
  func reference(
    to d: Function.ID, implementedFor witness: FrontEnd.Conformance
  ) -> FunctionReference {
    var a = witness.arguments
    if let m = program.traitMember(referredBy: d) {
      let r = program[m.trait.decl].receiver.id
      assert(a[r] == nil)
      a[r] = .type(witness.model)
    }
    return FunctionReference(to: d, in: self, specializedBy: a, in: witness.scope)
  }

  /// Returns a member reference to `d`, which is member of `receiver` accessed with capabilities
  /// `k`, specializing `d`'s type for `a` in `scopeOfUse`.
  mutating func memberCallee(
    referringTo d: AnyDeclID, memberOf receiver: AnyType, accessedWith k: AccessEffectSet,
    specializedBy a: GenericArguments, usedIn scopeOfUse: AnyScopeID
  ) -> Callee {
    // Check if `d`'s implementation is synthetic.
    if program.isRequirement(d) && !receiver.isSkolem {
      let t = program.traitDeclaring(d)!
      let c = program.conformance(of: receiver, to: t, exposedTo: scopeOfUse)!
      let f = demandDeclaration(lowering: c.implementations[d]!)
      return .direct(FunctionReference(to: f, in: self, specializedBy: a, in: scopeOfUse))
    } else if let m = MethodDecl.ID(d) {
      return .bundle(BundleReference(to: m, specializedBy: a, requesting: k))
    } else {
      return .direct(FunctionReference(to: d, in: &self, specializedBy: a, in: scopeOfUse))
    }
  }

  /// Returns the lowered declarations of `d`'s parameters.
  private func loweredParameters(of d: FunctionDecl.ID) -> [Parameter] {
    let declType = ArrowType(program[d].type)!
    let captures = declType.captures.lazy.map { (e) in
      program.canonical(e.type, in: program[d].scope)
    }

    var result: [Parameter] = zip(program.captures(of: d), captures).map({ (c, e) in
      if let t = RemoteType(e) {
        return Parameter(decl: c, type: ParameterType(t))
      } else {
        return Parameter(decl: c, type: ParameterType(declType.receiverEffect, e))
      }
    })

    result.append(contentsOf: program.ast[d].parameters.map(pairedWithLoweredType(parameter:)))
    return result
  }

  /// Returns the lowered declarations of `d`'s parameters.
  private func loweredParameters(of d: MethodImpl.ID) -> [Parameter] {
    let bundle = MethodDecl.ID(program[d].scope)!
    let inputs = program.ast[bundle].parameters
    let r = Parameter(AnyDeclID(program[d].receiver), capturedAs: program[d].receiver.type)
    return [r] + inputs.map(pairedWithLoweredType(parameter:))
  }

  /// Returns the lowered declarations of `d`'s parameters.
  ///
  /// `d`'s receiver comes first and is followed by `d`'s formal parameters, from left to right.
  private func loweredParameters(of d: InitializerDecl.ID) -> [Parameter] {
    var result: [Parameter] = []
    result.append(pairedWithLoweredType(parameter: program.ast[d].receiver))
    result.append(contentsOf: program.ast[d].parameters.map(pairedWithLoweredType(parameter:)))
    return result
  }

  /// Returns the lowered declarations of `d`'s parameters.
  private func loweredParameters(of d: SubscriptImpl.ID) -> [Parameter] {
    let declType = SubscriptImplType(program[d].type)!
    let captures = declType.captures.lazy.map { (e) in
      program.canonical(e.type, in: program[d].scope)
    }

    var result: [Parameter] = zip(program.captures(of: d), captures).map({ (c, e) in
      if let t = RemoteType(e) {
        return Parameter(decl: c, type: ParameterType(t))
      } else {
        return Parameter(decl: c, type: ParameterType(declType.receiverEffect, e))
      }
    })

    let bundle = SubscriptDecl.ID(program[d].scope)!
    let inputs = program.ast[bundle].parameters
    result.append(contentsOf: inputs.map(pairedWithLoweredType(parameter:)))
    return result
  }

  /// Returns `d`, which declares a parameter, paired with its lowered type.
  private func pairedWithLoweredType(parameter d: ParameterDecl.ID) -> Parameter {
    let t = program.canonical(program[d].type, in: program[d].scope)
    return .init(decl: AnyDeclID(d), type: ParameterType(t)!)
  }

  /// Appends to `inputs` the parameters corresponding to the given `captures` passed `effect`,
  /// canonicalizing theirs type in `scopeOfUse`.
  private func appendCaptures(
    _ captures: [TupleType.Element], passed effect: AccessEffect, to inputs: inout [Parameter],
    canonicalizedIn scopeOfUse: AnyScopeID
  ) {
    inputs.reserveCapacity(captures.count)
    for c in captures {
      switch program.canonical(c.type, in: scopeOfUse).base {
      case let p as RemoteType:
        precondition(p.access != .yielded, "cannot lower yielded parameter")
        inputs.append(.init(decl: nil, type: ParameterType(p)))
      case let p:
        precondition(effect != .yielded, "cannot lower yielded parameter")
        inputs.append(.init(decl: nil, type: ParameterType(effect, ^p)))
      }
    }
  }

  /// Appends `parameters` to `inputs`, canonicalizing their types in `scopeOfUse`.
  private func appendParameters(
    _ parameters: [CallableTypeParameter], to inputs: inout [Parameter],
    canonicalizedIn scopeOfUse: AnyScopeID
  ) {
    inputs.reserveCapacity(parameters.count)
    for p in parameters {
      let t = ParameterType(program.canonical(p.type, in: scopeOfUse))!
      precondition(t.access != .yielded, "cannot lower yielded parameter")
      inputs.append(.init(decl: nil, type: t))
    }
  }

  /// Returns a pointer to the witness table of `t` used in `scopeOfUse`.
  mutating func demandWitnessTable(_ t: AnyType, in scopeOfUse: AnyScopeID) -> WitnessTable {
    let cs = loweredConformances(of: t, exposedTo: scopeOfUse)
    return WitnessTable(for: t, conformingTo: cs, in: scopeOfUse)
  }

  /// Returns the lowered conformances of `model` that are exposed to `useScope`.
  private mutating func loweredConformances(
    of model: AnyType, exposedTo useScope: AnyScopeID
  ) -> Set<IR.Conformance> {
    guard let conformances = program.conformances[model] else { return [] }

    var result: Set<IR.Conformance> = []
    for concept in conformances.keys {
      let c = program.conformance(of: model, to: concept, exposedTo: useScope)!
      result.insert(loweredConformance(c))
    }
    return result
  }

  /// Returns the lowered form of `c`.
  private mutating func loweredConformance(_ c: FrontEnd.Conformance) -> IR.Conformance {
    var implementations = IR.Conformance.ImplementationMap()
    for (r, i) in c.implementations where (r.kind != AssociatedTypeDecl.self) {
      let f = demandDeclaration(lowering: i)
      implementations[r] = .function(FunctionReference(to: f, in: self))
    }
    return .init(concept: c.concept, implementations: implementations)
  }

  /// Returns a map from `f`'s generic arguments to their skolemized form.
  ///
  /// - Requires: `f` is declared in `self`.
  public func specialization(in f: Function.ID) -> GenericArguments {
    var result = GenericArguments()
    for p in functions[f]!.genericParameters {
      guard
        let t = MetatypeType(program[p].type),
        let u = GenericTypeParameterType(t.instance)
      else {
        // TODO: Handle value parameters
        UNIMPLEMENTED()
      }

      result[p] = .type(^u)
    }
    return result
  }

}
