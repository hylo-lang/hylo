import FrontEnd
import OrderedCollections
import Utils

extension IR.Program {

  /// Generates the non-parametric reslient APIs of the modules in `self`.
  public mutating func depolymorphize() {
    for m in modules.keys {
      depolymorphize(m)
    }
  }

  /// Generates the non-parametric resilient API of `m`.
  public mutating func depolymorphize(_ m: Module.ID) {
    for k in modules[m]!.functions.keys {
      let f = modules[m]!.functions[k]!

      // Ignore declarations without definition.
      if f.entry == nil { continue }

      // All non-generic functions are deploymorphized.
      if !f.isGeneric {
        depolymorphize(k, definedIn: m)
        continue
      }

      // Public generic functions are existentialized.
      if f.linkage == .external {
        _ = existentialize(k)
      }
    }
  }

  /// Replaces uses of parametric types and functions in `f` with their monomorphic or existential
  /// counterparts.
  private mutating func depolymorphize(_ f: Function.ID, definedIn m: Module.ID) {
    for i in modules[m]![f].instructionIDs {
      switch modules[m]![i, in: f] {
      case is Call:
        depolymorphize(call: i, from: f, definedIn: m)
      case is Project:
        depolymorphize(project: i, from: f, definedIn: m)
      default:
        continue
      }
    }
  }

  /// Iff `i` is a call to a generic function, replaces it by an instruction applying a
  /// depolymorphized version of its callee.
  ///
  /// - Requires: `i` identifies a `CallInstruction`
  private mutating func depolymorphize(
    call i: InstructionID, from f: Function.ID, definedIn m: Module.ID
  ) {
    let s = modules[m]![i, in: f] as! Call
    guard
      let callee = s.callee.constant as? FunctionReference,
      !callee.specialization.isEmpty
    else { return }

    // TODO: Use existentialization unless the function is inlinable

    let g = monomorphize(callee, usedIn: modules[m]![f].scope(containing: i))
    let r = FunctionReference(to: g, in: modules[m]!)
    let new = modules[m]![f].makeCall(
      applying: .constant(r), to: Array(s.arguments), writingResultTo: s.output, at: s.site)
    modules[m]![f].replace(i, with: new)
  }

  /// Iff `i` is the projection through a generic subscript, replaces it by an instruction applying
  /// a depolymorphized version of its callee.
  ///
  /// - Requires: `i` identifies a `ProjectInstruction`
  private mutating func depolymorphize(
    project i: InstructionID, from f: Function.ID, definedIn m: Module.ID
  ) {
    let s = modules[m]![i, in: f] as! Project
    let r = s.functionReference
    guard !r.specialization.isEmpty else { return }

    // TODO: Use existentialization unless the subscript is inlinable

    let scope = modules[m]![f].scope(containing: i)
    let z = base.canonical(r.specialization, in: scope)
    let g = monomorphize(r.function, for: z, usedIn: scope)
    let new = modules[m]![f].makeProject(
      s.projection,
      applying: FunctionReference(to: g, in: modules[m]!, specializedBy: .empty, in: scope),
      to: Array(s.arguments), at: s.site)
    modules[m]![f].replace(i, with: new)
  }

  /// Returns a depolymorphized copy of `base` in which parametric parameters have been notionally
  /// replaced by parameters accepting existentials.
  ///
  /// The returned function takes `n` additional parameters where `n` is the length of `arguments`.
  /// For example, assume `base` is defined as the generic function below, which takes two generic
  /// parameters:
  ///
  ///      fun foo<T: P, s: Int>(a: T, b: T[s]) -> T
  ///
  /// Its existentialized form is a function:
  ///
  ///      fun foo_e(a: RawPointer, b, RawPointer, T: WitnessTable, s: Int) -> RawPointer
  ///
  /// The pair `(a, T)` is a notional existential container representing the first argument of the
  /// parametric function. The triple `(a, T, s)` represents the second argument.
  private mutating func existentialize(_ base: Function.ID) -> Function.ID {
    // TODO: Implement me
    return base
  }

  /// Returns the monomorphized form of `r` for use in `scopeOfUse`, reading definitions from `ir`.
  private mutating func monomorphize(
    _ r: FunctionReference, usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    monomorphize(r.function, for: r.specialization, usedIn: scopeOfUse)
  }

  /// Returns a reference to the monomorphized form of `f` for specialization `z` in `scopeOfUse`,
  /// reading definitions from `ir`.
  fileprivate mutating func monomorphize(
    _ f: Function.ID, for z: GenericArguments, usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    precondition(z.allSatisfy(\.value.isCanonical))

    let result = demandMonomorphizedDeclaration(of: f, for: z, usedIn: scopeOfUse)

    let target = base.module(containing: scopeOfUse)
    if modules[target]![result].entry != nil {
      return result
    }

    // First, collect the types and function references used in the function.
    let source = module(defining: f)
    var observer = MonomorphizationObserver()
    var ff = modules[source]![f]
    ff.observe(ff.instructionIDs, with: &observer)

    // Monomorphize the types.
    var transformedTypes: [AnyType: AnyType] = [:]
    for t in observer.types {
      transformedTypes[t] = monomorphize(t, for: z, usedIn: scopeOfUse)
    }

    // Monomorphize the function references.
    var transformedFunctions: [FunctionReference: FunctionReference] = [:]
    for c in observer.functionsToMonomorphize {
      let s = base.module(containing: scopeOfUse)
      let f = monomorphize(c.function, specializedBy: c.specialization, for: z, usedIn: scopeOfUse)
      transformedFunctions[c] = FunctionReference(to: f, in: modules[s]!)
    }

    var rewrittenBlock: [Block.ID: Block.ID] = [:]
    for b in modules[source]![f].blockIDs {
      rewrittenBlock[b] = modules[target]![result].appendBlock(in: modules[source]![b, in: f].scope)
    }

    // Transform `source`, monomorphizing it, and write the result to `result`.
    var monomorphizer = Monomorphizer(
      scopeOfUse: scopeOfUse,
      transformedTypes: transformedTypes, transformedFunctions: transformedFunctions,
      rewrittenBlock: rewrittenBlock
    )
    modules[target]![result].rewrite(
      f, from: modules[source]!, transformedBy: &monomorphizer,
      rewrittenBlock: rewrittenBlock,
      rewrittenGenericValue: modules[target]!.defineGenericValueArguments(z, in: result)
    )

    return result
  }

  /// Returns a monomorphized copy of `f` specialized by `z` for use in `scopeOfUse`.
  ///
  /// If `f` is a trait requirement, the result is a monomorphized version of that requirement's
  /// implementation, using `a` to identify the requirement's receiver. Otherwise, the result is
  /// a monomorphized copy of `f`.
  private mutating func monomorphize(
    _ f: Function.ID, specializedBy z: GenericArguments, for specialization: GenericArguments,
    usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let p = base.specialize(z, for: specialization, in: scopeOfUse)
    let q = base.canonical(p, in: scopeOfUse)
    if let m = base.requirementDeclaring(memberReferredBy: f) {
      return monomorphize(m.decl, requiredBy: m.trait, for: q, usedIn: scopeOfUse)
    } else {
      return monomorphize(f, for: q, usedIn: scopeOfUse)
    }
  }

  /// Returns a reference to the monomorphized form of requirement `r` for `specialization` in
  /// `scopeOfUse`, reading definitions from `ir`.
  fileprivate mutating func monomorphize(
    _ r: AnyDeclID, requiredBy trait: TraitType,
    for specialization: GenericArguments, usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let receiver = base[trait.decl].receiver.id
    let model = specialization[receiver]!.asType!

    guard let c = base.conformance(of: model, to: trait, exposedTo: scopeOfUse) else {
      fatalError("expected '\(model)' to conform to '\(trait)'")
    }

    let s = base.module(containing: scopeOfUse)
    let i = c.implementations[r]!
    let d = modules[s]!.demandDeclaration(lowering: i)

    // Nothing to do if the implementation isn't generic.
    if modules[s]![d].genericParameters.isEmpty {
      return d
    }

    // Otherwise, the generic arguments of the implementation are supplied by the conformance,
    // except for the argument to the receiver parameter. This parameter may be associated with
    // a trait other the one declaring the requirement if the implementation is in an extension.
    var monomorphizationArguments = c.arguments
    if case .explicit(let d) = i, let t = base.traitDeclaring(d), t != trait {
      monomorphizationArguments[base[t.decl].receiver] = .type(model)
    } else {
      monomorphizationArguments[receiver] = .type(model)
    }

    return monomorphize(d, for: monomorphizationArguments, usedIn: scopeOfUse)
  }

  /// Returns the canonical form of `t`, specialized for specialization `z` in `scopeOfUse`.
  fileprivate func monomorphize(
    _ t: AnyType, for z: GenericArguments, usedIn scopeOfUse: AnyScopeID
  ) -> AnyType {
    let u = base.specialize(t, for: z, in: scopeOfUse)
    return base.canonical(u, in: scopeOfUse)
  }

  /// Returns the IR function monomorphizing `f` for specialization `z` in `scopeOfUse`.
  private mutating func demandMonomorphizedDeclaration(
    of f: Function.ID, for z: GenericArguments, usedIn scopeOfUse: AnyScopeID
  ) -> Function.ID {
    let source = modules[module(defining: f)]![f]
    let target = base.module(containing: scopeOfUse)

    precondition(!source.genericParameters.isEmpty, "function is not generic")
    precondition(
      source.genericParameters.allSatisfy({ z[$0] != nil }),
      "incomplete monomorphization arguments")

    let result = Function.ID(monomorphized: f, for: z)
    if modules[target]!.functions[result] != nil {
      return result
    }

    let inputs = source.inputs.map { (p) in
      let t = monomorphize(p.type.bareType, for: z, usedIn: scopeOfUse)
      return Parameter(decl: p.decl, type: ParameterType(p.type.access, t))
    }
    let output = monomorphize(source.output, for: z, usedIn: scopeOfUse)
    let entity = Function(
      isSubscript: source.isSubscript,
      site: source.site,
      linkage: .module,
      genericParameters: [],
      inputs: inputs,
      output: output,
      blocks: [])
    modules[target]!.addFunction(entity, for: result)
    return result
  }

}

extension Module {

  /// Allocates and initializes storage for each generic value argument in `specialization` in the
  /// entry of `monomorphized`, returning a map from a generic value parameter to its corresponding
  /// allocation that preserves the order of `specialization`.
  fileprivate mutating func defineGenericValueArguments(
    _ specialization: GenericArguments,
    in monomorphized: Function.ID
  ) -> OrderedDictionary<GenericParameterDecl.ID, InstructionID> {
    let insertionSite = SourceRange.empty(at: self[monomorphized].site.start)
    let entry = self[monomorphized].entry!

    var genericValues = OrderedDictionary<GenericParameterDecl.ID, InstructionID>()

    for (k, v) in specialization {
      if v.asType != nil { continue }

      guard let w = v.asCompilerKnown(Int.self) else {
        UNIMPLEMENTED("arbitrary compile-time values")
      }

      let s = self[monomorphized].makeAllocStack(
        ^program.ast.coreType("Int")!, at: insertionSite, insertingAt: .end(of: entry))

      var log = DiagnosticSet()
      Emitter.withInstance(insertingIn: &self, reportingDiagnosticsTo: &log) { (e) in
        e.insertionFunction = monomorphized
        e.insertionPoint = .end(of: entry)
        e.lowering(at: insertionSite) { e in
          e._emitStore(int: w, to: .register(s))
        }
      }
      assert(log.isEmpty)

      genericValues[k] = s
    }

    return genericValues
  }

}

extension Function {
  /// Inserts a copy of `i`, which is in `source`, inside `self`, transforming its parts with
  /// `t`, having block correspondences in `rewrittenBlock`, and generic value correspondences in
  /// `rewrittenGenericValue`.
  fileprivate mutating func rewrite(
    _ f: Function.ID, from m: Module, transformedBy t: inout Monomorphizer,
    rewrittenBlock: [Block.ID: Block.ID],
    rewrittenGenericValue: OrderedDictionary<GenericParameterDecl.ID, InstructionID>
  ) {
    // Iterate over the basic blocks of the source function in a way that guarantees we always
    // visit definitions before their uses.
    let cfg = m[f].cfg()
    let sourceBlocks = DominatorTree(function: m[f], cfg: cfg).bfs
    for b in sourceBlocks {
      let s = Block.ID(b)
      let t = rewrittenBlock[s]!

      for i in m[f].instructions(in: s) {
        switch m[i, in: f] {
        case is GenericParameter:
          rewrite(genericParameter: i)
        case is Return:
          rewrite(return: i, to: t)
        default:
          rewrite(i, to: t)
        }
      }
    }

    /// Rewrites `i`, which is in `source`, at the end of `b`, which is in `self`.
    func rewrite(_ i: InstructionID, to b: Block.ID) {
      let j = self.rewrite(i, in: m[f], transformedBy: &t, at: .end(of: b))
      t.rewrittenInstruction[i] = j
    }

    /// Rewrites `i`, which is in `source`, at the end of `b`, which is in `self`.
    func rewrite(genericParameter i: InstructionID) {
      let s = m[i, in: f] as! GenericParameter
      t.rewrittenInstruction[i] = rewrittenGenericValue[s.parameter]!
    }

    /// Rewrites `i`, which is in `source`, at the end of `b`, which is in `self`.
    func rewrite(return i: InstructionID, to b: Block.ID) {
      let s = m[i, in: f] as! Return
      for i in rewrittenGenericValue.values.reversed() {
        _ = makeDeallocStack(for: .register(i), at: s.site, insertingAt: .end(of: b))
      }
      t.rewrittenInstruction[i] = makeReturn(at: s.site, insertingAt: .end(of: b))
    }
  }

}

/// Observer that collects the types and function references that need monomorphization.
private struct MonomorphizationObserver: InstructionObserver {

  /// The used types, which might be changed by monomorphization.
  private(set) var types: Set<AnyType> = []

  /// The set of functions that need monomorphization.
  private(set) var functionsToMonomorphize: Set<FunctionReference> = []

  /// Collect `t` for possible monomorphization.
  mutating func observe(_ t: AnyType) {
    types.insert(t)
  }

  /// Checks monomorphization functions/types in `c`.
  mutating func observe(_ o: Operand) {
    if let c = o.constant {
      observe(c)
    }
  }

  /// Nothing to observe for a block.
  func observe(_ b: Block.ID) {}

  /// Collects monomorphization functions/types in `c`.
  private mutating func observe(_ c: any Constant) {
    switch c {
    case let r as FunctionReference:
      observe(r)
      break
    case let t as MetatypeType:
      observe(^t)
      break
    default:
      break
    }
  }

  /// Collects `c` if it needs to be monomorphized.
  private mutating func observe(_ c: FunctionReference) {
    // Unspecialized references cannot refer to trait members, which are specialized for the
    // implicit `Self` parameter.
    if !c.specialization.isEmpty {
      functionsToMonomorphize.insert(c)
    }
  }

}

/// The monomorphization of a function.
private struct Monomorphizer: InstructionTransformer {

  /// The scope in which instructions are monomorphized.
  let scopeOfUse: AnyScopeID

  /// The transformed types that should be used in the monomorphized function.
  var transformedTypes: [AnyType: AnyType] = [:]

  /// The transformed function references that should be used in the monomorphized function.
  var transformedFunctions: [FunctionReference: FunctionReference] = [:]

  /// A map from basic block in `source` to its corresponding block in `result`.
  let rewrittenBlock: [Block.ID: Block.ID]

  /// A map from instruction in `source` to its corresponding instruction in `result`.
  var rewrittenInstruction: [InstructionID: InstructionID] = [:]

  /// Returns the canonical, monomorphized form of `t`.
  func transform(_ t: AnyType) -> AnyType {
    precondition(
      transformedTypes[t] != nil, "Type \(t) was not transformed; check if it was properly observed"
    )
    return transformedTypes[t]!
  }

  /// Returns a monomorphized copy of `o`.
  func transform(_ o: Operand) -> Operand {
    switch o {
    case .constant(let c):
      return .constant(transform(c))
    case .parameter(let b, let i):
      return .parameter(rewrittenBlock[b]!, i)
    case .register(let s):
      return .register(rewrittenInstruction[s]!)
    }
  }

  /// Returns a monomorphized copy of `b`.
  func transform(_ b: Block.ID) -> Block.ID {
    rewrittenBlock[b]!
  }

  /// Returns a monomorphized copy of `c`.
  private func transform(_ c: any Constant) -> any Constant {
    switch c {
    case let r as FunctionReference:
      return transform(r)
    case let t as MetatypeType:
      return MetatypeType(transform(^t))!
    default:
      return c
    }
  }

  /// Returns a monomorphized copy of `c`.
  private func transform(_ c: FunctionReference) -> FunctionReference {
    // Unspecialized references cannot refer to trait members, which are specialized for the
    // implicit `Self` parameter.
    if c.specialization.isEmpty {
      return c
    }

    return transformedFunctions[c]!
  }

}
