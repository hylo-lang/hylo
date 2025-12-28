import FrontEnd

extension IR.Program {

  /// Lower all projections in `self` into ramp + slide functions.
  public mutating func lowerProjections() {
    for m in modules.values {
      lowerProjections(in: m)
    }
  }

  /// Lowers the projections in `m` into a pair of ramp+slide functions.
  private mutating func lowerProjections(in m: Module) {
    for f in m.functions.keys where m[f].isSubscript {
      guard !m[f].blockIDs.isEmpty else { continue }
      guard let p = m.projectionSkeletons[f] else {
        fatalError("missing projection skeleton for projection function \(m.debugDescription(f))")
      }
      lowerProjection(f, skeleton: p, in: m)
    }
  }

  /// Lowers the projection function `f` with skeleton `skeleton` in module `m`.
  private mutating func lowerProjection(
    _ f: Function.ID, skeleton s: IR.ProjectionSkeleton, in m: Module
  ) {
    // print("Lowering projection @\(m[f].site):  \(m.debugDescription(f))")
    // if m[f].site.file.baseName != "projection" { return }  // TODO: remove this filter.
    generateRamp(for: f, skeleton: s, in: m.id)
    generateSlide(for: f, skeleton: s, in: m.id)
    // TODO: delete old function
  }

  /// Generate ramp function corresponding to `f`, with skeleton `s`, in `m`.
  private mutating func generateRamp(for f: Function.ID, skeleton s: ProjectionSkeleton, in m: Module.ID) {
    let r = modules[m]!.demandRampDeclaration(for: f)
    // TODO
    generateEmptyBody(for: r, copying: f, in: m)
  }

  /// Generate slide function corresponding to `f`, with skeleton `s`, in `m`.
  private mutating func generateSlide(for f: Function.ID, skeleton s: ProjectionSkeleton, in m: Module.ID) {
    let s = modules[m]!.demandSlideDeclaration(for: f)
    // TODO
    generateEmptyBody(for: s, copying: f, in: m)
  }

  // TODO: remove this
  private mutating func generateEmptyBody(for f: Function.ID, copying g: Function.ID, in m: Module.ID) {
    precondition(f != g)
    let source = modules[m]![g]
    let b = modules[m]![f].appendBlock(in: source[source.entry!].scope)
    var ds = DiagnosticSet()
    Emitter.withInstance(insertingIn: &modules[m]!, reportingDiagnosticsTo: &ds) { (e) in
      e.insertionFunction = f
      e.insertionPoint = .end(of: b)
      e._return()
    }
  }

}

extension IR.Module {

  /// Returns the IR function representing the ramp of projection `f`.
  ///
  /// Signature:
  /// > fun Projection.ramp(let Continuation, <parameters>, set <yield-type>) -> Continuation
  fileprivate mutating func demandRampDeclaration(for f: Function.ID) -> Function.ID {
    let result = Function.ID(projectionRamp: f)
    if functions[result] != nil {
      return result
    }

    let source = self[f]
    var inputs = source.inputs
    let c = continuationType()
    inputs.insert(Parameter(decl: nil, type: ParameterType(.`let`, c)), at: 0)
    inputs.append(Parameter(decl: nil, type: ParameterType(.`set`, source.output)))

    let entity = Function(
      isSubscript: false,
      site: source.site,
      linkage: .module,
      genericParameters: source.genericParameters,
      inputs: inputs,
      output: c,
      blocks: [])
    addFunction(entity, for: result)
    return result
  }

  /// Returns the IR function representing the slide of projection `f`.
  ///
  /// Signature:
  /// > fun Projection.slide(let Builtin.ptr, let Continuation) -> {}
  fileprivate mutating func demandSlideDeclaration(for f: Function.ID) -> Function.ID {
    let result = Function.ID(projectionSlide: f)
    if self.functions[result] != nil {
      return result
    }

    let source = self[f]
    let inputs: [Parameter] = [
      Parameter(decl: nil, type: ParameterType(.`let`, AnyType(BuiltinType.ptr))),
      Parameter(decl: nil, type: ParameterType(.`let`, continuationType()))
    ]

    let entity = Function(
      isSubscript: false,
      site: source.site,
      linkage: .module,
      genericParameters: source.genericParameters,
      inputs: inputs,
      output: .void,
      blocks: [])
    addFunction(entity, for: result)
    return result
  }

  /// Returns the type of a continuation.
  private func continuationType() -> AnyType {
    let t = AnyType(BuiltinType.ptr)
    return AnyType(TupleType([
      TupleType.Element(label: "resumeFunction", type: t),
      TupleType.Element(label: "stackBase", type: t)
    ]))
  }

}