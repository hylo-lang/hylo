import Core

extension TypedProgram {

  /// Creates an instance wrapping `syntax`.
  ///
  /// - Throws: `Diagnostics` if type-checking fails.
  /// - Parameters
  ///   - inferenceTracingRange: the region in which to trace type inference, if any.
  ///   - diagnostics: a channel for errors and warnings.
  /// - Precondition: `syntax` includes the core module.
  public init(
    _ syntax: AST,
    tracingInferenceIn inferenceTracingRange: SourceRange? = nil,
    diagnostics: inout Diagnostics
  ) throws {
    precondition(syntax.isCoreModuleLoaded, "TypedProgram: No core module in AST.")

    var checker = TypeChecker(
      program: ScopedProgram(syntax),
      isBuiltinModuleVisible: true,
      tracingInferenceIn: inferenceTracingRange)

    for m in syntax.modules {
      checker.isBuiltinModuleVisible = syntax[m].canAccessBuiltins
      checker.check(module: m)
    }

    diagnostics.report(checker.diagnostics)
    try diagnostics.throwOnError()

    self = .init(
      annotating: checker.program,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      implicitCaptures: checker.implicitCaptures,
      referredDecls: checker.referredDecls,
      foldedSequenceExprs: checker.foldedSequenceExprs)
  }

}
