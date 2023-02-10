import Core

extension TypedProgram {

  /// Creates an instance wrapping `syntax`, with the core module imported.
  ///
  /// - Throws: `Diagnostics` if type-checking fails.
  /// - Parameters
  ///   - inferenceTracingRange: the region in which to trace type inference, if any.
  ///   - diagnostics: a channel for errors and warnings.
  public init(
    _ syntax: AST,
    tracingInferenceIn inferenceTracingRange: SourceRange? = nil,
    diagnostics: inout Diagnostics
  ) throws {
    let fullSyntax = syntax.withCoreModule()

    var checker = TypeChecker(
      program: ScopedProgram(fullSyntax),
      isBuiltinModuleVisible: true,
      tracingInferenceIn: inferenceTracingRange)

    for m in fullSyntax.modules {
      checker.isBuiltinModuleVisible = fullSyntax[m].canAccessBuiltins
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
