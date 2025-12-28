import FrontEnd

/// A program lowered to Hylo IR.
public struct Program: FrontEnd.Program {

  public var ast: FrontEnd.AST { base.ast }

  public var nodeToScope: ASTProperty<AnyScopeID> { base.nodeToScope }

  public var scopeToDecls: ASTProperty<DeclIDs> { base.scopeToDecls }

  public var varToBinding: [VarDecl.ID: BindingDecl.ID] { base.varToBinding }

  /// The high-level form of the program.
  public let base: TypedProgram

  /// A map from module ID to its lowered form.
  public var modules: [Module.ID: IR.Module]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [Module.ID: IR.Module]) {
    precondition(modules.values.elementCount(where: { $0.entryFunction != nil }) <= 1)
    self.base = syntax
    self.modules = modules
  }

  /// The identity of the entry module.
  public var entry: Module.ID? {
    modules.first(where: { $0.value.entryFunction != nil })?.key
  }

  /// Applies `p` to the modules in `self`.
  public mutating func applyPass(_ p: ModulePass) {
    switch p {
    case .depolymorphize:
      depolymorphize()
    case .inline:
      inlineCalls(where: .hasNoControlFlow)
    }
  }

  /// Returns the module containing the unique definition of `f`.
  public func module(defining f: Function.ID) -> Module.ID {
    switch f.value {
    case .lowered(let d):
      return base.module(containing: base[d].scope)
    case .monomorphized:
      UNIMPLEMENTED()
    case .existentialized(let i):
      return module(defining: i)
    case .synthesized(let d):
      return base.module(containing: d.scope)
    case .projectionRamp(let b):
      return module(defining: b)
    case .projectionSlide(let b):
      return module(defining: b)
    }
  }

}
