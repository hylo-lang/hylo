import Core
import FrontEnd

/// A program lowered to Hylo IR.
public struct Program: Core.Program {

  public var ast: Core.AST { base.ast }

  public var nodeToScope: Core.ASTProperty<Core.AnyScopeID> { base.nodeToScope }

  public var scopeToDecls: Core.ASTProperty<Core.DeclIDs> { base.scopeToDecls }

  public var varToBinding: [Core.VarDecl.ID: Core.BindingDecl.ID] { base.varToBinding }

  /// The high-level form of the program.
  public let base: TypedProgram

  /// A map from module ID to its lowered form.
  public private(set) var modules: [ModuleDecl.ID: IR.Module]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [ModuleDecl.ID: IR.Module]) {
    precondition(modules.values.elementCount(where: { $0.entryFunction != nil }) <= 1)
    self.base = syntax
    self.modules = modules
  }

  /// The identity of the entry module.
  public var entry: ModuleDecl.ID? {
    modules.first(where: { $0.value.entryFunction != nil })?.key
  }

  /// Applies `p` to the modules in `self`.
  public mutating func applyPass(_ p: ModulePass) {
    for k in ast.modules {
      modules[k]!.applyPass(p, in: self)
    }
  }

  /// Returns the module containing the unique definition of `f`.
  public func module(defining f: Function.ID) -> ModuleDecl.ID {
    switch f.value {
    case .lowered(let d):
      return base.module(containing: base[d].scope)
    case .monomorphized:
      UNIMPLEMENTED()
    case .existentialized(let i):
      return module(defining: i)
    case .synthesized(let d):
      return base.module(containing: d.scope)
    }
  }

}
