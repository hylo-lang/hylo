import Core

/// A program lowered to Val IR.
public struct Program: Core.Program {
  public var ast: Core.AST { syntax.ast }

  public var nodeToScope: Core.ASTProperty<Core.AnyScopeID> { syntax.nodeToScope }

  public var scopeToDecls: Core.ASTProperty<[Core.AnyDeclID]> { syntax.scopeToDecls }

  public var varToBinding: [Core.VarDecl.ID: Core.BindingDecl.ID] { syntax.varToBinding }

  /// The high-level form of the program.
  public let syntax: TypedProgram

  /// A map from module ID to its lowered form.
  public private(set) var modules: [ModuleDecl.ID: IR.Module]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [ModuleDecl.ID: IR.Module]) {
    precondition(modules.values.elementCount(where: { $0.entryFunction != nil }) <= 1)
    self.syntax = syntax
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
      return syntax.module(containing: syntax[d].scope)
    case .loweredSubscript(let d):
      return syntax.module(containing: syntax[d].scope)
    case .monomorphized:
      fatalError("not implemented")
    case .existentialized(let i):
      return module(defining: i)
    case .synthesized:
      fatalError("not implemented")
    }
  }

}
