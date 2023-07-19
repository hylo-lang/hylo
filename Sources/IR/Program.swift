import Core

/// A program lowered to Val IR.
public struct Program: Core.Program {
  public var ast: Core.AST { base.ast }

  public var nodeToScope: Core.ASTProperty<Core.AnyScopeID> { base.nodeToScope }

  public var scopeToDecls: Core.ASTProperty<[Core.AnyDeclID]> { base.scopeToDecls }

  public var varToBinding: [Core.VarDecl.ID: Core.BindingDecl.ID] { base.varToBinding }

  /// The high-level form of the program.
  public let base: TypedProgram

  /// A map from module ID to its lowered form.
  public private(set) var modules: [ModuleDecl.ID: IR.ModuleUnderConstruction]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [ModuleDecl.ID: IR.ModuleUnderConstruction]) {
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
    case .loweredSubscript(let d):
      return base.module(containing: base[d].scope)
    case .monomorphized:
      fatalError("not implemented")
    case .existentialized(let i):
      return module(defining: i)
    case .synthesized:
      fatalError("not implemented")
    }
  }

  // MARK: Mangling

  /// Returns the mangled name of `t`.
  public func mangle(_ t: AnyType) -> String {
    switch t.base {
    case let u as ProductType:
      return mangle(u)
    case let u as TraitType:
      return mangle(u)
    default:
      fatalError()
    }
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: ProductType) -> String {
    base.debugName(decl: t.decl)
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: TraitType) -> String {
    base.debugName(decl: t.decl)
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: WitnessTable) -> String {
    var result = "__VWT" + mangle(t.witness)
    for c in t.conformances {
      result.append(mangle(^c.concept))
      result.append(String(describing: c.source.rawValue))
    }
    return result
  }

  /// Returns the mangled name of `f`.
  public func mangle(_ f: Function.ID) -> String {
    switch f.value {
    case .lowered(let d):
      return mangle(d)
    case .loweredSubscript(let d):
      return mangle(d)
    case .monomorphized(let f, let a):
      return mangle(f) + "<\(list: a.values)>"
    case .synthesized(let d, let t):
      return "\(d)\(t)"
    case .existentialized:
      fatalError("not implemented")
    }
  }

  /// Returns the mangled name of `f`.
  public func mangle<T: DeclID>(_ d: T) -> String {
    if let f = FunctionDecl.ID(d) {
      return ast[f].llvmName ?? base.debugName(decl: f)
    } else {
      return base.debugName(decl: d)
    }
  }

}
