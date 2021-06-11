import AST
import Basic

/// A VIL declaration emitter.
///
/// This class is the entry point to VIL code generation phase, which lowers a type checked module
/// declaration to a VIL module.
///
/// - Important: Do not emit VIL code for unchecked or ill-formed ASTs. The emitter assumes that
///   all declarations are successfully well-typed; its behavior is undefined otherwise.
public final class Emitter: DeclVisitor {

  public typealias DeclResult = Void

  /// The context in which the pass runs.
  public let context: AST.Context

  /// The VIL builder used by the emitter.
  public let builder: Builder

  /// Creates a new declaration emitter.
  ///
  /// - Parameters:
  ///   - context: The context in which the declarations are being emitted.
  ///   - builder: The builder being used to generate VIL code.
  public init(context: AST.Context, builder: Builder) {
    self.context = context
    self.builder = builder
  }

  public func visit(_ decl: ModuleDecl) {
    decl.forEach({ $0.accept(self) })
  }

  public func visit(_ decl: BaseFunDecl) {
    FunctionEmitter(parent: self, funDecl: decl).emit()
  }

  public func visit(_ decl: FunDecl) {
    visit(decl as BaseFunDecl)
  }

  public func visit(_ decl: CtorDecl) {
    visit(decl as BaseFunDecl)
  }

  public func visit(_ decl: ViewTypeDecl) {
    // Views are abstract constructs; there's nothing to emit.
  }

  public func visit(_ decl: ProductTypeDecl) {
    // Emit the the type's witness table(s).
    for conformance in decl.conformanceTable.values {
      var entries: [(decl: BaseFunDecl, impl: Function)] = []
      for (req, impl) in conformance.entries {
        if let reqFunDecl = req as? BaseFunDecl {
          let implFunDecl = impl as! BaseFunDecl
          let function = emit(witness: implFunDecl, for: reqFunDecl)
          entries.append((reqFunDecl, function))
        }
      }

      let table = WitnessTable(
        type: decl.instanceType as! NominalType, view: conformance.viewType, entries: entries)
      builder.module.witnessTables.append(table)
    }

    // Emit the direct members of the declaration.
    decl.members.forEach({ $0.accept(self) })
  }

  public func visit(_ decl: TypeExtDecl) {
    decl.members.forEach({ $0.accept(self) })
  }

  public func visit(_ node: AbstractTypeDecl) -> Void {
    fatalError("not implemented")
  }

  public func visit(_ decl: AliasTypeDecl) {
    // FIXME: Emit the type's witness table.
  }

  public func visit(_ decl: PatternBindingDecl) {
    if decl.isMember {
      // If the decl is a stored member of a type declaration, we're done.
      if decl.varDecls[0].hasStorage { return }

      // FIXME: Handle initializers in synthetized constructors.
    }

    // FIXME: Handle global variables.
    fatalError("not implemented")
  }

  public func visit(_ decl: ImportDecl) {
    fatalError("not implemented")
  }

  public func visit(_ decl: VarDecl) {
    fatalError("unreachable")
  }

  public func visit(_ decl: FunParamDecl) {
    fatalError("unreachable")
  }

  public func visit(_ decl: GenericTypeDecl) {
    fatalError("unreachable")
  }

  public func visit(_ decl: NominalTypeDecl) {
    fatalError("unreachable")
  }

  public func visit(_ decl: GenericParamDecl) {
    fatalError("unreachable")
  }

  /// Emits a function wrapping the implementation satisfying a conformance requirement.
  ///
  /// - Parameters:
  ///   - impl: The declaration of the function that implements the requirement.
  ///   - req: The declaration of the function requirement.
  private func emit(witness impl: BaseFunDecl, for req: BaseFunDecl) -> Function {
    // Create the VIL function object.
    var mangler = Mangler()
    mangler.append(witnessImpl: impl, for: req)
    let name = mangler.finalize()
    let function = builder.getOrCreateFunction(name: name, type: req.unappliedType as! FunType)

    // Create the function's entry point.
    var args = function.type.paramTypes.map({ type -> Value in
      return ArgumentValue(type: type, function: function)
    })
    builder.block = function.createBasicBlock(arguments: args)

    // Emit the function's body.
    if !(req is CtorDecl) {
      // Unless the function is a constructor, we have to open the self parameter.
      let openedSelfType = impl.selfDecl!.type
      if req.isMutating {
        args[0] = builder.buildOpenExistentialAddr(container: args[0], type: .lower(openedSelfType))

        if !impl.isMutating {
          args[0] = builder.buildLoad(lvalue: args[0])
        }
      } else {
        assert(!impl.isMutating)
        args[0] = builder.buildOpenExistential(container: args[0], type: .lower(openedSelfType))
      }
    }

    let openedFun = builder.getOrCreateFunction(from: impl)
    let ret = builder.buildApply(fun: FunRef(function: openedFun), args: args)
    builder.buildRet(value: ret)

    return function
  }

}
