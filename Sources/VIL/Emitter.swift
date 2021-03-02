import AST
import Basic

/// A VIL emitter.
///
/// This class is the entry point to VIL code generation phase, which lowers a type checked module
/// declaration to a VIL module.
///
/// - Important: Do not emit VIL code for unchecked or ill-formed ASTs. The emitter assumes that
///   all declarations are successfully well-typed; its behavior is undefined otherwise.
public final class Emitter {

  /// The context in which the pass runs.
  public let context: AST.Context

  /// The VIL builder used by the emitter.
  public let builder: Builder

  public init(context: AST.Context, builder: Builder) {
    self.context = context
    self.builder = builder
  }

  public func emit(moduleDecl: ModuleDecl) {
    for decl in moduleDecl {
      emit(decl: decl)
    }
  }

  func emit(decl: Decl) {
    switch decl {
    case is ViewTypeDecl:
      // Views are abstract constructs; there's nothing to emit.
      return

    case let typeDecl as ProductTypeDecl:
      emit(decl: typeDecl)

    case let pdDecl as PatternBindingDecl:
      emit(decl: pdDecl)

    case let funDecl as BaseFunDecl:
      // Emit a function.
      let functionEmitter = FunctionEmitter(parent: self, funDecl: funDecl)
      functionEmitter.emit()

    case let moduleDecl as ModuleDecl:
      emit(moduleDecl: moduleDecl)

    default:
      fatalError("I don't know how to emit '\(decl)'")
    }
  }

  func emit(decl: ProductTypeDecl) {
    // Emit the the type's witness table(s).
    for conformance in decl.conformanceTable.values {
      var entries: [(decl: BaseFunDecl, impl: Function)] = []
      for (req, impl) in conformance.entries {
        if let reqFunDecl = req as? BaseFunDecl {
          let implFunDecl = impl as! BaseFunDecl
          let function = emit(witness: implFunDecl, forReq: reqFunDecl)
          entries.append((reqFunDecl, function))
        }
      }

      let table = WitnessTable(
        type: decl.instanceType as! NominalType, view: conformance.viewType, entries: entries)
      builder.module.witnessTables.append(table)
    }

    // Emit the direct members of the declaration.
    for memberDecl in decl.members {
      emit(decl: memberDecl)
    }
  }

  func emit(decl: PatternBindingDecl) {
    if decl.isMember {
      // If the decl is a stored member of a type declaration, we're done.
      if decl.varDecls.allSatisfy({ varDecl in varDecl.hasStorage }) {
        return
      }

      // FIXME: Handle initializers in synthetized constructors.
    }

    // FIXME: Handle global variables.
    fatalError()
  }

  /// Emits a function wrapping the implementation satisfying a conformance requirement.
  ///
  /// - Parameters:
  ///   - impl: The declaration of the function that implements the requirement.
  ///   - req: The declaration of the function requirement.
  private func emit(witness impl: BaseFunDecl, forReq req: BaseFunDecl) -> Function {
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
