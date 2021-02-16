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
    for decl in moduleDecl.decls {
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
    }

    // Emit the members of the declaration.
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

      // FIXME: Handle initializers in snythetized constructors.
    }

    // FIXME: Handle global variables.
    fatalError()
  }

}
