import AST
import Basic

/// A VIL emitter.
///
/// This class is the entry point to VIL code generation phase, which lowers a type checked module
/// declaration to a VIL module.
public final class Emitter {

  /// The context in which the pass runs.
  public let context: AST.Context

  /// The VIL builder used by the emitter.
  public let builder: Builder

  /// The declaration of `self` in the context of the member function being emitted.
  private var localSelfDecl: Value?

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
    guard decl.state >= .typeChecked else { return }

    switch decl {
    case let typeDecl as NominalTypeDecl:
      // Emit the members of the declaration.
      for memberDecl in typeDecl.members {
        emit(decl: memberDecl)
      }

    case let pdDecl as PatternBindingDecl:
      if pdDecl.isMember {
        // If the decl is a stored member of a type declaration, we're done.
        if pdDecl.varDecls.allSatisfy({ decl in decl.hasStorage }) {
          return
        }

        // FIXME: Handle initializers in snythetized constructors.
      }

      // FIXME: Handle global variables.
      fatalError()

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

}
