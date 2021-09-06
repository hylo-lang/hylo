import AST
import Basic

extension Diag {

  static func bindingWithNoUse(decl: ValueDecl) -> Diag {
    return Diag("'\(decl.name)' was never used", level: .warning, anchor: decl.range)
  }

  static func useBeforeInit(alloc: AllocStackInst, anchor: SourceRange?) -> Diag {
    if let decl = alloc.decl {
      return Diag("'\(decl.name)' is used before being initialized", anchor: anchor)
    } else {
      return Diag("use before initialization at \(alloc)", anchor: anchor)
    }
  }

}
