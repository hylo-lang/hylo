import AST
import Basic

extension Diag {

  static func bindingWithNoUse(decl: ValueDecl) -> Diag {
    return Diag("'\(decl.name)' is never used", level: .warning, anchor: decl.range)
  }

  static func useBeforeInit(location: Value, anchor: SourceRange?) -> Diag {
    if let decl = (location as? AllocStackInst)?.decl {
      return Diag("'\(decl.name)' is used before being initialized", anchor: anchor)
    } else {
      return Diag("use before initialization at \(location)", anchor: anchor)
    }
  }

  static func useAfterMove(location: Value, consumer: Inst) -> Diag {
    return Diag("use after move")
  }

  static func partiallyMovedCopy(location: Value, consumer: Inst) -> Diag {
    return Diag("cannot copy: value is partially moved")
  }

  static func partiallyMovedDrop(location: Value, consumer: Inst) -> Diag {
    return Diag("cannot drop: value is partially moved")
  }

}
