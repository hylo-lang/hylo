@testable import Compiler

func identifier(_ value: String) -> SourceRepresentable<Identifier> {
  SourceRepresentable(value: value, range: nil)
}

func typeName(_ value: String) -> NameTypeExpr {
  NameTypeExpr(domain: nil, identifier: identifier(value), arguments: [])
}

extension AST {

  mutating func insertTypeName(_ name: String) -> NodeID<NameTypeExpr> {
    let names = name.split(separator: ".")
    precondition(names.count > 0)

    let base = NameTypeExpr(
      domain: nil,
      identifier: SourceRepresentable(value: String(names[0])),
      arguments: [])
    return names[1...].reduce(insert(base), { domain, next in
      insert(NameTypeExpr(
        domain: AnyTypeExprID(domain),
        identifier: SourceRepresentable(value: String(next)),
        arguments: []))
    })
  }

}
