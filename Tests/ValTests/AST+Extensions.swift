import Compiler

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
