import Basic

public struct NodePrinter: NodeVisitor {

  public typealias Result = String

  public init(context: Context) {
    self.context = context
  }

  let context: Context

  func encode(_ node: Node?) -> String {
    switch node {
    case let n as Decl   : return n.accept(self)
    case let n as Stmt   : return n.accept(self)
    case let n as Expr   : return n.accept(self)
    case let n as Pattern: return n.accept(self)
    case let n as Sign   : return n.accept(self)
    default: return "null"
    }
  }

  func encode(_ nodes: [Node]) -> String {
    return "[" + nodes.map(encode(_:)).joined(separator: ", ") + "]"
  }

  func encode(_ clause: GenericClause?) -> String {
    guard let clause = clause else { return "null" }

    return """
    {
    "params": \(encode(clause.params)),
    "typeReqs": \(encode(clause.typeReqs))
    }
    """
  }

  func encode(_ typeReqs: [TypeReq]) -> String {
    let reqs: [String] = typeReqs.reduce(into: [], { result, req in
      result.append("""
      {
      "kind": "\(req.kind)",
      "lhs": \(encode(req.lhs)),
      "rhs": \(encode(req.rhs))
      }
      """)
    })
    return "[\(reqs.joined(separator: ", "))]"
  }

  func encode(_ string: String?) -> String {
    return string.map({ "\"\($0)\"" }) ?? "null"
  }

  func encode(refToDecl decl: Decl) -> String {
    return "\"\(decl.debugID)\""
  }

  func encode(refToSpace space: DeclSpace?) -> String {
    guard let space = space else { return "null" }
    if let decl = space as? Decl {
      return encode(refToDecl: decl)
    } else {
      return "\"\(type(of: space))\""
    }
  }

  func encode(_ range: SourceRange?) -> String {
    guard let range = range,
          let source = context.sourceManager.source(containing: range.lowerBound)
    else {
      return "null"
    }

    let start = source.lineColumnIndices(at: range.lowerBound)
    let end = source.lineColumnIndices(at: range.upperBound)
    return "\"\(source.url.path):\(start.line):\(start.column) - \(end.line):\(end.column)\""
  }

  func valueDeclHeader<N>(_ node: N) -> String where N: ValueDecl {
    return """
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "type": "\(node.type)",
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "name": "\(node.name)"
    """
  }

  func typeDeclHeader<N>(_ node: N) -> String where N: TypeDecl {
    return """
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "type": "\(node.type)",
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "name": "\(node.name)"
    """
  }

  func exprHeader<N>(_ node: N) -> String where N: Expr {
    return """
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "type": "\(node.type)"
    """
  }

  func patternHeader<N>(_ node: N) -> String where N: Pattern {
    return """
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "type": "\(node.type)"
    """
  }

  func signHeader<N>(_ node: N) -> String where N: Sign {
    return """
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "type": "\(node.type)"
    """
  }

  public func visit(_ node: ModuleDecl) -> String {
    let units = "[" + node.units.map(visit(_:)).joined(separator: ", ") + "]"

    return """
    {
    "class": "\(type(of: node))",
    "name": "\(node.name)",
    "units": \(units)
    }
    """
  }

  public func visit(_ unit: FileUnit) -> String {
    let path = (unit as? SourceUnit)?.source.url.path ?? ""

    return """
    {
    "class": "\(type(of: unit))",
    "parentDeclSpace": \(encode(refToSpace: unit.parentDeclSpace)),
    "path": "\(path)",
    "decls": \(encode(unit.decls))
    }
    """
  }

  public func visit(_ node: ImportDecl) -> String {
    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "name": \(encode(node.name))
    }
    """
  }

  public func visit(_ node: PatternBindingDecl) -> String {
    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "isMutable": \(node.isMutable),
    "pattern": \(node.pattern.accept(self)),
    "sign": \(encode(node.sign)),
    "initializer": \(encode(node.initializer))
    }
    """
  }

  public func visit(_ node: VarDecl) -> String {
    return """
    {
    \(valueDeclHeader(node)),
    "isMutable": \(node.isMutable)
    }
    """
  }

  public func visit(_ node: BaseFunDecl) -> String {
    let mods = node.modifiers.map({ mod in "\"\(mod)\"" })
      .joined(separator: ", ")

    return """
    {
    \(valueDeclHeader(node)),
    "declModifiers": [\(mods)],
    "genericClause": \(encode(node.genericClause)),
    "params": \(encode(node.params)),
    "retSign": \(encode(node.retSign)),
    "body": \(encode(node.body))
    }
    """
  }

  public func visit(_ node: FunDecl) -> String {
    visit(node as BaseFunDecl)
  }

  public func visit(_ node: CtorDecl) -> String {
    visit(node as BaseFunDecl)
  }

  public func visit(_ node: FunParamDecl) -> String {
    return """
    {
    \(valueDeclHeader(node)),
    "externalName": \(encode(node.externalName)),
    "sign": \(encode(node.sign))
    }
    """
  }

  public func visit(_ node: GenericTypeDecl) -> String {
    switch node {
    case let decl as NominalTypeDecl: return visit(decl)
    case let decl as AliasTypeDecl: return visit(decl)
    default:
      fatalError("unreachable")
    }
  }

  public func visit(_ node: NominalTypeDecl) -> String {
    switch node {
    case let decl as ProductTypeDecl: return visit(decl)
    case let decl as ViewTypeDecl: return visit(decl)
    default:
      fatalError("unreachable")
    }
  }

  public func visit(_ node: ProductTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "genericClause": \(encode(node.genericClause)),
    "inheritances": \(encode(node.inheritances)),
    "members": \(encode(node.members))
    }
    """
  }

  public func visit(_ node: ViewTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "inheritances": \(encode(node.inheritances)),
    "members": \(encode(node.members))
    }
    """
  }

  public func visit(_ node: AliasTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "aliasedSign": \(encode(node.aliasedSign))
    }
    """
  }

  public func visit(_ node: AbstractTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "inheritances": \(encode(node.name)),
    "typeReqs": \(encode(node.typeReqs))
    }
    """
  }

  public func visit(_ node: GenericParamDecl) -> String {
    return """
    {
    \(typeDeclHeader(node))
    }
    """
  }

  public func visit(_ node: TypeExtnDecl) -> String {
    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "extendedIdent": \(encode(node.extendedIdent)),
    "members": \(encode(node.members))
    }
    """
  }

  public func visit(_ node: BraceStmt) -> String {
    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "stmts": \(encode(node.stmts))
    }
    """
  }

  public func visit(_ node: RetStmt) -> String {
    let funDecl = node.funDecl.map(encode(refToDecl:)) ?? "null"

    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "value": \(encode(node.value)),
    "funDecl": \(funDecl)
    }
    """
  }

  public func visit(_ node: MatchCaseStmt) -> String {
    return """
    {
    "class": "\(type(of: node))",
    "range": \(encode(node.range)),
    "parentDeclSpace": \(encode(refToSpace: node.parentDeclSpace)),
    "pattern": \(encode(node.pattern)),
    "condition": \(encode(node.condition)),
    "body": \(encode(node.body))
    }
    """
  }

  public func visit(_ node: BoolLiteralExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(node.value)
    }
    """
  }

  public func visit(_ node: IntLiteralExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(node.value)
    }
    """
  }

  public func visit(_ node: FloatLiteralExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(node.value)
    }
    """
  }

  public func visit(_ node: StringLiteralExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(node.value)
    }
    """
  }

  public func visit(_ node: AssignExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "lvalue": \(encode(node.lvalue)),
    "rvalue": \(encode(node.rvalue))
    }
    """
  }

  public func visit(_ node: BaseCastExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(encode(node.value)),
    "sign": \(encode(node.sign))
    }
    """
  }

  public func visit(_ node: DynCastExpr) -> String {
    return visit(node as BaseCastExpr)
  }

  public func visit(_ node: UnsafeCastExpr) -> String {
    return visit(node as BaseCastExpr)
  }

  public func visit(_ node: TupleExpr) -> String {
    let elems = node.elems.map({ elem in
      """
      {
      "label": \(encode(elem.label)),
      "value": \(elem.value.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(exprHeader(node)),
    "elems": [\(elems)]
    }
    """
  }

  public func visit(_ node: CallExpr) -> String {
    let args = node.args.map({ arg in
      """
      {
      "label": \(encode(arg.label)),
      "value": \(arg.value.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(exprHeader(node)),
    "fun": \(node.fun.accept(self)),
    "args": [\(args)]
    }
    """
  }

  public func visit(_ node: UnresolvedDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "name": "\(node.name)"
    }
    """
  }

  public func visit(_ node: UnresolvedQualDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "namespace": \(encode(node.namespace)),
    "name": "\(node.name)"
    }
    """
  }

  public func visit(_ node: OverloadedDeclRefExpr) -> String {
    let declSet = node.declSet
      .map(encode(refToDecl:))
      .joined(separator: ", ")

    return """
    {
    \(exprHeader(node)),
    "declSet": [\(declSet)]
    }
    """
  }

  public func visit(_ node: DeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "decl": \(encode(refToDecl: node.decl))
    }
    """
  }

  public func visit(_ node: TypeDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "decl": \(encode(refToDecl: node.decl))
    }
    """
  }

  public func visit(_ node: UnresolvedMemberExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "base": \(encode(node.base)),
    "memberName": "\(node.memberName)"
    }
    """
  }

  public func visit(_ node: MemberDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "base": \(node.base.accept(self)),
    "decl": \(encode(refToDecl: node.decl))
    }
    """
  }

  public func visit(_ node: TupleMemberExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "base": \(node.base.accept(self)),
    "memberIndex": \(node.memberIndex)
    }
    """
  }

  public func visit(_ node: AsyncExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(encode(node.value))
    }
    """
  }

  public func visit(_ node: AwaitExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(encode(node.value))
    }
    """
  }

  public func visit(_ node: AddrOfExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value": \(encode(node.value))
    }
    """
  }

  public func visit(_ node: MatchExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "subject": \(encode(node.subject)),
    "cases": \(encode(node.cases))
    }
    """
  }

  public func visit(_ node: WildcardExpr) -> String {
    return """
    {
    \(exprHeader(node))
    }
    """
  }

  public func visit(_ node: ErrorExpr) -> String {
    return """
    {
    \(exprHeader(node))
    }
    """
  }

  public func visit(_ node: NamedPattern) -> String {
    return """
    {
    \(patternHeader(node)),
    "decl": \(encode(node.decl))
    }
    """
  }

  public func visit(_ node: TuplePattern) -> String {
    let elems = node.elems.map({ elem in
      """
      {
      "label": \(encode(elem.label)),
      "pattern": \(elem.pattern.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(patternHeader(node)),
    "elems": [\(elems)]
    }
    """
  }

  public func visit(_ node: BindingPattern) -> String {
    return """
    {
    \(patternHeader(node)),
    "subpattern": \(node.subpattern.accept(self)),
    "sign": \(encode(node.sign))
    }
    """
  }

  public func visit(_ node: WildcardPattern) -> String {
    return """
    {
    \(patternHeader(node))
    }
    """
  }

  public func visit(_ node: TupleSign) -> String {
    let elems = node.elems.map({ elem in
      """
      {
      "label": \(encode(elem.label)),
      "sign": \(elem.sign.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(signHeader(node)),
    "elems": [\(elems)]
    }
    """
  }

  public func visit(_ node: FunSign) -> String {
    return """
    {
    \(signHeader(node)),
    "paramSign": \(node.paramSign.accept(self)),
    "retSign": \(node.retSign.accept(self)),
    "isVolatile": \(node.isVolatile)
    }
    """
  }

  public func visit(_ node: AsyncSign) -> String {
    return """
    {
    \(signHeader(node)),
    "base": \(node.base.accept(self))
    }
    """
  }

  public func visit(_ node: InoutSign) -> String {
    return """
    {
    \(signHeader(node)),
    "base": \(node.base.accept(self))
    }
    """
  }

  public func visit(_ node: UnionSign) -> String {
    return """
    {
    \(signHeader(node)),
    "elems": \(encode(node.elems))
    }
    """
  }

  public func visit(_ node: ViewCompSign) -> String {
    return """
    {
    \(signHeader(node)),
    "views": \(encode(node.views))
    }
    """
  }

  public func visit(_ node: BareIdentSign) -> String {
    return """
    {
    \(signHeader(node)),
    "name": "\(node.name)"
    }
    """
  }

  public func visit(_ node: SpecializedIdentSign) -> String {
    return """
    {
    \(signHeader(node)),
    "name": "\(node.name)",
    "args": \(encode(node.args))
    }
    """
  }

  public func visit(_ node: CompoundIdentSign) -> String {
    return """
    {
    \(signHeader(node)),
    "components": \(encode(node.components))
    }
    """
  }

  public func visit(_ node: ErrorSign) -> String {
    return """
    {
    \(signHeader(node))
    }
    """
  }

}
