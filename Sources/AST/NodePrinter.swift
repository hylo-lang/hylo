import Basic

public struct NodePrinter: NodeVisitor {

  public typealias Result = String

  public init(context: Context) {
    self.context = context
  }

  let context: Context

  public func print() {
    var stream = StandardOutput()
    print(to: &stream)
  }

  public func print<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("[")
    var isFirst = true
    for module in context.modules.values {
      if isFirst {
        isFirst = false
      } else {
        stream.write(",")
      }
      stream.write(visit(module))
    }
    stream.write("]")
  }

  func encode(_ node: Node?) -> String {
    switch node {
    case let n as Decl    : return n.accept(self)
    case let n as Stmt    : return n.accept(self)
    case let n as Expr    : return n.accept(self)
    case let n as Pattern : return n.accept(self)
    case let n as TypeRepr: return n.accept(self)
    default               : return "null"
    }
  }

  func encode(_ nodes: [Node]) -> String {
    return "[" + nodes.map(encode(_:)).joined(separator: ", ") + "]"
  }

  func encode(_ string: String?) -> String {
    return string.map({ "\"\($0)\"" }) ?? "null"
  }

  func encode(_ identifier: ObjectIdentifier) -> String {
    return "\"" + String(Int(bitPattern: identifier), radix: 36) + "\""
  }

  func encode(referenceTo decl: Decl) -> String {
    return "\"" + decl.debugID + "\""
  }

  func encode(_ range: SourceRange) -> String {
    guard let sf = context.sourceManager.source(containing: range.lowerBound) else {
      return "null"
    }

    let start = sf.lineColumnIndices(at: range.lowerBound)
    let end = sf.lineColumnIndices(at: range.upperBound)
    return "\"\(sf.url.path):\(start.line):\(start.column) - \(end.line):\(end.column)\""
  }

  struct StandardOutput: TextOutputStream {

    func write(_ string: String) {
      Swift.print(string, terminator: "")
    }

  }

  func valueDeclHeader<N>(_ node: N) -> String where N: ValueDecl {
    return """
    "class"           : "\(type(of: node))",
    "id"              : \(encode(ObjectIdentifier(node))),
    "range"           : \(encode(node.range)),
    "type"            : "\(node.type)",
    "name"            : "\(node.name)"
    """
  }

  func typeDeclHeader<N>(_ node: N) -> String where N: TypeDecl {
    return """
    "class"           : "\(type(of: node))",
    "id"              : \(encode(ObjectIdentifier(node))),
    "range"           : \(encode(node.range)),
    "type"            : "\(node.type)",
    "name"            : "\(node.name)"
    """
  }

  func exprHeader<N>(_ node: N) -> String where N: Expr {
    return """
    "class"           : "\(type(of: node))",
    "range"           : \(encode(node.range)),
    "type"            : "\(node.type)"
    """
  }

  func patternHeader<N>(_ node: N) -> String where N: Pattern {
    return """
    "class"           : "\(type(of: node))",
    "range"           : \(encode(node.range)),
    "type"            : "\(node.type)"
    """
  }

  func typeReprHeader<N>(_ node: N) -> String where N: TypeRepr {
    return """
    "class"           : "\(type(of: node))",
    "range"           : \(encode(node.range)),
    "type"            : "\(node.type)"
    """
  }

  public func visit(_ node: Module) -> String {
    return """
    {
    "class"           : "\(type(of: node))",
    "id"              : "\(node.id)",
    "decls"           : \(encode(node.decls))
    }
    """
  }

  public func visit(_ node: PatternBindingDecl) -> String {
    return """
    {
    "class"           : "\(type(of: node))",
    "id"              : \(encode(ObjectIdentifier(node))),
    "range"           : \(encode(node.range)),
    "isMutable"       : \(node.isMutable),
    "pattern"         : \(node.pattern.accept(self)),
    "sign"            : \(encode(node.sign)),
    "initializer"     : \(encode(node.initializer))
    }
    """
  }

  public func visit(_ node: VarDecl) -> String {
    return """
    {
    \(valueDeclHeader(node)),
    "isMutable"       : \(node.isMutable)
    }
    """
  }

  public func visit(_ node: AbstractFunDecl) -> String {
    let mods = node.declModifiers.map({ mod in "\"\(mod)\"" })
      .joined(separator: ", ")

    return """
    {
    \(valueDeclHeader(node)),
    "declModifiers"   : [\(mods)],
    "genericParams"   : \(encode(node.genericParams)),
    "params"          : \(encode(node.params)),
    "retSign"         : \(encode(node.retSign)),
    "body"            : \(encode(node.body))
    }
    """
  }

  public func visit(_ node: FunDecl) -> String {
    visit(node as AbstractFunDecl)
  }

  public func visit(_ node: CtorDecl) -> String {
    visit(node as AbstractFunDecl)
  }

  public func visit(_ node: FunParamDecl) -> String {
    return """
    {
    \(valueDeclHeader(node)),
    "externalName"    : \(encode(node.externalName)),
    "sign"            : \(encode(node.sign))
    }
    """
  }

  public func visit(_ node: AbstractNominalTypeDecl) -> String {
    switch node {
    case let decl as ProductTypeDecl: return visit(decl)
    case let decl as ViewTypeDecl:    return visit(decl)
    default:
      fatalError("unreachable")
    }
  }

  public func visit(_ node: ProductTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "inheritances"    : \(encode(node.inheritances)),
    "members"         : \(encode(node.members))
    }
    """
  }

  public func visit(_ node: ViewTypeDecl) -> String {
    return """
    {
    \(typeDeclHeader(node)),
    "inheritances"    : \(encode(node.inheritances)),
    "members"         : \(encode(node.members))
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

  public func visit(_ node: TypeExtDecl) -> String {
    return """
    {
    "class"           : "\(type(of: node))",
    "id"              : \(encode(ObjectIdentifier(node))),
    "range"           : \(encode(node.range)),
    "extendedIdent"   : \(encode(node.extendedIdent)),
    "members"         : \(encode(node.members))
    }
    """
  }

  public func visit(_ node: BraceStmt) -> String {
    return """
    {
    "class"           : "\(type(of: node))",
    "range"           : \(encode(node.range)),
    "statements"      : \(encode(node.statements))
    }
    """
  }

  public func visit(_ node: RetStmt) -> String {
    return """
    {
    "class"           : "\(type(of: node))",
    "range"           : \(encode(node.range)),
    "value"           : \(encode(node.value))
    }
    """
  }

  public func visit(_ node: IntLiteralExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value"           : \(node.value)
    }
    """
  }

  public func visit(_ node: AssignExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "lvalue"          : \(encode(node.lvalue)),
    "rvalue"          : \(encode(node.rvalue))
    }
    """
  }

  public func visit(_ node: CallExpr) -> String {
    let args = node.args.map({ arg in
      """
      {
      "label"   : \(encode(arg.label)),
      "value"   : \(arg.value.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(exprHeader(node)),
    "fun"             : \(node.fun.accept(self)),
    "args"            : [\(args)]
    }
    """
  }

  public func visit(_ node: UnresolvedDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "name"            : "\(node.name)"
    }
    """
  }

  public func visit(_ node: QualDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "namespace"       : \(encode(node.namespace)),
    "name"            : "\(node.name)"
    }
    """
  }

  public func visit(_ node: OverloadedDeclRefExpr) -> String {
    let declSet = node.declSet
      .map(encode(referenceTo:))
      .joined(separator: ", ")

    return """
    {
    \(exprHeader(node)),
    "declSet"         : [\(declSet)]
    }
    """
  }

  public func visit(_ node: DeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "decl"            : \(encode(referenceTo: node.decl))
    }
    """
  }

  public func visit(_ node: TypeDeclRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "decl"            : \(encode(referenceTo: node.decl))
    }
    """
  }

  public func visit(_ node: UnresolvedMemberExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "base"            : \(encode(node.base)),
    "memberName"      : "\(node.memberName)"
    }
    """
  }

  public func visit(_ node: MemberRefExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "base"            : \(node.base.accept(self)),
    "decl"            : \(encode(referenceTo: node.decl))
    }
    """
  }

  public func visit(_ node: AddrOfExpr) -> String {
    return """
    {
    \(exprHeader(node)),
    "value"           : \(encode(node.value))
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

  public func visit(_ node: NamedPattern) -> String {
    return """
    {
    \(patternHeader(node)),
    "decl"            : \(encode(node.decl))
    }
    """
  }

  public func visit(_ node: TuplePattern) -> String {
    let elems = node.elems.map({ elem in
      """
      {
      "label"   : \(encode(elem.label)),
      "pattern" : \(elem.pattern.accept(self))
      }
      """
    })
    .joined(separator: ", ")

    return """
    {
    \(patternHeader(node)),
    "elems"           : [\(elems)]
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

  public func visit(_ node: UnqualTypeRepr) -> String {
    return """
    {
    \(typeReprHeader(node)),
    "name"            : "\(node.name)"
    }
    """
  }

  public func visit(_ node: CompoundTypeRepr) -> String {
    return """
    {
    \(typeReprHeader(node)),
    "components"      : \(encode(node.components))
    }
    """
  }

}
