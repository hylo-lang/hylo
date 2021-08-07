import Antlr4
import Basic
import Parser

public final class ParseTreeTransformer: ValVisitor<Any> {

  public init(sourceFile: SourceFile, module: ModuleDecl?, context: Context) {
    self.sourceFile = sourceFile
    self.module = module
    self.currentSpace = module
    self.context = context
  }

  /// The source file from which the parse tree was extracted.
  public let sourceFile: SourceFile

  /// The module containing the parse tree's declarations.
  public let module: ModuleDecl?

  /// The AST context into which the module is being loaded.
  public let context: Context

  /// The current declaration space.
  private var currentSpace: DeclSpace?

  /// A reference to the unresolved type.
  private var unresolvedType: UnresolvedType { context.unresolvedType }

  public override func visitFile(_ ctx: ValParser.FileContext) -> Any {
    let unit = SourceUnit(source: sourceFile)
    unit.parentDeclSpace = module
    currentSpace = unit

    expand(decls: ctx.decl(), into: &unit.decls)
    module?.units.append(unit)
    return unit
  }

  public override func visitBraceStmt(_ ctx: ValParser.BraceStmtContext) -> Any {
    // Create a stub of the code block.
    let block = BraceStmt(statements: [], range: range(of: ctx))
    block.parentDeclSpace = currentSpace

    // Update the current decl space.
    let parentDeclSpace = currentSpace
    currentSpace = block
    defer { currentSpace = parentDeclSpace }

    // Visit the block's statements.
    expand(decls: ctx.statement(), into: &block.stmts)
    return block
  }

  public override func visitDeclBlock(_ ctx: ValParser.DeclBlockContext) -> Any {
    var decls: [Decl] = []
    expand(decls: ctx.decl(), into: &decls)
    return decls
  }

  public override func visitStatement(_ ctx: ValParser.StatementContext) -> Any {
    let node = ctx.children![0].accept(self)!
    if let match = node as? MatchExpr {
      match.isSubexpr = false
    }
    return node
  }

  public override func visitDecl(_ ctx: ValParser.DeclContext) -> Any {
    return ctx.children![0].accept(self)!
  }

  public override func visitImportDecl(_ ctx: ValParser.ImportDeclContext) -> Any {
    let stmt = ImportDecl(name: ctx.NAME()!.getText(), range: range(of: ctx))
    stmt.parentDeclSpace = currentSpace
    return stmt
  }

  public override func visitDeclModifierList(_ ctx: ValParser.DeclModifierListContext) -> Any {
    return ctx.declModifier().map({ (modifier) -> DeclModifier in
      switch modifier.getText() {
      case "mut"      : return DeclModifier(kind: .mut, range: range(of: modifier))
      case "static"   : return DeclModifier(kind: .static, range: range(of: modifier))
      case "moveonly" : return DeclModifier(kind: .moveonly, range: range(of: modifier))
      default     : fatalError("unreachable")
      }
    })
  }

  public override func visitPatternBindingDecl(_ ctx: ValParser.PatternBindingDeclContext) -> Any {
    // Create a stub for the declaration.
    let keyword     = ctx.varDeclKeyword()!
    let pattern     = ctx.pattern()!.accept(self) as! Pattern
    let sign        = ctx.typeRepr().map({ repr in repr.accept(self) as! Sign })
    let initializer = ctx.expr().map({ expr in expr.accept(self) as! Expr })

    let decl = PatternBindingDecl(
      isMutable   : keyword.getText() == "var",
      pattern     : pattern,
      sign        : sign,
      initializer : initializer,
      introRange  : range(of: keyword),
      range       : range(of: ctx))
    decl.parentDeclSpace = currentSpace

    // Associate each introduced variable declaration to the new pattern binding declaration.
    for pattern in decl.pattern.namedPatterns {
      pattern.decl.isMutable = decl.isMutable
      pattern.decl.patternBindingDecl = decl
      decl.varDecls.append(pattern.decl)
    }

    return decl
  }

  public override func visitFunDecl(_ ctx: ValParser.FunDeclContext) -> Any {
    // Build the declaration modifiers.
    let modifiers = ctx.declModifierList().map({ mods in
      mods.accept(self) as! [DeclModifier]
    }) ?? []

    // Create the function declaration.
    let decl: BaseFunDecl
    switch ctx.funDeclKeyword()!.getText() {
    case "fun":
      let id = ctx.funName().map({ name in
        Ident(name: name.getText(), range: range(of: name))
      })

      decl = FunDecl(ident: id, modifiers: modifiers, type: unresolvedType, range: range(of: ctx))

    case "new":
      decl = CtorDecl(
        modifiers: modifiers,
        type: unresolvedType,
        introRange: range(of: ctx.funDeclKeyword()!),
        range: range(of: ctx))

    default:
      fatalError("unreachable")
    }

    if modifiers.contains(where: { mod in mod.kind == .mut }) {
      decl.props.insert(.isMutating)
    }

    if (currentSpace is NominalTypeDecl || currentSpace is TypeExtnDecl) {
      if !(decl is CtorDecl) {
        decl.props.insert(.isMember)
      }
    } else if decl is CtorDecl {
      preconditionFailure("constructor declared outside of a type declaration")
    }

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the remainder of the declaration.
    if let genericClause = ctx.genericClause() {
      decl.genericClause = (genericClause.accept(self) as! GenericClause)
    }
    if let params = ctx.funParamList() {
      decl.params = params.accept(self) as! [FunParamDecl]
    }
    if let sign = ctx.funRetAnnot() {
      precondition(!(decl is CtorDecl), "constructors do not have explicit return types")
      decl.retSign = (sign.accept(self) as! Sign)
    }
    if let body = ctx.braceStmt() {
      decl.body = (body.accept(self) as! BraceStmt)
    }

    return decl
  }

  public override func visitFunParamList(_ ctx: ValParser.FunParamListContext) -> Any {
    return ctx.funParam().map({ param in param.accept(self) as! FunParamDecl })
  }

  public override func visitFunParam(_ ctx: ValParser.FunParamContext) -> Any {
    let name = ctx.NAME()!.getText()
    var externalName: String? = ctx.funParamExtName()?.getText() ?? name
    if externalName == "_" {
      externalName = nil
    }

    let decl = FunParamDecl(
      name: name,
      externalName: externalName,
      typeSign: nil,
      type: unresolvedType,
      range: range(of: ctx))
    decl.parentDeclSpace = currentSpace

    if let repr = ctx.typeRepr() {
      decl.sign = (repr.accept(self) as! Sign)
    }

    return decl
  }

  public override func visitFunRetAnnot(_ ctx: ValParser.FunRetAnnotContext) -> Any {
    return ctx.typeRepr()!.accept(self)!
  }

  public override func visitGenericClause(_ ctx: ValParser.GenericClauseContext) -> Any {
    let params = ctx.genericParamList()!.accept(self) as! [GenericParamDecl]
    let typeReqs = ctx.typeReqClause().map({ reqs in
      reqs.accept(self) as! [TypeReq]
    }) ?? []
    return GenericClause(params: params, typeReqs: typeReqs, range: range(of: ctx))
  }

  public override func visitGenericParamList(_ ctx: ValParser.GenericParamListContext) -> Any {
    return ctx.NAME().map({ (name) -> GenericParamDecl in
      let decl = GenericParamDecl(
        name: name.getText(),
        type: unresolvedType,
        range: range(of: name.getSymbol()!))
      decl.parentDeclSpace = currentSpace
      decl.type = context.genericParamType(decl: decl).kind

      return decl
    })
  }

  public override func visitTypeReqClause(_ ctx: ValParser.TypeReqClauseContext) -> Any {
    return ctx.typeReqList()!.accept(self)!
  }

  public override func visitTypeReqList(_ ctx: ValParser.TypeReqListContext) -> Any {
    return ctx.typeReq().map({ typeReq in typeReq.accept(self) as! TypeReq })
  }

  public override func visitSameTypeReq(_ ctx: ValParser.SameTypeReqContext) -> Any {
    let lhs = ctx.identTypeRepr()!.accept(self) as! IdentSign
    let rhs = ctx.typeRepr()!.accept(self) as! Sign
    return TypeReq(kind: .equality, lhs: lhs, rhs: rhs, range: range(of: ctx))
  }

  public override func visitViewConfReq(_ ctx: ValParser.ViewConfReqContext) -> Any {
    let lhs = ctx.identTypeRepr(0)!.accept(self) as! IdentSign
    let rhs = ctx.identTypeRepr(1)!.accept(self) as! IdentSign
    return TypeReq(kind: .conformance, lhs: lhs, rhs: rhs, range: range(of: ctx))
  }

  public override func visitTypeDecl(_ ctx: ValParser.TypeDeclContext) -> Any {
    return ctx.children![0].accept(self)!
  }

  public override func visitProductTypeDecl(_ ctx: ValParser.ProductTypeDeclContext) -> Any {
    // Create the declaration.
    let decl = ProductTypeDecl(
      name: "",
      type: unresolvedType,
      range: range(of: ctx))
    decl.type = context.productType(decl: decl).kind

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the declaration's head.
    let head = ctx.typeDeclHead()!.accept(self) as! TypeDeclHead
    decl.name = head.name
    decl.genericClause = head.genericClause
    decl.inheritances = head.inheritances

    // Visit the declaration's members.
    decl.members = ctx.declBlock()!.accept(self) as! [Decl]

    return decl
  }

  public override func visitAliasTypeDecl(_ ctx: ValParser.AliasTypeDeclContext) -> Any {
    // Create the declaration.
    let decl = AliasTypeDecl(
      name: "",
      aliasedSign: BareIdentSign(ident: Ident(name: "", range: .invalid), type: unresolvedType),
      type: unresolvedType,
      range: range(of: ctx))

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the declaration's head.
    let head = ctx.typeDeclHead()!.accept(self) as! TypeDeclHead
    decl.name = head.name
    decl.genericClause = head.genericClause
    decl.inheritances = head.inheritances

    // Visit the aliased signature.
    decl.aliasedSign = ctx.typeRepr()!.accept(self) as! Sign

    return decl
  }

  public override func visitAbstractTypeDecl(_ ctx: ValParser.AbstractTypeDeclContext) -> Any {
    // Create the declaration.
    let decl = AbstractTypeDecl(
      name : ctx.NAME()!.getText(),
      type : unresolvedType,
      range: range(of: ctx))
    decl.parentDeclSpace = currentSpace
    decl.type = context.genericParamType(decl: decl).kind

    /// Visit the declaration's inheriance and requirement clauses.
    if let clause = ctx.inheritanceClause() {
      decl.inheritances = clause.accept(self) as! [IdentSign]
    }
    if let clause = ctx.typeReqClause() {
      decl.typeReqs = clause.accept(self) as! [TypeReq]
    }

    return decl
  }

  public override func visitViewTypeDecl(_ ctx: ValParser.ViewTypeDeclContext) -> Any {
    // Create the declaration.
    let decl = ViewTypeDecl(
      name : ctx.NAME()!.getText(),
      type : unresolvedType,
      range: range(of: ctx))
    decl.type = context.viewType(decl: decl).kind

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the declaration's head.
    if let inheritanceClause = ctx.inheritanceClause() {
      decl.inheritances = inheritanceClause.accept(self) as! [IdentSign]
    }

    // Visit the declaration's members.
    decl.members = ctx.declBlock()!.accept(self) as! [Decl]

    return decl
  }

  public override func visitTypeDeclHead(_ ctx: ValParser.TypeDeclHeadContext) -> Any {
    let genericClause: GenericClause?
    if let clause = ctx.genericClause() {
      genericClause = (clause.accept(self) as! GenericClause)
    } else {
      genericClause = nil
    }

    let inheritances: [IdentSign]
    if let clause = ctx.inheritanceClause() {
      inheritances = clause.accept(self) as! [IdentSign]
    } else {
      inheritances = []
    }

    return TypeDeclHead(
      name: ctx.NAME()!.getText(),
      genericClause: genericClause,
      inheritances: inheritances)
  }

  public override func visitExtDecl(_ ctx: ValParser.ExtDeclContext) -> Any {
    let ident = ctx.identTypeRepr()!.accept(self) as! IdentSign
    let decl = TypeExtnDecl(extendedIdent: ident, members: [], range: range(of: ctx))

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    decl.members = ctx.declBlock()!.accept(self) as! [Decl]
    return decl
  }

  public override func visitInheritanceClause(_ ctx: ValParser.InheritanceClauseContext) -> Any {
    return ctx.identTypeRepr().map({ ident in ident.accept(self) as! IdentSign })
  }

  public override func visitCtrl(_ ctx: ValParser.CtrlContext) -> Any {
    return ctx.children![0].accept(self)!
  }

  public override func visitRetStmt(_ ctx: ValParser.RetStmtContext) -> Any {
    let value = ctx.expr().map({ expr in expr.accept(self) as! Expr })
    let stmt = RetStmt(value: value, range: range(of: ctx))
    currentSpace?.spacesUpToRoot
      .first(where: { $0 is BaseFunDecl })
      .map({ stmt.funDecl = ($0 as! BaseFunDecl) })

    return stmt
  }

  public override func visitPattern(_ ctx: ValParser.PatternContext) -> Any {
    return ctx.children![0].accept(self) as! Pattern
  }

  public override func visitNamedPattern(_ ctx: ValParser.NamedPatternContext) -> Any {
    // Create a variable declaration for the pattern.
    let ident = Ident(name: ctx.getText(), range: range(of: ctx))
    let decl = VarDecl(ident: ident, type: unresolvedType, range: range(of: ctx))
    decl.parentDeclSpace = currentSpace

    // Create the pattern.
    return NamedPattern(decl: decl, type: unresolvedType, range: range(of: ctx))
  }

  public override func visitTuplePattern(_ ctx: ValParser.TuplePatternContext) -> Any {
    let pattern = TuplePattern(
      elems: [],
      type: unresolvedType,
      range: range(of: ctx))

    if let elemList = ctx.tuplePatternElemList() {
      pattern.elems = elemList.accept(self) as! [TuplePattern.Elem]
    }

    return pattern
  }

  public override func visitTuplePatternElemList(_ ctx: ValParser.TuplePatternElemListContext) -> Any {
    return ctx.tuplePatternElem().map({ elem in elem.accept(self) as! TuplePattern.Elem })
  }

  public override func visitTuplePatternElem(_ ctx: ValParser.TuplePatternElemContext) -> Any {
    let label = ctx.NAME()?.getText()
    let pattern = ctx.pattern()!.accept(self) as! Pattern
    return TuplePattern.Elem(label: label, pattern: pattern, range: range(of: ctx))
  }

  public override func visitBindingPattern(_ ctx: ValParser.BindingPatternContext) -> Any {
    let keyword    = ctx.varDeclKeyword()!
    let subpattern = ctx.pattern()!.accept(self) as! Pattern
    let sign       = ctx.typeRepr().map({ repr in repr.accept(self) as! Sign })

    let pattern = BindingPattern(
      isMutable   : keyword.getText() == "var",
      subpattern  : subpattern,
      sign        : sign,
      type        : unresolvedType,
      keywordRange: range(of: keyword),
      range       : range(of: ctx))

//    // Associate each introduced variable declaration to the new pattern binding declaration.
//    for pattern in decl.pattern.namedPatterns {
//      pattern.decl.patternBindingDecl = decl
//      decl.varDecls.append(pattern.decl)
//    }

    return pattern
  }

  public override func visitWildcardPattern(_ ctx: ValParser.WildcardPatternContext) -> Any {
    let pattern = WildcardPattern(type: unresolvedType, range: range(of: ctx))
    return pattern
  }

  public override func visitTypeRepr(_ ctx: ValParser.TypeReprContext) -> Any {
    let lhs = ctx.maxtermTypeRepr()!.accept(self) as! Sign

    let base: Sign
    if let rhs = ctx.typeRepr().map({ repr in repr.accept(self) as! Sign }) {
      let range = lhs.range.lowerBound ..< rhs.range.upperBound
      base = FunSign(paramSign: lhs, retSign: rhs, type: unresolvedType, range: range)
    } else {
      base = lhs
    }

    guard let modifier = ctx.typeModifier() else {
      return base
    }

    switch modifier.getText() {
    case "async":
      return AsyncSign(
        base: base,
        type: unresolvedType,
        modifierRange: range(of: modifier),
        range: range(of: ctx))

    case "mut":
      return InoutSign(
        base: base,
        type: unresolvedType,
        modifierRange: range(of: modifier),
        range: range(of: ctx))

    default:
      fatalError("unreachable")
    }
  }

  public override func visitMaxtermTypeRepr(_ ctx: ValParser.MaxtermTypeReprContext) -> Any {
    let terms = ctx.mintermTypeRepr().map({ term in term.accept(self) as! Sign })
    assert(!terms.isEmpty)

    if terms.count == 1 {
      return terms[0]
    } else {
      return UnionSign(elems: terms, type: unresolvedType, range: range(of: ctx))
    }
  }

  public override func visitMintermTypeRepr(_ ctx: ValParser.MintermTypeReprContext) -> Any {
    let primaries = ctx.primaryTypeRepr().map({ primary in primary.accept(self) as! Sign })
    assert(!primaries.isEmpty)

    if primaries.count == 1 {
      return primaries[0]
    } else {
      return ViewCompSign(views: primaries, type: unresolvedType, range: range(of: ctx))
    }
  }

  public override func visitPrimaryTypeRepr(_ ctx: ValParser.PrimaryTypeReprContext) -> Any {
    return ctx.children![0].accept(self) as! Sign
  }

  public override func visitIdentTypeRepr(_ ctx: ValParser.IdentTypeReprContext) -> Any {
    let components = ctx.unqualTypeRepr().map({ repr in repr.accept(self) as! IdentCompSign })
    return CompoundIdentSign.create(components)
  }

  public override func visitUnqualTypeRepr(_ ctx: ValParser.UnqualTypeReprContext) -> Any {
    let ident = self.ident(ctx.NAME()!)
    if let argList = ctx.genericArgList() {
      let args = argList.accept(self) as! [Sign]
      return SpecializedIdentSign(
        ident: ident, args: args, type: unresolvedType, range: range(of: ctx))
    } else {
      return BareIdentSign(ident: ident, type: unresolvedType)
    }
  }

  public override func visitGenericArgList(_ ctx: ValParser.GenericArgListContext) -> Any {
    return ctx.typeRepr().map({ arg in arg.accept(self) as! Sign })
  }

  public override func visitTupleTypeRepr(_ ctx: ValParser.TupleTypeReprContext) -> Any {
    let elems = ctx.tupleTypeElemList()
      .map({ elems in elems.accept(self) as! [TupleSignElem] }) ?? []
    return TupleSign(elems: elems, type: unresolvedType, range: range(of: ctx))
  }

  public override func visitTupleTypeElemList(_ ctx: ValParser.TupleTypeElemListContext) -> Any {
    return ctx.tupleTypeElem().map({ elem in elem.accept(self) as! TupleSignElem })
  }

  public override func visitTupleTypeElem(_ ctx: ValParser.TupleTypeElemContext) -> Any {
    let label = ctx.NAME()?.getText()
    let sign = ctx.typeRepr()!.accept(self) as! Sign
    return TupleSignElem(label: label, sign: sign, range: range(of: ctx))
  }

  public override func visitBinaryExpr(_ ctx: ValParser.BinaryExprContext) -> Any {
    typealias Link = (loc: InfixOperatorLoc, expr: Expr)

    /// Transforms a sequence of expressions and operators into a binary tree.
    func tree(head: Expr, tail: ArraySlice<Link>) -> Expr {
      let loc: InfixOperatorLoc
      let lhs: Expr
      let rhs: Expr

      switch tail.count {
      case 0:
        return head

      case 1:
        (loc, rhs) = tail.first!
        lhs = head

      default:
        // Search for the root of the expression's tree, applying precedence and associativity.
        var index = tail.startIndex
        for (i, link) in tail.enumerated() {
          if link.loc.infixOp == tail[index].loc.infixOp {
            switch link.loc.infixOp.associativity {
            case .left  : index = i
            case .right : break
            case nil    : preconditionFailure("adjacent operators are not associative")
            }
          } else if link.loc.infixOp.precedence < tail[index].loc.infixOp.precedence {
            index = i
          }
        }

        // Build the branches.
        loc = tail[index].loc
        lhs = tree(head: head, tail: tail[tail.startIndex ..< index])
        rhs = tree(head: tail[index].expr, tail: tail[(index + 1)...])
      }

      // Compute the range of the sub-expression.
      let rg = lhs.range.lowerBound ..< rhs.range.upperBound

      // The assignment operator has a dedicated expression.
      if loc.infixOp == .copy {
        return AssignExpr(lvalue: lhs, rvalue: rhs, range: rg)
      }

      // Create an infix call.
      let memberIdent = Ident(name: loc.infixOp.rawValue, range: loc.range)
      let infixFun = UnresolvedMemberExpr(
        base: lhs, ident: memberIdent, type: unresolvedType, range: loc.range)
      let infixCall = CallExpr(
        fun: infixFun,
        args: [CallArg(value: rhs, range: rhs.range)],
        type: unresolvedType,
        range: rg)
      return infixCall
    }

    // Build a sequence of expressions, separated by infix operators.
    let head = ctx.prefixExpr()?.accept(self) as! Expr
    let tail = ctx.binaryTrailer().map({ (pair) -> Link in
      let loc = pair.infixOper()!.accept(self) as! InfixOperatorLoc
      return (loc: loc, expr: pair.prefixExpr()!.accept(self) as! Expr)
    })

    // Transform the sequence into a tree, applying precedence and associativity.
    return tree(head: head, tail: tail[0...])
  }

  public override func visitPrefixExpr(_ ctx: ValParser.PrefixExprContext) -> Any {
    let expr = ctx.postfixExpr()!.accept(self) as! Expr

    // Process the prefix operator, if any.
    guard let op = ctx.prefixOper() else { return expr }
    let loc = op.accept(self) as! PrefixOperatorLoc

    // A few operators have dedicated representations.
    switch loc.prefixOp {
    case .amp:
      return AddrOfExpr(value: expr, type: unresolvedType, range: range(of: ctx))
    case .async:
      return AsyncExpr(value: expr, type: unresolvedType, range: range(of: ctx))
    case .await:
      return AwaitExpr(value: expr, type: unresolvedType, range: range(of: ctx))
    default:
      // Other operators are parsed as call expressions.
      let memberIdent = Ident(name: loc.prefixOp.rawValue, range: loc.range)
      let prefixFun = UnresolvedMemberExpr(
        base: expr, ident: memberIdent, type: unresolvedType, range: loc.range)
      return CallExpr(
        fun: prefixFun,
        args: [CallArg(value: expr, range: expr.range)],
        type: unresolvedType,
        range: range(of: ctx))
    }
  }

  public override func visitCallExpr(_ ctx: ValParser.CallExprContext) -> Any {
    let fun = ctx.postfixExpr()!.accept(self) as! Expr
    let expr = CallExpr(fun: fun, args: [], type: unresolvedType, range: range(of: ctx))

    if let argList = ctx.argList() {
      expr.args = argList.accept(self) as! [CallArg]
    }

    return expr
  }

  public override func visitArgList(_ ctx: ValParser.ArgListContext) -> Any {
    return ctx.arg().map({ arg in arg.accept(self) as! CallArg })
  }

  public override func visitArg(_ ctx: ValParser.ArgContext) -> Any {
    let label = ctx.NAME()?.getText()
    let value = ctx.expr()!.accept(self) as! Expr
    return CallArg(label: label, value: value, range: range(of: ctx))
  }

  public override func visitMemberExpr(_ ctx: ValParser.MemberExprContext) -> Any {
    let base = ctx.postfixExpr()!.accept(self) as! Expr
    switch ctx.memberIdent()!.accept(self) {
    case let name as String:
      let memberIdent = Ident(name: name, range: range(of: ctx.memberIdent()!))
      return UnresolvedMemberExpr(
        base: base, ident: memberIdent, type: unresolvedType, range: range(of: ctx))

    case let index as Int:
      return TupleMemberExpr(
        base: base, memberIndex: index, type: unresolvedType, range: range(of: ctx))

    default:
      fatalError("unreachable")
    }
  }

  public override func visitMemberIdent(_ ctx: ValParser.MemberIdentContext) -> Any {
    if let name = ctx.NAME() {
      return name.getText()
    } else if let index = ctx.INT() {
      return Int(index.getText())!
    } else {
      fatalError("unreachable")
    }
  }

  public override func visitCastExpr(_ ctx: ValParser.CastExprContext) -> Any {
    let value = ctx.postfixExpr()!.accept(self) as! Expr
    let opLoc = ctx.castOper()!.accept(self) as! CastOperatorLoc
    let sign  = ctx.typeRepr()!.accept(self) as! Sign

    switch opLoc.castOp {
    case .dynCast:
      return DynCastExpr(value: value, sign: sign, type: unresolvedType, range: range(of: ctx))
    case .unsafeCast:
      return UnsafeCastExpr(value: value, sign: sign, type: unresolvedType, range: range(of: ctx))
    default:
      fatalError("not implemented")
    }
  }

  public override func visitPrimaryExpr(_ ctx: ValParser.PrimaryExprContext) -> Any {
    return ctx.primary()!.accept(self) as! Expr
  }

  public override func visitPrimary(_ ctx: ValParser.PrimaryContext) -> Any {
    return ctx.children![0].accept(self) as! Expr
  }

  public override func visitInteger(_ ctx: ValParser.IntegerContext) -> Any {
    let value = ctx.INT()!.getText().filter({ $0 != "_" })
    let expr = IntLiteralExpr(
      value: Int(value)!, type: unresolvedType, range: range(of: ctx))
    return expr
  }

  public override func visitIdent(_ ctx: ValParser.IdentContext) -> Any {
    let names = ctx.NAME()
    precondition(!names.isEmpty)

    if names.count == 1 {
      return UnresolvedDeclRefExpr(
        ident: ident(names[0]),
        type: unresolvedType,
        range: range(of: names[0].getSymbol()!))
    }

    let ns = CompoundIdentSign
      .create(names.dropLast().map({ (name) -> BareIdentSign in
        return BareIdentSign(ident: ident(name), type: unresolvedType)
      }))

    return UnresolvedQualDeclRefExpr(
      namespace: ns, ident: ident(names.last!), type: unresolvedType, range: range(of: ctx))
  }

  public override func visitTuple(_ ctx: ValParser.TupleContext) -> Any {
    let elems = ctx.tupleElemList().map({ elems in elems.accept(self) as! [TupleElem] }) ?? []
    return TupleExpr(elems: elems, type: unresolvedType, range: range(of: ctx))
  }

  public override func visitTupleElemList(_ ctx: ValParser.TupleElemListContext) -> Any {
    return ctx.tupleElem().map({ elem in elem.accept(self) as! TupleElem })
  }

  public override func visitTupleElem(_ ctx: ValParser.TupleElemContext) -> Any {
    let label = ctx.NAME()?.getText()
    let value = ctx.expr()!.accept(self) as! Expr
    return TupleElem(label: label, value: value, range: range(of: ctx))
  }

  public override func visitMatch(_ ctx: ValParser.MatchContext) -> Any {
    let subject = ctx.expr()!.accept(self) as! Expr
    let cases = ctx.matchBody()!.accept(self) as! [MatchCaseStmt]
    return MatchExpr(
      isSubExpr : true,
      subject   : subject,
      cases     : cases,
      type      : unresolvedType,
      range     : range(of: ctx))
  }

  public override func visitMatchBody(_ ctx: ValParser.MatchBodyContext) -> Any {
    return ctx.matchCase().map({ stmt in stmt.accept(self) as! MatchCaseStmt })
  }

  public override func visitMatchCase(_ ctx: ValParser.MatchCaseContext) -> Any {
    // Create a stub of the case statement.
    let stmt = MatchCaseStmt(
      pattern: WildcardPattern(type: unresolvedType, range: .invalid),
      condition: nil,
      body: BraceStmt(statements: [], range: .invalid),
      range: range(of: ctx))
    stmt.parentDeclSpace = currentSpace

    // Update the current decl space.
    let parentDeclSpace = currentSpace
    currentSpace = stmt
    defer { currentSpace = parentDeclSpace }

    // Visit the statement.
    stmt.pattern = ctx.pattern()!.accept(self) as! Pattern
    stmt.condition = ctx.matchCaseCond().map({ expr in expr.accept(self) as! Expr })
    stmt.body = ctx.braceStmt()!.accept(self) as! BraceStmt

    return stmt
  }

  public override func visitWildcard(_ ctx: ValParser.WildcardContext) -> Any {
    let expr = WildcardExpr(type: unresolvedType, range: range(of: ctx))
    return expr
  }

  public override func visitPrefixOper(_ ctx: ValParser.PrefixOperContext) -> Any {
    guard let prefixOp = PrefixOperator(rawValue: ctx.getText()) else {
      preconditionFailure("undefined prefix operator '\(ctx.getText())'")
    }
    return PrefixOperatorLoc(prefixOp: prefixOp, range: range(of: ctx))
  }

  public override func visitInfixOper(_ ctx: ValParser.InfixOperContext) -> Any {
    guard let infixOp = InfixOperator(rawValue: ctx.getText()) else {
      preconditionFailure("undefined infix operator '\(ctx.getText())'")
    }
    return InfixOperatorLoc(infixOp: infixOp, range: range(of: ctx))
  }

  public override func visitCastOper(_ ctx: ValParser.CastOperContext) -> Any {
    guard let castOp = CastOperator(rawValue: ctx.getText()) else {
      preconditionFailure("undefined cast operator '\(ctx.getText())'")
    }
    return CastOperatorLoc(castOp: castOp, range: range(of: ctx))
  }

  /// Expands a sequence of declaration contexts into an array of nodes.
  ///
  /// This method is meant to handle situations in which statements from concrete syntax gets get
  /// expanded into multiple abstract nodes.
  private func expand<T>(
    decls: [ParserRuleContext],
    into list: inout [T]
  ) {
    for ctx in decls {
      switch ctx.accept(self) {
      case let node as T   : list.append(node)
      case let nodes as [T]: list.append(contentsOf: nodes)
      default: fatalError("unreachable")
      }
    }
  }

  private func ident(_ node: TerminalNode) -> Ident {
    return Ident(name: node.getText(), range: range(of: node.getSymbol()!))
  }

  /// Returns the range of the specified rule context in the source file.
  private func range(of ctx: Antlr4.ParserRuleContext) -> Range<String.Index> {
    let i = sourceFile.startIndex
    guard let start = ctx.getStart()?.getStartIndex(),
          let stop = ctx.getStop()?.getStopIndex()
    else { return i ..< i }
    return sourceFile.index(i, offsetBy: start) ..< sourceFile.index(i, offsetBy: stop + 1)
  }

  /// Returns the range of the specified token in the source file.
  private func range(of token: Token) -> Range<String.Index> {
    let i = sourceFile.startIndex
    let start = token.getStartIndex()
    let stop = token.getStopIndex()
    return sourceFile.index(i, offsetBy: start) ..< sourceFile.index(i, offsetBy: stop + 1)
  }

}

/// The head of a product or alias type declaration.
fileprivate struct TypeDeclHead {

  let name: String

  let genericClause: GenericClause?

  let inheritances: [IdentSign]

}

/// A prefix operator, together with its source location.
fileprivate struct PrefixOperatorLoc {

  let prefixOp: PrefixOperator

  let range: SourceRange

}

/// An infix operator, together with its source location.
fileprivate struct InfixOperatorLoc {

  let infixOp: InfixOperator

  let range: SourceRange

}

/// An cast operator, together with its source location.
fileprivate struct CastOperatorLoc {

  let castOp: CastOperator

  let range: SourceRange

}
