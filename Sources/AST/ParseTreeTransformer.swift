import Antlr4
import Basic
import Parser

public final class ParseTreeTransformer: ValVisitor<Any> {

  public init(sourceFile: SourceFile, module: Module?, context: Context) {
    self.sourceFile = sourceFile
    self.module = module
    self.currentSpace = module
    self.context = context
  }

  /// The source file from which the parse tree was extracted.
  public let sourceFile: SourceFile

  /// The module containing the parse tree's declarations.
  public let module: Module?

  /// The AST context into which the module is being loaded.
  public let context: Context

  /// The current declaration space.
  private var currentSpace: DeclSpace?

  /// A reference to the unresolved type.
  private var unresolvedType: UnresolvedType { context.unresolvedType }

  public override func visitFile(_ ctx: ValParser.FileContext) -> Any {
    var stmts: [Node] = []
    expand(decls: ctx.statement(), into: &stmts)
    module?.statements.append(contentsOf: stmts)
    return stmts
  }

  public override func visitCodeBlock(_ ctx: ValParser.CodeBlockContext) -> Any {
    // Create a stub of the code block.
    let block = BraceStmt(statements: [], range: range(of: ctx))
    block.parentDeclSpace = currentSpace

    // Update the current decl space.
    let parentDeclSpace = currentSpace
    currentSpace = block
    defer { currentSpace = parentDeclSpace }

    /// Visit the block's statements.
    expand(decls: ctx.statement(), into: &block.statements)
    return block
  }

  public override func visitDeclBlock(_ ctx: ValParser.DeclBlockContext) -> Any {
    var decls: [Node] = []
    expand(decls: ctx.decl(), into: &decls)
    return decls as! [Decl]
  }

  public override func visitStatement(_ ctx: ValParser.StatementContext) -> Any {
    return ctx.children![0].accept(self)!
  }

  public override func visitDecl(_ ctx: ValParser.DeclContext) -> Any {
    return ctx.children![0].accept(self)!
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
    let declKeyword = ctx.varDeclKeyword()!
    let pattern     = ctx.pattern()!.accept(self) as! Pattern
    let typeSign    = ctx.typeRepr().map({ repr in repr.accept(self) as! TypeRepr })
    let initializer = ctx.expr().map({ expr in expr.accept(self) as! Expr })

    // Create the pattern binding declaration.
    let decl = PatternBindingDecl(
      isMutable: declKeyword.getText() == "var",
      pattern: pattern,
      typeSign: typeSign,
      initializer: initializer,
      declKeywordRange: range(of: declKeyword),
      range: range(of: ctx))
    decl.parentDeclSpace = currentSpace

    // Associate each introduced variable declaration to the new pattern binding declaration.
    for pattern in decl.pattern.namedPatterns {
      pattern.decl.patternBindingDecl = decl
    }

    return decl
  }

  public override func visitFunDecl(_ ctx: ValParser.FunDeclContext) -> Any {
    let declModifiers = ctx.declModifierList().map({ $0.accept(self) as! [DeclModifier] }) ?? []

    // Create the function declaration.
    let decl: AbstractFunDecl
    switch ctx.funDeclKeyword()!.getText() {
    case "fun":
      let declName = ctx.funName()?.getText() ?? ""
      decl = FunDecl(
        name: declName, declModifiers: declModifiers, type: unresolvedType, range: range(of: ctx))
    case "new":
      decl = CtorDecl(declModifiers: declModifiers, type: unresolvedType, range: range(of: ctx))
    default:
      fatalError("unreachable")
    }

    if declModifiers.contains(where: { mod in mod.kind == .mut }) {
      decl.props.insert(.isMutating)
    }

    if (currentSpace is AbstractNominalTypeDecl || currentSpace is TypeExtDecl) {
      decl.props.insert(.isMember)
    } else if decl is CtorDecl {
      preconditionFailure("constructor declared outside of a type declaration")
    }

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the remainder of the declaration.
    if let params = ctx.paramList() {
      decl.params = params.accept(self) as! [FunParamDecl]
    }
    if let sign = ctx.funRetAnnot() {
      decl.retTypeSign = (sign.accept(self) as! TypeRepr)
    }
    if let body = ctx.codeBlock() {
      decl.body = (body.accept(self) as! BraceStmt)
    }

    // Build type stub for the function declaration.
    let paramTypeElems: [TupleType.Elem] = decl.params.map({ param in
      TupleType.Elem(label: param.externalName, type: TypeVar(context: context, node: param))
    })

    let retType: ValType
    if let sign = decl.retTypeSign {
      retType = TypeVar(context: context, node: sign)
    } else if decl is CtorDecl {
      precondition(decl.retTypeSign == nil)
      retType = TypeVar(context: context, node: decl)
    } else {
      retType = context.unitType
    }

    decl.type = context.funType(paramType: context.tupleType(paramTypeElems), retType: retType)
    return decl
  }

  public override func visitParamList(_ ctx: ValParser.ParamListContext) -> Any {
    return ctx.param().map({ param in param.accept(self) as! FunParamDecl })
  }

  public override func visitParam(_ ctx: ValParser.ParamContext) -> Any {
    let name = ctx.NAME()!.getText()
    let externalName = ctx.paramExtName()?.NAME()?.getText()

    let decl = FunParamDecl(
      name: name,
      externalName: externalName,
      typeSign: nil,
      type: unresolvedType,
      range: range(of: ctx))
    decl.type = TypeVar(context: context, node: decl)

    if let repr = ctx.typeRepr() {
      decl.typeSign = (repr.accept(self) as! TypeRepr)
    }

    return decl
  }

  public override func visitFunRetAnnot(_ ctx: ValParser.FunRetAnnotContext) -> Any {
    return ctx.typeRepr()!.accept(self)!
  }

  public override func visitTypeDecl(_ ctx: ValParser.TypeDeclContext) -> Any {
    // Create the type declaration.
    let declName = ctx.NAME()?.getText() ?? ""
    let decl: AbstractNominalTypeDecl
    switch ctx.typeDeclKeyword()!.getText() {
    case "type":
      decl = ProductTypeDecl(
        name: declName, type: unresolvedType, range: range(of: ctx))
      decl.type = context.productType(decl: decl as! ProductTypeDecl).kind

    case "view":
      decl = ViewTypeDecl(
        name: declName, type: unresolvedType, range: range(of: ctx))
      decl.type = context.viewType(decl: decl as! ViewTypeDecl).kind

    default:
      fatalError("unreachable")
    }

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    // Visit the remainder of the declaration.
    decl.members = ctx.declBlock()!.accept(self) as! [Decl]
    if let viewConfClause = ctx.viewConfClause() {
      decl.inheritances = viewConfClause.accept(self) as! [IdentTypeRepr]
    }
    return decl
  }

  public override func visitExtDecl(_ ctx: ValParser.ExtDeclContext) -> Any {
    let ident = ctx.identTypeRepr()!.accept(self) as! IdentTypeRepr
    let decl = TypeExtDecl(extendedIdent: ident, members: [], range: range(of: ctx))

    // Update the current decl space.
    decl.parentDeclSpace = currentSpace
    currentSpace = decl
    defer { currentSpace = decl.parentDeclSpace }

    decl.members = ctx.declBlock()!.accept(self) as! [Decl]
    return decl
  }

  public override func visitViewConfClause(_ ctx: ValParser.ViewConfClauseContext) -> Any {
    return ctx.identTypeRepr().map({ ident in ident.accept(self) as! IdentTypeRepr })
  }

  public override func visitCtrl(_ ctx: ValParser.CtrlContext) -> Any {
    return ctx.children![0].accept(self)!
  }

  public override func visitRetStmt(_ ctx: ValParser.RetStmtContext) -> Any {
    let value = ctx.expr().map({ expr in expr.accept(self) as! Expr })
    return RetStmt(value: value, range: range(of: ctx))
  }

  public override func visitPattern(_ ctx: ValParser.PatternContext) -> Any {
    return ctx.children![0].accept(self) as! Pattern
  }

  public override func visitNamedPattern(_ ctx: ValParser.NamedPatternContext) -> Any {
    // Create a variable declaration for the pattern.
    let decl = VarDecl(name: ctx.getText(), type: unresolvedType, range: range(of: ctx))
    decl.type = TypeVar(context: context, node: decl)
    decl.parentDeclSpace = currentSpace

    // Create the pattern.
    return NamedPattern(decl: decl, range: range(of: ctx))
  }

  public override func visitTuplePattern(_ ctx: ValParser.TuplePatternContext) -> Any {
    let pattern = TuplePattern(
      elems: [],
      type: unresolvedType,
      range: range(of: ctx))
    pattern.type = TypeVar(context: context, node: pattern)

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

  public override func visitWildcardPattern(_ ctx: ValParser.WildcardPatternContext) -> Any {
    let pattern = WildcardPattern(type: unresolvedType, range: range(of: ctx))
    pattern.type = TypeVar(context: context, node: pattern)
    return pattern
  }

  public override func visitTypeRepr(_ ctx: ValParser.TypeReprContext) -> Any {
    return ctx.children![0].accept(self) as! TypeRepr
  }

  public override func visitIdentTypeRepr(_ ctx: ValParser.IdentTypeReprContext) -> Any {
    let names = ctx.NAME()
    return CompoundTypeRepr.create(names.map({ (name) -> UnqualTypeRepr in
      UnqualTypeRepr(
        name: name.getText(), type: unresolvedType, range: range(of: name.getSymbol()!))
    }))
  }

  public override func visitExpr(_ ctx: ValParser.ExprContext) -> Any {
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

      // The assignment operator has a dedicated expression.
      if loc.infixOp == .copy {
        return AssignExpr(lvalue: lhs, rvalue: rhs, range: range(of: ctx))
      }

      let infixFun = UnresolvedMemberExpr(
        base: lhs, memberName: loc.infixOp.rawValue, type: unresolvedType, range: loc.range)
      infixFun.type = TypeVar(context: context, node: infixFun)
      let infixCall = CallExpr(
        callee: infixFun,
        args: [CallArg(value: rhs, range: rhs.range)],
        type: unresolvedType,
        range: range(of: ctx))
      infixCall.type = TypeVar(context: context, node: infixCall)
      return infixCall
    }

    let head = ctx.preExpr()?.accept(self) as! Expr
    let tail = ctx.binExpr().map({ (pair) -> Link in
      let loc = pair.infixOper()!.accept(self) as! InfixOperatorLoc
      return (loc: loc, expr: pair.preExpr()!.accept(self) as! Expr)
    })

    return tree(head: head, tail: tail[0...])
  }


  public override func visitPreExpr(_ ctx: ValParser.PreExprContext) -> Any {
    let expr = ctx.postExpr()!.accept(self) as! Expr

    guard let loc = ctx.prefixOper().map({ op in op.accept(self) as! PrefixOperatorLoc }) else {
      return expr
    }


    // The "address-of" operator has a dedicated expression.
    if loc.prefixOp == .amp {
      return AddrOfExpr(value: expr, type: unresolvedType, range: range(of: ctx))
    }

    let prefixFun = UnresolvedMemberExpr(
      base: expr, memberName: loc.prefixOp.rawValue, type: unresolvedType, range: loc.range)
    let prefixCall = CallExpr(
      callee: prefixFun,
      args: [CallArg(value: expr, range: expr.range)],
      type: unresolvedType,
      range: range(of: ctx))
    prefixCall.type = TypeVar(context: context, node: prefixCall)
    return prefixCall
  }

  public override func visitCallExpr(_ ctx: ValParser.CallExprContext) -> Any {
    let callee = ctx.postExpr()!.accept(self) as! Expr
    let expr = CallExpr(callee: callee, args: [], type: unresolvedType, range: range(of: ctx))
    expr.type = TypeVar(context: context, node: expr)

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
    let base = ctx.postExpr()!.accept(self) as! Expr
    let memberName = ctx.NAME()!.getText()
    let expr = UnresolvedMemberExpr(
      base: base, memberName: memberName, type: unresolvedType, range: range(of: ctx))
    expr.type = TypeVar(context: context, node: expr)
    return expr
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
    expr.type = TypeVar(context: context, node: expr)
    return expr
  }

  public override func visitIdent(_ ctx: ValParser.IdentContext) -> Any {
    let names = ctx.NAME()
    precondition(!names.isEmpty)

    if names.count == 1 {
      return UnresolvedDeclRefExpr(
        name: names[0].getText(),
        type: unresolvedType,
        range: range(of: names[0].getSymbol()!))
    }

    let ns = CompoundTypeRepr
      .create(names.dropLast().map({ (name) -> UnqualTypeRepr in
        UnqualTypeRepr(
          name: name.getText(), type: unresolvedType, range: range(of: name.getSymbol()!))
      }))
    return QualDeclRefExpr(
      namespace: ns, name: names.last!.getText(), type: unresolvedType, range: range(of: ctx))
  }

  public override func visitWildcard(_ ctx: ValParser.WildcardContext) -> Any {
    let expr = WildcardExpr(type: unresolvedType, range: range(of: ctx))
    expr.type = TypeVar(context: context, node: expr)
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

  /// Expands a sequence of declaration contexts into an array of nodes.
  ///
  /// This method is meant to handle situations in which statements from concrete syntax gets get
  /// expanded into multiple abstract nodes.
  private func expand(
    decls: [ParserRuleContext],
    into list: inout [Node]
  ) {
    for ctx in decls {
      switch ctx.accept(self) {
      case let node as Node   : list.append(node)
      case let nodes as [Node]: list.append(contentsOf: nodes)
      default: fatalError("unreachable")
      }
    }
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
