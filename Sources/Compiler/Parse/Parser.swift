import Durian
import Utils

/// # Notes:
///
/// Be careful when writing rules that start with an optional symbol that may be recognized at
/// the beginning of the following construct. A naive combinator may not be able to backtrack.
/// For example:
///
///     let p0 = maybe(foo.and(bar)).and(ham)
///     let p1 = foo.and(bar).or(ham)
///
/// Both `p0` and `p1` will fail to recognize inputs recognized by `ham` if `foo` can recognize
/// the same prefix, as the latter will throw a committing failure it applies `bar` rather than
/// backtracking. A correct definition is:
///
///     let p2 = attempt(foo.and(bar)).or(foo)

/// A namespace for the routines of Val's parser.
public enum Parser {

  /// A parse error.
  public struct ParseError: Error {

    let message: String

    let location: SourceLocation

    init(_ message: String, at location: SourceLocation) {
      self.message = message
      self.location = location
    }

  }

  /// Parses the declarations of `input`, inserts them into `ast[module]`.
  ///
  /// - Returns: `(decls, diagnostics)` where `diagnostics` are the diagnostics produced by the
  ///   parser and `decls` is the ID of the set of parsed declarations, or `nil` if the parser
  ///   failed to process `input`.
  public static func parse(
    _ input: SourceFile,
    into module: NodeID<ModuleDecl>,
    in ast: inout AST
  ) -> (decls: NodeID<TopLevelDeclSet>?, diagnostics: [Diagnostic]) {
    var context = ParserContext(ast: ast, lexer: Lexer(tokenizing: input))

    let decls: NodeID<TopLevelDeclSet>?
    do {
      // Parse the file.
      if let d = try Self.sourceFile.parse(&context) {
        // Make sure we consumed the entire file.
        if let head = context.peek() {
          throw ParseError("expected EOF", at: head.range.first())
        }

        // Parser succeeded.
        context.ast[module].sources.append(d)
        decls = d
      } else {
        decls = nil
      }
    } catch let error {
      var diagnostic = Diagnostic(level: .error, message: "")
      if let error = error as? ParseError {
        diagnostic.message = error.message
        diagnostic.location = error.location
        diagnostic.window = Diagnostic.Window(range: error.location ..< error.location)
      } else {
        diagnostic.message = error.localizedDescription
      }
      context.diagnostics.append(diagnostic)
      decls = nil
    }

    ast = context.ast
    return (decls: decls, diagnostics: context.diagnostics)
  }

  // MARK: Declarations

  private static func anyDecl<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserContext, AnyDeclID>
  where Base.Context == ParserContext, Base.Element: DeclID
  {
    AnyCombinator(parse: { (context) in
      try base.parse(&context).map(AnyDeclID.init(_:))
    })
  }

  static let sourceFile = (
    zeroOrMany(take(.semi))
      .and(zeroOrMany(moduleScopeDecl.and(zeroOrMany(take(.semi))).first))
      .map({ (context, tree) -> NodeID<TopLevelDeclSet> in
        context.ast.insert(TopLevelDeclSet(decls: tree.1))
      })
  )

  static let moduleScopeDecl = (
    decorated(decl: oneOf([
      anyDecl(importDecl),
      anyDecl(namespaceDecl),
      anyDecl(typeAliasDecl),
      anyDecl(productTypeDecl),
      anyDecl(traitDecl),
      anyDecl(extensionDecl),
      anyDecl(conformanceDecl),
      anyDecl(bindingDecl),
      anyDecl(functionDecl),
      anyDecl(subscriptDecl),
      anyDecl(operatorDecl),
    ]))
  )

  static let importDecl = (
    take(.import).and(take(.name))
      .map({ (context, tree) -> NodeID<ImportDecl> in
        let id = context.ast.insert(ImportDecl(
          identifier: SourceRepresentable(token: tree.1, in: context.lexer.source)))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let namespaceDecl: Recursive<ParserContext, NodeID<NamespaceDecl>> = (
    Recursive(_namespaceDecl.parse(_:))
  )

  private static let _namespaceDecl = (
    namespaceHead.and(namespaceBody)
      .map({ (context, tree) -> NodeID<NamespaceDecl> in
        let id = context.ast.insert(NamespaceDecl(
          identifier: SourceRepresentable(token: tree.0.1, in: context.lexer.source),
          members: tree.1))

        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let namespaceHead = (
    take(.namespace).and(take(.name))
  )

  static let namespaceBody = settingFlags(.parsingNamespace, apply: (
    take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(namespaceMember.and(zeroOrMany(take(.semi))).first))
      .and(zeroOrMany(take(.semi))).and(take(.rBrace))
      .map({ (_, tree) -> [AnyDeclID] in tree.0.0.1 })
  ))

  static let namespaceMember = (
    decorated(decl: oneOf([
      anyDecl(namespaceDecl),
      anyDecl(typeAliasDecl),
      anyDecl(productTypeDecl),
      anyDecl(traitDecl),
      anyDecl(extensionDecl),
      anyDecl(conformanceDecl),
      anyDecl(bindingDecl),
      anyDecl(functionDecl),
      anyDecl(subscriptDecl),
    ]))
  )

  static let typeAliasDecl = (
    typeAliasHead.and(typeAliasBody)
      .map({ (context, tree) -> NodeID<TypeAliasDecl> in
        let id = context.ast.insert(TypeAliasDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: context.lexer.source),
          genericClause: tree.0.1,
          body: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let typeAliasHead = (
    take(.typealias).and(take(.name)).and(maybe(genericClause))
  )

  static let typeAliasBody = (
    take(.assign).and(typeExpr)
      .map({ (_, tree) -> TypeAliasDecl.Body in
          .typeExpr(tree.1)
      })
  )

  static let productTypeDecl: Recursive<ParserContext, NodeID<ProductTypeDecl>> = (
    Recursive(_productTypeDecl.parse(_:))
  )

  private static let _productTypeDecl = (
    productTypeHead.and(productTypeBody)
      .map({ (context, tree) -> NodeID<ProductTypeDecl> in
        let id = context.ast.insert(ProductTypeDecl(
          identifier: SourceRepresentable(token: tree.0.0.0.1, in: context.lexer.source),
          genericClause: tree.0.0.1,
          conformances: tree.0.1 ?? [],
          members: tree.1))

        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let productTypeHead = (
    take(.type).and(take(.name)).and(maybe(genericClause)).and(maybe(conformanceList))
  )

  static let productTypeBody = settingFlags(.parsingProductBody, apply: (
    take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(productTypeMember.and(zeroOrMany(take(.semi))).first))
      .and(zeroOrMany(take(.semi))).and(take(.rBrace))
      .map({ (_, tree) -> [AnyDeclID] in tree.0.0.1 })
  ))

  static let productTypeMember = (
    decorated(decl: oneOf([
      anyDecl(typeAliasOrProductTypeDecl),
      anyDecl(initDecl),
      anyDecl(deinitDecl),
      anyDecl(functionDecl),
      anyDecl(bindingDecl),
      anyDecl(subscriptDecl),
      anyDecl(propertyDecl),
    ]))
  )

  static let typeAliasOrProductTypeDecl = (
    Apply<ParserContext, AnyDeclID>({ (context) -> AnyDeclID? in
      let backup = context.backup()
      do {
        if let element = try typeAliasDecl.parse(&context) { return AnyDeclID(element) }
      } catch {}
      context.restore(from: backup)
      return try productTypeDecl.parse(&context).map(AnyDeclID.init(_:))
    })
  )

  static let traitDecl = (
    traitHead.and(traitBody)
      .map({ (context, tree) -> NodeID<TraitDecl> in
        let id = context.ast.insert(TraitDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: context.lexer.source),
          refinements: tree.0.1 ?? [],
          members: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let traitHead = (
    take(.trait).and(take(.name)).and(maybe(conformanceList))
  )

  static let traitBody = settingFlags(.parsingBindingPattern, apply: (
    take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(traitMember.and(zeroOrMany(take(.semi))).first))
      .and(zeroOrMany(take(.semi))).and(take(.rBrace))
      .map({ (_, tree) -> [AnyDeclID] in tree.0.0.1 })
  ))

  static let traitMember = (
    oneOf([
      anyDecl(associatedTypeDecl),
      anyDecl(associatedValueDecl),
      anyDecl(initDecl),
      anyDecl(functionDecl),
      anyDecl(subscriptDecl),
      anyDecl(propertyDecl),
    ])
  )

  static let associatedTypeDecl = (
    take(.type).and(take(.name))
      .and(maybe(conformanceList))
      .and(maybe(whereClause))
      .and(maybe(take(.assign).and(typeExpr).second))
      .map({ (context, tree) -> NodeID<AssociatedTypeDecl> in
        let id = context.ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(token: tree.0.0.0.1, in: context.lexer.source),
          conformances: tree.0.0.1 ?? [],
          whereClause: tree.0.1,
          defaultValue: tree.1))
        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let associatedValueDecl = (
    take(nameTokenWithValue: "value").and(take(.name))
      .and(maybe(whereClause))
      .and(maybe(take(.assign).and(expr).second))
      .map({ (context, tree) -> NodeID<AssociatedValueDecl> in
        let id = context.ast.insert(AssociatedValueDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: context.lexer.source),
          whereClause: tree.0.1,
          defaultValue: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let conformanceDecl = (
    conformancHead.and(conformanceBody)
      .map({ (context, tree) -> NodeID<ConformanceDecl> in
        let id = context.ast.insert(ConformanceDecl(
          subject: tree.0.0.0.1,
          conformances: tree.0.0.1,
          whereClause: tree.0.1,
          members: tree.1))
        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let conformancHead = (
    take(.conformance).and(typeExpr).and(conformanceList).and(maybe(whereClause))
  )

  static let conformanceBody = extensionBody

  static let extensionDecl = (
    extensionHead.and(extensionBody)
      .map({ (context, tree) -> NodeID<ExtensionDecl> in
        let id = context.ast.insert(ExtensionDecl(
          subject: tree.0.0.1,
          whereClause: tree.0.1,
          members: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let extensionHead = (
    take(.extension).and(typeExpr).and(maybe(whereClause))
  )

  static let extensionBody = settingFlags(.parsingExtensionBody, apply: (
    take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(extensionMember.and(zeroOrMany(take(.semi))).first))
      .and(zeroOrMany(take(.semi))).and(take(.rBrace))
      .map({ (_, tree) -> [AnyDeclID] in tree.0.0.1 })
  ))

  static let extensionMember = (
    decorated(decl: oneOf([
      anyDecl(typeAliasOrProductTypeDecl),
      anyDecl(initDecl),
      anyDecl(functionDecl),
      anyDecl(subscriptDecl),
      anyDecl(propertyDecl),
    ]))
  )

  static let bindingDecl = (
    bindingPattern.and(maybe(take(.assign).and(expr)))
      .map({ (context, tree) -> NodeID<BindingDecl> in
        let id = context.ast.insert(BindingDecl(pattern: tree.0, initializer: tree.1?.1))
        context.ast.ranges[id] = context.ast.ranges[tree.0]!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let deinitDecl = (
    take(.deinit).and(deinitBody)
      .map({ (context, tree) -> NodeID<FunDecl> in
        let id = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .deinit, range: tree.0.range),
          body: .block(tree.1)))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let deinitBody = settingFlags(.parsingFunctionBody, apply: braceStmt)

  static let initDecl = Choose(
    memberwiseInitDecl,
    or: initHead
      .and(take(.lParen).and(maybe(parameterList)).and(take(.rParen)))
      .and(initBody)
      .map({ (context, tree) -> NodeID<FunDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)

        let id = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .`init`, range: head.0.range),
          genericClause: head.1,
          parameters: signature.0.1 ?? [],
          body: .block(body)))

        context.ast.ranges[id] = head.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let initHead = (
    take(.`init`).and(maybe(genericClause))
  )

  static let initBody = settingFlags(.parsingFunctionBody, apply: braceStmt)

  static let functionDecl = (
    functionHead.and(functionSignature).and(maybe(functionBody))
      .map({ (context, tree) -> NodeID<FunDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)
        let identifier: FunctionDeclIdentifier = head.0.0

        let id = context.ast.insert(FunDecl(
          introducer: identifier.introducer,
          receiverEffect: signature.receiverEffect,
          notation: identifier.notation,
          identifier: identifier.stem,
          genericClause: head.0.1,
          captures: head.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          body: body))

        let startRange = identifier.notation?.range ?? identifier.introducer.range!
        context.ast.ranges[id] = startRange.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let memberwiseInitDecl = (
    take(nameTokenWithValue: "memberwise").and(take(.`init`))
      .map({ (context, tree) -> NodeID<FunDecl> in
        let id = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .memberwiseInit, range: tree.0.range)))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let functionHead = (
    functionDeclIdentifier.and(maybe(genericClause)).and(maybe(captureList))
  )

  typealias FunctionSignature = (
    parameters: [NodeID<ParameterDecl>],
    receiverEffect: SourceRepresentable<ReceiverEffect>?,
    output: AnyTypeExprID?
  )

  static let functionSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(maybe(take(.arrow).and(typeExpr)))
      .map({ (context, tree) -> FunctionSignature in
        (parameters: tree.0.0.0.1 ?? [], receiverEffect: tree.0.1, output: tree.1?.1)
      })
  )

  typealias FunctionDeclIdentifier = (
    introducer: SourceRepresentable<FunDecl.Introducer>,
    stem: SourceRepresentable<String>?,
    notation: SourceRepresentable<OperatorNotation>?
  )

  static let functionDeclIdentifier = (
    namedFunctionDeclIdentifier.or(operatorFunctionDeclIdentifier)
  )

  static let namedFunctionDeclIdentifier = (
    take(.fun).and(take(.name))
      .map({ (context, tree) -> FunctionDeclIdentifier in
        (
          introducer: SourceRepresentable(value: .`fun`, range: tree.0.range),
          stem: SourceRepresentable(token: tree.1, in: context.lexer.source),
          notation: nil
        )
      })
  )

  static let operatorFunctionDeclIdentifier = (
    operatorNotation.and(take(.fun)).and(operator_)
      .map({ (context, tree) -> FunctionDeclIdentifier in
        (
          introducer: SourceRepresentable(value: .`fun`, range: tree.0.1.range),
          stem: tree.1,
          notation: tree.0.0
        )
      })
  )

  static let functionBody = settingFlags(.parsingFunctionBody, apply: TryCatch(
    trying: methodBundleBody
      .map({ (context, impls) -> FunDecl.Body in .bundle(impls) }),
    orCatchingAndApplying: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (context, tree) -> FunDecl.Body in .expr(tree.0.1) }),
      orCatchingAndApplying: braceStmt
        .map({ (context, id) -> FunDecl.Body in .block(id) })
    )
  ))

  static let methodBundleBody = (
    take(.lBrace).and(methodImpl+).and(take(.rBrace))
      .map({ (context, tree) -> [NodeID<MethodImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        for implID in tree.0.1 {
          let introducer = context.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted {
            context.diagnostics.append(.duplicateMethodIntroducer(at: introducer.range))
          }
        }

        return tree.0.1
      })
  )

  static let methodImpl = (
    methodIntroducer.and(maybe(methodImplBody))
      .map({ (context, tree) -> NodeID<MethodImplDecl> in
        let id = context.ast.insert(MethodImplDecl(introducer: tree.0, body: tree.1))
        context.ast.ranges[id] = tree.0.range!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let methodImplBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (context, tree) -> MethodImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (context, id) -> MethodImplDecl.Body in .block(id) })
  )

  static let methodIntroducer = translate([
    .let  : ImplIntroducer.let,
    .inout: ImplIntroducer.inout,
    .set  : ImplIntroducer.set,
    .sink : ImplIntroducer.sink,
  ])

  static let propertyDecl = (
    take(.property).and(take(.name))
      .and(maybe(receiverEffect))
      .and(take(.colon).and(typeExpr))
      .and(maybe(subscriptBody))
      .map({ (context, tree) -> NodeID<SubscriptDecl> in
        let identifier = SourceRepresentable(token: tree.0.0.0.1, in: context.lexer.source)
        let id = context.ast.insert(SubscriptDecl(
          introducer: SourceRepresentable(value: .property, range: tree.0.0.0.0.range),
          receiverEffect: tree.0.0.1,
          identifier: identifier,
          output: tree.0.1.1,
          impls: tree.1 ?? []))

        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let subscriptDecl = (
    subscriptHead.and(subscriptSignature).and(maybe(subscriptBody))
      .map({ (context, tree) -> NodeID<SubscriptDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)
        let identifier = head.0.0.1.map({
          SourceRepresentable(token: $0, in: context.lexer.source)
        })

        let id = context.ast.insert(SubscriptDecl(
          introducer: SourceRepresentable(value: .subscript, range: head.0.0.0.range),
          receiverEffect: signature.receiverEffect,
          identifier: identifier,
          genericClause: head.0.1,
          explicitCaptures: head.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          impls: body ?? []))

        context.ast.ranges[id] = head.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let subscriptHead = (
    take(.subscript).and(maybe(take(.name))).and(maybe(genericClause)).and(maybe(captureList))
  )

  typealias SubscriptSignature = (
    parameters: [NodeID<ParameterDecl>],
    receiverEffect: SourceRepresentable<ReceiverEffect>?,
    output: AnyTypeExprID
  )

  static let subscriptSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(take(.colon).and(typeExpr))
      .map({ (context, tree) -> SubscriptSignature in
        (parameters: tree.0.0.0.1 ?? [], receiverEffect: tree.0.1, output: tree.1.1)
      })
  )

  static let subscriptBody = settingFlags(.parsingSubscriptBody, apply: TryCatch(
    trying: subscriptBundleBody,
    orCatchingAndApplying: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (context, tree) -> [NodeID<SubscriptImplDecl>] in
          [context.ast.insert(SubscriptImplDecl(
            introducer: SourceRepresentable(value: .let),
            body: .expr(tree.0.1)))]
        }),
      orCatchingAndApplying: braceStmt
        .map({ (context, id) -> [NodeID<SubscriptImplDecl>] in
          if context.ast[id].stmts.isEmpty {
            throw ParseError(
              "expected subscript implementation",
              at: context.ast.ranges[id]!.last()!)
          }

          return [context.ast.insert(SubscriptImplDecl(
            introducer: SourceRepresentable(value: .let),
            body: .block(id)))]
        })
    )
  ))

  static let subscriptBundleBody = (
    take(.lBrace).and(subscriptImpl+).and(take(.rBrace))
      .map({ (context, tree) -> [NodeID<SubscriptImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        for implID in tree.0.1 {
          let introducer = context.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted {
            context.diagnostics.append(.duplicateSubscriptIntroducer(at: introducer.range))
          }
        }

        return tree.0.1
      })
  )

  static let subscriptImpl = (
    subscriptIntroducer.and(maybe(subscriptImplBody))
      .map({ (context, tree) -> NodeID<SubscriptImplDecl> in
        let id = context.ast.insert(SubscriptImplDecl(introducer: tree.0, body: tree.1))
        context.ast.ranges[id] = tree.0.range!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let subscriptImplBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (context, tree) -> SubscriptImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (context, id) -> SubscriptImplDecl.Body in .block(id) })
  )

  static let subscriptIntroducer = translate([
    .let  : ImplIntroducer.let,
    .inout: ImplIntroducer.inout,
    .set  : ImplIntroducer.set,
    .sink : ImplIntroducer.sink,
  ])

  static let parameterList = (
    parameterDecl.and(zeroOrMany(take(.comma).and(parameterDecl).second))
      .map({ (_, tree) -> [NodeID<ParameterDecl>] in [tree.0] + tree.1 })
  )

  static let parameterDecl = (
    parameterInterface
      .and(maybe(take(.colon).and(parameterTypeExpr)))
      .and(maybe(take(.assign).and(expr)))
      .map({ (context, tree) -> NodeID<ParameterDecl> in
        let id = context.ast.insert(ParameterDecl(
          label: tree.0.0.label,
          identifier: tree.0.0.name,
          annotation: tree.0.1?.1,
          defaultValue: tree.1?.1))

        context.ast.ranges[id] = SourceRange(
          in: context.lexer.source,
          from: tree.0.0.label?.range!.lowerBound ?? tree.0.0.name.range!.lowerBound,
          to: context.currentIndex)
        return id
      })
  )

  typealias ParameterInterface = (
    label: SourceRepresentable<Identifier>?,
    name: SourceRepresentable<Identifier>
  )

  static let parameterInterface = (
    Apply<ParserContext, ParameterInterface>({ (context) in
      // Parse a label or bail out.
      guard let labelCandidate = context.take(if: { $0.isLabel || $0.kind == .under }) else {
        return nil
      }

      // Assume the first token is a label and attempt to parse a name.
      if let nameCandidate = context.take(.name) {
        if labelCandidate.kind == .under {
          // case `_ name`
          return (
            label: nil,
            name: SourceRepresentable(token: nameCandidate, in: context.lexer.source))
        } else {
          // case `label name`
          return (
            label: SourceRepresentable(token: labelCandidate, in: context.lexer.source),
            name: SourceRepresentable(token: nameCandidate, in: context.lexer.source))
        }
      }

      // Assume the first token is the name.
      if labelCandidate.kind == .name {
        // case `<no-label> name`
        let name = SourceRepresentable(token: labelCandidate, in: context.lexer.source)
        return (label: name, name: name)
      }

      throw ParseError("expected parameter name", at: labelCandidate.range.first())
    })
  )

  static let operatorDecl = (
    take(.operator).and(operatorNotation).and(operator_)
      .and(maybe(take(.colon).and(precedenceGroup)))
      .map({ (context, tree) -> NodeID<OperatorDecl> in
        let id = context.ast.insert(OperatorDecl(
          notation: tree.0.0.1,
          name: tree.0.1,
          precedenceGroup: tree.1?.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(
          by: tree.1?.1.range?.upperBound ?? tree.0.1.range!.upperBound)
        return id
      })
  )

  static let operator_ = (
    Apply<ParserContext, SourceRepresentable<Identifier>>({ (context) in
      context.takeOperator()
    })
  )

  static let operatorNotation = translate([
    .infix  : OperatorNotation.infix,
    .prefix : OperatorNotation.prefix,
    .postfix: OperatorNotation.postfix,
  ])

  static let precedenceGroup = ContextualKeyword<PrecedenceGroup>()

  static let memberModifier = (
    take(.static)
      .map({ (_, token) -> SourceRepresentable<MemberModifier> in
        SourceRepresentable(value: .static, range: token.range)
      })
  )

  static let accessModifier = (
    take(.public)
      .map({ (_, token) -> SourceRepresentable<AccessModifier> in
        SourceRepresentable(value: .public, range: token.range)
      })
  )

  /// Applies `decl` after having parsed its attributes and modifiers, and tries to apply those
  /// to the parsed declaration, producing hard failures only if `decl` did or if at least one
  /// attribute or modifier has been parsed.
  private static func decorated<Decl: Combinator>(decl: Decl) -> Apply<ParserContext, AnyDeclID>
  where Decl.Context == ParserContext, Decl.Element == AnyDeclID
  {
    Apply({ (context) in
      guard let startIndex = context.peek()?.range.lowerBound else { return nil }

      // Parse attributes and modifiers.
      var attributes: [SourceRepresentable<Attribute>] = []
      while let a = try declAttribute.parse(&context) {
        attributes.append(a)
      }
      let access = try accessModifier.parse(&context)
      let member = try memberModifier.parse(&context)

      // Parse the declaration.
      guard let declID = try decl.parse(&context) else {
        if attributes.isEmpty && (access == nil) && (member == nil) {
          return nil
        } else {
          throw ParseError("expected declaration", at: context.currentLocation)
        }
      }

      switch declID.kind {
      case .bindingDecl:
        let id = NodeID<BindingDecl>(rawValue: declID.rawValue)
        context.ast[id].attributes = attributes
        context.ast[id].accessModifier = access
        context.ast[id].memberModifier = member

      case .conformanceDecl:
        let id = NodeID<ConformanceDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .importDecl:
        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let a = access {
          context.diagnostics.append(.unexpectedDeclModifier(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .extensionDecl:
        let id = NodeID<ExtensionDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .funDecl:
        let id = NodeID<FunDecl>(rawValue: declID.rawValue)
        context.ast[id].attributes = attributes
        context.ast[id].accessModifier = access
        context.ast[id].memberModifier = member

      case .operatorDecl:
        let id = NodeID<OperatorDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .namespaceDecl:
        let id = NodeID<NamespaceDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .productTypeDecl:
        let id = NodeID<ProductTypeDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .subscriptDecl:
        let id = NodeID<SubscriptDecl>(rawValue: declID.rawValue)
        context.ast[id].attributes = attributes
        context.ast[id].accessModifier = access
        context.ast[id].memberModifier = member

      case .traitDecl:
        let id = NodeID<TraitDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .typeAliasDecl:
        let id = NodeID<TypeAliasDecl>(rawValue: declID.rawValue)
        context.ast[id].accessModifier = access

        if let a = attributes.first {
          context.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          context.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      default:
        unreachable("unexpected declaration")
      }

      context.ast.ranges[declID]?.lowerBound = startIndex
      return declID
    })
  }

  static let captureList = (
    take(.lBrack)
      .and(bindingDecl.and(zeroOrMany(take(.comma).and(bindingDecl).second)))
      .and(take(.rBrack))
      .map({ (_, tree) -> [NodeID<BindingDecl>] in [tree.0.1.0] + tree.0.1.1 })
  )

  static let genericClause = (
    take(.lAngle).and(genericParameterList).and(maybe(whereClause)).and(take(.rAngle))
      .map({ (context, tree) -> SourceRepresentable<GenericClause> in
        return SourceRepresentable(
          value: GenericClause(parameters: tree.0.0.1, whereClause: tree.0.1),
          range: tree.0.0.0.range.upperBounded(by: context.currentIndex))
      })
  )

  static let genericParameterList = (
    genericParameter.and(zeroOrMany(take(.comma).and(genericParameter).second))
      .map({ (_, tree) -> [GenericParamDeclID] in [tree.0] + tree.1 })
  )

  static let genericParameter = (
    genericValueParameter.or(genericTypeParameter)
  )

  static let genericTypeParameter = (
    maybe(typeAttribute).andCollapsingSoftFailures(take(.name))
      .and(maybe(take(.colon).and(traitComposition)))
      .and(maybe(take(.assign).and(typeExpr)))
      .map({ (context, tree) -> GenericParamDeclID in
        let id = context.ast.insert(GenericTypeParamDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: context.lexer.source),
          conformances: tree.0.1?.1 ?? [],
          defaultValue: tree.1?.1))

        context.ast.ranges[id] = SourceRange(
          in: context.lexer.source,
          from: tree.0.0.0?.range.lowerBound ?? tree.0.0.1.range.lowerBound,
          to: context.currentIndex)
        return .type(id)
      })
  )

  static let genericValueParameter = (
    valueAttribute.and(take(.name))
      .and(take(.colon).and(typeExpr))
      .and(maybe(take(.assign).and(expr)))
      .map({ (context, tree) -> GenericParamDeclID in
        let id = context.ast.insert(GenericValueParamDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: context.lexer.source),
          annotation: tree.0.1.1,
          defaultValue: tree.1?.1))

        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return .value(id)
      })
  )

  static let conformanceList = (
    take(.colon).and(nameTypeExpr).and(zeroOrMany(take(.comma).and(nameTypeExpr).second))
      .map({ (context, tree) -> [NodeID<NameTypeExpr>] in [tree.0.1] + tree.1 })
  )

  // MARK: Value expressions

  private static func anyExpr<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserContext, AnyExprID>
  where Base.Context == ParserContext, Base.Element: ExprID
  {
    AnyCombinator(parse: { (context) in
      try base.parse(&context).map(AnyExprID.init(_:))
    })
  }

  static let expr: Recursive<ParserContext, AnyExprID> = (
    Recursive(infixExpr.parse(_:))
  )

  static let infixExpr = (
    Apply<ParserContext, AnyExprID>({ (context) -> AnyExprID? in
      guard let lhs = try infixExprHead.parse(&context) else { return nil }
      let leftRange = context.ast.ranges[lhs]!

      // Nothing more to parse if there isn't any whitespace before the next token.
      if !context.hasLeadingWhitespace { return lhs }

      // type-casting-tail
      if let oper = context.take(.cast) {
        if !context.hasLeadingWhitespace {
          context.diagnostics.append(.infixOperatorRequiresWhitespaces(at: oper.range))
        }

        guard let rhs = try typeExpr.parse(&context) else {
          throw ParseError("expected type expression", at: context.currentLocation)
        }

        let castKind: CastExpr.Kind
        switch context.lexer.source[oper.range] {
        case "as":
          castKind = .up
        case "as!":
          castKind = .down
        case "as!!":
          castKind = .builtinPointerConversion
        default:
          unreachable()
        }

        let cast = context.ast.insert(CastExpr(left: lhs, right: rhs, kind: castKind))
        context.ast.ranges[cast] = leftRange.upperBounded(by: context.currentIndex)
        return AnyExprID(cast)
      }

      // infix-operator-tail (with assign)
      if let oper = context.take(.assign) {
        if !context.hasLeadingWhitespace {
          context.diagnostics.append(.infixOperatorRequiresWhitespaces(at: oper.range))
        }

        guard let rhs = try prefixExpr.parse(&context) else {
          throw ParseError("expected expression", at: context.currentLocation)
        }

        let assign = context.ast.insert(AssignExpr(left: lhs, right: rhs))
        context.ast.ranges[assign] = leftRange.upperBounded(by: context.currentIndex)
        return AnyExprID(assign)
      }

      // infix-operator-tail
      var tail: SequenceExpr.UnfoldedTail = []
      while let operatorName = context.takeOperator() {
        if !context.hasLeadingWhitespace {
          context.diagnostics.append(.infixOperatorRequiresWhitespaces(at: operatorName.range))
        }

        guard let operand = try prefixExpr.parse(&context) else {
          throw ParseError("expected type expression", at: context.currentLocation)
        }

        tail.append(SequenceExpr.TailElement(operatorName: operatorName, operand: operand))
      }

      if tail.isEmpty {
        return lhs
      } else {
        let sequence = context.ast.insert(SequenceExpr.unfolded(head: lhs, tail: tail))
        context.ast.ranges[sequence] = leftRange.upperBounded(by: context.currentIndex)
        return AnyExprID(sequence)
      }
    })
  )

  static let infixExprHead = (
    anyExpr(asyncExpr).or(anyExpr(awaitExpr)).or(prefixExpr)
  )

  static let asyncExpr = TryCatch(
    trying: asyncExprInline,
    orCatchingAndApplying: asyncExprBlock
  )

  static let asyncExprBlock = (
    asyncExprHead.and(take(.arrow)).and(typeExpr).and(asyncExprBody)
      .map({ (context, tree) -> NodeID<AsyncExpr> in
        let decl = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.0.0.range),
          receiverEffect: tree.0.0.0.1,
          output: tree.0.1,
          body: .block(tree.1),
          isInExprContext: true))
        context.ast.ranges[decl] = tree.0.0.0.0.0.range.upperBounded(by: context.currentIndex)

        let id = context.ast.insert(AsyncExpr(decl: decl))
        context.ast.ranges[id] = context.ast.ranges[decl]
        return id
      })
  )

  static let asyncExprBody = settingFlags(.parsingFunctionBody, apply: braceStmt)

  static let asyncExprInline = (
    asyncExprHead.and(expr)
      .map({ (context, tree) -> NodeID<AsyncExpr> in
        let decl = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.range),
          receiverEffect: tree.0.1,
          captures: tree.0.0.1 ?? [],
          body: .expr(tree.1),
          isInExprContext: true))
        context.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: context.currentIndex)

        let id = context.ast.insert(AsyncExpr(decl: decl))
        context.ast.ranges[id] = context.ast.ranges[decl]
        return id
      })
  )

  static let asyncExprHead = (
    take(.async).and(maybe(captureList)).and(maybe(receiverEffect))
  )

  static let awaitExpr = (
    take(.await).and(expr)
      .map({ (context, tree) -> NodeID<AwaitExpr> in
        let id = context.ast.insert(AwaitExpr(operand: tree.1))
        context.ast.ranges[id] = tree.0.range.upperBounded(
          by: context.ast.ranges[tree.1]!.upperBound)
        return id
      })
  )

  static let prefixExpr = Choose(
    postfixExpr,
    or: prefixOperator.and(withoutLeadingWhitespace(postfixExpr))
      .map({ (context, tree) -> AnyExprID in
        let callee = context.ast.insert(NameExpr(
          domain: .expr(tree.1),
          name: SourceRepresentable(
            value: Name(stem: tree.0.value, notation: .prefix),
            range: tree.0.range)))
        context.ast.ranges[callee] = tree.0.range!.upperBounded(
          by: context.ast.ranges[tree.1]!.upperBound)

        let call = context.ast.insert(FunCallExpr(callee: AnyExprID(callee)))
        context.ast.ranges[call] = context.ast.ranges[callee]
        return AnyExprID(call)
      })
  )

  static let prefixOperator = (
    Apply<ParserContext, SourceRepresentable<Identifier>>({ (context) in
      if let t = context.peek(), t.isPrefixOperatorHead {
        return context.takeOperator()
      } else {
        return nil
      }
    })
  )

  static let postfixExpr = (
    compoundExpr.and(maybe(withoutLeadingWhitespace(postfixOperator)))
      .map({ (context, tree) -> AnyExprID in
        if let oper = tree.1 {
          let callee = context.ast.insert(NameExpr(
            domain: .expr(tree.0),
            name: SourceRepresentable(
              value: Name(stem: oper.value, notation: .postfix), range: oper.range)))
          context.ast.ranges[callee] = context.ast.ranges[tree.0]!.upperBounded(
            by: oper.range!.upperBound)

          let call = context.ast.insert(FunCallExpr(callee: AnyExprID(callee)))
          context.ast.ranges[call] = context.ast.ranges[callee]
          return AnyExprID(call)
        } else {
          return tree.0
        }
      })
  )

  static let postfixOperator = (
    Apply<ParserContext, SourceRepresentable<Identifier>>({ (context) in
      if let t = context.peek(), t.isPostfixOperatorHead {
        return context.takeOperator()
      } else {
        return nil
      }
    })
  )

  static let compoundExpr = (
    Apply<ParserContext, AnyExprID>({ (context) -> AnyExprID? in
      var backup = context.backup()
      let base: AnyExprID?

      do {
        // Parse a primary expression first.
        base = try primaryExpr.parse(&context)
      } catch let primaryExprParseError {
        // Parsing a primary expression returned a hard failure.
        swap(&context, &backup)
        do {
          // Parse a static value member.
          base = try staticValueMemberExpr.parse(&context).map(AnyExprID.init(_:))
        } catch {
          // Parsing a static value member failed too; return the first error.
          swap(&context, &backup)
          throw primaryExprParseError
        }
      }

      // Base is `nil` if and only if `primaryExpr` returned a soft error.
      var head: AnyExprID
      if let b = try base ?? staticValueMemberExpr.parse(&context).map(AnyExprID.init(_:)) {
        head = b
      } else {
        return nil
      }
      let headRange = context.ast.ranges[head]!

      while true {
        // value-member-expr
        if context.take(.dot) != nil {
          // labeled-member-expr
          if let member = try primaryDeclRef.parse(&context) {
            context.ast[member].domain = .expr(head)
            context.ast.ranges[member] = headRange.upperBounded(by: context.currentIndex)
            head = AnyExprID(member)
            continue
          }

          // indexed-member-expr
          if let index = context.takeMemberIndex() {
            head = AnyExprID(context.ast.insert(TupleMemberExpr(tuple: head, index: index)))
            context.ast.ranges[head] = headRange.upperBounded(by: context.currentIndex)
            continue
          }

          throw ParseError("expected member name", at: context.currentLocation)
        }

        // Exit if there's a new line before the next token.
        guard let next = context.peek(),
              !context.hasNewline(inCharacterStreamUpTo: next.range.lowerBound)
        else { break }

        // function-call-expr
        if context.take(.lParen) != nil {
          let arguments = try argumentList.parse(&context) ?? []
          guard context.take(.rParen) != nil else {
            throw ParseError("expected ')'", at: context.currentLocation)
          }

          head = AnyExprID(context.ast.insert(FunCallExpr(
            callee: head, arguments: arguments)))
          context.ast.ranges[head] = headRange.upperBounded(by: context.currentIndex)
          continue
        }

        // subscript-call-expr
        if context.take(.lBrack) != nil {
          let arguments = try argumentList.parse(&context) ?? []
          guard context.take(.rBrack) != nil else {
            throw ParseError("expected ']'", at: context.currentLocation)
          }

          head = AnyExprID(context.ast.insert(SubscriptCallExpr(
            callee: head, arguments: arguments)))
          context.ast.ranges[head] = headRange.upperBounded(by: context.currentIndex)
          continue
        }

        break
      }

      return head
    })
  )

  static let argumentList = (
    callArgument.and(zeroOrMany(take(.comma).and(callArgument).second))
      .map({ (_, tree) -> [CallArgument] in [tree.0] + tree.1 })
  )

  static let callArgument = (
    Apply<ParserContext, CallArgument>({ (context) in
      let backup = context.backup()

      // Parse a labeled arrgument.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let value = try expr.parse(&context) {
            return CallArgument(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled argument.
      context.restore(from: backup)
      if let value = try expr.parse(&context) {
        return CallArgument(value:value)
      }

      return nil
    })
  )

  static let staticValueMemberExpr = (
    primaryTypeExpr.and(take(.dot)).and(primaryDeclRef)
      .map({ (context, tree) -> NodeID<NameExpr> in
        context.ast[tree.1].domain = .type(tree.0.0)
        context.ast.ranges[tree.1] = context.ast.ranges[tree.0.0]!.upperBounded(
          by: context.ast.ranges[tree.1]!.upperBound)
        return tree.1
      })
  )

  static let primaryExpr = (
    oneOf([
      anyExpr(booleanLiteral),
      anyExpr(integerLiteral),
      anyExpr(floatingPointLiteral),
      anyExpr(stringLiteral),
      anyExpr(compoundLiteral),
      anyExpr(primaryDeclRef),
      anyExpr(implicitMemberRef),
      anyExpr(lambdaExpr),
      anyExpr(matchExpr),
      anyExpr(conditionalExpr),
      anyExpr(tupleExpr),
      anyExpr(inoutExpr),
      anyExpr(nilExpr),
    ])
  )

  static let booleanLiteral = (
    take(.bool)
      .map({ (context, token) -> NodeID<BooleanLiteralExpr> in
        let id = context.ast.insert(BooleanLiteralExpr(
          value: context.lexer.source[token.range] == "true"))
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let integerLiteral = (
    take(.int)
      .map({ (context, token) -> NodeID<IntegerLiteralExpr> in
        let id = context.ast.insert(IntegerLiteralExpr(
          value: context.lexer.source[token.range].filter({ $0 != "_" })))
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let floatingPointLiteral = (
    take(.float)
      .map({ (context, token) -> NodeID<FloatLiteralExpr> in
        let id = context.ast.insert(FloatLiteralExpr(
          value: context.lexer.source[token.range].filter({ $0 != "_" })))
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let stringLiteral = (
    take(.string)
      .map({ (context, token) -> NodeID<StringLiteralExpr> in
        let id = context.ast.insert(StringLiteralExpr(
          value: String(context.lexer.source[token.range].dropFirst().dropLast())))
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let compoundLiteral = TryCatch(
    trying: bufferLiteral
      .map({ (_, id) -> AnyExprID in AnyExprID(id) }),
    orCatchingAndApplying: mapLiteral
      .map({ (_, id) -> AnyExprID in AnyExprID(id) }))

  static let bufferLiteral = (
    take(.lBrack).and(maybe(bufferComponentList)).and(take(.rBrack))
      .map({ (context, tree) -> NodeID<BufferLiteralExpr> in
        let id = context.ast.insert(BufferLiteralExpr(elements: tree.0.1 ?? []))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let bufferComponentList = (
    expr.and(zeroOrMany(take(.comma).and(expr).second))
      .map({ (context, tree) -> [AnyExprID] in [tree.0] + tree.1 })
  )

  static let mapLiteral = (
    take(.lBrack).and(mapComponentList.or(mapComponentEmptyList)).and(take(.rBrack))
      .map({ (context, tree) -> NodeID<MapLiteralExpr> in
        let id = context.ast.insert(MapLiteralExpr(elements: tree.0.1))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let mapComponentEmptyList = (
    take(.colon)
      .map({ (_, _) -> [MapLiteralExpr.Element] in [] })
  )

  static let mapComponentList = (
    mapComponent.and(zeroOrMany(take(.comma).and(mapComponent).second))
      .map({ (context, tree) -> [MapLiteralExpr.Element] in [tree.0] + tree.1 })
  )

  static let mapComponent = (
    expr.and(take(.colon)).and(expr)
      .map({ (_, tree) -> MapLiteralExpr.Element in
        MapLiteralExpr.Element(key: tree.0.0, value: tree.1)
      })
  )

  static let primaryDeclRef = (
    identifierExpr.and(maybe(staticArgumentList))
      .map({ (context, tree) -> NodeID<NameExpr> in
        let id = context.ast.insert(NameExpr(name: tree.0, arguments: tree.1 ?? []))
        context.ast.ranges[id] = tree.0.range!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let implicitMemberRef = (
    take(.dot).and(primaryDeclRef)
      .map({ (context, tree) -> NodeID<NameExpr> in
        context.ast[tree.1].domain = .implicit
        context.ast.ranges[tree.1] = tree.0.range.upperBounded(
          by: context.ast.ranges[tree.1]!.upperBound)
        return tree.1
      })
  )

  static let lambdaExpr = (
    take(.fun).and(maybe(captureList)).and(functionSignature).and(lambdaBody)
      .map({ (context, tree) -> NodeID<LambdaExpr> in
        let signature = tree.0.1

        let decl = context.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.range),
          receiverEffect: signature.receiverEffect,
          captures: tree.0.0.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          body: tree.1,
          isInExprContext: true))
        context.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: context.currentIndex)

        let id = context.ast.insert(LambdaExpr(decl: decl))
        context.ast.ranges[id] = context.ast.ranges[decl]
        return id
      })
  )

  static let lambdaBody = settingFlags(.parsingFunctionBody, apply: TryCatch(
    trying: methodBundleBody
      .map({ (context, impls) -> FunDecl.Body in .bundle(impls) }),
    orCatchingAndApplying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (context, tree) -> FunDecl.Body in .expr(tree.0.1) })
  ))

  static let matchExpr = (
    take(.match).and(expr).and(take(.lBrace)).and(zeroOrMany(matchCase)).and(take(.rBrace))
      .map({ (context, tree) -> NodeID<MatchExpr> in
        let id = context.ast.insert(MatchExpr(subject: tree.0.0.0.1, cases: tree.0.1))
        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let matchCase = (
    pattern.and(maybe(take(.where).and(expr))).and(matchCaseBody)
      .map({ (context, tree) -> NodeID<MatchCase> in
        let id = context.ast.insert(MatchCase(
          pattern: tree.0.0, condition: tree.0.1?.1, body: tree.1))
        context.ast.ranges[id] = context.ast.ranges[tree.0.0]!.upperBounded(
          by: context.currentIndex)
        return id
      })
  )

  static let matchCaseBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (context, tree) -> MatchCase.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (context, id) -> MatchCase.Body in .block(id) })
  )

  static let conditionalExpr: Recursive<ParserContext, NodeID<CondExpr>> = (
    Recursive(_conditionalExpr.parse(_:))
  )

  private static let _conditionalExpr = (
    take(.if).and(conditionalClause).and(conditionalExprBody).and(maybe(conditionalTail))
      .map({ (context, tree) -> NodeID<CondExpr> in
        let id = context.ast.insert(CondExpr(
          condition: tree.0.0.1, success: tree.0.1, failure: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let conditionalClause = (
    conditionalClauseItem.and(zeroOrMany(take(.comma).and(conditionalClauseItem).second))
      .map({ (_, tree) -> [ConditionItem] in [tree.0] + tree.1 })
  )

  static let conditionalClauseItem = Choose(
    bindingPattern.and(take(.assign)).and(expr)
      .map({ (context, tree) -> ConditionItem in
        let id = context.ast.insert(BindingDecl(pattern: tree.0.0, initializer: tree.1))
        context.ast.ranges[id] = context.ast.ranges[tree.0.0]!.upperBounded(
          by: context.currentIndex)
        return .decl(id)
      }),
    or: expr
      .map({ (_, id) -> ConditionItem in .expr(id) })
  )

  static let conditionalExprBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (context, tree) -> CondExpr.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (context, id) -> CondExpr.Body in .block(id) })
  )

  static let conditionalTail = (
    take(.else).and(TryCatch(
      trying: conditionalExpr
        .map({ (_, id) -> CondExpr.Body in .expr(AnyExprID(id)) }),
      orCatchingAndApplying: conditionalExprBody
        .map({ (_, body) -> CondExpr.Body in body })))
    .map({ (_, tree) -> CondExpr.Body in tree.1 })
  )

  static let inoutExpr = (
    take(.ampersand).and(withoutLeadingWhitespace(expr))
      .map({ (context, tree) -> NodeID<InoutExpr> in
        let id = context.ast.insert(InoutExpr(
          operatorRange: tree.0.range, subexpr: tree.1))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let tupleExpr = (
    take(.lParen).and(maybe(tupleExprElementList)).and(take(.rParen))
      .map({ (context, tree) -> NodeID<TupleExpr> in
        let id = context.ast.insert(TupleExpr(elements: tree.0.1 ?? []))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let tupleExprElementList = (
    tupleExprElement.and(zeroOrMany(take(.comma).and(tupleExprElement).second))
      .map({ (_, tree) -> [TupleExpr.Element] in [tree.0] + tree.1 })
  )

  static let tupleExprElement = (
    Apply<ParserContext, TupleExpr.Element>({ (context) in
      let backup = context.backup()

      // Parse a labeled element.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let value = try expr.parse(&context) {
            return TupleExpr.Element(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      context.restore(from: backup)
      if let value = try expr.parse(&context) {
        return TupleExpr.Element(value: value)
      }

      return nil
    })
  )

  static let nilExpr = (
    take(.nil)
      .map({ (context, token) -> NodeID<NilExpr> in
        let id = context.ast.insert(NilExpr())
        context.ast.ranges[id] = token.range
        return id
      })
  )

  // MARK: Patterns

  private static func anyPattern<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserContext, AnyPatternID>
  where Base.Context == ParserContext, Base.Element: PatternID
  {
    AnyCombinator(parse: { (context) in
      try base.parse(&context).map(AnyPatternID.init(_:))
    })
  }

  static let pattern: Recursive<ParserContext, AnyPatternID> = (
    Recursive(_pattern.parse(_:))
  )

  private static let _pattern = (
    oneOf([
      anyPattern(bindingPattern),
      anyPattern(exprPattern),
      anyPattern(tuplePattern),
      anyPattern(wildcardPattern),
    ])
  )

  static let bindingPattern = (
    bindingIntroducer
      .and(settingFlags(.parsingBindingPattern, apply: oneOf([
        anyPattern(namePattern),
        anyPattern(tuplePattern),
        anyPattern(wildcardPattern),
      ])))
      .and(maybe(take(.colon).and(typeExpr)))
      .map({ (context, tree) -> NodeID<BindingPattern> in
        let id = context.ast.insert(BindingPattern(
          introducer: tree.0.0,
          subpattern: tree.0.1,
          annotation: tree.1?.1))
        context.ast.ranges[id] = tree.0.0.range!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let bindingIntroducer = translate([
    .let  : BindingPattern.Introducer.let,
    .var  : BindingPattern.Introducer.var,
    .inout: BindingPattern.Introducer.inout,
  ])

  static let exprPattern = (
    Apply<ParserContext, AnyPatternID>({ (context) -> AnyPatternID? in
      // Attempt to parse tuples as patterns as deeply as possible.
      if let patternID = try tuplePattern.parse(&context) {
        return AnyPatternID(patternID)
      }

      // Attempt to parse a name pattern if `parsingBindingPattern` is set.
      if context.flags[.parsingBindingPattern] {
        if let patternID = try namePattern.parse(&context) {
          return AnyPatternID(patternID)
        }
      }

      // Default to an expression.
      guard let exprID = try expr.parse(&context) else { return nil }
      let id = context.ast.insert(ExprPattern(expr: exprID))
      context.ast.ranges[id] = context.ast.ranges[exprID]
      return AnyPatternID(id)
    })
  )

  static let namePattern = (
    take(.name)
      .map({ (context, token) -> NodeID<NamePattern> in
        let declID = context.ast.insert(VarDecl(
          identifier: SourceRepresentable(token: token, in: context.lexer.source)))
        context.ast.ranges[declID] = token.range

        let id = context.ast.insert(NamePattern(decl: declID))
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let tuplePattern = (
    take(.lParen).and(maybe(tuplePatternElementList)).and(take(.rParen))
      .map({ (context, tree) -> NodeID<TuplePattern> in
        let id = context.ast.insert(TuplePattern(elements: tree.0.1 ?? []))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: tree.1.range.upperBound)
        return id
      })
  )

  static let tuplePatternElementList = (
    tuplePatternElement.and(zeroOrMany(take(.comma).and(tuplePatternElement).second))
      .map({ (_, tree) -> [TuplePattern.Element] in [tree.0] + tree.1 })
  )

  static let tuplePatternElement = (
    Apply<ParserContext, TuplePattern.Element>({ (context) in
      let backup = context.backup()

      // Parse a labeled element.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let value = try pattern.parse(&context) {
            return TuplePattern.Element(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              pattern: value)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      context.restore(from: backup)
      if let value = try pattern.parse(&context) {
        return TuplePattern.Element(pattern: value)
      }

      return nil
    })
  )

  static let wildcardPattern = (
    take(.under)
      .map({ (context, token) -> NodeID<WildcardPattern> in
        let id = context.ast.insert(WildcardPattern())
        context.ast.ranges[id] = token.range
        return id
      })
  )

  // MARK: Statements

  private static func anyStmt<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserContext, AnyStmtID>
  where Base.Context == ParserContext, Base.Element: StmtID
  {
    AnyCombinator(parse: { (context) in
      try base.parse(&context).map(AnyStmtID.init(_:))
    })
  }

  static let stmt: Recursive<ParserContext, AnyStmtID> = (
    Recursive(_stmt.parse(_:))
  )

  static let _stmt = (
    oneOf([
      anyStmt(braceStmt),
      anyStmt(discardStmt),
      anyStmt(doWhileStmt),
      anyStmt(whileStmt),
      anyStmt(forStmt),
      anyStmt(returnStmt),
      anyStmt(yieldStmt),
      anyStmt(breakStmt),
      anyStmt(continueStmt),
      anyStmt(bindingStmt),
      anyStmt(declStmt),
      anyStmt(exprStmt),
    ])
  )

  static let braceStmt = (
    take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(stmt.and(zeroOrMany(take(.semi))).first))
      .and(take(.rBrace))
      .map({ (context, tree) -> NodeID<BraceStmt> in
        let id = context.ast.insert(BraceStmt(stmts: tree.0.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let discardStmt = (
    take(.under).and(take(.assign)).and(expr)
      .map({ (context, tree) -> NodeID<DiscardStmt> in
        let id = context.ast.insert(DiscardStmt(expr: tree.1))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let doWhileStmt = (
    take(.do).and(loopBody).and(take(.while)).and(expr)
      .map({ (context, tree) -> NodeID<DoWhileStmt> in
        let id = context.ast.insert(DoWhileStmt(body: tree.0.0.1, condition: tree.1))
        context.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let whileStmt = (
    take(.while).and(conditionalClause).and(loopBody)
      .map({ (context, tree) -> NodeID<WhileStmt> in
        let id = context.ast.insert(WhileStmt(condition: tree.0.1, body: tree.1))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let forStmt = (
    take(.for).and(bindingPattern).and(forRange).and(maybe(forFilter)).and(loopBody)
      .map({ (context, tree) -> NodeID<ForStmt> in
        let decl = context.ast.insert(BindingDecl(pattern: tree.0.0.0.1))
        context.ast.ranges[decl] = context.ast.ranges[tree.0.0.0.1]

        let id = context.ast.insert(ForStmt(
          binding: decl, domain: tree.0.0.1, filter: tree.0.1, body: tree.1))
        context.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let forRange = (
    take(.in).and(expr).second
  )

  static let forFilter = (
    take(.where).and(expr).second
  )

  static let loopBody = settingFlags(.parsingLoopBody, apply: braceStmt)

  static let returnStmt = (
    take(.return).and(maybe(onSameLine(expr)))
      .map({ (context, tree) -> NodeID<ReturnStmt> in
        let id = context.ast.insert(ReturnStmt(value: tree.1))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let yieldStmt = (
    take(.yield).and(onSameLine(expr))
      .map({ (context, tree) -> NodeID<YieldStmt> in
        let id = context.ast.insert(YieldStmt(value: tree.1))
        context.ast.ranges[id] = tree.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let breakStmt = (
    take(.break)
      .map({ (context, token) -> NodeID<BreakStmt> in
        let id = context.ast.insert(BreakStmt())
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let continueStmt = (
    take(.break)
      .map({ (context, token) -> NodeID<ContinueStmt> in
        let id = context.ast.insert(ContinueStmt())
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let bindingStmt = (
    Apply<ParserContext, AnyStmtID>({ (context) -> AnyStmtID? in
      let backup = context.backup()
      do {
        if let element = try conditionalBindingStmt.parse(&context) { return AnyStmtID(element) }
      } catch {}
      context.restore(from: backup)

      if let decl = try bindingDecl.parse(&context) {
        let id = context.ast.insert(DeclStmt(decl: AnyDeclID(decl)))
        context.ast.ranges[id] = context.ast.ranges[decl]
        return AnyStmtID(id)
      } else {
        return nil
      }
    })
  )

  static let conditionalBindingStmt = (
    bindingDecl.and(take(.else)).and(conditionalBindingFallback)
      .map({ (context, tree) -> NodeID<CondBindingStmt> in
        let bindingRange = context.ast.ranges[tree.0.0]!

        if context.ast[tree.0.0].initializer == nil {
          throw ParseError(
            "conditional binding requires an initializer", at: bindingRange.first())
        }

        let id = context.ast.insert(CondBindingStmt(
          binding: tree.0.0, fallback: tree.1))
        context.ast.ranges[id] = bindingRange.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let conditionalBindingFallback = (
    conditionalBindingFallbackStmt.or(conditionalBindingFallbackExpr)
  )

  static let conditionalBindingFallbackExpr = (
    expr.map({ (_, id) -> CondBindingStmt.Fallback in .expr(id) })
  )

  static let conditionalBindingFallbackStmt = (
    oneOf([
      anyStmt(breakStmt),
      anyStmt(continueStmt),
      anyStmt(returnStmt),
      anyStmt(braceStmt),
    ])
    .map({ (_, id) -> CondBindingStmt.Fallback in .exit(id) })
  )

  static let declStmt = (
    localDecl
      .map({ (context, decl) -> NodeID<DeclStmt> in
        let id = context.ast.insert(DeclStmt(decl: decl))
        context.ast.ranges[id] = context.ast.ranges[decl]
        return id
      })
  )

  static let localDecl = (
    oneOf([
      anyDecl(typeAliasDecl),
      anyDecl(productTypeDecl),
      anyDecl(extensionDecl),
      anyDecl(conformanceDecl),
      anyDecl(functionDecl),
      anyDecl(subscriptDecl),
    ])
  )

  static let exprStmt = (
    expr
      .map({ (context, expr) -> NodeID<ExprStmt> in
        let id = context.ast.insert(ExprStmt(expr: expr))
        context.ast.ranges[id] = context.ast.ranges[expr]
        return id
      })
  )

  // MARK: Type expressions

  private static func anyTypeExpr<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserContext, AnyTypeExprID>
  where Base.Context == ParserContext, Base.Element: TypeExprID
  {
    AnyCombinator(parse: { (context) in
      try base.parse(&context).map(AnyTypeExprID.init(_:))
    })
  }

  static let typeExpr: Recursive<ParserContext, AnyTypeExprID> = (
    Recursive(unionTypeExpr.parse(_:))
  )

  // static let storedProjectionTypeExpr = ?

  static let unionTypeExpr = (
    modifiedTypeExpr.and(zeroOrMany(take(.pipe).and(modifiedTypeExpr).second))
      .map({ (context, tree) -> AnyTypeExprID in
        if tree.1.isEmpty {
          return tree.0
        } else {
          let elements = [tree.0] + tree.1
          let id = context.ast.insert(UnionTypeExpr(elements: elements))
          context.ast.ranges[id] = context.ast.ranges[tree.0]!.upperBounded(
            by: context.currentIndex)
          return AnyTypeExprID(id)
        }
      })
  )

  static let modifiedTypeExpr = (
    Apply<ParserContext, AnyTypeExprID>({ (context) -> AnyTypeExprID? in
      guard let head = context.peek() else { return nil }

      switch head.kind {
      case .async:
        // async-type-expr
        _ = context.take()
        guard let operand = try typeExpr.parse(&context) else {
          throw ParseError("expected type expression", at: context.currentLocation)
        }

        let id = context.ast.insert(AsyncTypeExpr(operand: operand))
        context.ast.ranges[id] = head.range.upperBounded(by: context.currentIndex)
        return AnyTypeExprID(id)

      case .indirect:
        // indirect-type-expr
        _ = context.take()
        guard let operand = try typeExpr.parse(&context) else {
          throw ParseError("expected type expression", at: context.currentLocation)
        }

        let id = context.ast.insert(IndirectTypeExpr(operand: operand))
        context.ast.ranges[id] = head.range.upperBounded(by: context.currentIndex)
        return AnyTypeExprID(id)

      case .any:
        // existential-type-expr
        _ = context.take()
        guard let traits = try traitComposition.parse(&context) else {
          throw ParseError("expected trait composition", at: context.currentLocation)
        }
        let clause = try whereClause.parse(&context)

        let id = context.ast.insert(ExistentialTypeExpr(traits: traits, whereClause: clause))
        context.ast.ranges[id] = head.range.upperBounded(
          by: clause?.range?.upperBound ?? context.ast.ranges[traits.last!]!.upperBound)
        return AnyTypeExprID(id)

      default:
        return try compoundTypeExpr.parse(&context)
      }
    })
  )

  static let nameTypeExpr = (
    compoundTypeExpr
      .map({ (context, id) -> NodeID<NameTypeExpr> in
        if let converted = NodeID<NameTypeExpr>(converting: id) {
          return converted
        } else {
          throw ParseError("expected type name", at: context.ast.ranges[id]!.first())
        }
      })
  )

  static let compoundTypeExpr = (
    Apply<ParserContext, AnyTypeExprID>({ (context) -> AnyTypeExprID? in
      guard var head = try primaryTypeExpr.parse(&context) else { return nil }
      let headRange = context.ast.ranges[head]!

      while true {
        if context.take(.dot) != nil {
          guard let member = try primaryTypeDeclRef.parse(&context) else {
            throw ParseError("expected type member name", at: context.currentLocation)
          }

          context.ast[member].domain = head
          context.ast.ranges[member] = headRange.upperBounded(by: context.currentIndex)
          head = AnyTypeExprID(member)
          continue
        }

        if context.take(.twoColons) != nil {
          guard let lens = try primaryTypeExpr.parse(&context) else {
            throw ParseError("expected focus", at: context.currentLocation)
          }

          let id = context.ast.insert(ConformanceLensTypeExpr(subject: head, lens: lens))
          context.ast.ranges[id] = headRange.upperBounded(by: context.currentIndex)
          head = AnyTypeExprID(id)
          continue
        }

        break
      }

      return head
    })
  )

  static let primaryTypeExpr = (
    oneOf([
      anyTypeExpr(primaryTypeDeclRef),
      anyTypeExpr(tupleTypeExpr),
      anyTypeExpr(lambdaOrParenthesizedTypeExpr),
      anyTypeExpr(wildcardTypeExpr),
    ])
  )

  static let primaryTypeDeclRef = (
    typeIdentifier.and(maybe(staticArgumentList))
      .map({ (context, tree) -> NodeID<NameTypeExpr> in
        let id = context.ast.insert(NameTypeExpr(identifier: tree.0, arguments: tree.1 ?? []))
        context.ast.ranges[id] = tree.0.range!.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let tupleTypeExpr = (
    take(.lBrace).and(maybe(tupleTypeExprElementList)).and(take(.rBrace))
      .map({ (context, tree) -> NodeID<TupleTypeExpr> in
        let id = context.ast.insert(TupleTypeExpr(elements: tree.0.1 ?? []))
        context.ast.ranges[id] = tree.0.0.range.upperBounded(by: tree.1.range.upperBound)
        return id
      })
  )

  static let tupleTypeExprElementList = (
    tupleTypeExprElement.and(zeroOrMany(take(.comma).and(tupleTypeExprElement).second))
      .map({ (_, tree) -> [TupleTypeExpr.Element] in [tree.0] + tree.1 })
  )

  static let tupleTypeExprElement = (
    Apply<ParserContext, TupleTypeExpr.Element>({ (context) in
      let backup = context.backup()

      // Parse a labeled element.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let type = try typeExpr.parse(&context) {
            return TupleTypeExpr.Element(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              type: type)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      context.restore(from: backup)
      if let type = try typeExpr.parse(&context) {
        return TupleTypeExpr.Element(type: type)
      }

      return nil
    })
  )

  static let lambdaOrParenthesizedTypeExpr = (
    Apply<ParserContext, AnyTypeExprID>({ (context) -> AnyTypeExprID? in
      if context.peek()?.kind != .lParen { return nil }

      let backup = context.backup()
      do {
        return try lambdaTypeExpr.parse(&context).map(AnyTypeExprID.init(_:))
      } catch {
        context.restore(from: backup)
        return try parenthesizedTypeExpr.parse(&context)
      }
    })
  )

  static let parenthesizedTypeExpr = (
    take(.lParen).and(typeExpr).and(take(.rParen))
      .map({ (context, tree) -> AnyTypeExprID in tree.0.1 })
  )

  static let lambdaTypeExpr = Choose(
    typeErasedLambdaTypeExpr,
    or: lambdaEnvironment.and(typeErasedLambdaTypeExpr)
      .map({ (context, tree) in
        context.ast[tree.1].environment = tree.0
        context.ast.ranges[tree.1]!.lowerBound = tree.0.range!.lowerBound
        return tree.1
      })
  )

  static let typeErasedLambdaTypeExpr = (
    take(.lParen).and(maybe(lambdaParameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(take(.arrow))
      .and(typeExpr)
      .map({ (context, tree) -> NodeID<LambdaTypeExpr> in
        let id = context.ast.insert(LambdaTypeExpr(
          receiverEffect: tree.0.0.1,
          parameters: tree.0.0.0.0.1 ?? [],
          output: tree.1))
        context.ast.ranges[id] = tree.0.0.0.0.0.range.upperBounded(by: context.currentIndex)
        return id
      })
  )

  static let lambdaParameterList = (
    lambdaParameter.and(zeroOrMany(take(.comma).and(lambdaParameter).second))
      .map({ (_, tree) -> [LambdaTypeExpr.Parameter] in [tree.0] + tree.1 })
  )

  static let lambdaParameter = (
    Apply<ParserContext, LambdaTypeExpr.Parameter>({ (context) in
      let backup = context.backup()

      // Parse a labeled parameter.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let type = try parameterTypeExpr.parse(&context) {
            return LambdaTypeExpr.Parameter(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              type: type)
          }
        }
      }

      // Backtrack and parse an unlabeled parameter.
      context.restore(from: backup)
      if let type = try parameterTypeExpr.parse(&context) {
        return LambdaTypeExpr.Parameter(type: type)
      }

      return nil
    })
  )

  static let lambdaEnvironment = (
    lambdaThinEnvironment.or(lambdaCustomEnvironment)
  )

  static let lambdaThinEnvironment = (
    Apply<ParserContext, SourceRepresentable<AnyTypeExprID>>({ (context) in
      if let keyword = context.take(nameTokenWithValue: "thin") {
        let id = context.ast.insert(TupleTypeExpr())
        context.ast.ranges[id] = keyword.range
        return SourceRepresentable(value: AnyTypeExprID(id), range: keyword.range)
      } else {
        return nil
      }
    })
  )

  static let lambdaCustomEnvironment = (
    take(.lBrack).and(typeExpr).and(take(.rBrack))
      .map({ (context, tree) -> SourceRepresentable<AnyTypeExprID> in
        SourceRepresentable(
          value: tree.0.1,
          range: tree.0.0.range.upperBounded(by: tree.1.range.upperBound))
      })
  )

  static let wildcardTypeExpr = (
    take(.under)
      .map({ (context, token) -> NodeID<WildcardTypeExpr> in
        let id = context.ast.insert(WildcardTypeExpr())
        context.ast.ranges[id] = token.range
        return id
      })
  )

  static let receiverEffect = translate([
    .inout  : ReceiverEffect.inout,
    .sink   : ReceiverEffect.sink,
    .yielded: ReceiverEffect.yielded,
  ])

  static let parameterTypeExpr = (
    maybe(passingConvention)
      .andCollapsingSoftFailures(typeExpr)
      .map({ (context, tree) -> NodeID<ParameterTypeExpr> in
        let id = context.ast.insert(ParameterTypeExpr(
          convention: tree.0 ?? SourceRepresentable(value: .let),
          bareType: tree.1))

        context.ast.ranges[id] = (
          tree.0?.range.map({ $0.upperBounded(by: context.currentIndex) })
          ?? context.ast.ranges[tree.1])

        return id
      })
  )

  static let passingConvention = translate([
    .let    : PassingConvention.let,
    .inout  : PassingConvention.inout,
    .set    : PassingConvention.set,
    .sink   : PassingConvention.sink,
    .yielded: PassingConvention.yielded,
  ])

  static let staticArgumentList = (
    take(.lAngle)
      .and(staticArgument.and(zeroOrMany(take(.comma).and(staticArgument).second)))
      .and(take(.rAngle))
      .map({ (_, tree) -> [GenericArgument] in [tree.0.1.0] + tree.0.1.1 })
  )

  static let staticArgument = (
    Apply<ParserContext, GenericArgument>({ (context) in
      let backup = context.backup()

      // Parse a labeled value argument.
      if let label = context.take(if: { $0.isLabel }) {
        if context.take(.colon) != nil {
          if let value = try staticArgumentValue.parse(&context) {
            return GenericArgument(
              label: SourceRepresentable(token: label, in: context.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled value argument.
      context.restore(from: backup)
      if let value = try staticArgumentValue.parse(&context) {
        return GenericArgument(value: value)
      }

      return nil
    })
  )

  static let staticArgumentValue = (
    Apply<ParserContext, GenericArgument.Value>({ (context) in
      if let value = try expr.parse(&context) {
        return .expr(value)
      }

      if let value = try typeExpr.parse(&context) {
        return .type(value)
      }

      return nil
    })
  )

  static let whereClause = (
    take(.where).and(whereClauseConstraintList)
      .map({ (context, tree) -> SourceRepresentable<WhereClause> in
        SourceRepresentable(
          value: WhereClause(constraints: tree.1),
          range: tree.0.range.upperBounded(by: context.currentIndex))
      })
  )

  static let whereClauseConstraintList = (
    whereClauseConstraint.and(zeroOrMany(take(.comma).and(whereClauseConstraint).second))
      .map({ (context, tree) -> [SourceRepresentable<WhereClause.ConstraintExpr>] in
        [tree.0] + tree.1
      })
  )

  static let whereClauseConstraint = (
    typeConstraint.or(valueConstraint)
  )

  static let typeConstraint = (
    Apply<ParserContext, SourceRepresentable<WhereClause.ConstraintExpr>>({ (context) in
      guard let lhs = try nameTypeExpr.parse(&context) else { return nil }

      // equality-constraint
      if context.take(.equal) != nil {
        guard let rhs = try typeExpr.parse(&context) else {
          throw ParseError("expected type expression", at: context.currentLocation)
        }
        return SourceRepresentable(
          value: .equality(l: lhs, r: rhs),
          range: context.ast.ranges[lhs]!.upperBounded(by: context.currentIndex))
      }

      // conformance-constraint
      if context.take(.colon) != nil {
        guard let traits = try traitComposition.parse(&context) else {
          throw ParseError("expected trait composition", at: context.currentLocation)
        }
        return SourceRepresentable(
          value: .conformance(l: lhs, traits: traits),
          range: context.ast.ranges[lhs]!.upperBounded(by: context.currentIndex))
      }

      throw ParseError("expected constraint operator", at: context.currentLocation)
    })
  )

  static let valueConstraint = (
    valueAttribute.and(expr)
      .map({ (context, tree) -> SourceRepresentable<WhereClause.ConstraintExpr> in
        SourceRepresentable(
          value: .value(tree.1),
          range: tree.0.range.upperBounded(by: context.currentIndex))
      })
  )

  static let traitComposition = (
    nameTypeExpr.and(zeroOrMany(take(.ampersand).and(nameTypeExpr).second))
      .map({ (context, tree) -> TraitComposition in [tree.0] + tree.1 })
  )

  // MARK: Identifiers

  static let identifierExpr = (
    entityIdentifier.and(maybe(take(.dot).and(methodIntroducer)))
      .map({ (context, tree) -> SourceRepresentable<Name> in
        if let (_, introducer) = tree.1 {
          var name = tree.0
          name.value.introducer = introducer.value
          name.range = name.range!.upperBounded(by: introducer.range!.upperBound)
          return name
        } else {
          return tree.0
        }
      })
  )

  static let entityIdentifier = (
    Apply<ParserContext, SourceRepresentable<Name>>({ (context) in
      switch context.peek()?.kind {
      case .name, .under:
        // function-entity-identifier
        let head = context.take()!
        var result = SourceRepresentable(
          value: Name(stem: String(context.lexer.source[head.range])),
          range: head.range)

        if context.currentCharacter == "(" {
          var labels: [String?] = []
          let backup = context.backup()
          _ = context.take()

          while !context.hasLeadingWhitespace {
            if context.take(.under) != nil {
              labels.append(nil)
            } else if let label = context.take(if: { $0.isLabel }) {
              labels.append(String(context.lexer.source[label.range]))
            } else {
              break
            }

            if context.takeWithoutSkippingWhitespace(.colon) == nil {
              break
            }

            if let end = context.takeWithoutSkippingWhitespace(.rParen) {
              if !labels.isEmpty {
                let range = head.range.upperBounded(by: end.range.upperBound)
                result.value.labels = labels
                return result
              }
              break
            }
          }

          context.restore(from: backup)
        }

        return result

      case .infix, .prefix, .postfix:
        // operator-entity-identifier
        let head = context.take()!

        if context.hasLeadingWhitespace {
          throw ParseError("expected operator", at: context.currentLocation)
        }
        guard let oper = context.takeOperator() else {
          throw ParseError("expected operator", at: context.currentLocation)
        }

        let stem = String(context.lexer.source[oper.range!])
        let range = head.range.upperBounded(by: oper.range!.upperBound)

        switch head.kind {
        case .infix:
          return SourceRepresentable(value: Name(stem: stem, notation: .infix))
        case .prefix:
          return SourceRepresentable(value: Name(stem: stem, notation: .prefix))
        case .postfix:
          return SourceRepresentable(value: Name(stem: stem, notation: .postfix))
        default:
          unreachable()
        }

      default:
        return nil
      }
    })
  )

  static let typeIdentifier = (
    take(.name)
      .map({ (context, token) -> SourceRepresentable<Identifier> in
        SourceRepresentable(value: String(context.lexer.source[token.range]), range: token.range)
      })
  )

  // MARK: Attributes

  static let declAttribute = (
    take(.attribute).and(maybe(attributeArgumentList))
      .map({ (context, tree) -> SourceRepresentable<Attribute> in
        SourceRepresentable(
          value: Attribute(
            name: SourceRepresentable(token: tree.0, in: context.lexer.source),
            arguments: tree.1 ?? []),
          range: tree.0.range.upperBounded(by: context.currentIndex))
      })
  )

  static let attributeArgumentList = (
    take(.lParen)
      .and(attributeArgument).and(zeroOrMany(take(.comma).and(attributeArgument).second))
      .and(take(.rParen))
      .map({ (context, tree) -> [Attribute.Argument] in [tree.0.0.1] + tree.0.1 })
  )

  static let attributeArgument = (
    stringAttributeArgument.or(integerAttributeArgument)
  )

  static let stringAttributeArgument = (
    take(.string)
      .map({ (context, token) -> Attribute.Argument in
        let value = String(context.lexer.source[token.range].dropFirst().dropLast())
        return .string(SourceRepresentable(value: value, range: token.range))
      })
  )

  static let integerAttributeArgument = (
    take(.int)
      .map({ (context, token) -> Attribute.Argument in
        if let value = Int(context.lexer.source[token.range]) {
          return .integer(SourceRepresentable(value: value, range: token.range))
        } else {
          throw ParseError("invalid integer literal", at: token.range.first())
        }
      })
  )

  static let typeAttribute = attribute("@type")

  static let valueAttribute = attribute("@value")

}

extension Parser {

  /// A combinator that parses tokens with a specific kind.
  struct TakeKind: Combinator {

    typealias Context = ParserContext

    typealias Element = Token

    /// The kind of the token to consume.
    let kind: Token.Kind

    func parse(_ context: inout ParserContext) throws -> Token? {
      context.take(kind)
    }

  }

  /// A combinator that parses contextual keywords.
  struct ContextualKeyword<T: RawRepresentable>: Combinator where T.RawValue == String {

    typealias Context = ParserContext

    typealias Element = SourceRepresentable<T>

    func parse(_ context: inout ParserContext) throws -> Element? {
      if let next = context.peek(), next.kind == .name {
        if let value = T(rawValue: String(context.lexer.source[next.range])) {
          _ = context.take()
          return SourceRepresentable(value: value, range: next.range)
        }
      }
      return nil
    }

  }

  /// Creates a combinator that parses tokens with the specified kind.
  static func take(_ kind: Token.Kind) -> TakeKind {
    TakeKind(kind: kind)
  }

  /// Creates a combinator that parses name tokens with the specified value.
  static func take(nameTokenWithValue value: String) -> Apply<ParserContext, Token> {
    Apply({ (context) in context.take(nameTokenWithValue: value) })
  }

  /// Creates a combinator that parses attribute tokens with the specified name.
  static func attribute(_ name: String) -> Apply<ParserContext, Token> {
    Apply({ (context) in context.take(attribute: name) })
  }

  /// Creates a combinator that translates token kinds to instances of type.
  static func translate<T>(
    _ table: [Token.Kind: T]
  ) -> Apply<ParserContext, SourceRepresentable<T>> {
    Apply({ (context) in
      guard let head = context.peek() else { return nil }
      if let translation = table[head.kind] {
        _ = context.take()
        return SourceRepresentable(value: translation, range: head.range)
      } else {
        return nil
      }
    })
  }

  /// Creates a combinator that sets `flags` before applying `base` and restores them to their
  /// previous state afterward.
  static func settingFlags<Base: Combinator>(
    _ flags: ParserContext.Flags,
    apply base: Base
  ) -> Apply<ParserContext, Base.Element>
  where Base.Context == ParserContext
  {
    Apply({ (context) in
      let oldFlags = context.flags
      defer { context.flags = oldFlags }
      context.flags = context.flags | flags
      return try base.parse(&context)
    })
  }

  /// Creates a combinator that unsets `flags` before applying `base` and restores them to their
  /// previous state afterward.
  static func unsettingFlags<Base: Combinator>(
    _ flags: ParserContext.Flags,
    apply base: Base
  ) -> Apply<ParserContext, Base.Element>
  where Base.Context == ParserContext
  {
    Apply({ (context) in
      let oldFlags = context.flags
      defer { context.flags = oldFlags }
      context.flags = context.flags - flags
      return try base.parse(&context)
    })
  }

  /// Creates a combinator that applies `base` only if its input is not preceeded by whitespaces.
  static func withoutLeadingWhitespace<Base: Combinator>(
    _ base: Base
  ) -> Apply<ParserContext, Base.Element>
  where Base.Context == ParserContext
  {
    Apply({ (context) in try context.hasLeadingWhitespace ? nil : base.parse(&context) })
  }

  /// Creates a combinator that applies `base` only if its input is not preceeded by newlines.
  static func onSameLine<Base: Combinator>(
    _ base: Base
  ) -> Apply<ParserContext, Base.Element>
  where Base.Context == ParserContext
  {
    Apply({ (context) in
      if let t = context.peek() {
        return try context.hasNewline(inCharacterStreamUpTo: t.range.lowerBound)
          ? nil
          : base.parse(&context)
      } else {
        // Let `base` handle end of stream.
        return try base.parse(&context)
      }
    })
  }

}

fileprivate extension SourceRepresentable where Part == Identifier {

  init(token: Token, in source: SourceFile) {
    self.init(value: String(source[token.range]), range: token.range)
  }

}

fileprivate extension Diagnostic {

  static func accessModifierAtNonLocalScope(at range: SourceRange?) -> Diagnostic {
    .error("access modifier cannot be used in a local scope", range: range)
  }

  static func duplicateMethodIntroducer(at range: SourceRange?) -> Diagnostic {
    .error("duplicate method introducer", range: range)
  }

  static func duplicateSubscriptIntroducer(at range: SourceRange?) -> Diagnostic {
    .error("duplicate subscript introducer", range: range)
  }

  static func infixOperatorRequiresWhitespaces(at range: SourceRange?) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", range: range)
  }

  static func memberModifierAtNonTypeScope(at range: SourceRange?) -> Diagnostic {
    .error("member modifier can only be used on member declarations", range: range)
  }

  static func unexpectedDeclAttribute(at range: SourceRange?) -> Diagnostic {
    .error("unexpected declaration attribute", range: range)
  }

  static func unexpectedDeclModifier(at range: SourceRange?) -> Diagnostic {
    .error("unexpected declaration modifier", range: range)
  }

}
