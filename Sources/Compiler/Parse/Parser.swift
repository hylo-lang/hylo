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
    var state = ParserState(ast: ast, lexer: Lexer(tokenizing: input))

    let decls: NodeID<TopLevelDeclSet>?
    do {
      // Parse the file.
      if let d = try Self.sourceFile.parse(&state) {
        // Make sure we consumed the entire file.
        if let head = state.peek() {
          throw ParseError("expected EOF", at: head.range.first())
        }

        // Parser succeeded.
        state.ast[module].addSourceFile(d)
        decls = d
      } else {
        decls = nil
      }
    } catch let error {
      if let error = error as? ParseError {
        state.diagnostics.append(
          Diagnostic(
            level: .error,
            message: error.message,
            location: error.location,
            window: Diagnostic.Window(range: error.location ..< error.location)))
      } else {
        state.diagnostics.append(Diagnostic(level: .error, message: error.localizedDescription))
      }
      decls = nil
    }

    ast = state.ast
    return (decls: decls, diagnostics: state.diagnostics)
  }

  // MARK: Declarations

  private static func anyDecl<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyDeclID>
  where Base.Context == ParserState, Base.Element: DeclID
  {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyDeclID.init(_:))
    })
  }

  static let sourceFile = (
    zeroOrMany(take(.semi))
      .and(zeroOrMany(moduleScopeDecl.and(zeroOrMany(take(.semi))).first))
      .map({ (state, tree) -> NodeID<TopLevelDeclSet> in
        state.ast.insert(TopLevelDeclSet(decls: tree.1))
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
      .map({ (state, tree) -> NodeID<ImportDecl> in
        let id = state.ast.insert(ImportDecl(
          identifier: SourceRepresentable(token: tree.1, in: state.lexer.source)))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let namespaceDecl: Recursive<ParserState, NodeID<NamespaceDecl>> = (
    Recursive(_namespaceDecl.parse(_:))
  )

  private static let _namespaceDecl = (
    namespaceHead.and(namespaceBody)
      .map({ (state, tree) -> NodeID<NamespaceDecl> in
        let id = state.ast.insert(NamespaceDecl(
          identifier: SourceRepresentable(token: tree.0.1, in: state.lexer.source),
          members: tree.1))

        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let namespaceHead = (
    take(.namespace).and(take(.name))
  )

  static let namespaceBody = inContext(.namespaceBody, apply: (
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
      .map({ (state, tree) -> NodeID<TypeAliasDecl> in
        let id = state.ast.insert(TypeAliasDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          genericClause: tree.0.1,
          body: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
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

  static let productTypeDecl: Recursive<ParserState, NodeID<ProductTypeDecl>> = (
    Recursive(_productTypeDecl.parse(_:))
  )

  private static let _productTypeDecl = (
    productTypeHead.and(productTypeBody)
      .map({ (state, tree) -> NodeID<ProductTypeDecl> in
        /// Returns the the first memberwise initializer declaration in `members` or synthesizes
        /// an implicit one, appends it into `members`, and returns it.
        func memberwiseInitDecl(_ members: inout [AnyDeclID]) -> NodeID<FunDecl> {
          for member in members where member.kind == .funDecl {
            let m = NodeID<FunDecl>(rawValue: member.rawValue)
            if state.ast[m].introducer.value == .memberwiseInit { return m }
          }

          let receiver = state.ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "self")))
          let m = state.ast.insert(FunDecl(
            introducer: SourceRepresentable(value: .memberwiseInit),
            receiver: receiver))
          members.append(AnyDeclID(m))
          return m
        }

        var members = tree.1
        let memberwiseInit = memberwiseInitDecl(&members)
        let id = state.ast.insert(ProductTypeDecl(
          identifier: SourceRepresentable(token: tree.0.0.0.1, in: state.lexer.source),
          genericClause: tree.0.0.1,
          conformances: tree.0.1 ?? [],
          members: members,
          memberwiseInit: memberwiseInit
        ))

        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let productTypeHead = (
    take(.type).and(take(.name)).and(maybe(genericClause)).and(maybe(conformanceList))
  )

  static let productTypeBody = inContext(.productBody, apply: (
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
    Apply<ParserState, AnyDeclID>({ (state) -> AnyDeclID? in
      let backup = state.backup()
      do {
        if let element = try typeAliasDecl.parse(&state) { return AnyDeclID(element) }
      } catch {}
      state.restore(from: backup)
      return try productTypeDecl.parse(&state).map(AnyDeclID.init(_:))
    })
  )

  static let traitDecl = (
    traitHead.and(traitBody)
      .map({ (state, tree) -> NodeID<TraitDecl> in
        let id = state.ast.insert(TraitDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          refinements: tree.0.1 ?? [],
          members: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let traitHead = (
    take(.trait).and(take(.name)).and(maybe(conformanceList))
  )

  static let traitBody = inContext(.traitBody, apply: (
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
      .map({ (state, tree) -> NodeID<AssociatedTypeDecl> in
        let id = state.ast.insert(AssociatedTypeDecl(
          identifier: SourceRepresentable(token: tree.0.0.0.1, in: state.lexer.source),
          conformances: tree.0.0.1 ?? [],
          whereClause: tree.0.1,
          defaultValue: tree.1))
        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let associatedValueDecl = (
    take(nameTokenWithValue: "value").and(take(.name))
      .and(maybe(whereClause))
      .and(maybe(take(.assign).and(expr).second))
      .map({ (state, tree) -> NodeID<AssociatedValueDecl> in
        let id = state.ast.insert(AssociatedValueDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          whereClause: tree.0.1,
          defaultValue: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let conformanceDecl = (
    conformancHead.and(conformanceBody)
      .map({ (state, tree) -> NodeID<ConformanceDecl> in
        let id = state.ast.insert(ConformanceDecl(
          subject: tree.0.0.0.1,
          conformances: tree.0.0.1,
          whereClause: tree.0.1,
          members: tree.1))
        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let conformancHead = (
    take(.conformance).and(typeExpr).and(conformanceList).and(maybe(whereClause))
  )

  static let conformanceBody = extensionBody

  static let extensionDecl = (
    extensionHead.and(extensionBody)
      .map({ (state, tree) -> NodeID<ExtensionDecl> in
        let id = state.ast.insert(ExtensionDecl(
          subject: tree.0.0.1,
          whereClause: tree.0.1,
          members: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let extensionHead = (
    take(.extension).and(typeExpr).and(maybe(whereClause))
  )

  static let extensionBody = inContext(.extensionBody, apply: (
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
      .map({ (state, tree) -> NodeID<BindingDecl> in
        let id = state.ast.insert(BindingDecl(pattern: tree.0, initializer: tree.1?.1))
        state.ast.ranges[id] = state.ast.ranges[tree.0]!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let deinitDecl = (
    take(.deinit).and(deinitBody)
      .map({ (state, tree) -> NodeID<FunDecl> in
        let receiver = state.ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .deinit, range: tree.0.range),
          receiver: receiver,
          body: .block(tree.1)))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let deinitBody = inContext(.functionBody, apply: braceStmt)

  static let initDecl = Choose(
    memberwiseInitDecl,
    or: initHead
      .and(take(.lParen).and(maybe(parameterList)).and(take(.rParen)))
      .and(initBody)
      .map({ (state, tree) -> NodeID<FunDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)

        let receiver = state.ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .`init`, range: head.0.range),
          genericClause: head.1,
          parameters: signature.0.1 ?? [],
          receiver: receiver,
          body: .block(body)))

        state.ast.ranges[id] = head.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let initHead = (
    take(.`init`).and(maybe(genericClause))
  )

  static let initBody = inContext(.functionBody, apply: braceStmt)

  static let functionDecl = (
    functionHead.and(functionSignature).and(maybe(functionBody))
      .map({ (state, tree) -> NodeID<FunDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)
        let identifier: FunctionDeclIdentifier = head.0.0

        let receiver: NodeID<ParameterDecl>?
        if case .bundle = body {
          receiver = nil
        } else if !state.isParsingTypeBody {
          receiver = nil
        } else {
          receiver = state.ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "self")))
        }

        let id = state.ast.insert(FunDecl(
          introducer: identifier.introducer,
          receiverEffect: signature.receiverEffect,
          notation: identifier.notation,
          identifier: identifier.stem,
          genericClause: head.0.1,
          captures: head.1 ?? [],
          parameters: signature.parameters,
          receiver: receiver,
          output: signature.output,
          body: body))

        let startRange = identifier.notation?.range ?? identifier.introducer.range!
        state.ast.ranges[id] = startRange.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let memberwiseInitDecl = (
    take(nameTokenWithValue: "memberwise").and(take(.`init`))
      .map({ (state, tree) -> NodeID<FunDecl> in
        let receiver = state.ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .memberwiseInit, range: tree.0.range),
          receiver: receiver))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
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
      .map({ (state, tree) -> FunctionSignature in
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
      .map({ (state, tree) -> FunctionDeclIdentifier in
        (
          introducer: SourceRepresentable(value: .`fun`, range: tree.0.range),
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source),
          notation: nil
        )
      })
  )

  static let operatorFunctionDeclIdentifier = (
    operatorNotation.and(take(.fun)).and(operator_)
      .map({ (state, tree) -> FunctionDeclIdentifier in
        (
          introducer: SourceRepresentable(value: .`fun`, range: tree.0.1.range),
          stem: tree.1,
          notation: tree.0.0
        )
      })
  )

  static let functionBody = inContext(.functionBody, apply: TryCatch(
    trying: methodBundleBody
      .map({ (state, impls) -> FunDecl.Body in .bundle(impls) }),
    orCatchingAndApplying: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (state, tree) -> FunDecl.Body in .expr(tree.0.1) }),
      orCatchingAndApplying: braceStmt
        .map({ (state, id) -> FunDecl.Body in .block(id) })
    )
  ))

  static let methodBundleBody = (
    take(.lBrace).and(methodImpl+).and(take(.rBrace))
      .map({ (state, tree) -> [NodeID<MethodImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        for implID in tree.0.1 {
          let introducer = state.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted {
            state.diagnostics.append(.duplicateMethodIntroducer(at: introducer.range))
          }
        }

        return tree.0.1
      })
  )

  static let methodImpl = (
    methodIntroducer.and(maybe(methodImplBody))
      .map({ (state, tree) -> NodeID<MethodImplDecl> in
        let receiver = state.ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = state.ast.insert(MethodImplDecl(
          introducer: tree.0,
          receiver: receiver,
          body: tree.1))
        state.ast.ranges[id] = tree.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let methodImplBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> MethodImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> MethodImplDecl.Body in .block(id) })
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
      .map({ (state, tree) -> NodeID<SubscriptDecl> in
        let identifier = SourceRepresentable(token: tree.0.0.0.1, in: state.lexer.source)
        let id = state.ast.insert(SubscriptDecl(
          introducer: SourceRepresentable(value: .property, range: tree.0.0.0.0.range),
          receiverEffect: tree.0.0.1,
          identifier: identifier,
          output: tree.0.1.1,
          impls: tree.1 ?? []))

        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let subscriptDecl = (
    subscriptHead.and(subscriptSignature).and(maybe(subscriptBody))
      .map({ (state, tree) -> NodeID<SubscriptDecl> in
        let (head, signature, body) = (tree.0.0, tree.0.1, tree.1)
        let identifier = head.0.0.1.map({
          SourceRepresentable(token: $0, in: state.lexer.source)
        })

        let id = state.ast.insert(SubscriptDecl(
          introducer: SourceRepresentable(value: .subscript, range: head.0.0.0.range),
          receiverEffect: signature.receiverEffect,
          identifier: identifier,
          genericClause: head.0.1,
          explicitCaptures: head.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          impls: body ?? []))

        state.ast.ranges[id] = head.0.0.0.range.upperBounded(by: state.currentIndex)
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
      .map({ (state, tree) -> SubscriptSignature in
        (parameters: tree.0.0.0.1 ?? [], receiverEffect: tree.0.1, output: tree.1.1)
      })
  )

  static let subscriptBody = inContext(.subscriptBody, apply: TryCatch(
    trying: subscriptBundleBody,
    orCatchingAndApplying: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (state, tree) -> [NodeID<SubscriptImplDecl>] in
          let receiver = state.ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "self")))
          let id = state.ast.insert(SubscriptImplDecl(
            introducer: SourceRepresentable(value: .let),
            receiver: receiver,
            body: .expr(tree.0.1)))
          return [id]
        }),
      orCatchingAndApplying: braceStmt
        .map({ (state, brace) -> [NodeID<SubscriptImplDecl>] in
          if state.ast[brace].stmts.isEmpty {
            throw ParseError(
              "expected subscript implementation",
              at: state.ast.ranges[brace]!.last()!)
          }
          let receiver = state.ast.insert(ParameterDecl(
            identifier: SourceRepresentable(value: "self")))
          let id = state.ast.insert(SubscriptImplDecl(
            introducer: SourceRepresentable(value: .let),
            receiver: receiver,
            body: .block(brace)))
          return [id]
        })
    )
  ))

  static let subscriptBundleBody = (
    take(.lBrace).and(subscriptImpl+).and(take(.rBrace))
      .map({ (state, tree) -> [NodeID<SubscriptImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        for implID in tree.0.1 {
          let introducer = state.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted {
            state.diagnostics.append(.duplicateSubscriptIntroducer(at: introducer.range))
          }
        }

        return tree.0.1
      })
  )

  static let subscriptImpl = (
    subscriptIntroducer.and(maybe(subscriptImplBody))
      .map({ (state, tree) -> NodeID<SubscriptImplDecl> in
        let receiver = state.ast.insert(ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = state.ast.insert(SubscriptImplDecl(
          introducer: tree.0,
          receiver: receiver,
          body: tree.1))
        state.ast.ranges[id] = tree.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let subscriptImplBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> SubscriptImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> SubscriptImplDecl.Body in .block(id) })
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
      .map({ (state, tree) -> NodeID<ParameterDecl> in
        let id = state.ast.insert(ParameterDecl(
          label: tree.0.0.label,
          identifier: tree.0.0.name,
          annotation: tree.0.1?.1,
          defaultValue: tree.1?.1))

        state.ast.ranges[id] = SourceRange(
          in: state.lexer.source,
          from: tree.0.0.label?.range!.lowerBound ?? tree.0.0.name.range!.lowerBound,
          to: state.currentIndex)
        return id
      })
  )

  typealias ParameterInterface = (
    label: SourceRepresentable<Identifier>?,
    name: SourceRepresentable<Identifier>
  )

  static let parameterInterface = (
    Apply<ParserState, ParameterInterface>({ (state) in
      // Parse a label or bail out.
      guard let labelCandidate = state.take(if: { $0.isLabel || $0.kind == .under }) else {
        return nil
      }

      // Assume the first token is a label and attempt to parse a name.
      if let nameCandidate = state.take(.name) {
        if labelCandidate.kind == .under {
          // case `_ name`
          return (
            label: nil,
            name: SourceRepresentable(token: nameCandidate, in: state.lexer.source))
        } else {
          // case `label name`
          return (
            label: SourceRepresentable(token: labelCandidate, in: state.lexer.source),
            name: SourceRepresentable(token: nameCandidate, in: state.lexer.source))
        }
      }

      // Assume the first token is the name.
      if labelCandidate.kind == .name {
        // case `<no-label> name`
        let name = SourceRepresentable(token: labelCandidate, in: state.lexer.source)
        return (label: name, name: name)
      }

      throw ParseError("expected parameter name", at: labelCandidate.range.first())
    })
  )

  static let operatorDecl = (
    take(.operator).and(operatorNotation).and(operator_)
      .and(maybe(take(.colon).and(precedenceGroup)))
      .map({ (state, tree) -> NodeID<OperatorDecl> in
        let id = state.ast.insert(OperatorDecl(
          notation: tree.0.0.1,
          name: tree.0.1,
          precedenceGroup: tree.1?.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(
          by: tree.1?.1.range?.upperBound ?? tree.0.1.range!.upperBound)
        return id
      })
  )

  static let operator_ = (
    Apply<ParserState, SourceRepresentable<Identifier>>({ (state) in
      state.takeOperator()
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
  private static func decorated<Decl: Combinator>(decl: Decl) -> Apply<ParserState, AnyDeclID>
  where Decl.Context == ParserState, Decl.Element == AnyDeclID
  {
    Apply({ (state) in
      guard let startIndex = state.peek()?.range.lowerBound else { return nil }

      // Parse attributes and modifiers.
      var attributes: [SourceRepresentable<Attribute>] = []
      while let a = try declAttribute.parse(&state) {
        attributes.append(a)
      }
      let access = try accessModifier.parse(&state)
      let member = try memberModifier.parse(&state)

      // Parse the declaration.
      guard let declID = try decl.parse(&state) else {
        if attributes.isEmpty && (access == nil) && (member == nil) {
          return nil
        } else {
          throw ParseError("expected declaration", at: state.currentLocation)
        }
      }

      switch declID.kind {
      case .bindingDecl:
        let id = NodeID<BindingDecl>(rawValue: declID.rawValue)
        state.ast[id].incorporate(
          attributes: attributes, accessModifier: access, memberModifier: member)

      case .conformanceDecl:
        let id = NodeID<ConformanceDecl>(rawValue: declID.rawValue)
        state.ast[id].incorporate(access)

        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .importDecl:
        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let a = access {
          state.diagnostics.append(.unexpectedDeclModifier(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .extensionDecl:
        let id = NodeID<ExtensionDecl>(rawValue: declID.rawValue)
        state.ast[id].incorporate(access)

        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .funDecl:
        let id = NodeID<FunDecl>(rawValue: declID.rawValue)
        state.ast[id].incorporate(
          attributes: attributes, accessModifier: access, memberModifier: member)

      case .operatorDecl:
        if let a = access {
          state.ast[NodeID<OperatorDecl>(rawValue: declID.rawValue)].incorporate(a)
        }

        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .namespaceDecl:
        if let a = access {
          state.ast[NodeID<NamespaceDecl>(rawValue: declID.rawValue)].incorporate(a)
        }

        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .productTypeDecl:
        if let a = access {
          state.ast[NodeID<ProductTypeDecl>(rawValue: declID.rawValue)].incorporate(a)
        }
        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .subscriptDecl:
        let id = NodeID<SubscriptDecl>(rawValue: declID.rawValue)
        state.ast[id].incorporate(
          attributes: attributes, accessModifier: access, memberModifier: member)

      case .traitDecl:
        if let a = access {
          let id = NodeID<TraitDecl>(rawValue: declID.rawValue)
          state.ast[id].incorporate(a)
        }

        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      case .typeAliasDecl:
        if let a = access {
          let id = NodeID<TypeAliasDecl>(rawValue: declID.rawValue)
          state.ast[id].incorporate(a)
        }
        if let a = attributes.first {
          state.diagnostics.append(.unexpectedDeclAttribute(at: a.range))
        }
        if let m = member {
          state.diagnostics.append(.unexpectedDeclModifier(at: m.range))
        }

      default:
        unreachable("unexpected declaration")
      }

      state.ast.ranges[declID]?.lowerBound = startIndex
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
      .map({ (state, tree) -> SourceRepresentable<GenericClause> in
        return SourceRepresentable(
          value: GenericClause(parameters: tree.0.0.1, whereClause: tree.0.1),
          range: tree.0.0.0.range.upperBounded(by: state.currentIndex))
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
      .map({ (state, tree) -> GenericParamDeclID in
        let id = state.ast.insert(GenericTypeParamDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          conformances: tree.0.1?.1 ?? [],
          defaultValue: tree.1?.1))

        state.ast.ranges[id] = SourceRange(
          in: state.lexer.source,
          from: tree.0.0.0?.range.lowerBound ?? tree.0.0.1.range.lowerBound,
          to: state.currentIndex)
        return .type(id)
      })
  )

  static let genericValueParameter = (
    valueAttribute.and(take(.name))
      .and(take(.colon).and(typeExpr))
      .and(maybe(take(.assign).and(expr)))
      .map({ (state, tree) -> GenericParamDeclID in
        let id = state.ast.insert(GenericValueParamDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          annotation: tree.0.1.1,
          defaultValue: tree.1?.1))

        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return .value(id)
      })
  )

  static let conformanceList = (
    take(.colon).and(nameTypeExpr).and(zeroOrMany(take(.comma).and(nameTypeExpr).second))
      .map({ (state, tree) -> [NodeID<NameTypeExpr>] in [tree.0.1] + tree.1 })
  )

  // MARK: Value expressions

  private static func anyExpr<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyExprID>
  where Base.Context == ParserState, Base.Element: ExprID
  {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyExprID.init(_:))
    })
  }

  static let expr: Recursive<ParserState, AnyExprID> = (
    Recursive(infixExpr.parse(_:))
  )

  static let infixExpr = (
    Apply<ParserState, AnyExprID>({ (state) -> AnyExprID? in
      guard var lhs = try infixExprHead.parse(&state) else { return nil }

      while state.hasLeadingWhitespace {
        // type-casting-tail
        if let infixOperator = state.take(.cast) {
          try appendInfixTail(to: &lhs, forCastOperator: infixOperator, in: &state)
          continue
        }

        // infix-operator-tail (with assign)
        if let infixOperator = state.take(.assign) {
          try appendInfixTail(to: &lhs, forAssignOperator: infixOperator, in: &state)
          continue
        }

        // infix-operator-tail
        if try !appendInfixTail(to: &lhs, in: &state) { break }
      }

      return lhs
    })
  )

  /// Parses a type expression from the stream, then transforms `lhs` into a `CastExpr`, using
  /// `infixOperator` to determine the cast kind.
  private static func appendInfixTail(
    to lhs: inout AnyExprID,
    forCastOperator infixOperator: Token,
    in state: inout ParserState
  ) throws {
    if !state.hasLeadingWhitespace {
      state.diagnostics.append(.infixOperatorRequiresWhitespaces(at: infixOperator.range))
    }

    guard let rhs = try typeExpr.parse(&state) else {
      throw ParseError("expected type expression", at: state.currentLocation)
    }

    let castKind: CastExpr.Kind
    switch state.lexer.source[infixOperator.range] {
    case "as":
      castKind = .up
    case "as!":
      castKind = .down
    case "as!!":
      castKind = .builtinPointerConversion
    default:
      unreachable()
    }

    let expr = state.ast.insert(CastExpr(left: lhs, right: rhs, kind: castKind))
    state.ast.ranges[expr] = state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex)
    lhs = AnyExprID(expr)
  }

  /// Parses a prefix expression from the stream, then transforms `lhs` into an `AssignExpr`.
  private static func appendInfixTail(
    to lhs: inout AnyExprID,
    forAssignOperator infixOperator: Token,
    in state: inout ParserState
  ) throws {
    if !state.hasLeadingWhitespace {
      state.diagnostics.append(.infixOperatorRequiresWhitespaces(at: infixOperator.range))
    }

    guard let rhs = try prefixExpr.parse(&state) else {
      throw ParseError("expected expression", at: state.currentLocation)
    }

    let expr = state.ast.insert(AssignExpr(left: lhs, right: rhs))
    state.ast.ranges[expr] = state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex)
    lhs = AnyExprID(expr)
  }

  /// Parses a sequence of pairs of infix operators and prefix expressions from the stream. If the
  /// sequence isn't empty, transforms `lhs` into a `SequenceExpr` and returns `true`. Otherwise,
  /// returns `false.
  private static func appendInfixTail(
    to lhs: inout AnyExprID,
    in state: inout ParserState
  ) throws -> Bool {
    var tail: SequenceExpr.UnfoldedTail = []

    while true {
      let backup = state.backup()

      // Look for the next operator.
      guard let operatorName = state.takeOperator() else { break }

      // If there isn't any leading whitespace before the next expression but the operator is on a
      // different line, we may be looking at the start of a prefix expression.
      if !state.hasLeadingWhitespace {
        let rangeBefore = state.ast.ranges[lhs]!.upperBound ..< operatorName.range!.lowerBound
        if state.lexer.source.contents[rangeBefore].contains(where: { $0.isNewline }) {
          state.restore(from: backup)
          break
        }

        state.diagnostics.append(.infixOperatorRequiresWhitespaces(at: operatorName.range))
      }

      // Now we can commit to parse an operand.
      guard let operand = try prefixExpr.parse(&state) else {
        throw ParseError("expected type expression", at: state.currentLocation)
      }
      tail.append(SequenceExpr.TailElement(operatorName: operatorName, operand: operand))
    }

    // Nothing to transform if the tail is empty.
    if tail.isEmpty { return false }

    let expr = state.ast.insert(SequenceExpr.unfolded(head: lhs, tail: tail))
    state.ast.ranges[expr] = state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex)
    lhs = AnyExprID(expr)
    return true
  }

  static let infixExprHead = (
    anyExpr(asyncExpr).or(anyExpr(awaitExpr)).or(prefixExpr)
  )

  static let asyncExpr = TryCatch(
    trying: asyncExprInline,
    orCatchingAndApplying: asyncExprBlock
  )

  static let asyncExprBlock = (
    asyncExprHead.and(take(.arrow)).and(typeExpr).and(asyncExprBody)
      .map({ (state, tree) -> NodeID<AsyncExpr> in
        let decl = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.0.0.range),
          receiverEffect: tree.0.0.0.1,
          output: tree.0.1,
          body: .block(tree.1),
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = state.ast.insert(AsyncExpr(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let asyncExprBody = inContext(.functionBody, apply: braceStmt)

  static let asyncExprInline = (
    asyncExprHead.and(expr)
      .map({ (state, tree) -> NodeID<AsyncExpr> in
        let decl = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.range),
          receiverEffect: tree.0.1,
          captures: tree.0.0.1 ?? [],
          body: .expr(tree.1),
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = state.ast.insert(AsyncExpr(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let asyncExprHead = (
    take(.async).and(maybe(captureList)).and(maybe(receiverEffect))
  )

  static let awaitExpr = (
    take(.await).and(expr)
      .map({ (state, tree) -> NodeID<AwaitExpr> in
        let id = state.ast.insert(AwaitExpr(operand: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)
        return id
      })
  )

  static let prefixExpr = Choose(
    postfixExpr,
    or: prefixOperator.and(withoutLeadingWhitespace(postfixExpr))
      .map({ (state, tree) -> AnyExprID in
        let callee = state.ast.insert(NameExpr(
          domain: .expr(tree.1),
          name: SourceRepresentable(
            value: Name(stem: tree.0.value, notation: .prefix),
            range: tree.0.range)))
        state.ast.ranges[callee] = tree.0.range!.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)

        let call = state.ast.insert(FunCallExpr(callee: AnyExprID(callee)))
        state.ast.ranges[call] = state.ast.ranges[callee]
        return AnyExprID(call)
      })
  )

  static let prefixOperator = (
    Apply<ParserState, SourceRepresentable<Identifier>>({ (state) in
      if let t = state.peek(), t.isPrefixOperatorHead {
        return state.takeOperator()
      } else {
        return nil
      }
    })
  )

  static let postfixExpr = (
    compoundExpr.and(maybe(withoutLeadingWhitespace(postfixOperator)))
      .map({ (state, tree) -> AnyExprID in
        if let oper = tree.1 {
          let callee = state.ast.insert(NameExpr(
            domain: .expr(tree.0),
            name: SourceRepresentable(
              value: Name(stem: oper.value, notation: .postfix), range: oper.range)))
          state.ast.ranges[callee] = state.ast.ranges[tree.0]!.upperBounded(
            by: oper.range!.upperBound)

          let call = state.ast.insert(FunCallExpr(callee: AnyExprID(callee)))
          state.ast.ranges[call] = state.ast.ranges[callee]
          return AnyExprID(call)
        } else {
          return tree.0
        }
      })
  )

  static let postfixOperator = (
    Apply<ParserState, SourceRepresentable<Identifier>>({ (state) in
      if let t = state.peek(), t.isPostfixOperatorHead {
        return state.takeOperator()
      } else {
        return nil
      }
    })
  )

  static let compoundExpr = (
    Apply<ParserState, AnyExprID>({ (state) -> AnyExprID? in
      var backup = state.backup()
      let base: AnyExprID?

      do {
        // Parse a primary expression first.
        base = try primaryExpr.parse(&state)
      } catch let primaryExprParseError {
        // Parsing a primary expression returned a hard failure.
        swap(&state, &backup)
        do {
          // Parse a static value member.
          base = try staticValueMemberExpr.parse(&state).map(AnyExprID.init(_:))
        } catch {
          // Parsing a static value member failed too; return the first error.
          swap(&state, &backup)
          throw primaryExprParseError
        }
      }

      // Base is `nil` if and only if `primaryExpr` returned a soft error.
      var head: AnyExprID
      if let b = try base ?? staticValueMemberExpr.parse(&state).map(AnyExprID.init(_:)) {
        head = b
      } else {
        return nil
      }
      let headRange = state.ast.ranges[head]!

      while true {
        // value-member-expr
        if state.take(.dot) != nil {
          // labeled-member-expr
          if let member = try primaryDeclRef.parse(&state) {
            state.ast[member].incorporate(domain: .expr(head))
            state.ast.ranges[member] = headRange.upperBounded(by: state.currentIndex)
            head = AnyExprID(member)
            continue
          }

          // indexed-member-expr
          if let index = state.takeMemberIndex() {
            head = AnyExprID(state.ast.insert(TupleMemberExpr(tuple: head, index: index)))
            state.ast.ranges[head] = headRange.upperBounded(by: state.currentIndex)
            continue
          }

          throw ParseError("expected member name", at: state.currentLocation)
        }

        // Exit if there's a new line before the next token.
        guard let next = state.peek(),
              !state.hasNewline(inCharacterStreamUpTo: next.range.lowerBound)
        else { break }

        // function-call-expr
        if state.take(.lParen) != nil {
          let arguments = try argumentList.parse(&state) ?? []
          guard state.take(.rParen) != nil else {
            throw ParseError("expected ')'", at: state.currentLocation)
          }

          head = AnyExprID(state.ast.insert(FunCallExpr(
            callee: head, arguments: arguments)))
          state.ast.ranges[head] = headRange.upperBounded(by: state.currentIndex)
          continue
        }

        // subscript-call-expr
        if state.take(.lBrack) != nil {
          let arguments = try argumentList.parse(&state) ?? []
          guard state.take(.rBrack) != nil else {
            throw ParseError("expected ']'", at: state.currentLocation)
          }

          head = AnyExprID(state.ast.insert(SubscriptCallExpr(
            callee: head, arguments: arguments)))
          state.ast.ranges[head] = headRange.upperBounded(by: state.currentIndex)
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
    Apply<ParserState, CallArgument>({ (state) in
      let backup = state.backup()

      // Parse a labeled arrgument.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let value = try expr.parse(&state) {
            return CallArgument(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled argument.
      state.restore(from: backup)
      if let value = try expr.parse(&state) {
        return CallArgument(value:value)
      }

      return nil
    })
  )

  static let staticValueMemberExpr = (
    primaryTypeExpr.and(take(.dot)).and(primaryDeclRef)
      .map({ (state, tree) -> NodeID<NameExpr> in
        state.ast[tree.1].incorporate(domain: .type(tree.0.0))
        state.ast.ranges[tree.1] = state.ast.ranges[tree.0.0]!.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)
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
      .map({ (state, token) -> NodeID<BooleanLiteralExpr> in
        let id = state.ast.insert(BooleanLiteralExpr(
          value: state.lexer.source[token.range] == "true"))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let integerLiteral = (
    take(.int)
      .map({ (state, token) -> NodeID<IntegerLiteralExpr> in
        let id = state.ast.insert(IntegerLiteralExpr(
          value: state.lexer.source[token.range].filter({ $0 != "_" })))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let floatingPointLiteral = (
    take(.float)
      .map({ (state, token) -> NodeID<FloatLiteralExpr> in
        let id = state.ast.insert(FloatLiteralExpr(
          value: state.lexer.source[token.range].filter({ $0 != "_" })))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let stringLiteral = (
    take(.string)
      .map({ (state, token) -> NodeID<StringLiteralExpr> in
        let id = state.ast.insert(StringLiteralExpr(
          value: String(state.lexer.source[token.range].dropFirst().dropLast())))
        state.ast.ranges[id] = token.range
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
      .map({ (state, tree) -> NodeID<BufferLiteralExpr> in
        let id = state.ast.insert(BufferLiteralExpr(elements: tree.0.1 ?? []))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let bufferComponentList = (
    expr.and(zeroOrMany(take(.comma).and(expr).second))
      .map({ (state, tree) -> [AnyExprID] in [tree.0] + tree.1 })
  )

  static let mapLiteral = (
    take(.lBrack).and(mapComponentList.or(mapComponentEmptyList)).and(take(.rBrack))
      .map({ (state, tree) -> NodeID<MapLiteralExpr> in
        let id = state.ast.insert(MapLiteralExpr(elements: tree.0.1))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let mapComponentEmptyList = (
    take(.colon)
      .map({ (_, _) -> [MapLiteralExpr.Element] in [] })
  )

  static let mapComponentList = (
    mapComponent.and(zeroOrMany(take(.comma).and(mapComponent).second))
      .map({ (state, tree) -> [MapLiteralExpr.Element] in [tree.0] + tree.1 })
  )

  static let mapComponent = (
    expr.and(take(.colon)).and(expr)
      .map({ (_, tree) -> MapLiteralExpr.Element in
        MapLiteralExpr.Element(key: tree.0.0, value: tree.1)
      })
  )

  static let primaryDeclRef = (
    identifierExpr.and(maybe(staticArgumentList))
      .map({ (state, tree) -> NodeID<NameExpr> in
        let id = state.ast.insert(NameExpr(name: tree.0, arguments: tree.1 ?? []))
        state.ast.ranges[id] = tree.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let implicitMemberRef = (
    take(.dot).and(primaryDeclRef)
      .map({ (state, tree) -> NodeID<NameExpr> in
        state.ast[tree.1].incorporate(domain: .implicit)
        state.ast.ranges[tree.1] = tree.0.range.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)
        return tree.1
      })
  )

  static let lambdaExpr = (
    take(.fun).and(maybe(captureList)).and(functionSignature).and(lambdaBody)
      .map({ (state, tree) -> NodeID<LambdaExpr> in
        let signature = tree.0.1

        let decl = state.ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun, range: tree.0.0.0.range),
          receiverEffect: signature.receiverEffect,
          captures: tree.0.0.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          body: tree.1,
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = state.ast.insert(LambdaExpr(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let lambdaBody = inContext(.functionBody, apply: TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> FunDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> FunDecl.Body in .block(id) })
  ))

  static let matchExpr = (
    take(.match).and(expr).and(take(.lBrace)).and(zeroOrMany(matchCase)).and(take(.rBrace))
      .map({ (state, tree) -> NodeID<MatchExpr> in
        let id = state.ast.insert(MatchExpr(subject: tree.0.0.0.1, cases: tree.0.1))
        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let matchCase = (
    pattern.and(maybe(take(.where).and(expr))).and(matchCaseBody)
      .map({ (state, tree) -> NodeID<MatchCase> in
        let id = state.ast.insert(MatchCase(
          pattern: tree.0.0, condition: tree.0.1?.1, body: tree.1))
        state.ast.ranges[id] = state.ast.ranges[tree.0.0]!.upperBounded(
          by: state.currentIndex)
        return id
      })
  )

  static let matchCaseBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> MatchCase.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> MatchCase.Body in .block(id) })
  )

  static let conditionalExpr: Recursive<ParserState, NodeID<CondExpr>> = (
    Recursive(_conditionalExpr.parse(_:))
  )

  private static let _conditionalExpr = (
    take(.if).and(conditionalClause).and(conditionalExprBody).and(maybe(conditionalTail))
      .map({ (state, tree) -> NodeID<CondExpr> in
        let id = state.ast.insert(CondExpr(
          condition: tree.0.0.1, success: tree.0.1, failure: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let conditionalClause = (
    conditionalClauseItem.and(zeroOrMany(take(.comma).and(conditionalClauseItem).second))
      .map({ (_, tree) -> [ConditionItem] in [tree.0] + tree.1 })
  )

  static let conditionalClauseItem = Choose(
    bindingPattern.and(take(.assign)).and(expr)
      .map({ (state, tree) -> ConditionItem in
        let id = state.ast.insert(BindingDecl(pattern: tree.0.0, initializer: tree.1))
        state.ast.ranges[id] = state.ast.ranges[tree.0.0]!.upperBounded(
          by: state.currentIndex)
        return .decl(id)
      }),
    or: expr
      .map({ (_, id) -> ConditionItem in .expr(id) })
  )

  static let conditionalExprBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> CondExpr.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> CondExpr.Body in .block(id) })
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
      .map({ (state, tree) -> NodeID<InoutExpr> in
        let id = state.ast.insert(InoutExpr(
          operatorRange: tree.0.range, subject: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let tupleExpr = (
    take(.lParen).and(maybe(tupleExprElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TupleExpr> in
        let id = state.ast.insert(TupleExpr(elements: tree.0.1 ?? []))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let tupleExprElementList = (
    tupleExprElement.and(zeroOrMany(take(.comma).and(tupleExprElement).second))
      .map({ (_, tree) -> [TupleExpr.Element] in [tree.0] + tree.1 })
  )

  static let tupleExprElement = (
    Apply<ParserState, TupleExpr.Element>({ (state) in
      let backup = state.backup()

      // Parse a labeled element.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let value = try expr.parse(&state) {
            return TupleExpr.Element(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      state.restore(from: backup)
      if let value = try expr.parse(&state) {
        return TupleExpr.Element(value: value)
      }

      return nil
    })
  )

  static let nilExpr = (
    take(.nil)
      .map({ (state, token) -> NodeID<NilExpr> in
        let id = state.ast.insert(NilExpr())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  // MARK: Patterns

  private static func anyPattern<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyPatternID>
  where Base.Context == ParserState, Base.Element: PatternID
  {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyPatternID.init(_:))
    })
  }

  static let pattern: Recursive<ParserState, AnyPatternID> = (
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
      .and(inContext(.bindingPattern, apply: oneOf([
        anyPattern(namePattern),
        anyPattern(tuplePattern),
        anyPattern(wildcardPattern),
      ])))
      .and(maybe(take(.colon).and(typeExpr)))
      .map({ (state, tree) -> NodeID<BindingPattern> in
        let id = state.ast.insert(BindingPattern(
          introducer: tree.0.0,
          subpattern: tree.0.1,
          annotation: tree.1?.1))
        state.ast.ranges[id] = tree.0.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let bindingIntroducer = (
    Apply<ParserState, SourceRepresentable<BindingPattern.Introducer>>({ (state) in
      guard let head = state.peek() else { return nil }

      let introducer: BindingPattern.Introducer
      switch head.kind {
      case .let:
        _ = state.take()
        introducer = .let

      case .var:
        _ = state.take()
        introducer = .var

      case .inout:
        _ = state.take()
        introducer = .inout

      case .sink:
        _ = state.take()
        guard state.take(.let) != nil else {
          throw ParseError("expected 'let'", at: state.currentLocation)
        }
        introducer = .sinklet

      default:
        return nil
      }

      return SourceRepresentable(
        value: introducer,
        range: head.range.upperBounded(by: state.currentIndex))
    })
  )

  static let exprPattern = (
    Apply<ParserState, AnyPatternID>({ (state) -> AnyPatternID? in
      // Attempt to parse tuples as patterns as deeply as possible.
      if let patternID = try tuplePattern.parse(&state) {
        return AnyPatternID(patternID)
      }

      // Attempt to parse a name pattern if we're in the context of a binding pattern.
      if state.contexts.last == .bindingPattern {
        if let patternID = try namePattern.parse(&state) {
          return AnyPatternID(patternID)
        }
      }

      // Default to an expression.
      guard let exprID = try expr.parse(&state) else { return nil }
      let id = state.ast.insert(ExprPattern(expr: exprID))
      state.ast.ranges[id] = state.ast.ranges[exprID]
      return AnyPatternID(id)
    })
  )

  static let namePattern = (
    take(.name)
      .map({ (state, token) -> NodeID<NamePattern> in
        let declID = state.ast.insert(VarDecl(
          identifier: SourceRepresentable(token: token, in: state.lexer.source)))
        state.ast.ranges[declID] = token.range

        let id = state.ast.insert(NamePattern(decl: declID))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let tuplePattern = (
    take(.lParen).and(maybe(tuplePatternElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TuplePattern> in
        let id = state.ast.insert(TuplePattern(elements: tree.0.1 ?? []))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: tree.1.range.upperBound)
        return id
      })
  )

  static let tuplePatternElementList = (
    tuplePatternElement.and(zeroOrMany(take(.comma).and(tuplePatternElement).second))
      .map({ (_, tree) -> [TuplePattern.Element] in [tree.0] + tree.1 })
  )

  static let tuplePatternElement = (
    Apply<ParserState, TuplePattern.Element>({ (state) in
      let backup = state.backup()

      // Parse a labeled element.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let value = try pattern.parse(&state) {
            return TuplePattern.Element(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              pattern: value)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      state.restore(from: backup)
      if let value = try pattern.parse(&state) {
        return TuplePattern.Element(pattern: value)
      }

      return nil
    })
  )

  static let wildcardPattern = (
    take(.under)
      .map({ (state, token) -> NodeID<WildcardPattern> in
        let id = state.ast.insert(WildcardPattern())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  // MARK: Statements

  private static func anyStmt<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyStmtID>
  where Base.Context == ParserState, Base.Element: StmtID
  {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyStmtID.init(_:))
    })
  }

  static let stmt: Recursive<ParserState, AnyStmtID> = (
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
      .map({ (state, tree) -> NodeID<BraceStmt> in
        let id = state.ast.insert(BraceStmt(stmts: tree.0.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let discardStmt = (
    take(.under).and(take(.assign)).and(expr)
      .map({ (state, tree) -> NodeID<DiscardStmt> in
        let id = state.ast.insert(DiscardStmt(expr: tree.1))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let doWhileStmt = (
    take(.do).and(loopBody).and(take(.while)).and(expr)
      .map({ (state, tree) -> NodeID<DoWhileStmt> in
        let id = state.ast.insert(DoWhileStmt(body: tree.0.0.1, condition: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let whileStmt = (
    take(.while).and(conditionalClause).and(loopBody)
      .map({ (state, tree) -> NodeID<WhileStmt> in
        let id = state.ast.insert(WhileStmt(condition: tree.0.1, body: tree.1))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let forStmt = (
    take(.for).and(bindingPattern).and(forRange).and(maybe(forFilter)).and(loopBody)
      .map({ (state, tree) -> NodeID<ForStmt> in
        let decl = state.ast.insert(BindingDecl(pattern: tree.0.0.0.1))
        state.ast.ranges[decl] = state.ast.ranges[tree.0.0.0.1]

        let id = state.ast.insert(ForStmt(
          binding: decl, domain: tree.0.0.1, filter: tree.0.1, body: tree.1))
        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let forRange = (
    take(.in).and(expr).second
  )

  static let forFilter = (
    take(.where).and(expr).second
  )

  static let loopBody = inContext(.loopBody, apply: braceStmt)

  static let returnStmt = (
    take(.return).and(maybe(onSameLine(expr)))
      .map({ (state, tree) -> NodeID<ReturnStmt> in
        let id = state.ast.insert(ReturnStmt(value: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let yieldStmt = (
    take(.yield).and(onSameLine(expr))
      .map({ (state, tree) -> NodeID<YieldStmt> in
        let id = state.ast.insert(YieldStmt(value: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let breakStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<BreakStmt> in
        let id = state.ast.insert(BreakStmt())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let continueStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<ContinueStmt> in
        let id = state.ast.insert(ContinueStmt())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let bindingStmt = (
    Apply<ParserState, AnyStmtID>({ (state) -> AnyStmtID? in
      let backup = state.backup()
      do {
        if let element = try conditionalBindingStmt.parse(&state) { return AnyStmtID(element) }
      } catch {}
      state.restore(from: backup)

      if let decl = try bindingDecl.parse(&state) {
        let id = state.ast.insert(DeclStmt(decl: AnyDeclID(decl)))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return AnyStmtID(id)
      } else {
        return nil
      }
    })
  )

  static let conditionalBindingStmt = (
    bindingDecl.and(take(.else)).and(conditionalBindingFallback)
      .map({ (state, tree) -> NodeID<CondBindingStmt> in
        let bindingRange = state.ast.ranges[tree.0.0]!

        if state.ast[tree.0.0].initializer == nil {
          throw ParseError(
            "conditional binding requires an initializer", at: bindingRange.first())
        }

        let id = state.ast.insert(CondBindingStmt(
          binding: tree.0.0, fallback: tree.1))
        state.ast.ranges[id] = bindingRange.upperBounded(by: state.currentIndex)
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
      .map({ (state, decl) -> NodeID<DeclStmt> in
        let id = state.ast.insert(DeclStmt(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
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
      .map({ (state, expr) -> NodeID<ExprStmt> in
        let id = state.ast.insert(ExprStmt(expr: expr))
        state.ast.ranges[id] = state.ast.ranges[expr]
        return id
      })
  )

  // MARK: Type expressions

  private static func anyTypeExpr<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyTypeExprID>
  where Base.Context == ParserState, Base.Element: TypeExprID
  {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyTypeExprID.init(_:))
    })
  }

  static let typeExpr: Recursive<ParserState, AnyTypeExprID> = (
    Recursive(unionTypeExpr.parse(_:))
  )

  // static let storedProjectionTypeExpr = ?

  static let unionTypeExpr = (
    modifiedTypeExpr.and(zeroOrMany(take(.pipe).and(modifiedTypeExpr).second))
      .map({ (state, tree) -> AnyTypeExprID in
        if tree.1.isEmpty {
          return tree.0
        } else {
          let elements = [tree.0] + tree.1
          let id = state.ast.insert(UnionTypeExpr(elements: elements))
          state.ast.ranges[id] = state.ast.ranges[tree.0]!.upperBounded(
            by: state.currentIndex)
          return AnyTypeExprID(id)
        }
      })
  )

  static let modifiedTypeExpr = (
    Apply<ParserState, AnyTypeExprID>({ (state) -> AnyTypeExprID? in
      guard let head = state.peek() else { return nil }

      switch head.kind {
      case .async:
        // async-type-expr
        _ = state.take()
        guard let operand = try typeExpr.parse(&state) else {
          throw ParseError("expected type expression", at: state.currentLocation)
        }

        let id = state.ast.insert(AsyncTypeExpr(operand: operand))
        state.ast.ranges[id] = head.range.upperBounded(by: state.currentIndex)
        return AnyTypeExprID(id)

      case .indirect:
        // indirect-type-expr
        _ = state.take()
        guard let operand = try typeExpr.parse(&state) else {
          throw ParseError("expected type expression", at: state.currentLocation)
        }

        let id = state.ast.insert(IndirectTypeExpr(operand: operand))
        state.ast.ranges[id] = head.range.upperBounded(by: state.currentIndex)
        return AnyTypeExprID(id)

      case .any:
        // existential-type-expr
        _ = state.take()
        guard let traits = try traitComposition.parse(&state) else {
          throw ParseError("expected trait composition", at: state.currentLocation)
        }
        let clause = try whereClause.parse(&state)

        let id = state.ast.insert(ExistentialTypeExpr(traits: traits, whereClause: clause))
        state.ast.ranges[id] = head.range.upperBounded(
          by: clause?.range?.upperBound ?? state.ast.ranges[traits.last!]!.upperBound)
        return AnyTypeExprID(id)

      default:
        return try compoundTypeExpr.parse(&state)
      }
    })
  )

  static let nameTypeExpr = (
    compoundTypeExpr
      .map({ (state, id) -> NodeID<NameTypeExpr> in
        if let converted = NodeID<NameTypeExpr>(id) {
          return converted
        } else {
          throw ParseError("expected type name", at: state.ast.ranges[id]!.first())
        }
      })
  )

  static let compoundTypeExpr = (
    Apply<ParserState, AnyTypeExprID>({ (state) -> AnyTypeExprID? in
      guard var head = try primaryTypeExpr.parse(&state) else { return nil }
      let headRange = state.ast.ranges[head]!

      while true {
        if state.take(.dot) != nil {
          guard let member = try primaryTypeDeclRef.parse(&state) else {
            throw ParseError("expected type member name", at: state.currentLocation)
          }

          state.ast[member].incorporate(domain: head)
          state.ast.ranges[member] = headRange.upperBounded(by: state.currentIndex)
          head = AnyTypeExprID(member)
          continue
        }

        if state.take(.twoColons) != nil {
          guard let lens = try primaryTypeExpr.parse(&state) else {
            throw ParseError("expected focus", at: state.currentLocation)
          }

          let id = state.ast.insert(ConformanceLensTypeExpr(subject: head, lens: lens))
          state.ast.ranges[id] = headRange.upperBounded(by: state.currentIndex)
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
      .map({ (state, tree) -> NodeID<NameTypeExpr> in
        let id = state.ast.insert(NameTypeExpr(identifier: tree.0, arguments: tree.1 ?? []))
        state.ast.ranges[id] = tree.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let tupleTypeExpr = (
    take(.lBrace).and(maybe(tupleTypeExprElementList)).and(take(.rBrace))
      .map({ (state, tree) -> NodeID<TupleTypeExpr> in
        let id = state.ast.insert(TupleTypeExpr(elements: tree.0.1 ?? []))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: tree.1.range.upperBound)
        return id
      })
  )

  static let tupleTypeExprElementList = (
    tupleTypeExprElement.and(zeroOrMany(take(.comma).and(tupleTypeExprElement).second))
      .map({ (_, tree) -> [TupleTypeExpr.Element] in [tree.0] + tree.1 })
  )

  static let tupleTypeExprElement = (
    Apply<ParserState, TupleTypeExpr.Element>({ (state) in
      let backup = state.backup()

      // Parse a labeled element.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let type = try typeExpr.parse(&state) {
            return TupleTypeExpr.Element(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              type: type)
          }
        }
      }

      // Backtrack and parse an unlabeled element.
      state.restore(from: backup)
      if let type = try typeExpr.parse(&state) {
        return TupleTypeExpr.Element(type: type)
      }

      return nil
    })
  )

  static let lambdaOrParenthesizedTypeExpr = (
    Apply<ParserState, AnyTypeExprID>({ (state) -> AnyTypeExprID? in
      switch state.peek()?.kind {
      case .lBrack:
        // The expression starts with a left bracket; assume it's a lambda.
        return try lambdaTypeExpr.parse(&state).map(AnyTypeExprID.init(_:))

      case .lParen:
        // The expression starts with a left parenthesis; assume it's a lambda and fall back to
        // a parenthesized expression if that fails.
        let backup = state.backup()
        do {
          return try lambdaTypeExpr.parse(&state).map(AnyTypeExprID.init(_:))
        } catch {
          state.restore(from: backup)
          return try parenthesizedTypeExpr.parse(&state)
        }

      default:
        return nil
      }
    })
  )

  static let parenthesizedTypeExpr = (
    take(.lParen).and(typeExpr).and(take(.rParen))
      .map({ (state, tree) -> AnyTypeExprID in tree.0.1 })
  )

  static let lambdaTypeExpr = Choose(
    typeErasedLambdaTypeExpr,
    or: lambdaEnvironment.and(typeErasedLambdaTypeExpr)
      .map({ (state, tree) in
        state.ast[tree.1].incorporate(environment: tree.0)
        state.ast.ranges[tree.1]!.lowerBound = tree.0.range!.lowerBound
        return tree.1
      })
  )

  static let typeErasedLambdaTypeExpr = (
    take(.lParen).and(maybe(lambdaParameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(take(.arrow))
      .and(typeExpr)
      .map({ (state, tree) -> NodeID<LambdaTypeExpr> in
        let id = state.ast.insert(LambdaTypeExpr(
          receiverEffect: tree.0.0.1,
          parameters: tree.0.0.0.0.1 ?? [],
          output: tree.1))
        state.ast.ranges[id] = tree.0.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let lambdaParameterList = (
    lambdaParameter.and(zeroOrMany(take(.comma).and(lambdaParameter).second))
      .map({ (_, tree) -> [LambdaTypeExpr.Parameter] in [tree.0] + tree.1 })
  )

  static let lambdaParameter = (
    Apply<ParserState, LambdaTypeExpr.Parameter>({ (state) in
      let backup = state.backup()

      // Parse a labeled parameter.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let type = try parameterTypeExpr.parse(&state) {
            return LambdaTypeExpr.Parameter(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              type: type)
          }
        }
      }

      // Backtrack and parse an unlabeled parameter.
      state.restore(from: backup)
      if let type = try parameterTypeExpr.parse(&state) {
        return LambdaTypeExpr.Parameter(type: type)
      }

      return nil
    })
  )

  static let lambdaEnvironment = (
    take(.lBrack).and(maybe(typeExpr)).and(take(.rBrack))
      .map({ (state, tree) -> SourceRepresentable<AnyTypeExprID> in
        let range = tree.0.0.range.upperBounded(by: tree.1.range.upperBound)
        if let expr = tree.0.1 {
          return SourceRepresentable(value: expr, range: range)
        } else {
          let expr = state.ast.insert(TupleTypeExpr())
          state.ast.ranges[expr] = SourceRange(
            in: state.lexer.source,
            from: tree.0.0.range.upperBound,
            to: tree.1.range.lowerBound)
          return SourceRepresentable(value: AnyTypeExprID(expr), range: range)
        }
      })
  )

  static let wildcardTypeExpr = (
    take(.under)
      .map({ (state, token) -> NodeID<WildcardTypeExpr> in
        let id = state.ast.insert(WildcardTypeExpr())
        state.ast.ranges[id] = token.range
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
      .map({ (state, tree) -> NodeID<ParameterTypeExpr> in
        let id = state.ast.insert(ParameterTypeExpr(
          convention: tree.0 ?? SourceRepresentable(value: .let),
          bareType: tree.1))

        state.ast.ranges[id] = (
          tree.0?.range.map({ $0.upperBounded(by: state.currentIndex) })
          ?? state.ast.ranges[tree.1])

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
    Apply<ParserState, GenericArgument>({ (state) in
      let backup = state.backup()

      // Parse a labeled value argument.
      if let label = state.take(if: { $0.isLabel }) {
        if state.take(.colon) != nil {
          if let value = try staticUnlabaledArgument.parse(&state) {
            return GenericArgument(
              label: SourceRepresentable(token: label, in: state.lexer.source),
              value: value)
          }
        }
      }

      // Backtrack and parse an unlabeled value argument.
      state.restore(from: backup)
      if let value = try staticUnlabaledArgument.parse(&state) {
        return GenericArgument(value: value)
      }

      return nil
    })
  )

  static let staticUnlabaledArgument = (
    staticValueArgument.or(staticTypeArgument)
  )

  static let staticTypeArgument = (
    maybe(typeAttribute).and(typeExpr)
      .map({ (state, tree) -> GenericArgument.Value in
        .type(tree.1)
      })
  )

  static let staticValueArgument = (
    valueAttribute.and(expr)
      .map({ (state, tree) -> GenericArgument.Value in
        .expr(tree.1)
      })
  )

  static let whereClause = (
    take(.where).and(whereClauseConstraintList)
      .map({ (state, tree) -> SourceRepresentable<WhereClause> in
        SourceRepresentable(
          value: WhereClause(constraints: tree.1),
          range: tree.0.range.upperBounded(by: state.currentIndex))
      })
  )

  static let whereClauseConstraintList = (
    whereClauseConstraint.and(zeroOrMany(take(.comma).and(whereClauseConstraint).second))
      .map({ (state, tree) -> [SourceRepresentable<WhereClause.ConstraintExpr>] in
        [tree.0] + tree.1
      })
  )

  static let whereClauseConstraint = (
    typeConstraint.or(valueConstraint)
  )

  static let typeConstraint = (
    Apply<ParserState, SourceRepresentable<WhereClause.ConstraintExpr>>({ (state) in
      guard let lhs = try nameTypeExpr.parse(&state) else { return nil }

      // equality-constraint
      if state.take(.equal) != nil {
        guard let rhs = try typeExpr.parse(&state) else {
          throw ParseError("expected type expression", at: state.currentLocation)
        }
        return SourceRepresentable(
          value: .equality(l: lhs, r: rhs),
          range: state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex))
      }

      // conformance-constraint
      if state.take(.colon) != nil {
        guard let traits = try traitComposition.parse(&state) else {
          throw ParseError("expected trait composition", at: state.currentLocation)
        }
        return SourceRepresentable(
          value: .conformance(l: lhs, traits: traits),
          range: state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex))
      }

      throw ParseError("expected constraint operator", at: state.currentLocation)
    })
  )

  static let valueConstraint = (
    valueAttribute.and(expr)
      .map({ (state, tree) -> SourceRepresentable<WhereClause.ConstraintExpr> in
        SourceRepresentable(
          value: .value(tree.1),
          range: tree.0.range.upperBounded(by: state.currentIndex))
      })
  )

  static let traitComposition = (
    nameTypeExpr.and(zeroOrMany(take(.ampersand).and(nameTypeExpr).second))
      .map({ (state, tree) -> TraitComposition in [tree.0] + tree.1 })
  )

  // MARK: Identifiers

  static let identifierExpr = (
    entityIdentifier.and(maybe(take(.dot).and(methodIntroducer)))
      .map({ (state, tree) -> SourceRepresentable<Name> in
        if let (_, introducer) = tree.1 {
          return tree.0.introduced(by: introducer)
        } else {
          return tree.0
        }
      })
  )

  static let entityIdentifier = (
    Apply<ParserState, SourceRepresentable<Name>>({ (state) in
      switch state.peek()?.kind {
      case .name, .under:
        // function-entity-identifier
        let head = state.take()!
        var labels: [String?] = []

        if state.currentCharacter == "(" {
          let backup = state.backup()
          _ = state.take()
          var closeParenFound = false
          defer {
            // Backtrack if we didn't find a closing parenthesis or if there are no labels. That
            // will let the argument-list parser pickup after the identifier to either catch a
            // parse error in the former case (no closing parenthesis) or parse an empty argument
            // list in the latter (no labels).
            // Note: `foo()` is *not* a valid name, it's a function call.
            if !closeParenFound || labels.isEmpty { state.restore(from: backup) }
          }

          while !state.hasLeadingWhitespace {
            if state.take(.under) != nil {
              labels.append(nil)
            } else if let label = state.take(if: { $0.isLabel }) {
              labels.append(String(state.lexer.source[label.range]))
            } else {
              break
            }

            if state.takeWithoutSkippingWhitespace(.colon) == nil {
              break
            }

            if let end = state.takeWithoutSkippingWhitespace(.rParen) {
              closeParenFound = true
              break
            }
          }
        }

        return SourceRepresentable(
          value: Name(stem: String(state.lexer.source[head.range]), labels: labels),
          range: head.range)


      case .infix, .prefix, .postfix:
        // operator-entity-identifier
        let head = state.take()!

        if state.hasLeadingWhitespace {
          throw ParseError("expected operator", at: state.currentLocation)
        }
        guard let oper = state.takeOperator() else {
          throw ParseError("expected operator", at: state.currentLocation)
        }

        let stem = String(state.lexer.source[oper.range!])
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
      .map({ (state, token) -> SourceRepresentable<Identifier> in
        SourceRepresentable(value: String(state.lexer.source[token.range]), range: token.range)
      })
  )

  // MARK: Attributes

  static let declAttribute = (
    take(.attribute).and(maybe(attributeArgumentList))
      .map({ (state, tree) -> SourceRepresentable<Attribute> in
        SourceRepresentable(
          value: Attribute(
            name: SourceRepresentable(token: tree.0, in: state.lexer.source),
            arguments: tree.1 ?? []),
          range: tree.0.range.upperBounded(by: state.currentIndex))
      })
  )

  static let attributeArgumentList = (
    take(.lParen)
      .and(attributeArgument).and(zeroOrMany(take(.comma).and(attributeArgument).second))
      .and(take(.rParen))
      .map({ (state, tree) -> [Attribute.Argument] in [tree.0.0.1] + tree.0.1 })
  )

  static let attributeArgument = (
    stringAttributeArgument.or(integerAttributeArgument)
  )

  static let stringAttributeArgument = (
    take(.string)
      .map({ (state, token) -> Attribute.Argument in
        let value = String(state.lexer.source[token.range].dropFirst().dropLast())
        return .string(SourceRepresentable(value: value, range: token.range))
      })
  )

  static let integerAttributeArgument = (
    take(.int)
      .map({ (state, token) -> Attribute.Argument in
        if let value = Int(state.lexer.source[token.range]) {
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

    typealias Context = ParserState

    typealias Element = Token

    /// The kind of the token to consume.
    let kind: Token.Kind

    func parse(_ state: inout ParserState) throws -> Token? {
      state.take(kind)
    }

  }

  /// A combinator that parses contextual keywords.
  struct ContextualKeyword<T: RawRepresentable>: Combinator where T.RawValue == String {

    typealias Context = ParserState

    typealias Element = SourceRepresentable<T>

    func parse(_ state: inout ParserState) throws -> Element? {
      if let next = state.peek(), next.kind == .name {
        if let value = T(rawValue: String(state.lexer.source[next.range])) {
          _ = state.take()
          return SourceRepresentable(value: value, range: next.range)
        }
      }
      return nil
    }

  }

  /// A combinator that updates the parsing contexts.
  struct WrapInContext<Base: Combinator>: Combinator where Base.Context == ParserState {

    typealias Context = ParserState

    typealias Element = Base.Element

    /// The context in which `base` should be applied.
    let context: ParserState.Context

    /// The underlying combinator.
    public let base: Base

    func parse(_ state: inout ParserState) throws -> Element? {
      state.contexts.append(context)
      defer { state.contexts.removeLast() }
      return try base.parse(&state)
    }

  }

  /// Creates a combinator that parses tokens with the specified kind.
  static func take(_ kind: Token.Kind) -> TakeKind {
    TakeKind(kind: kind)
  }

  /// Creates a combinator that parses name tokens with the specified value.
  static func take(nameTokenWithValue value: String) -> Apply<ParserState, Token> {
    Apply({ (state) in state.take(nameTokenWithValue: value) })
  }

  /// Creates a combinator that parses attribute tokens with the specified name.
  static func attribute(_ name: String) -> Apply<ParserState, Token> {
    Apply({ (state) in state.take(attribute: name) })
  }

  /// Creates a combinator that translates token kinds to instances of type.
  static func translate<T>(
    _ table: [Token.Kind: T]
  ) -> Apply<ParserState, SourceRepresentable<T>> {
    Apply({ (state) in
      guard let head = state.peek() else { return nil }
      if let translation = table[head.kind] {
        _ = state.take()
        return SourceRepresentable(value: translation, range: head.range)
      } else {
        return nil
      }
    })
  }

  /// Creates a combinator that pushes `context` to the parser state before applying, and pops
  /// that context afterward.
  static func inContext<Base: Combinator>(
    _ context: ParserState.Context,
    apply base: Base
  ) -> WrapInContext<Base> {
    WrapInContext(context: context, base: base)
  }

  /// Creates a combinator that applies `base` only if its input is not preceeded by whitespaces.
  static func withoutLeadingWhitespace<Base: Combinator>(
    _ base: Base
  ) -> Apply<ParserState, Base.Element>
  where Base.Context == ParserState
  {
    Apply({ (state) in try state.hasLeadingWhitespace ? nil : base.parse(&state) })
  }

  /// Creates a combinator that applies `base` only if its input is not preceeded by newlines.
  static func onSameLine<Base: Combinator>(
    _ base: Base
  ) -> Apply<ParserState, Base.Element>
  where Base.Context == ParserState
  {
    Apply({ (state) in
      if let t = state.peek() {
        return try state.hasNewline(inCharacterStreamUpTo: t.range.lowerBound)
          ? nil
          : base.parse(&state)
      } else {
        // Let `base` handle end of stream.
        return try base.parse(&state)
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
