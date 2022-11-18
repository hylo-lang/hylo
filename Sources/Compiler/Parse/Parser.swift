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

  /// Parses the declarations of `input`, inserts them into `ast[module]`.
  ///
  /// - Returns: `(translation, diagnostics)` where `diagnostics` are the diagnostics produced by
  ///   the parser and `translation` is the ID of parsed translation unit.
  public static func parse(
    _ input: SourceFile,
    into module: NodeID<ModuleDecl>,
    in ast: inout AST
  ) throws -> (translation: NodeID<TopLevelDeclSet>, diagnostics: [Diagnostic]) {
    // Initialize the parser's state.
    var state = ParserState(ast: ast, lexer: Lexer(tokenizing: input))

    // Parse the source file.
    let translation: NodeID<TopLevelDeclSet>
    do {
      translation = try Self.parseSourceFile(in: &state)
      state.ast[module].addSourceFile(translation)
    } catch let error as DiagnosedError {
      // Rethrow the error adding the diagnostics generated so far.
      throw DiagnosedError(state.diagnostics + error.diagnostics)
    }

    // Make sure the entire input was consumed.
    assert(state.peek() == nil, "expected EOF")

    // Return if no error was encountered; otherwise, throw.
    if state.diagnostics.contains(where: { $0.level == .error }) {
      throw DiagnosedError(state.diagnostics)
    } else {
      ast = state.ast
      return (translation: translation, diagnostics: state.diagnostics)
    }
  }

  /// Parses an instance of `TopLevelDeclSet`.
  static func parseSourceFile(in state: inout ParserState) throws -> NodeID<TopLevelDeclSet> {
    var members: [AnyDeclID] = []

    while let head = state.peek() {
      // Ignore semicolons.
      if state.take(.semi) != nil { continue }

      // Attempt to parse a member.
      do {
        if let member = try parseDecl(in: &state) {
          members.append(member)
          continue
        }
      } catch let error as DiagnosedError {
        state.diagnostics.append(contentsOf: error.diagnostics)
        continue
      } catch let error {
        state.diagnostics.append(Diagnostic(level: .error, message: error.localizedDescription))
        continue
      }

      // Attempt to recover.
      _ = state.take()
      switch head.kind {
      case .unterminatedBlockComment:
        // Nothing to parse after an unterminated block comment.
        state.diagnostics.append(.diagnose(
          unterminatedCommentEndingAt: head.range.last() ?? head.range.first()))
        break

      case .unterminatedString:
        // Nothing to parse after an unterminated string.
        state.diagnostics.append(.diagnose(
          unterminatedStringEndingAt: head.range.last() ?? head.range.first()))
        break

      default:
        state.diagnostics.append(.diagnose(unexpectedToken: head))

        // Attempt to recover at the next new line.
        while let next = state.peek() {
          if state.hasNewline(before: next) { break }
          _ = state.take()
        }
      }
    }

    return try state.ast.insert(wellFormed: TopLevelDeclSet(decls: members))
  }

  // MARK: Declarations

  /// Parses a declaration prologue in `state` and then calls `continuation`.
  static func parseDeclPrologue<R>(
    in state: inout ParserState,
    then continuation: (_ prologue: DeclPrologue, _ state: inout ParserState) throws -> R?
  ) throws -> R? {
    guard let startIndex = state.peek()?.range.lowerBound else { return nil }
    var isPrologueEmpty = true

    // Parse attributes.
    var attributes: [SourceRepresentable<Attribute>] = []
    while let a = try Parser.declAttribute.parse(&state) {
      attributes.append(a)
      isPrologueEmpty = false
    }

    // Parse modifiers.
    var accessModifiers: Set<SourceRepresentable<AccessModifier>> = []
    var memberModifiers: Set<SourceRepresentable<MemberModifier>> = []
    while true {
      if let access = try Parser.accessModifier.parse(&state) {
        isPrologueEmpty = false

        // Catch access modifiers declared after member modifiers.
        if let member = memberModifiers.first {
          state.diagnostics.append(.diagnose(
            memberModifier: member,
            appearsBeforeAccessModifier: access))
        }

        // Catch duplicate access modifiers.
        else if !accessModifiers.insert(access).inserted {
          state.diagnostics.append(.diagnose(duplicateAccessModifier: access))
        }

        // Look for the next modifier.
        continue
      }

      if let member = try Parser.memberModifier.parse(&state) {
        isPrologueEmpty = false

        // Catch member modifiers declared at non-type scope.
        if !state.atTypeScope {
          state.diagnostics.append(.diagnose(unexpectedMemberModifier: member))
        }

        // Catch duplicate member modifiers.
        else if !memberModifiers.insert(member).inserted {
          state.diagnostics.append(.diagnose(duplicateMemberModifier: member))
        }

        // Look for the next modifier.
        continue
      }

      break
    }

    // Apply the continuation.
    let prologue = DeclPrologue(
      isEmpty: isPrologueEmpty,
      startIndex: startIndex,
      attributes: attributes,
      accessModifiers: accessModifiers,
      memberModifiers: memberModifiers)
    return try continuation(prologue, &state)
  }

  /// Parses a declaration in `state`.
  static func parseDecl(in state: inout ParserState) throws -> AnyDeclID? {
    func continuation(
      prologue: DeclPrologue,
      state: inout ParserState
    ) throws -> AnyDeclID? {
      // Look ahead to select the appropriate declaration parser.
      switch state.peek()?.kind {
      case .let, .inout, .var, .sink:
        return try parseBindingDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .fun, .infix, .postfix, .prefix:
        return try parseFunctionOrMethodDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .`init`:
        return try parseInitDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .subscript:
        return try parseSubscriptDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .property:
        return try parsePropertyDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .namespace:
        return try parseNamespaceDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .trait:
        return try parseTraitDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .type:
        if state.atTraitScope {
          return try parseAssociatedTypeDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        } else {
          return try parseProductTypeDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        }

      case .typealias:
        return try parseTypeAliasDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .conformance:
        return try parseConformanceDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .extension:
        return try parseExtensionDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .import:
        return try parseImportDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .operator:
        return try parseOperatorDecl(withPrologue: prologue, in: &state)
          .map(AnyDeclID.init)

      case .name:
        let introducer = state.lexer.source[state.peek()!.range]
        if introducer == "value" && state.atTypeScope {
          // Note: associated values are parsed at any type scope to produce better diagnostics
          // when they are not at trait scope.
          return try parseAssociatedValueDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        }
        if introducer == "memberwise" && state.atTypeScope {
          return try parseMemberwiseInitDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        }

      default:
        break
      }

      if prologue.isEmpty {
        return nil
      } else {
        throw DiagnosedError(expected("declaration", at: state.currentLocation))
      }
    }

    // Note: this return statement must follow the declaration of `continuation` to work around
    // an apparent bug in swiftc. See: https://github.com/apple/swift/issues/62136
    return try parseDeclPrologue(in: &state, then: continuation)
  }

  /// Parses the body of a type declaration, adding `context` to `state.contexts` while parsing
  /// each member declaration.
  ///
  /// - Note: The function never returns a soft failure. It will throw if it can't parse the left
  ///   brace of the body, even if it didn't consume any token from the stream.
  ///
  /// - Parameters:
  ///   - state: A mutable projection of the parser's state.
  ///   - context: The parser context in which members should be parsed.
  private static func parseTypeDeclBody(
    in state: inout ParserState,
    wrappedIn context: ParserState.Context
  ) throws -> [AnyDeclID] {
    // Parse the left delimiter.
    guard let opener = state.take(.lBrace) else {
      throw DiagnosedError(expected("'{'", at: state.currentLocation))
    }

    // Push the context.
    state.contexts.append(context)
    defer { state.contexts.removeLast() }

    // Parse the members.
    var members: [AnyDeclID] = []
    while true {
      // Ignore semicolons.
      if state.take(.semi) != nil { continue }

      // Exit if we found the right delimiter.
      if state.take(.rBrace) != nil { break }

      // Attempt to parse a member.
      do {
        if let member = try parseDecl(in: &state) {
          members.append(member)
          continue
        }
      } catch let error as DiagnosedError {
        state.diagnostics.append(contentsOf: error.diagnostics)
        continue
      }

      // Nothing was consumed. Skip the next token or, if we reached EOF, diagnose a missing right
      // delimiter and exit.
      guard let head = state.take() else {
        state.diagnostics.append(expected(
          "'}'",
          at: state.currentLocation,
          children: [.error("to match this '{'", range: opener.range)]
        ))
        break
      }

      // Diagnose the error.
      state.diagnostics.append(expected("declaration", at: head.range.first()))

      // Skip tokens until we find a right delimiter or the start of another declaration.
      state.skip(while: { (next) in !next.mayBeginDecl && (next.kind != .rBrace) })
    }

    return members
  }

  /// Parses an instance of `AssociatedTypeDecl`.
  static func parseAssociatedTypeDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<AssociatedTypeDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.type).and(take(.name))
        .and(maybe(conformanceList))
        .and(maybe(whereClause))
        .and(maybe(take(.assign).and(typeExpr).second))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Associated type declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Associated type declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw DiagnosedError(prologue.accessModifiers.map(
        Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `AssociatedTypeDecl`.
    let decl = try state.ast.insert(wellFormed: AssociatedTypeDecl(
      introducerRange: parts.0.0.0.0.range,
      identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
      conformances: parts.0.0.1 ?? [],
      whereClause: parts.0.1,
      defaultValue: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `AssociatedValueDecl`.
  static func parseAssociatedValueDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<AssociatedValueDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(nameTokenWithValue: "value").and(take(.name))
        .and(maybe(whereClause))
        .and(maybe(take(.assign).and(expr).second))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Associated value declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Associated value declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw DiagnosedError(prologue.accessModifiers.map(
        Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `AssociatedValueDecl`.
    let decl = try state.ast.insert(wellFormed: AssociatedValueDecl(
      introducerRange: parts.0.0.0.range,
      identifier: SourceRepresentable(token: parts.0.0.1, in: state.lexer.source),
      whereClause: parts.0.1,
      defaultValue: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `BindingDecl`.
  static func parseBindingDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<BindingDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      bindingPattern
        .and(maybe(take(.assign).and(expr).second))
    )
    guard let (pattern, initializer) = try parser.parse(&state) else { return nil }

    // Create a new `BindingDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: BindingDecl(
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      memberModifier: prologue.memberModifiers.first,
      pattern: pattern,
      initializer: initializer))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `ConformanceDecl`.
  static func parseConformanceDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ConformanceDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.conformance).and(typeExpr)
        .and(conformanceList)
        .and(maybe(whereClause))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .extensionBody) }))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Conformance declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Conformance declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ConformanceDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: ConformanceDecl(
      accessModifier: prologue.accessModifiers.first,
      subject: parts.0.0.0.1,
      conformances: parts.0.0.1,
      whereClause: parts.0.1,
      members: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `ExtensionDecl`.
  static func parseExtensionDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ExtensionDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.extension).and(typeExpr)
        .and(maybe(whereClause))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .extensionBody) }))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Extension declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Extension declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw DiagnosedError(prologue.accessModifiers.map(
        Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ExtensionDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: ExtensionDecl(
      accessModifier: prologue.accessModifiers.first,
      subject: parts.0.0.1,
      whereClause: parts.0.1,
      members: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `FunctionDecl` or `MethodDecl`.
  static func parseFunctionOrMethodDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> AnyDeclID? {
    // Parse the parts of the declaration.
    let parser = (
      functionDeclHead
        .and(functionDeclSignature)
        .and(maybe(functionOrMethodDeclBody))
    )
    guard let ((head, signature), body) = try parser.parse(&state) else { return nil }

    switch body {
    case .method(let impls):
      return try AnyDeclID(buildMethodDecl(
        prologue: prologue,
        head: head,
        signature: signature,
        impls: impls,
        in: &state))

    case .function(let body):
      return try AnyDeclID(buildFunctionDecl(
        prologue: prologue,
        head: head,
        signature: signature,
        body: body,
        in: &state))

    case nil:
      return try AnyDeclID(buildFunctionDecl(
        prologue: prologue,
        head: head,
        signature: signature,
        body: nil,
        in: &state))
    }
  }

  /// Builds a new instance of `FunctionDecl` from its parsed parts.
  private static func buildFunctionDecl(
    prologue: DeclPrologue,
    head: FunctionDeclHead,
    signature: FunctionDeclSignature,
    body: FunctionDecl.Body?,
    in state: inout ParserState
  ) throws -> NodeID<FunctionDecl> {
    // Non-static member function declarations require an implicit receiver parameter.
    let receiver: NodeID<ParameterDecl>?
    if state.atTypeScope && !prologue.isStatic {
      receiver = try state.ast.insert(wellFormed: ParameterDecl(
        identifier: SourceRepresentable(value: "self")))
    } else {
      receiver = nil
    }

    // Create a new `FunctionDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: FunctionDecl(
      introducerRange: head.name.introducerRange,
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      memberModifier: prologue.memberModifiers.first,
      receiverEffect: signature.receiverEffect,
      notation: head.name.notation,
      identifier: head.name.stem,
      genericClause: head.genericClause,
      explicitCaptures: head.captures,
      parameters: signature.parameters,
      receiver: receiver,
      output: signature.output,
      body: body))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Builds a new instance of `Method` from its parsed parts.
  private static func buildMethodDecl(
    prologue: DeclPrologue,
    head: FunctionDeclHead,
    signature: FunctionDeclSignature,
    impls: [NodeID<MethodImplDecl>],
    in state: inout ParserState
  ) throws -> NodeID<MethodDecl> {
    // Method declarations cannot be static.
    if let modifier = prologue.memberModifiers.first(where: { (m) in m.value == .static }) {
      throw DiagnosedError(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Method declarations cannot have a receiver effect.
    if let effect = signature.receiverEffect {
      throw DiagnosedError(.diagnose(unexpectedEffect: effect))
    }

    // Method declarations cannot have captures.
    if let capture = head.captures.first {
      throw DiagnosedError(.diagnose(unexpectedCapture: state.ast[state.ast[capture].pattern]))
    }

    // Create a new `MethodDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: MethodDecl(
      introducerRange: head.name.introducerRange,
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      notation: head.name.notation,
      identifier: head.name.stem,
      genericClause: head.genericClause,
      parameters: signature.parameters,
      output: signature.output,
      impls: impls))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `ImportDecl`.
  static func parseImportDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ImportDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.import).and(take(.name))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Import declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Import declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw DiagnosedError(prologue.accessModifiers.map(
        Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ImportDecl`.
    let decl = try state.ast.insert(wellFormed: ImportDecl(
      introducerRange: parts.0.range,
      identifier: SourceRepresentable(token: parts.1, in: state.lexer.source)))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `InitializerDecl`.
  static func parseInitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<InitializerDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      initDeclHead
        .and(initDeclSignature)
        .and(initDeclBody)
    )
    guard let ((head, signature), body) = try parser.parse(&state) else { return nil }

    // Init declarations cannot be static.
    if let modifier = prologue.memberModifiers.first(where: { (m) in m.value == .static }) {
      throw DiagnosedError(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Init declarations require an implicit receiver parameter.
    let receiver = try state.ast.insert(wellFormed: ParameterDecl(
      identifier: SourceRepresentable(value: "self")))

    // Create a new `InitializerDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.isEmpty)
    let decl = try! state.ast.insert(wellFormed: InitializerDecl(
      introducer: head.introducer,
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      genericClause: head.genericClause,
      parameters: signature,
      receiver: receiver,
      body: body))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `InitializerDecl`.
  static func parseMemberwiseInitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<InitializerDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(nameTokenWithValue: "memberwise").and(take(.`init`))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Init declarations cannot be static.
    if let modifier = prologue.memberModifiers.first(where: { (m) in m.value == .static }) {
      throw DiagnosedError(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Init declarations require an implicit receiver parameter.
    let receiver = try state.ast.insert(wellFormed: ParameterDecl(
      identifier: SourceRepresentable(value: "self")))

    // Create a new `InitializerDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: InitializerDecl(
      introducer: SourceRepresentable(value: .memberwiseInit, range: parts.0.range),
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      genericClause: nil,
      parameters: [],
      receiver: receiver,
      body: nil))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `NamespaceDecl`.
  static func parseNamespaceDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<NamespaceDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.namespace).and(take(.name))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .namespaceBody) }))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Namespace declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Namespace declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `NamespaceDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: NamespaceDecl(
      introducerRange: parts.0.0.range,
      accessModifier: prologue.accessModifiers.first,
      identifier: SourceRepresentable(token: parts.0.1, in: state.lexer.source),
      members: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `OperatorDecl`.
  static func parseOperatorDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<OperatorDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.operator).and(operatorNotation)
        .and(operator_)
        .and(maybe(take(.colon).and(precedenceGroup).second))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Operator declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Operator declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `OperatorDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: OperatorDecl(
      introducerRange: parts.0.0.0.range,
      accessModifier: prologue.accessModifiers.first,
      notation: parts.0.0.1,
      name: parts.0.1,
      precedenceGroup: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `SubscriptDecl` representing a property declaration.
  static func parsePropertyDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<SubscriptDecl>? {
    guard let (head, signature) = try propertyDeclHead.and(propertyDeclSignature).parse(&state)
    else { return nil }

    guard let impls = try parseSubscriptDeclBody(
      in: &state,
      asNonStaticMember: state.atTypeScope && !prologue.isStatic)
    else {
      throw DiagnosedError(expected("'{'", at: state.currentLocation))
    }

    // Create a new `SubscriptDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: SubscriptDecl(
      introducer: head.introducer,
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      memberModifier: prologue.memberModifiers.first,
      identifier: head.stem,
      genericClause: nil,
      explicitCaptures: [],
      parameters: nil,
      output: signature,
      impls: impls))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `SubscriptDecl`.
  static func parseSubscriptDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<SubscriptDecl>? {
    guard let (head, signature) = try subscriptDeclHead.and(subscriptDeclSignature).parse(&state)
    else { return nil }

    guard let impls = try parseSubscriptDeclBody(
      in: &state,
      asNonStaticMember: state.atTypeScope && !prologue.isStatic)
    else {
      throw DiagnosedError(expected("'{'", at: state.currentLocation))
    }

    // Create a new `SubscriptDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: SubscriptDecl(
      introducer: head.introducer,
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      memberModifier: prologue.memberModifiers.first,
      identifier: head.stem,
      genericClause: head.genericClause,
      explicitCaptures: head.captures,
      parameters: signature.parameters,
      output: signature.output,
      impls: impls))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  static func parseSubscriptDeclBody(
    in state: inout ParserState,
    asNonStaticMember isNonStaticMember: Bool
  ) throws -> [NodeID<SubscriptImplDecl>]? {
    // Push the context.
    state.contexts.append(.subscriptBody)
    defer { state.contexts.removeLast() }

    // Attempt to parse a subscript implementation body and fall back to a bundle.
    let backup = state.backup()
    do {
      if let body = try subscriptImplDeclBody.parse(&state) {
        let impl = try buildSubscriptImplDecl(
          in: &state,
          withIntroducer: SourceRepresentable(value: .let),
          body: body,
          asNonStaticMember: isNonStaticMember)
        return [impl]
      }
    } catch {
      state.restore(from: backup)
    }

    // Parse the left delimiter.
    if state.take(.lBrace) == nil { return nil }

    // Parse the subscript implementations.
    var impls: [NodeID<SubscriptImplDecl>] = []
    var introducers: Set<ImplIntroducer> = []
    var duplicateIntroducer: SourceRepresentable<ImplIntroducer>? = nil

    while true {
      // Exit if we found the right delimiter.
      if state.take(.rBrace) != nil { break }

      // Parse an implementation.
      if let (introducer, body) = try subscriptImplDecl.parse(&state) {
        let impl = try buildSubscriptImplDecl(
          in: &state,
          withIntroducer: introducer,
          body: body,
          asNonStaticMember: isNonStaticMember)
        impls.append(impl)

        if !introducers.insert(introducer.value).inserted { duplicateIntroducer = introducer }
      } else{
        state.diagnostics.append(.diagnose(expected: .rBrace, at: state.currentLocation))
        break
      }
    }

    if let introducer = duplicateIntroducer {
      throw DiagnosedError(.diagnose(duplicateImplementationIntroducer: introducer))
    } else {
      return impls
    }
  }

  private static func buildSubscriptImplDecl(
    in state: inout ParserState,
    withIntroducer introducer: SourceRepresentable<ImplIntroducer>,
    body: SubscriptImplDecl.Body?,
    asNonStaticMember isNonStaticMember: Bool
  ) throws -> NodeID<SubscriptImplDecl> {
    // Non-static member subscript declarations require an implicit receiver parameter.
    let receiver: NodeID<ParameterDecl>?
    if isNonStaticMember {
      receiver = try state.ast.insert(wellFormed: ParameterDecl(
        identifier: SourceRepresentable(value: "self")))
    } else {
      receiver = nil
    }

    // Create a new `SubscriptImplDecl`.
    let decl = try state.ast.insert(wellFormed: SubscriptImplDecl(
      introducer: introducer,
      receiver: receiver,
      body: body))

    if let startIndex = introducer.range?.lowerBound {
      state.ast.ranges[decl] = state.range(from: startIndex)
    } else {
      switch body! {
      case .expr(let id):
        state.ast.ranges[decl] = state.ast.ranges[id]
      case .block(let id):
        state.ast.ranges[decl] = state.ast.ranges[id]
      }
    }

    return decl
  }

  /// Parses an instance of `TraitDecl`.
  static func parseTraitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<TraitDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.trait).and(take(.name))
        .and(maybe(conformanceList))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .traitBody) }))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Trait declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Create a new `TraitDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: TraitDecl(
      accessModifier: prologue.accessModifiers.first,
      identifier: SourceRepresentable(token: parts.0.0.1, in: state.lexer.source),
      refinements: parts.0.1 ?? [],
      members: parts.1))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Parses an instance of `ProductTypeDecl`.
  static func parseProductTypeDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ProductTypeDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.type).and(take(.name))
        .and(maybe(genericClause))
        .and(maybe(conformanceList))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .productBody) }))
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Product type declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Product type declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Retrieve or synthesize the type's memberwise initializer.
    var members = parts.1
    let memberwiseInit = findOrSynthesizeMemberwiseInitDecl(in: &members, updating: &state)

    // Create a new `ProductTypeDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: ProductTypeDecl(
      accessModifier: prologue.accessModifiers.first,
      identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
      genericClause: parts.0.0.1,
      conformances: parts.0.1 ?? [],
      members: members,
      memberwiseInit: memberwiseInit
    ))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  /// Returns the first memberwise initializer declaration in `members` or synthesizes an implicit
  /// one, appends it into `members`, and returns it.
  private static func findOrSynthesizeMemberwiseInitDecl(
    in members: inout [AnyDeclID],
    updating state: inout ParserState
  ) -> NodeID<InitializerDecl> {
    for member in members where member.kind == InitializerDecl.self {
      let m = NodeID<InitializerDecl>(rawValue: member.rawValue)
      if state.ast[m].introducer.value == .memberwiseInit { return m }
    }

    let receiver = try! state.ast.insert(wellFormed: ParameterDecl(
      identifier: SourceRepresentable(value: "self")))
    let m = try! state.ast.insert(wellFormed: InitializerDecl(
      introducer: SourceRepresentable(value: .memberwiseInit),
      attributes: [],
      accessModifier: nil,
      genericClause: nil,
      parameters: [],
      receiver: receiver,
      body: nil))
    members.append(AnyDeclID(m))
    return m
  }

  /// Parses an instance of `TypeAliasDecl`.
  static func parseTypeAliasDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<TypeAliasDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.typealias).and(take(.name))
        .and(maybe(genericClause))
        .and(take(.assign))
        .and(typeExpr)
    )
    guard let parts = try parser.parse(&state) else { return nil }

    // Type alias declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw DiagnosedError(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Type alias declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw DiagnosedError(prologue.memberModifiers.map(
        Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `TypeAliasDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: TypeAliasDecl(
      accessModifier: prologue.accessModifiers.first,
      identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
      genericClause: parts.0.0.1,
      body: .typeExpr(parts.1)))
    state.ast.ranges[decl] = state.range(from: prologue.startIndex)
    return decl
  }

  static let functionDecl = (
    Apply<ParserState, NodeID<FunctionDecl>>({ (state) -> NodeID<FunctionDecl>? in
      // Parse a function or method declaration.
      guard let decl = try parseDeclPrologue(in: &state, then: parseFunctionOrMethodDecl) else {
        return nil
      }

      // Catch illegal method declarations.
      switch decl.kind {
      case FunctionDecl.self:
        return NodeID<FunctionDecl>(rawValue: decl.rawValue)

      case MethodDecl.self:
        let d = NodeID<MethodDecl>(rawValue: decl.rawValue)
        throw DiagnosedError(.error(
          "method bundle declaration is not allowed here",
          range: state.ast[d].introducerRange))

      default:
        unreachable()
      }
    })
  )

  static let functionDeclHead = (
    functionDeclName.and(maybe(genericClause)).and(maybe(captureList))
      .map({ (state, tree) -> FunctionDeclHead in
        FunctionDeclHead(
          name: tree.0.0,
          genericClause: tree.0.1,
          captures: tree.1 ?? [])
      })
  )

  static let functionDeclName = (
    functionDeclIdentifier.or(functionDeclOperator)
  )

  static let functionDeclIdentifier = (
    take(.fun).and(take(.name))
      .map({ (state, tree) -> FunctionDeclName in
        FunctionDeclName(
          introducerRange: tree.0.range,
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source),
          notation: nil
        )
      })
  )

  static let functionDeclOperator = (
    operatorNotation.and(take(.fun)).and(operator_)
      .map({ (state, tree) -> FunctionDeclName in
        FunctionDeclName(
          introducerRange: tree.0.1.range,
          stem: tree.1,
          notation: tree.0.0
        )
      })
  )

  static let functionDeclSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(maybe(take(.arrow).and(typeExpr)))
      .map({ (state, tree) -> FunctionDeclSignature in
        FunctionDeclSignature(
          parameters: tree.0.0.0.1 ?? [],
          receiverEffect: tree.0.1,
          output: tree.1?.1)
      })
  )

  static let functionOrMethodDeclBody = TryCatch(
    trying: methodDeclBody
      .map({ (state, body) -> FunctionOrMethodDeclBody in .method(body) }),
    orCatchingAndApplying: functionDeclBody
      .map({ (state, body) -> FunctionOrMethodDeclBody in .function(body) })
  )

  static let functionDeclBody = inContext(.functionBody, apply: TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> FunctionDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> FunctionDecl.Body in .block(id) })
  ))

  static let methodDeclBody = (
    take(.lBrace).and(methodImplDecl+).and(take(.rBrace))
      .map({ (state, tree) -> [NodeID<MethodImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        var duplicateIntroducer: SourceRepresentable<ImplIntroducer>? = nil
        for implID in tree.0.1 {
          let introducer = state.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted { duplicateIntroducer = introducer }
        }

        if let introducer = duplicateIntroducer {
          throw DiagnosedError(.diagnose(duplicateImplementationIntroducer: introducer))
        } else {
          return tree.0.1
        }
      })
  )

  static let methodImplDecl = (
    methodIntroducer.and(maybe(methodImplBody))
      .map({ (state, tree) -> NodeID<MethodImplDecl> in
        let receiver = try state.ast.insert(wellFormed: ParameterDecl(
          identifier: SourceRepresentable(value: "self")))
        let id = try state.ast.insert(wellFormed: MethodImplDecl(
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

  static let initDeclHead = (
    take(.`init`).and(maybe(genericClause))
      .map({ (state, tree) -> InitDeclHead in
        InitDeclHead(
          introducer: SourceRepresentable(value: .`init`, range: tree.0.range),
          genericClause: tree.1)
      })
  )

  static let initDeclSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .map({ (state, tree) -> [NodeID<ParameterDecl>] in
        tree.0.1 ?? []
      })
  )

  static let initDeclBody = inContext(.functionBody, apply: braceStmt)

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

  static let propertyDeclHead = (
    take(.property).and(take(.name))
      .map({ (state, tree) -> PropertyDeclHead in
        PropertyDeclHead(
          introducer: SourceRepresentable(value: .property, range: tree.0.range),
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source))
      })
  )

  static let propertyDeclSignature = (
    take(.colon).and(typeExpr).second
  )

  static let subscriptDeclHead = (
    take(.subscript).and(maybe(take(.name))).and(maybe(genericClause)).and(maybe(captureList))
      .map({ (state, tree) -> SubscriptDeclHead in
        SubscriptDeclHead(
          introducer: SourceRepresentable(value: .subscript, range: tree.0.0.0.range),
          stem: tree.0.0.1.map({ SourceRepresentable(token: $0, in: state.lexer.source) }),
          genericClause: tree.0.1,
          captures: tree.1 ?? [])
      })
  )

  static let subscriptDeclSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(take(.colon).and(typeExpr))
      .map({ (state, tree) -> SubscriptDeclSignature in
        SubscriptDeclSignature(parameters: tree.0.0.1 ?? [], output: tree.1.1)
      })
  )

  static let subscriptImplDecl = (
    subscriptIntroducer.and(maybe(subscriptImplDeclBody))
  )

  static let subscriptImplDeclBody = TryCatch(
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

  static let bindingDecl = (
    Apply<ParserState, NodeID<BindingDecl>>({ (state) -> NodeID<BindingDecl>? in
      switch state.peek()?.kind {
      case .let, .inout, .var, .sink:
        return try parseDeclPrologue(in: &state, then: parseBindingDecl(withPrologue:in:))
      default:
        return nil
      }
    })
  )

  static let parameterList = (
    parameterDecl.and(zeroOrMany(take(.comma).and(parameterDecl).second))
      .map({ (_, tree) -> [NodeID<ParameterDecl>] in [tree.0] + tree.1 })
  )

  static let parameterDecl = (
    parameterInterface
      .and(maybe(take(.colon).and(parameterTypeExpr)))
      .and(maybe(take(.assign).and(expr)))
      .map({ (state, tree) -> NodeID<ParameterDecl> in
        let id = try state.ast.insert(wellFormed: ParameterDecl(
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

      throw DiagnosedError(expected("parameter name", at: labelCandidate.range.first()))
    })
  )

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

  static let captureList = inContext(.captureList, apply: (
    take(.lBrack)
      .and(bindingDecl.and(zeroOrMany(take(.comma).and(bindingDecl).second)))
      .and(take(.rBrack))
      .map({ (_, tree) -> [NodeID<BindingDecl>] in [tree.0.1.0] + tree.0.1.1 })
  ))

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
        let id = try state.ast.insert(wellFormed: GenericTypeParamDecl(
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
        let id = try state.ast.insert(wellFormed: GenericValueParamDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          annotation: tree.0.1.1,
          defaultValue: tree.1?.1))

        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return .value(id)
      })
  )

  static let conformanceList = (
    take(.colon).and(nameTypeExpr).and(zeroOrMany(take(.comma).and(nameTypeExpr).second))
      .map({ (state, tree) -> [NodeID<NameExpr>] in [tree.0.1] + tree.1 })
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
      state.diagnostics.append(.diagnose(infixOperatorRequiresWhitespacesAt: infixOperator.range))
    }

    guard let rhs = try typeExpr.parse(&state) else {
      throw DiagnosedError(expected("type expression", at: state.currentLocation))
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

    let expr = try state.ast.insert(wellFormed: CastExpr(left: lhs, right: rhs, kind: castKind))
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
      state.diagnostics.append(.diagnose(infixOperatorRequiresWhitespacesAt: infixOperator.range))
    }

    guard let rhs = try prefixExpr.parse(&state) else {
      throw DiagnosedError(expected("expression", at: state.currentLocation))
    }

    let expr = try state.ast.insert(wellFormed: AssignExpr(left: lhs, right: rhs))
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
    var tail: [SequenceExpr.TailElement] = []

    while true {
      let backup = state.backup()

      // Look for the next operator.
      guard let operatorStem = state.takeOperator() else { break }

      if !state.hasLeadingWhitespace {
        // If there isn't any leading whitespace before the next expression but the operator is on
        // a different line, we may be looking at the start of a prefix expression.
        let rangeBefore = state.ast.ranges[lhs]!.upperBound ..< operatorStem.range!.lowerBound
        if state.lexer.source.contents[rangeBefore].contains(where: { $0.isNewline }) {
          state.restore(from: backup)
          break
        }

        // Otherwise, complain about missing whitespaces.
        state.diagnostics.append(.diagnose(
          infixOperatorRequiresWhitespacesAt: operatorStem.range))
      }

      // Commit to parse an operand.
      guard let operand = try prefixExpr.parse(&state) else {
        throw DiagnosedError(expected("expression", at: state.currentLocation))
      }

      let `operator` = try state.ast.insert(wellFormed: NameExpr(
        name: SourceRepresentable(
          value: Name(stem: operatorStem.value, notation: .infix))))
      tail.append(SequenceExpr.TailElement(operator: `operator`, operand: operand))
    }

    // Nothing to transform if the tail is empty.
    if tail.isEmpty { return false }

    let expr = try state.ast.insert(wellFormed: SequenceExpr(head: lhs, tail: tail))
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
        let decl = try state.ast.insert(wellFormed: FunctionDecl(
          introducerRange: tree.0.0.0.0.0.range,
          receiverEffect: tree.0.0.0.1,
          output: tree.0.1,
          body: .block(tree.1),
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = try state.ast.insert(wellFormed: AsyncExpr(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let asyncExprBody = inContext(.functionBody, apply: braceStmt)

  static let asyncExprInline = (
    asyncExprHead.and(expr)
      .map({ (state, tree) -> NodeID<AsyncExpr> in
        let decl = try state.ast.insert(wellFormed: FunctionDecl(
          introducerRange: tree.0.0.0.range,
          receiverEffect: tree.0.1,
          explicitCaptures: tree.0.0.1 ?? [],
          body: .expr(tree.1),
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = try state.ast.insert(wellFormed: AsyncExpr(decl: decl))
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
        let id = try state.ast.insert(wellFormed: AwaitExpr(operand: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)
        return id
      })
  )

  static let prefixExpr = Choose(
    postfixExpr,
    or: prefixOperator.and(withoutLeadingWhitespace(postfixExpr))
      .map({ (state, tree) -> AnyExprID in
        let callee = try state.ast.insert(wellFormed: NameExpr(
          domain: .expr(tree.1),
          name: SourceRepresentable(
            value: Name(stem: tree.0.value, notation: .prefix),
            range: tree.0.range)))
        state.ast.ranges[callee] = tree.0.range!.upperBounded(
          by: state.ast.ranges[tree.1]!.upperBound)

        let call = try state.ast.insert(wellFormed: FunCallExpr(callee: AnyExprID(callee)))
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
          let callee = try state.ast.insert(wellFormed: NameExpr(
            domain: .expr(tree.0),
            name: SourceRepresentable(
              value: Name(stem: oper.value, notation: .postfix), range: oper.range)))
          state.ast.ranges[callee] = state.ast.ranges[tree.0]!.upperBounded(
            by: oper.range!.upperBound)

          let call = try state.ast.insert(wellFormed: FunCallExpr(callee: AnyExprID(callee)))
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
            head = try AnyExprID(state.ast.insert(wellFormed: TupleMemberExpr(
              tuple: head,
              index: index)))
            state.ast.ranges[head] = headRange.upperBounded(by: state.currentIndex)
            continue
          }

          throw DiagnosedError(expected("member name", at: state.currentLocation))
        }

        // Exit if there's a new line before the next token.
        guard let next = state.peek(), !state.hasNewline(before: next) else { break }

        // function-call-expr
        if state.take(.lParen) != nil {
          let arguments = try argumentList.parse(&state) ?? []
          guard state.take(.rParen) != nil else {
            throw DiagnosedError(expected("')'", at: state.currentLocation))
          }

          head = try AnyExprID(state.ast.insert(wellFormed: FunCallExpr(
            callee: head, arguments: arguments)))
          state.ast.ranges[head] = headRange.upperBounded(by: state.currentIndex)
          continue
        }

        // subscript-call-expr
        if state.take(.lBrack) != nil {
          let arguments = try argumentList.parse(&state) ?? []
          guard state.take(.rBrack) != nil else {
            throw DiagnosedError(expected("']'", at: state.currentLocation))
          }

          head = try AnyExprID(state.ast.insert(wellFormed: SubscriptCallExpr(
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
        let id = try state.ast.insert(wellFormed: BooleanLiteralExpr(
          value: state.lexer.source[token.range] == "true"))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let integerLiteral = (
    take(.int)
      .map({ (state, token) -> NodeID<IntegerLiteralExpr> in
        let id = try state.ast.insert(wellFormed: IntegerLiteralExpr(
          value: state.lexer.source[token.range].filter({ $0 != "_" })))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let floatingPointLiteral = (
    take(.float)
      .map({ (state, token) -> NodeID<FloatLiteralExpr> in
        let id = try state.ast.insert(wellFormed: FloatLiteralExpr(
          value: state.lexer.source[token.range].filter({ $0 != "_" })))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let stringLiteral = (
    take(.string)
      .map({ (state, token) -> NodeID<StringLiteralExpr> in
        let id = try state.ast.insert(wellFormed: StringLiteralExpr(
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
        let id = try state.ast.insert(wellFormed: BufferLiteralExpr(elements: tree.0.1 ?? []))
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
        let id = try state.ast.insert(wellFormed: MapLiteralExpr(elements: tree.0.1))
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
        let id = try state.ast.insert(wellFormed: NameExpr(name: tree.0, arguments: tree.1 ?? []))
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
    take(.fun).and(maybe(captureList)).and(functionDeclSignature).and(lambdaBody)
      .map({ (state, tree) -> NodeID<LambdaExpr> in
        let signature = tree.0.1

        let decl = try state.ast.insert(wellFormed: FunctionDecl(
          introducerRange: tree.0.0.0.range,
          receiverEffect: signature.receiverEffect,
          explicitCaptures: tree.0.0.1 ?? [],
          parameters: signature.parameters,
          output: signature.output,
          body: tree.1,
          isInExprContext: true))
        state.ast.ranges[decl] = tree.0.0.0.range.upperBounded(by: state.currentIndex)

        let id = try state.ast.insert(wellFormed: LambdaExpr(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let lambdaBody = inContext(.functionBody, apply: TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> FunctionDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> FunctionDecl.Body in .block(id) })
  ))

  static let matchExpr = (
    take(.match).and(expr).and(take(.lBrace)).and(zeroOrMany(matchCase)).and(take(.rBrace))
      .map({ (state, tree) -> NodeID<MatchExpr> in
        let id = try state.ast.insert(wellFormed: MatchExpr(
          subject: tree.0.0.0.1,
          cases: tree.0.1))
        state.ast.ranges[id] = tree.0.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let matchCase = (
    pattern.and(maybe(take(.where).and(expr))).and(matchCaseBody)
      .map({ (state, tree) -> NodeID<MatchCase> in
        let id = try state.ast.insert(wellFormed: MatchCase(
          pattern: tree.0.0,
          condition: tree.0.1?.1,
          body: tree.1))
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
        let id = try state.ast.insert(wellFormed: CondExpr(
          condition: tree.0.0.1,
          success: tree.0.1,
          failure: tree.1))
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
        let id = try state.ast.insert(wellFormed: BindingDecl(
          pattern: tree.0.0,
          initializer: tree.1))
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
        let id = try state.ast.insert(wellFormed: InoutExpr(
          operatorRange: tree.0.range,
          subject: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let tupleExpr = (
    take(.lParen).and(maybe(tupleExprElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TupleExpr> in
        let id = try state.ast.insert(wellFormed: TupleExpr(elements: tree.0.1 ?? []))
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
        let id = try state.ast.insert(wellFormed: NilExpr())
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
        let id = try state.ast.insert(wellFormed: BindingPattern(
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
          throw DiagnosedError(expected("'let'", at: state.currentLocation))
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
      let id = try state.ast.insert(wellFormed: ExprPattern(expr: exprID))
      state.ast.ranges[id] = state.ast.ranges[exprID]
      return AnyPatternID(id)
    })
  )

  static let namePattern = (
    take(.name)
      .map({ (state, token) -> NodeID<NamePattern> in
        let declID = try state.ast.insert(wellFormed: VarDecl(
          identifier: SourceRepresentable(token: token, in: state.lexer.source)))
        state.ast.ranges[declID] = token.range

        let id = try state.ast.insert(wellFormed: NamePattern(decl: declID))
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let tuplePattern = (
    take(.lParen).and(maybe(tuplePatternElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TuplePattern> in
        let id = try state.ast.insert(wellFormed: TuplePattern(elements: tree.0.1 ?? []))
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
        let id = try state.ast.insert(wellFormed: WildcardPattern())
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
        let id = try state.ast.insert(wellFormed: BraceStmt(stmts: tree.0.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let discardStmt = (
    take(.under).and(take(.assign)).and(expr)
      .map({ (state, tree) -> NodeID<DiscardStmt> in
        let id = try state.ast.insert(wellFormed: DiscardStmt(expr: tree.1))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let doWhileStmt = (
    take(.do).and(loopBody).and(take(.while)).and(expr)
      .map({ (state, tree) -> NodeID<DoWhileStmt> in
        let id = try state.ast.insert(wellFormed: DoWhileStmt(body: tree.0.0.1, condition: tree.1))
        state.ast.ranges[id] = tree.0.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let whileStmt = (
    take(.while).and(conditionalClause).and(loopBody)
      .map({ (state, tree) -> NodeID<WhileStmt> in
        let id = try state.ast.insert(wellFormed: WhileStmt(condition: tree.0.1, body: tree.1))
        state.ast.ranges[id] = tree.0.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let forStmt = (
    take(.for).and(bindingPattern).and(forRange).and(maybe(forFilter)).and(loopBody)
      .map({ (state, tree) -> NodeID<ForStmt> in
        let decl = try state.ast.insert(wellFormed: BindingDecl(
          pattern: tree.0.0.0.1,
          initializer: nil))
        state.ast.ranges[decl] = state.ast.ranges[tree.0.0.0.1]

        let id = try state.ast.insert(wellFormed: ForStmt(
          binding: decl,
          domain: tree.0.0.1,
          filter: tree.0.1,
          body: tree.1))
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
        let id = try state.ast.insert(wellFormed: ReturnStmt(value: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let yieldStmt = (
    take(.yield).and(onSameLine(expr))
      .map({ (state, tree) -> NodeID<YieldStmt> in
        let id = try state.ast.insert(wellFormed: YieldStmt(value: tree.1))
        state.ast.ranges[id] = tree.0.range.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let breakStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<BreakStmt> in
        let id = try state.ast.insert(wellFormed: BreakStmt())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let continueStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<ContinueStmt> in
        let id = try state.ast.insert(wellFormed: ContinueStmt())
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
        let id = try state.ast.insert(wellFormed: DeclStmt(decl: AnyDeclID(decl)))
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
          throw DiagnosedError(.error(
            "conditional binding requires an initializer",
            range: bindingRange.upperBounded(by: bindingRange.lowerBound)))
        }

        let id = try state.ast.insert(wellFormed: CondBindingStmt(
          binding: tree.0.0,
          fallback: tree.1))
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
    Apply(parseDecl)
      .map({ (state, decl) -> NodeID<DeclStmt> in
        let id = try state.ast.insert(wellFormed: DeclStmt(decl: decl))
        state.ast.ranges[id] = state.ast.ranges[decl]
        return id
      })
  )

  static let exprStmt = (
    expr
      .map({ (state, expr) -> NodeID<ExprStmt> in
        let id = try state.ast.insert(wellFormed: ExprStmt(expr: expr))
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

  static let unionTypeExpr1: Recursive<ParserState, AnyTypeExprID> = (
    Recursive(unionTypeExpr.parse(_:))
  )

  static let typeExpr = unionTypeExpr1.or(expr)

  // static let storedProjectionTypeExpr = ?

  static let unionTypeExpr = (
    modifiedTypeExpr.and(zeroOrMany(take(.pipe).and(modifiedTypeExpr).second))
      .map({ (state, tree) -> AnyTypeExprID in
        if tree.1.isEmpty {
          return tree.0
        } else {
          let elements = [tree.0] + tree.1
          let id = try state.ast.insert(wellFormed: UnionTypeExpr(elements: elements))
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
      case .any:
        // existential-type-expr
        _ = state.take()
        guard let traits = try traitComposition.parse(&state) else {
          throw DiagnosedError(expected("trait composition", at: state.currentLocation))
        }
        let clause = try whereClause.parse(&state)

        let id = try state.ast.insert(wellFormed: ExistentialTypeExpr(
          traits: traits,
          whereClause: clause))
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
      .map({ (state, id) -> NodeID<NameExpr> in
        if let converted = NodeID<NameExpr>(id) {
          return converted
        } else {
          throw DiagnosedError(expected("type name", at: state.ast.ranges[id]!.first()))
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
            throw DiagnosedError(expected("type member name", at: state.currentLocation))
          }

          state.ast[member].incorporate(domain: .type(head))
          state.ast.ranges[member] = headRange.upperBounded(by: state.currentIndex)
          head = AnyTypeExprID(member)
          continue
        }

        if state.take(.twoColons) != nil {
          guard let lens = try primaryTypeExpr.parse(&state) else {
            throw DiagnosedError(expected("focus", at: state.currentLocation))
          }

          let id = try state.ast.insert(wellFormed: ConformanceLensTypeExpr(
            subject: head,
            lens: lens))
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
      .map({ (state, tree) -> NodeID<NameExpr> in
        let id = try state.ast.insert(wellFormed: NameExpr(
          name: tree.0,
          arguments: tree.1 ?? []))
        state.ast.ranges[id] = tree.0.range!.upperBounded(by: state.currentIndex)
        return id
      })
  )

  static let tupleTypeExpr = (
    take(.lBrace).and(maybe(tupleTypeExprElementList)).and(take(.rBrace))
      .map({ (state, tree) -> NodeID<TupleTypeExpr> in
        let id = try state.ast.insert(wellFormed: TupleTypeExpr(elements: tree.0.1 ?? []))
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
        let id = try state.ast.insert(wellFormed: LambdaTypeExpr(
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
          let expr = try state.ast.insert(wellFormed: TupleTypeExpr())
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
        let id = try state.ast.insert(wellFormed: WildcardTypeExpr())
        state.ast.ranges[id] = token.range
        return id
      })
  )

  static let receiverEffect = translate([
    .inout: ReceiverEffect.inout,
    .sink : ReceiverEffect.sink,
  ])

  static let parameterTypeExpr = (
    maybe(passingConvention)
      .andCollapsingSoftFailures(typeExpr)
      .map({ (state, tree) -> NodeID<ParameterTypeExpr> in
        let id = try state.ast.insert(wellFormed: ParameterTypeExpr(
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
          throw DiagnosedError(expected("type expression", at: state.currentLocation))
        }
        return SourceRepresentable(
          value: .equality(l: lhs, r: rhs),
          range: state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex))
      }

      // conformance-constraint
      if state.take(.colon) != nil {
        guard let traits = try traitComposition.parse(&state) else {
          throw DiagnosedError(expected("trait composition", at: state.currentLocation))
        }
        return SourceRepresentable(
          value: .conformance(l: lhs, traits: traits),
          range: state.ast.ranges[lhs]!.upperBounded(by: state.currentIndex))
      }

      throw DiagnosedError(expected("constraint operator", at: state.currentLocation))
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
          throw DiagnosedError(expected("operator", at: state.currentLocation))
        }
        guard let oper = state.takeOperator() else {
          throw DiagnosedError(expected("operator", at: state.currentLocation))
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
      .map({ (state, token) -> SourceRepresentable<Name> in
        SourceRepresentable(
          value: Name(stem: String(state.lexer.source[token.range])),
          range: token.range)
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
          throw DiagnosedError(.error("invalid integer literal", range: token.range))
        }
      })
  )

  static let typeAttribute = attribute("@type")

  static let valueAttribute = attribute("@value")

  // MARK: Helpers

  /// Creates a parse error describing failure to parse `subject` at `location`.
  private static func expected(
    _ subject: String,
    at location: SourceLocation,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    .error("expected \(subject)", range: location ..< location, children: children)
  }

  private static func diagnose(
    _ makeDiagnostic: @escaping (inout ParserState) -> Diagnostic
  ) -> (inout ParserState) -> Error {
    { (s) in DiagnosedError(makeDiagnostic(&s)) }
  }

}

/// The attributes and modifiers preceeding a declaration.
struct DeclPrologue {

  /// Indicates whether the prologue is empty.
  let isEmpty: Bool

  /// The index of the first character in the prologue.
  let startIndex: String.Index

  /// The attributes in the prologue.
  let attributes: [SourceRepresentable<Attribute>]

  /// The access modifiers in the prologue.
  let accessModifiers: Set<SourceRepresentable<AccessModifier>>

  /// The member modifiers in the prologue.
  let memberModifiers: Set<SourceRepresentable<MemberModifier>>

  /// Indicates whether the prologue contains the `static` member modifier.
  var isStatic: Bool {
    memberModifiers.contains(where: { (m) in m.value == .static })
  }

}

/// The parsed name of a function declaration.
struct FunctionDeclName {

  /// The source range of the `fun` introducer.
  let introducerRange: SourceRange

  /// The stem of the declared identifier.
  let stem: SourceRepresentable<String>

  /// The notation of the declared function, if any.
  let notation: SourceRepresentable<OperatorNotation>?

}

/// The parsed head of a function declaration.
struct FunctionDeclHead {

  /// The name of the declaration.
  let name: FunctionDeclName

  /// The generic clause of the declaration, if any.
  let genericClause: SourceRepresentable<GenericClause>?

  /// The capture list of the declaration.
  let captures: [NodeID<BindingDecl>]

}

/// The parsed signature of a function declaration.
struct FunctionDeclSignature {

  /// The parameters of the declaration.
  let parameters: [NodeID<ParameterDecl>]

  /// The receiver effect of the declaration, if any.
  let receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The return type annotation of the declaration, if any.
  let output: AnyTypeExprID?

}

/// The body of a function or method declaration.
enum FunctionOrMethodDeclBody {

  case function(FunctionDecl.Body)

  case method([NodeID<MethodImplDecl>])

}

/// The parsed head of an initializer declaration.
struct InitDeclHead {

  /// The introducer of the declaration.
  let introducer: SourceRepresentable<InitializerDecl.Introducer>

  /// The generic clause of the declaration, if any.
  let genericClause: SourceRepresentable<GenericClause>?

}

/// The parsed head of a subscript declaration.
struct SubscriptDeclHead {

  /// The introducer of the declaration.
  let introducer: SourceRepresentable<SubscriptDecl.Introducer>

  /// The stem of the declared identifier, if any.
  let stem: SourceRepresentable<String>?

  /// The generic clause of the declaration, if any.
  let genericClause: SourceRepresentable<GenericClause>?

  /// The capture list of the declaration.
  let captures: [NodeID<BindingDecl>]

}

/// The parsed head of a property declaration.
struct PropertyDeclHead {

  /// The introducer of the declaration.
  let introducer: SourceRepresentable<SubscriptDecl.Introducer>

  /// The stem of the declared identifier.
  let stem: SourceRepresentable<String>

}

/// The parsed signature of a subscript declaration.
struct SubscriptDeclSignature {

  /// The parameters of the declaration.
  let parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the declaration.
  let output: AnyTypeExprID

}

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
fileprivate func take(_ kind: Token.Kind) -> TakeKind {
  TakeKind(kind: kind)
}

/// Creates a combinator that parses name tokens with the specified value.
fileprivate func take(nameTokenWithValue value: String) -> Apply<ParserState, Token> {
  Apply({ (state) in state.take(nameTokenWithValue: value) })
}

/// Creates a combinator that parses attribute tokens with the specified name.
fileprivate func attribute(_ name: String) -> Apply<ParserState, Token> {
  Apply({ (state) in state.take(attribute: name) })
}

/// Creates a combinator that translates token kinds to instances of type.
fileprivate func translate<T>(
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
fileprivate func inContext<Base: Combinator>(
  _ context: ParserState.Context,
  apply base: Base
) -> WrapInContext<Base> {
  WrapInContext(context: context, base: base)
}

/// Creates a combinator that applies `base` only if its input is not preceeded by whitespaces.
fileprivate func withoutLeadingWhitespace<Base: Combinator>(
  _ base: Base
) -> Apply<ParserState, Base.Element>
where Base.Context == ParserState
{
  Apply({ (state) in try state.hasLeadingWhitespace ? nil : base.parse(&state) })
}

/// Creates a combinator that applies `base` only if its input is not preceeded by newlines.
fileprivate func onSameLine<Base: Combinator>(
  _ base: Base
) -> Apply<ParserState, Base.Element>
where Base.Context == ParserState
{
  Apply({ (state) in
    if let t = state.peek() {
      return try state.hasNewline(before: t)
        ? nil
        : base.parse(&state)
    } else {
      // Let `base` handle end of stream.
      return try base.parse(&state)
    }
  })
}

fileprivate extension SourceRepresentable where Part == Identifier {

  init(token: Token, in source: SourceFile) {
    self.init(value: String(source[token.range]), range: token.range)
  }

}

fileprivate extension Diagnostic {

  static func diagnose(expected kind: Token.Kind, at location: SourceLocation) -> Diagnostic {
    .error("expected '\(kind)'", range: location ..< location)
  }

  static func diagnose(infixOperatorRequiresWhitespacesAt range: SourceRange?) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", range: range)
  }

  static func diagnose(unexpectedToken token: Token) -> Diagnostic {
    .error("unexpected token '\(token.kind)'", range: token.range)
  }

  static func diagnose(unterminatedCommentEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated comment", range: endLocation ..< endLocation)
  }

  static func diagnose(unterminatedStringEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated string", range: endLocation ..< endLocation)
  }

}
