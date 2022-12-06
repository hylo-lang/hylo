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
          unterminatedCommentEndingAt: head.origin.last() ?? head.origin.first()))
        break

      case .unterminatedString:
        // Nothing to parse after an unterminated string.
        state.diagnostics.append(.diagnose(
          unterminatedStringEndingAt: head.origin.last() ?? head.origin.first()))
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
    guard let startIndex = state.peek()?.origin.lowerBound else { return nil }
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
        let introducer = state.lexer.source[state.peek()!.origin]
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

      // Exit if we find the right delimiter.
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
        state.diagnostics.append(
          expected(
            "'}'",
            at: state.currentLocation,
            children: [.error("to match this '{'", range: opener.origin)]
        ))
        break
      }

      // Diagnose the error.
      state.diagnostics.append(expected("declaration", at: head.origin.first()))

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
        .and(maybe(take(.assign).and(expr).second))
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
      introducerRange: parts.0.0.0.0.origin,
      identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
      conformances: parts.0.0.1 ?? [],
      whereClause: parts.0.1,
      defaultValue: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
      introducerRange: parts.0.0.0.origin,
      identifier: SourceRepresentable(token: parts.0.0.1, in: state.lexer.source),
      whereClause: parts.0.1,
      defaultValue: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
      initializer: initializer,
      origin: state.range(from: prologue.startIndex)))
    return decl
  }

  /// Parses an instance of `ConformanceDecl`.
  static func parseConformanceDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ConformanceDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.conformance).and(expr)
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
      members: parts.1,
      origin: state.range(from: prologue.startIndex)))
    return decl
  }

  /// Parses an instance of `ExtensionDecl`.
  static func parseExtensionDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ExtensionDecl>? {
    // Parse the parts of the declaration.
    let parser = (
      take(.extension).and(expr)
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
      members: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
        identifier: SourceRepresentable(value: "self"),
        origin: nil))
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
      body: body,
      origin: state.range(from: prologue.startIndex)))
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
      impls: impls,
      origin: state.range(from: prologue.startIndex)))
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
      introducerRange: parts.0.origin,
      identifier: SourceRepresentable(token: parts.1, in: state.lexer.source),
      origin: state.range(from: prologue.startIndex)))
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
      identifier: SourceRepresentable(value: "self"),
      origin: nil))

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
      body: body,
      origin: state.range(from: prologue.startIndex)))
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
      identifier: SourceRepresentable(value: "self"),
      origin: nil))

    // Create a new `InitializerDecl`.
    assert(prologue.accessModifiers.count <= 1)
    let decl = try state.ast.insert(wellFormed: InitializerDecl(
      introducer: SourceRepresentable(value: .memberwiseInit, range: parts.0.origin),
      attributes: prologue.attributes,
      accessModifier: prologue.accessModifiers.first,
      genericClause: nil,
      parameters: [],
      receiver: receiver,
      body: nil,
      origin: state.range(from: prologue.startIndex)))
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
      introducerRange: parts.0.0.origin,
      accessModifier: prologue.accessModifiers.first,
      identifier: SourceRepresentable(token: parts.0.1, in: state.lexer.source),
      members: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
      introducerRange: parts.0.0.0.origin,
      accessModifier: prologue.accessModifiers.first,
      notation: parts.0.0.1,
      name: parts.0.1,
      precedenceGroup: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
      impls: impls,
      origin: state.range(from: prologue.startIndex)))
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
      impls: impls,
      origin: state.range(from: prologue.startIndex)))
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
      // Exit if we find the right delimiter.
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
        identifier: SourceRepresentable(value: "self"),
        origin: nil))
    } else {
      receiver = nil
    }

    let origin: SourceRange
    if let startIndex = introducer.origin?.lowerBound {
      origin = state.range(from: startIndex)
    } else {
      switch body! {
      case .expr(let id):
        origin = state.ast[id].origin!
      case .block(let id):
        origin = state.ast[id].origin!
      }
    }

    // Create a new `SubscriptImplDecl`.
    let decl = try state.ast.insert(wellFormed: SubscriptImplDecl(
      introducer: introducer,
      receiver: receiver,
      body: body,
      origin: origin))

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
      members: parts.1,
      origin: state.range(from: prologue.startIndex)))
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
      memberwiseInit: memberwiseInit,
      origin: state.range(from: prologue.startIndex)))
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
      identifier: SourceRepresentable(value: "self"),
      origin: nil))
    let m = try! state.ast.insert(wellFormed: InitializerDecl(
      introducer: SourceRepresentable(value: .memberwiseInit),
      attributes: [],
      accessModifier: nil,
      genericClause: nil,
      parameters: [],
      receiver: receiver,
      body: nil,
      origin: nil))
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
        .and(expr)
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
      body: .typeExpr(parts.1),
      origin: state.range(from: prologue.startIndex)))
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
          introducerRange: tree.0.origin,
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source),
          notation: nil
        )
      })
  )

  static let functionDeclOperator = (
    operatorNotation.and(take(.fun)).and(operator_)
      .map({ (state, tree) -> FunctionDeclName in
        FunctionDeclName(
          introducerRange: tree.0.1.origin,
          stem: tree.1,
          notation: tree.0.0
        )
      })
  )

  static let functionDeclSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(maybe(receiverEffect))
      .and(maybe(take(.arrow).and(expr)))
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
          identifier: SourceRepresentable(value: "self"),
          origin: nil))
        return try state.ast.insert(wellFormed: MethodImplDecl(
          introducer: tree.0,
          receiver: receiver,
          body: tree.1,
          origin: tree.0.origin!.extended(upTo: state.currentIndex)))
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
          introducer: SourceRepresentable(value: .`init`, range: tree.0.origin),
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
          introducer: SourceRepresentable(value: .property, range: tree.0.origin),
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source))
      })
  )

  static let propertyDeclSignature = (
    take(.colon).and(expr).second
  )

  static let subscriptDeclHead = (
    take(.subscript).and(maybe(take(.name))).and(maybe(genericClause)).and(maybe(captureList))
      .map({ (state, tree) -> SubscriptDeclHead in
        SubscriptDeclHead(
          introducer: SourceRepresentable(value: .subscript, range: tree.0.0.0.origin),
          stem: tree.0.0.1.map({ SourceRepresentable(token: $0, in: state.lexer.source) }),
          genericClause: tree.0.1,
          captures: tree.1 ?? [])
      })
  )

  static let subscriptDeclSignature = (
    take(.lParen).and(maybe(parameterList)).and(take(.rParen))
      .and(take(.colon).and(expr))
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
        try state.ast.insert(wellFormed: ParameterDecl(
          label: tree.0.0.label,
          identifier: tree.0.0.name,
          annotation: tree.0.1?.1,
          defaultValue: tree.1?.1,
          origin: state.range(
            from: tree.0.0.label?.origin!.lowerBound ?? tree.0.0.name.origin!.lowerBound)))
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

      throw DiagnosedError(expected("parameter name", at: labelCandidate.origin.first()))
    })
  )

  static let memberModifier = (
    take(.static)
      .map({ (_, token) -> SourceRepresentable<MemberModifier> in
        SourceRepresentable(value: .static, range: token.origin)
      })
  )

  static let accessModifier = (
    take(.public)
      .map({ (_, token) -> SourceRepresentable<AccessModifier> in
        SourceRepresentable(value: .public, range: token.origin)
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
          range: tree.0.0.0.origin.extended(upTo: state.currentIndex))
      })
  )

  static let genericParameterList = (
    genericParameter.and(zeroOrMany(take(.comma).and(genericParameter).second))
      .map({ (_, tree) -> [NodeID<GenericParameterDecl>] in [tree.0] + tree.1 })
  )

  static let genericParameter = (
    maybe(typeAttribute).andCollapsingSoftFailures(take(.name))
      .and(maybe(take(.colon).and(traitComposition)))
      .and(maybe(take(.assign).and(expr)))
      .map({ (state, tree) -> NodeID<GenericParameterDecl> in
        try state.ast.insert(wellFormed: GenericParameterDecl(
          identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
          conformances: tree.0.1?.1 ?? [],
          defaultValue: tree.1?.1,
          origin: state.range(
            from: tree.0.0.0?.origin.lowerBound ?? tree.0.0.1.origin.lowerBound)))
      })
  )

  static let conformanceList = (
    take(.colon).and(nameTypeExpr).and(zeroOrMany(take(.comma).and(nameTypeExpr).second))
      .map({ (state, tree) -> [NodeID<NameExpr>] in [tree.0.1] + tree.1 })
  )

  // MARK: Expressions

  static let expr = Apply(parseExpr(in:))

  /// Parses an expression in `state`.
  static func parseExpr(in state: inout ParserState) throws -> AnyExprID? {
    // Parse an expression.
    guard var lhs = try parsePrefixExpr(in: &state) else { return nil }

    // Append infix tails.
    while state.hasLeadingWhitespace {
      // type-casting-tail
      if let infixOperator = state.take(.cast) {
        try appendInfixTail(to: &lhs, forCastOperator: infixOperator, in: &state)
        continue
      }

      // infix-operator-tail
      if try !appendInfixTail(to: &lhs, in: &state) { break }
    }

    return lhs
  }

  /// Parses a type expression from the stream, then transforms `lhs` into a `CastExpr`, using
  /// `infixOperator` to determine the cast kind.
  private static func appendInfixTail(
    to lhs: inout AnyExprID,
    forCastOperator infixOperator: Token,
    in state: inout ParserState
  ) throws {
    if !state.hasLeadingWhitespace {
      state.diagnostics.append(.diagnose(infixOperatorRequiresWhitespacesAt: infixOperator.origin))
    }

    guard let rhs = try expr.parse(&state) else {
      throw DiagnosedError(expected("type expression", at: state.currentLocation))
    }

    let castKind: CastExpr.Kind
    switch state.lexer.source[infixOperator.origin] {
    case "as":
      castKind = .up
    case "as!":
      castKind = .down
    case "as!!":
      castKind = .builtinPointerConversion
    default:
      unreachable()
    }

    let expr = try state.ast.insert(
      wellFormed: CastExpr(
        left: lhs,
        right: rhs,
        kind: castKind,
        origin: state.ast[lhs].origin!.extended(upTo: state.currentIndex)))
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
        let rangeBefore = state.ast[lhs].origin!.upperBound ..< operatorStem.origin!.lowerBound
        if state.lexer.source.contents[rangeBefore].contains(where: { $0.isNewline }) {
          state.restore(from: backup)
          break
        }

        // Otherwise, complain about missing whitespaces.
        state.diagnostics.append(.diagnose(
          infixOperatorRequiresWhitespacesAt: operatorStem.origin))
      }

      // If we can't parse an operand, the tail is empty.
      guard let operand = try parsePrefixExpr(in: &state) else {
        state.restore(from: backup)
        return false
      }

      let `operator` = try state.ast.insert(
        wellFormed: NameExpr(
          name: SourceRepresentable(value: Name(stem: operatorStem.value, notation: .infix)),
          origin: operatorStem.origin))
      tail.append(SequenceExpr.TailElement(operator: `operator`, operand: operand))
    }

    // Nothing to transform if the tail is empty.
    if tail.isEmpty { return false }

    let expr = try state.ast.insert(
      wellFormed: SequenceExpr(
        head: lhs,
        tail: tail,
        origin: state.ast[lhs].origin!.extended(upTo: state.currentIndex)))
    lhs = AnyExprID(expr)
    return true
  }

  private static func parsePrefixExpr(in state: inout ParserState) throws -> AnyExprID? {
    // Attempt to parse a prefix operator.
    if state.isNext(satisfying: { $0.isPrefixOperatorHead }) {
      let op = state.takeOperator()!

      // Parse an operand.
      let isSeparated = state.hasLeadingWhitespace
      let operand = try expect("expression", in: &state, parsedWith: parsePostfixExpr(in:))

      // There must be no space before the next expression.
      if isSeparated {
        state.diagnostics.append(.diagnose(separatedPrefixOperatorAt: op.origin))
      }

      let callee = try state.ast.insert(
        wellFormed: NameExpr(
          domain: .expr(operand),
          name: SourceRepresentable(
          value: Name(stem: op.value, notation: .prefix),
          range: op.origin),
          origin: state.range(from: op.origin!.lowerBound)))

      let call = try state.ast.insert(
        wellFormed: FunCallExpr(
          callee: AnyExprID(callee),
          arguments: [],
          origin: state.ast[callee].origin))
      return AnyExprID(call)
    }

    // Attempt to parse an inout expression.
    if let op = state.take(.ampersand) {
      // Parse an operand.
      let isSeparated = state.hasLeadingWhitespace
      let operand = try expect("expression", in: &state, parsedWith: parsePostfixExpr(in:))

      // There must be no space before the next expression.
      if isSeparated {
        state.diagnostics.append(.diagnose(separatedMutationMarkerAt: op.origin))
      }

      let expr = try state.ast.insert(
        wellFormed: InoutExpr(
          operatorRange: op.origin,
          subject: operand,
          origin: state.range(from: op.origin.lowerBound)))
      return AnyExprID(expr)
    }

    // Fall back to a postfix expression.
    return try parsePostfixExpr(in: &state)
  }

  private static func parsePostfixExpr(in state: inout ParserState) throws -> AnyExprID? {
    // Parse an operand.
    guard let operand = try parseCompoundExpr(in: &state) else { return nil }

    // Return the parser expression if it's followed by a whitespace.
    if state.hasLeadingWhitespace { return operand }

    // Parse a postfix operator.
    if state.isNext(satisfying: { $0.isPostfixOperatorHead }) {
      let op = state.takeOperator()!

      let callee = try state.ast.insert(
        wellFormed: NameExpr(
          domain: .expr(operand),
          name: SourceRepresentable(
          value: Name(stem: op.value, notation: .postfix),
          range: op.origin),
          origin: state.range(from: state.ast[operand].origin!.lowerBound)))

      let call = try state.ast.insert(
        wellFormed: FunCallExpr(
          callee: AnyExprID(callee),
          arguments: [],
          origin: state.ast[callee].origin))
      return AnyExprID(call)
    } else {
      return operand
    }
  }

  private static func parseCompoundExpr(in state: inout ParserState) throws -> AnyExprID? {
    // Parse a primary expression.
    guard var head = try parsePrimaryExpr(in: &state) else { return nil }
    let headOrigin = state.ast[head].origin!

    // Parse the components to append to the base expression.
    while true {
      // Handle tuple member and name expressions.
      if state.take(.dot) != nil {
        if let index = state.takeMemberIndex() {
          let expr = try state.ast.insert(
            wellFormed: TupleMemberExpr(
              tuple: head,
              index: index,
              origin: state.range(from: headOrigin.lowerBound)))
          head = AnyExprID(expr)
          continue
        }

        if let component = try parseNameExprComponent(in: &state) {
          let expr = try state.ast.insert(
            wellFormed: NameExpr(
              domain: .expr(head),
              name: component.name,
              arguments: component.arguments,
              origin: state.range(from: headOrigin.lowerBound)))
          head = AnyExprID(expr)
          continue
        }

        throw DiagnosedError(expected("member name", at: state.currentLocation))
      }

      // Handle conformance lens expressions.
      if state.take(.twoColons) != nil {
        // Note: We're using the `parsePrimaryExpr(in:)` parser rather that `parseExpr(in:)` so
        // that `A::P.T` is parsed as `(A::P).T`.
        let lens = try expect("expression", in: &state, parsedWith: parsePrimaryExpr(in:))
        let expr = try state.ast.insert(
          wellFormed: ConformanceLensTypeExpr(
            subject: head,
            lens: lens,
            origin: state.range(from: headOrigin.lowerBound)))
        head = AnyExprID(expr)
        continue
      }

      // Exit if there's a new line before the next token.
      guard let next = state.peek(),
            !state.hasNewline(before: next)
      else { break }

      // Handle function calls.
      if let argumentListOpener = state.take(.lParen) {
        let arguments = try parseCallArgumentListContents(in: &state) ?? []

        if state.take(.rParen) == nil {
          state.diagnostics.append(
            expected(
              "')'",
              at: state.currentLocation,
              children: [.error("to match this '('", range: argumentListOpener.origin)]))
        }

        let expr = try state.ast.insert(
          wellFormed: FunCallExpr(
            callee: head,
            arguments: arguments,
            origin: state.range(from: headOrigin.lowerBound)))
        head = AnyExprID(expr)
        continue
      }

      // Handle function calls.
      if let argumentListOpener = state.take(.lBrack) {
        let arguments = try parseCallArgumentListContents(in: &state) ?? []

        if state.take(.rBrack) == nil {
          state.diagnostics.append(
            expected(
              "']'",
              at: state.currentLocation,
              children: [.error("to match this '['", range: argumentListOpener.origin)]))
        }

        let expr = try state.ast.insert(
          wellFormed: SubscriptCallExpr(
            callee: head,
            arguments: arguments,
            origin: state.range(from: headOrigin.lowerBound)))
        head = AnyExprID(expr)
        continue
      }

      break
    }

    return head
  }

  private static func parseNameExpr(in state: inout ParserState) throws -> NodeID<NameExpr>? {
    guard let expr = try parseCompoundExpr(in: &state) else { return nil }
    if let converted = NodeID<NameExpr>(expr) {
      return converted
    } else {
      throw DiagnosedError(expected("name", at: state.ast[expr].origin!.first()))
    }
  }

  private static func parsePrimaryExpr(in state: inout ParserState) throws -> AnyExprID? {
    guard let head = state.peek() else { return nil }

    switch head.kind {
    case .bool:
      // Boolean literal.
      _ = state.take()
      let expr = try state.ast.insert(
        wellFormed: BooleanLiteralExpr(
          value: state.lexer.source[head.origin] == "true",
          origin: head.origin))
      return AnyExprID(expr)

    case .int:
      // Integer literal.
      _ = state.take()
      let expr = try state.ast.insert(
        wellFormed: IntegerLiteralExpr(
          value: state.lexer.source[head.origin].filter({ $0 != "_" }),
          origin: head.origin))
      return AnyExprID(expr)

    case .float:
      // Floating-point literal.
      _ = state.take()
      let expr = try state.ast.insert(
        wellFormed: FloatLiteralExpr(
          value: state.lexer.source[head.origin].filter({ $0 != "_" }),
          origin: head.origin))
      return AnyExprID(expr)

    case .string:
      // String literal.
      _ = state.take()
      let expr = try state.ast.insert(
        wellFormed: StringLiteralExpr(
          value: String(state.lexer.source[head.origin].dropFirst().dropLast()),
          origin: head.origin))
      return AnyExprID(expr)

    case .nil:
      // Nil literal.
      _ = state.take()
      let expr = try state.ast.insert(wellFormed: NilLiteralExpr(origin: head.origin))
      return AnyExprID(expr)

    case .under:
      // Wildcard expression.
      _ = state.take()
      let expr = try state.ast.insert(wellFormed: WildcardExpr(origin: head.origin))
      return AnyExprID(expr)

    case .any:
      // Existential type expression.
      return try parseExistentialTypeExpr(in: &state).map(AnyExprID.init)

    case .dot:
      // Implicit member reference.
      return try parseImplicitMemberDeclRefExpr(in: &state).map(AnyExprID.init)

    case .fun:
      // Lambda expression.
      return try parseLambdaExpr(in: &state).map(AnyExprID.init)

    case .if:
      // Conditional expression.
      return try parseConditionalExpr(in: &state).map(AnyExprID.init)

    case .match:
      // Match expression.
      return try parseMatchExpr(in: &state).map(AnyExprID.init)

    case .name:
      // Primary declaration reference.
      return try parsePrimaryDeclRefExpr(in: &state).map(AnyExprID.init)

    case .spawn:
      // Spawn expression.
      return try parseSpawnExpr(in: &state).map(AnyExprID.init)

    case .lBrace:
      // Tuple type expression.
      return try parseTupleTypeExpr(in: &state).map(AnyExprID.init)

    case .lParen:
      // A left parenthesis may start a type erased lambda type expression (e.g., `() -> T`), a
      // tuple expression (e.g., `(1, 2)`), or any parenthesized expression.
      return try parseLambdaTypeOrTupleExpr(in: &state)

    case .lBrack:
      // A left bracket may start a lambda type expression (e.g., `[any Copyable] () -> T`) or a
      // compound literal expression (e.g., `[x, y]`).
      return try parseLambdaTypeOrCompoundLiteralExpr(in: &state)

    default:
      return nil
    }
  }

  private static func parseExistentialTypeExpr(
    in state: inout ParserState
  ) throws -> NodeID<ExistentialTypeExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.any) else { return nil }

    // Parse the parts of the expression.
    let traits = try expect("trait composition", in: &state, parsedWith: traitComposition.parse)
    let clause = try whereClause.parse(&state)

    return try state.ast.insert(
      wellFormed: ExistentialTypeExpr(
        traits: traits,
        whereClause: clause,
        origin: introducer.origin.extended(
          toCover: clause?.origin ?? state.ast[traits.last!].origin!)))
  }

  private static func parsePrimaryDeclRefExpr(
    in state: inout ParserState
  ) throws -> NodeID<NameExpr>? {
    // Parse the name component.
    let component = try expect("identifier", in: &state, parsedWith: parseNameExprComponent(in:))

    return try state.ast.insert(
      wellFormed: NameExpr(
        domain: .none,
        name: component.name,
        arguments: component.arguments,
        origin: component.origin))
  }

  private static func parseImplicitMemberDeclRefExpr(
    in state: inout ParserState
  ) throws -> NodeID<NameExpr>? {
    // Parse the leading dot.
    guard let head = state.take(.dot) else { return nil }

    // Parse the name component.
    let component = try expect("identifier", in: &state, parsedWith: parseNameExprComponent(in:))

    return try state.ast.insert(
      wellFormed: NameExpr(
        domain: .implicit,
        name: component.name,
        arguments: component.arguments,
        origin: state.range(from: head.origin.lowerBound)))
  }

  private static func parseNameExprComponent(
    in state: inout ParserState
  ) throws -> NameExprComponent? {
    // Parse the name of the component.
    guard let name = try parseEntityName(in: &state) else { return nil }

    // If the next token is a left angle bracket, without any leading whitespace, parse a static
    // argument list.
    let arguments: [GenericArgument]
    if !state.hasLeadingWhitespace && state.isNext(.lAngle) {
      arguments = try expect(
        "static argument list",
        in: &state,
        parsedWith: parseStaticArgumentList(in:))
    } else {
      arguments = []
    }

    return NameExprComponent(
      origin: name.origin!.extended(upTo: state.currentIndex),
      name: name,
      arguments: arguments)
  }

  private static func parseStaticArgumentList(
    in state: inout ParserState
  ) throws -> [GenericArgument]? {
    // Parse the opening angle.
    guard let opener = state.take(.lAngle) else { return nil }

    // Parse the elements.
    var elements: [GenericArgument] = []
    var hasTrailingSeparator = false

    while true {
      // Parse one element.
      if let element = try staticArgument.parse(&state) {
        elements.append(element)

        // Look for a separator.
        hasTrailingSeparator = false
        if state.take(.comma) != nil {
          hasTrailingSeparator = true
          continue
        }
      }

      // If we get here, we either parsed an element not followed by a separator (1), or we got a
      // soft failure and didn't consume anything from the stream (2). In both case, we should
      // expect the right delimiter.
      if state.take(.rAngle) != nil { break }

      // If we got here by (2) but didn't parse any element yet, diagnose a missing delimiter and
      // exit the loop.
      if elements.isEmpty {
        state.diagnostics.append(expected(">", matching: opener, in: state))
        break
      }

      // If we got here by (1), diagnose a missing separator and try to parse the next element
      // unless we reached EOF. Otherwise, diagnose a missing expression and exit.
      if !hasTrailingSeparator {
        if let head = state.peek() {
          state.diagnostics.append(expected("',' separator", at: head.origin.first()))
          continue
        } else {
          state.diagnostics.append(expected(">", matching: opener, in: state))
          break
        }
      } else {
        state.diagnostics.append(expected("expression", at: state.currentLocation))
        break
      }
    }

    return elements
  }

  private static func parseEntityName(
    in state: inout ParserState
  ) throws -> SourceRepresentable<Name>? {
    try Apply(parseFunctionEntityName).or(Apply(parseOperatorEntityName)).parse(&state)
  }

  private static func parseFunctionEntityName(
    in state: inout ParserState
  ) throws -> SourceRepresentable<Name>? {
    // Parse the stem identifier.
    guard let identifier = state.take(if: { t in t.isOf(kind: [.name, .under]) }) else {
      return nil
    }

    // Parse the labels, if any.
    var labels: [String?] = []
    if !state.hasLeadingWhitespace && (state.peek()?.kind == .lParen) {
      let backup = state.backup()
      _ = state.take()
      var closeParenFound = false
      defer {
        // Backtrack if we didn't find a closing parenthesis or if there are no labels. That will
        // let the argument-list parser pickup after the identifier to either catch a parse error
        // in the former case (no closing parenthesis) or parse an empty argument list in the
        // latter (no labels).
        // Note: `foo()` is *not* a valid name, it's a function call.
        if !closeParenFound || labels.isEmpty { state.restore(from: backup) }
      }

      while !state.hasLeadingWhitespace {
        if state.take(.under) != nil {
          labels.append(nil)
        } else if let label = state.take(if: { $0.isLabel }) {
          labels.append(String(state.lexer.source[label.origin]))
        } else {
          break
        }

        if state.takeWithoutSkippingWhitespace(.colon) == nil {
          break
        }

        if state.takeWithoutSkippingWhitespace(.rParen) != nil {
          closeParenFound = true
          break
        }
      }
    }

    // Parse the method introducer, if any.
    let introducer: SourceRepresentable<ImplIntroducer>?
    if state.peek()?.kind == .dot {
      let backup = state.backup()
      _ = state.take()
      if let i = try methodIntroducer.parse(&state) {
        introducer = i
      } else {
        state.restore(from: backup)
        introducer = nil
      }
    } else {
      introducer = nil
    }

    return SourceRepresentable(
      value: Name(
        stem: String(state.lexer.source[identifier.origin]),
        labels: labels,
        introducer: introducer?.value),
      range: state.range(from: identifier.origin.lowerBound))
  }

  private static func parseOperatorEntityName(
    in state: inout ParserState
  ) throws -> SourceRepresentable<Name>? {
    // Parse the operator notation.
    guard let notation = state.take(if: { t in t.isOf(kind: [.infix, .prefix, .postfix]) }) else {
      return nil
    }

    // The notation must be immediately followed by an operator identifier.
    if state.hasLeadingWhitespace {
      throw DiagnosedError(expected("operator", at: state.currentLocation))
    }
    guard let identifier = state.takeOperator() else {
      throw DiagnosedError(expected("operator", at: state.currentLocation))
    }

    return SourceRepresentable(
      value: Name(stem: identifier.value, notation: OperatorNotation(notation)!),
      range: state.range(from: identifier.origin!.lowerBound))
  }

  private static func parseLambdaExpr(in state: inout ParserState) throws -> NodeID<LambdaExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.fun) else { return nil }

    // Parse the parts of the expression.
    let explicitCaptures = try captureList.parse(&state)
    let signature = try expect("signature", in: &state, parsedWith: functionDeclSignature.parse)
    let body = try expect("function body", in: &state, parsedWith: lambdaBody.parse)

    let decl = try state.ast.insert(
      wellFormed: FunctionDecl(
        introducerRange: introducer.origin,
        receiverEffect: signature.receiverEffect,
        explicitCaptures: explicitCaptures ?? [],
        parameters: signature.parameters,
        output: signature.output,
        body: body,
        isInExprContext: true,
        origin: state.range(from: introducer.origin.lowerBound)))
    return try state.ast.insert(
      wellFormed: LambdaExpr(decl: decl, origin: state.ast[decl].origin))
  }

  private static let lambdaBody = inContext(.functionBody, apply: TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> FunctionDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> FunctionDecl.Body in .block(id) })
  ))

  private static func parseConditionalExpr(in state: inout ParserState) throws -> NodeID<CondExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.if) else { return nil }

    // Parse the parts of the expression.
    let condition = try expect("condition", in: &state, parsedWith: conditionalClause.parse)
    let body = try expect("body", in: &state, parsedWith: conditionalExprBody.parse)

    // Parse the 'else' clause, if any.
    let elseClause: CondExpr.Body?
    if state.take(.else) != nil {
      if let e = try parseConditionalExpr(in: &state) {
        elseClause = .expr(AnyExprID(e))
      } else {
        elseClause = try expect("body", in: &state, parsedWith: conditionalExprBody.parse)
      }
    } else {
      elseClause = nil
    }

    return try state.ast.insert(
      wellFormed: CondExpr(
        condition: condition,
        success: body,
        failure: elseClause,
        origin: state.range(from: introducer.origin.lowerBound)))
  }

  private static let conditionalExprBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> CondExpr.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> CondExpr.Body in .block(id) })
  )

  private static func parseMatchExpr(in state: inout ParserState) throws -> NodeID<MatchExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.match) else { return nil }

    // Parse the parts of the expression.
    let subject = try expect("subject", in: &state, parsedWith: parseExpr(in:))
    let cases = try expect(
      "match body",
      in: &state,
      parsedWith: take(.lBrace).and(zeroOrMany(matchCase)).and(take(.rBrace)).parse)

    return try state.ast.insert(
      wellFormed: MatchExpr(
        subject: subject,
        cases: cases.0.1,
        origin: state.range(from: introducer.origin.lowerBound)))
  }

  static let matchCase = (
    pattern.and(maybe(take(.where).and(expr))).and(matchCaseBody)
      .map({ (state, tree) -> NodeID<MatchCase> in
        try state.ast.insert(wellFormed: MatchCase(
          pattern: tree.0.0,
          condition: tree.0.1?.1,
          body: tree.1,
          origin: state.ast[tree.0.0].origin!.extended(upTo: state.currentIndex)))
      })
  )

  private static let matchCaseBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> MatchCase.Body in .expr(tree.0.1) }),
    orCatchingAndApplying: braceStmt
      .map({ (state, id) -> MatchCase.Body in .block(id) })
  )

  private static func parseSpawnExpr(in state: inout ParserState) throws -> NodeID<SpawnExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.spawn) else { return nil }

    // Parse the parts of the expression.
    let explicitCaptures = try captureList.parse(&state) ?? []
    let effect = try receiverEffect.parse(&state)

    let output: AnyExprID?
    let body: FunctionDecl.Body
    if state.take(.arrow) != nil {
      output = try expect("expression", in: &state, parsedWith: parseExpr(in:))
      body = try expect("function body", in: &state, parsedWith: lambdaBody.parse)
    } else {
      output = nil
      body = try .expr(expect("expression", in: &state, parsedWith: parseExpr(in:)))
    }

    let decl = try state.ast.insert(
      wellFormed: FunctionDecl(
        introducerRange: introducer.origin,
        receiverEffect: effect,
        explicitCaptures: explicitCaptures,
        output: output,
        body: body,
        isInExprContext: true,
        origin: state.range(from: introducer.origin.lowerBound)))
    return try state.ast.insert(
      wellFormed: SpawnExpr(decl: decl, origin: state.ast[decl].origin))
  }

  private static func parseLambdaTypeOrTupleExpr(in state: inout ParserState) throws -> AnyExprID? {
    // Assume we're parsing a lambda type expression until we reach the point where we should
    // consume a right arrow. Commit to that choice only if there's one.
    let backup = state.backup()

    // Parse the opening parenthesis.
    guard let opener = state.take(.lParen) else { return nil }

    // Parse the parameters or backtrack and parse a tuple expression.
    let parameters: [LambdaTypeExpr.Parameter]
    do {
      parameters = try lambdaParameterListContents.parse(&state) ?? []
    } catch {
      state.restore(from: backup)
      return try parseTupleOrParenthesizedExpr(in: &state)
    }

    // Parse the closing parenthesis of the parameter list.
    if state.take(.rParen) == nil {
      state.diagnostics.append(
        expected(
          "')'",
          at: state.currentLocation,
          children: [.error("to match this '('", range: opener.origin)]
      ))
    }

    // Parse the remainder of the type expression.
    let effect = try receiverEffect.parse(&state)

    guard state.take(.arrow) != nil else {
      // If we didn't parse any effect and the parameter list is empty, assume we parsed an empty
      // tuple. Otherwise, backtrack and parse a tuple expression.
      if (effect == nil) && parameters.isEmpty {
        let expr = try state.ast.insert(
          wellFormed: TupleExpr(
            elements: [],
            origin: state.range(from: opener.origin.lowerBound)))
        return AnyExprID(expr)
      }

      state.restore(from: backup)
      return try parseTupleOrParenthesizedExpr(in: &state)
    }

    let output = try expect("type expression", in: &state, parsedWith: parseExpr(in:))

    let expr = try state.ast.insert(
      wellFormed: LambdaTypeExpr(
        receiverEffect: effect,
        environment: nil,
        parameters: parameters,
        output: output,
        origin: state.range(from: opener.origin.lowerBound)))
    return AnyExprID(expr)
  }

  private static func parseLambdaTypeOrCompoundLiteralExpr(
    in state: inout ParserState
  ) throws -> AnyExprID? {
    // Assume we're parsing a lambda type expression until we reach the point where we should
    // consume an effect or a right arrow. Commit to that choice if we successfully parsed a
    // non-empty parameter list at that point.
    let backup = state.backup()

    // Parse the opening bracket.
    guard let opener = state.take(.lBrack) else { return nil }

    // Parse the environment, if any.
    let environement = try parseExpr(in: &state)

    // If we don't find the closing bracket, backtrack and parse a compound literal.
    if state.take(.rBrack) == nil {
      state.restore(from: backup)
      return try parseCompoundLiteral(in: &state)
    }

    // If we don't find the opening parenthesis, assume we've parsed a buffer literal.
    if state.take(.lParen) == nil {
      let expr = try state.ast.insert(
        wellFormed: BufferLiteralExpr(
          elements: environement != nil ? [environement!] : [],
          origin: state.range(from: opener.origin.lowerBound)))
      return AnyExprID(expr)
    }

    // Parse the parameters or backtrack and parse a compound literal.
    let parameters: [LambdaTypeExpr.Parameter]
    do {
      parameters = try lambdaParameterListContents.parse(&state) ?? []
    } catch {
      state.restore(from: backup)
      return try parseCompoundLiteral(in: &state)
    }

    // Parse the closing parenthesis of the parameter list.
    if state.take(.rParen) == nil {
      state.diagnostics.append(
        expected(
          "')'",
          at: state.currentLocation,
          children: [.error("to match this '('", range: opener.origin)]
      ))
    }

    // Parse the remainder of the type expression.
    let effect = try receiverEffect.parse(&state)

    guard state.take(.arrow) != nil else {
      // Backtrack and parse a compound literal.
      state.restore(from: backup)
      return try parseCompoundLiteral(in: &state)
    }

    let output = try expect("type expression", in: &state, parsedWith: parseExpr(in:))

    // Synthesize the environment as an empty tuple if we parsed `[]`.
    let e = try environement ?? AnyExprID(
      state.ast.insert(wellFormed: TupleTypeExpr(elements: [], origin: nil)))

    let expr = try state.ast.insert(
      wellFormed: LambdaTypeExpr(
        receiverEffect: effect,
        environment: e,
        parameters: parameters,
        output: output,
        origin: state.range(from: opener.origin.lowerBound)))
    return AnyExprID(expr)
  }

  private static func parseTupleOrParenthesizedExpr(
    in state: inout ParserState
  ) throws -> AnyExprID? {
    // Parse the opening parenthesis.
    guard let opener = state.take(.lParen) else { return nil }

    // Parse the elements.
    var elements: [TupleExpr.Element] = []
    var hasTrailingSeparator = false

    while true {
      // Parse one element.
      if let element = try parseTupleExprElement(in: &state) {
        elements.append(element)

        // Look for a separator.
        hasTrailingSeparator = false
        if state.take(.comma) != nil {
          hasTrailingSeparator = true
          continue
        }
      }

      // If we get here, we parsed a tuple element not followed by a separator (1), or we got a
      // soft failure and didn't parse anything (2). In both case, we should expect the right
      // delimiter.
      if state.take(.rParen) != nil { break }

      // If we got here by (1) and reached EOF, diagnose a missing right delimiter and exit.
      // Otherwise, diagnose a missing separator and try to parse the next element.
      if !hasTrailingSeparator {
        if let head = state.peek() {
          state.diagnostics.append(expected("',' separator", at: head.origin.first()))
          continue
        } else {
          state.diagnostics.append(
            expected(
              "')'",
              at: state.currentLocation,
              children: [.error("to match this '('", range: opener.origin)]
          ))
          break
        }
      }

      // If we got here by (2), diagnose a missing expression and exit.
      state.diagnostics.append(expected("expression", at: state.currentLocation))
      break
    }

    // If there's only one element without any label and we didn't parse a trailing separator,
    // interpret the element's value as a parenthesized expression.
    if !hasTrailingSeparator && (elements.count == 1) && (elements[0].label == nil) {
      return elements[0].value
    }

    let expr = try state.ast.insert(
      wellFormed: TupleExpr(
        elements: elements,
        origin: state.range(from: opener.origin.lowerBound)))
    return AnyExprID(expr)
  }

  private static func parseTupleExprElement(
    in state: inout ParserState
  ) throws -> TupleExpr.Element? {
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
  }

  private static func parseTupleTypeExpr(in state: inout ParserState) throws -> NodeID<TupleTypeExpr>? {
    // Parse the opening brace.
    guard let opener = state.take(.lBrace) else { return nil }

    // Parse the elements.
    var elements: [TupleTypeExpr.Element] = []
    while true {
      if let element = try parseTupleTypeExprElement(in: &state) {
        // We parsed an element. Look for a separator.
        elements.append(element)
        if state.take(.comma) != nil { continue }
      } else if !elements.isEmpty {
        // We already parsed the beginning of anelements elements list. An elements was expected.
        throw DiagnosedError(expected("expression", at: state.currentLocation))
      }

      // If we get here, then either we parsed a tuple element not followed by a separator (1), or
      // we got a soft failure and didn't parse any element yet (2). In both case, we should expect
      // the right delimiter.
      if state.take(.rBrace) != nil { break }

      // If we got here by (2), diagnose a missing expression and exit. If we got here by (1) and
      // reached EOF, diagnose a missing right delimiter and exit. Otherwise, diagnose a missing
      // separator and try to parse the next element.
      if elements.isEmpty {
        state.diagnostics.append(expected("expression", at: state.currentLocation))
        break
      } else if let head = state.peek() {
        state.diagnostics.append(expected("',' separator", at: head.origin.first()))
        continue
      } else {
        state.diagnostics.append(
          expected(
            "'}'",
            at: state.currentLocation,
            children: [.error("to match this '{'", range: opener.origin)]
        ))
        break
      }
    }

    return try state.ast.insert(
      wellFormed: TupleTypeExpr(
        elements: elements,
        origin: state.range(from: opener.origin.lowerBound)))
  }

  private static func parseTupleTypeExprElement(
    in state: inout ParserState
  ) throws -> TupleTypeExpr.Element? {
    let backup = state.backup()

    // Parse a labeled element.
    if let label = state.take(if: { $0.isLabel }) {
      if state.take(.colon) != nil {
        if let type = try expr.parse(&state) {
          return TupleTypeExpr.Element(
            label: SourceRepresentable(token: label, in: state.lexer.source),
            type: type)
        }
      }
    }

    // Backtrack and parse an unlabeled element.
    state.restore(from: backup)
    if let type = try expr.parse(&state) {
      return TupleTypeExpr.Element(type: type)
    }

    return nil
  }

  private static func parseCompoundLiteral(in state: inout ParserState) throws -> AnyExprID? {
    let backup = state.backup()

    // Attempt to parse a map literal.
    do {
      if let expr = try mapLiteral.parse(&state) { return AnyExprID(expr) }
    } catch {}

    // Backtrack and parse a buffer literal.
    state.restore(from: backup)
    return try bufferLiteral.parse(&state).map(AnyExprID.init)
  }

  private static let bufferLiteral = (
    take(.lBrack).and(maybe(bufferComponentListContents)).and(take(.rBrack))
      .map({ (state, tree) -> NodeID<BufferLiteralExpr> in
        try state.ast.insert(wellFormed: BufferLiteralExpr(
          elements: tree.0.1 ?? [],
          origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  private static let bufferComponentListContents = (
    expr.and(zeroOrMany(take(.comma).and(expr).second))
      .map({ (state, tree) -> [AnyExprID] in [tree.0] + tree.1 })
  )

  private static let mapLiteral = (
    take(.lBrack).and(mapComponentListContents.or(mapComponentEmptyContents)).and(take(.rBrack))
      .map({ (state, tree) -> NodeID<MapLiteralExpr> in
        try state.ast.insert(wellFormed: MapLiteralExpr(
          elements: tree.0.1,
          origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  private static let mapComponentEmptyContents = (
    take(.colon)
      .map({ (_, _) -> [MapLiteralExpr.Element] in [] })
  )

  private static let mapComponentListContents = (
    mapComponent.and(zeroOrMany(take(.comma).and(mapComponent).second))
      .map({ (state, tree) -> [MapLiteralExpr.Element] in [tree.0] + tree.1 })
  )

  private static let mapComponent = (
    expr.and(take(.colon)).and(expr)
      .map({ (_, tree) -> MapLiteralExpr.Element in
        MapLiteralExpr.Element(key: tree.0.0, value: tree.1)
      })
  )

  private static func parseCallArgumentListContents(
    in state: inout ParserState
  ) throws -> [CallArgument]? {
    try callArgument
      .and(zeroOrMany(take(.comma).and(callArgument).second))
      .map({ (_, tree) -> [CallArgument] in [tree.0] + tree.1 })
      .parse(&state)
  }

  private static let callArgument = Apply(parseCallArgument(in:))

  private static func parseCallArgument(in state: inout ParserState) throws -> CallArgument? {
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
  }

  static let conditionalClause = (
    conditionalClauseItem.and(zeroOrMany(take(.comma).and(conditionalClauseItem).second))
      .map({ (_, tree) -> [ConditionItem] in [tree.0] + tree.1 })
  )

  static let conditionalClauseItem = Choose(
    bindingPattern.and(take(.assign)).and(expr)
      .map({ (state, tree) -> ConditionItem in
        let id = try state.ast.insert(wellFormed: BindingDecl(
          pattern: tree.0.0,
          initializer: tree.1,
          origin: state.ast[tree.0.0].origin!.extended(upTo: state.currentIndex)))
        return .decl(id)
      }),
    or: expr
      .map({ (_, id) -> ConditionItem in .expr(id) })
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
      .and(maybe(take(.colon).and(expr)))
      .map({ (state, tree) -> NodeID<BindingPattern> in
        try state.ast.insert(wellFormed: BindingPattern(
          introducer: tree.0.0,
          subpattern: tree.0.1,
          annotation: tree.1?.1,
          origin: tree.0.0.origin!.extended(upTo: state.currentIndex)))
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
        range: head.origin.extended(upTo: state.currentIndex))
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
      let id = try state.ast.insert(wellFormed: ExprPattern(
        expr: exprID,
        origin: state.ast[exprID].origin))
      return AnyPatternID(id)
    })
  )

  static let namePattern = (
    take(.name)
      .map({ (state, token) -> NodeID<NamePattern> in
        let declID = try state.ast.insert(wellFormed: VarDecl(
          identifier: SourceRepresentable(token: token, in: state.lexer.source)))
        return try state.ast.insert(wellFormed: NamePattern(decl: declID, origin: token.origin))
      })
  )

  static let tuplePattern = (
    take(.lParen).and(maybe(tuplePatternElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TuplePattern> in
        try state.ast.insert(wellFormed: TuplePattern(
          elements: tree.0.1 ?? [],
          origin: tree.0.0.origin.extended(upTo: tree.1.origin.upperBound)))
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
        return TuplePattern.Element(label: nil, pattern: value)
      }

      return nil
    })
  )

  static let wildcardPattern = (
    take(.under)
      .map({ (state, token) -> NodeID<WildcardPattern> in
        try state.ast.insert(wellFormed: WildcardPattern(origin: token.origin))
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
        try state.ast.insert(wellFormed: BraceStmt(
          stmts: tree.0.1,
          origin: tree.0.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let discardStmt = (
    take(.under).and(take(.assign)).and(expr)
      .map({ (state, tree) -> NodeID<DiscardStmt> in
        try state.ast.insert(wellFormed: DiscardStmt(
          expr: tree.1,
          origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let doWhileStmt = (
    take(.do).and(loopBody).and(take(.while)).and(expr)
      .map({ (state, tree) -> NodeID<DoWhileStmt> in
        try state.ast.insert(wellFormed: DoWhileStmt(
          body: tree.0.0.1,
          condition: tree.1,
          origin: tree.0.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let whileStmt = (
    take(.while).and(conditionalClause).and(loopBody)
      .map({ (state, tree) -> NodeID<WhileStmt> in
        try state.ast.insert(wellFormed: WhileStmt(
          condition: tree.0.1,
          body: tree.1,
          origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let forStmt = (
    take(.for).and(bindingPattern).and(forRange).and(maybe(forFilter)).and(loopBody)
      .map({ (state, tree) -> NodeID<ForStmt> in
        let decl = try state.ast.insert(wellFormed: BindingDecl(
          pattern: tree.0.0.0.1,
          initializer: nil,
          origin: state.ast[tree.0.0.0.1].origin))
        return try state.ast.insert(wellFormed: ForStmt(
          binding: decl,
          domain: tree.0.0.1,
          filter: tree.0.1,
          body: tree.1,
          origin: tree.0.0.0.0.origin.extended(upTo: state.currentIndex)))
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
        try state.ast.insert(wellFormed: ReturnStmt(
          value: tree.1,
          origin: tree.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let yieldStmt = (
    take(.yield).and(onSameLine(expr))
      .map({ (state, tree) -> NodeID<YieldStmt> in
        try state.ast.insert(wellFormed: YieldStmt(
          value: tree.1,
          origin: tree.0.origin.extended(upTo: state.currentIndex)))
      })
  )

  static let breakStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<BreakStmt> in
        try state.ast.insert(wellFormed: BreakStmt(origin: token.origin))
      })
  )

  static let continueStmt = (
    take(.break)
      .map({ (state, token) -> NodeID<ContinueStmt> in
        try state.ast.insert(wellFormed: ContinueStmt(origin: token.origin))
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
        let id = try state.ast.insert(wellFormed: DeclStmt(
          decl: AnyDeclID(decl),
          origin: state.ast[decl].origin))
        return AnyStmtID(id)
      } else {
        return nil
      }
    })
  )

  static let conditionalBindingStmt = (
    bindingDecl.and(take(.else)).and(conditionalBindingFallback)
      .map({ (state, tree) -> NodeID<CondBindingStmt> in
        let bindingRange = state.ast[tree.0.0].origin!

        if state.ast[tree.0.0].initializer == nil {
          throw DiagnosedError(.error(
            "conditional binding requires an initializer",
            range: bindingRange.extended(upTo: bindingRange.lowerBound)))
        }

        return try state.ast.insert(wellFormed: CondBindingStmt(
          binding: tree.0.0,
          fallback: tree.1,
          origin: bindingRange.extended(upTo: state.currentIndex)))
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
        try state.ast.insert(wellFormed: DeclStmt(decl: decl, origin: state.ast[decl].origin))
      })
  )

  static let exprStmt = Apply(parseExprOrAssignStmt(in:))

  private static func parseExprOrAssignStmt(in state: inout ParserState) throws -> AnyStmtID? {
    guard let lhs = try expr.parse(&state) else { return nil }

    // Return an expression statement unless the next token is `=`.
    guard let assign = state.take(.assign) else {
      let stmt = try state.ast.insert(
        wellFormed: ExprStmt(expr: lhs, origin: state.ast[lhs].origin))
      return AnyStmtID(stmt)
    }

    if !state.hasLeadingAndTrailingWhitespaces(assign) {
      state.diagnostics.append(.diagnose(assignOperatorRequiresWhitespaces: assign))
    }

    guard let rhs = try parsePrefixExpr(in: &state) else {
      throw DiagnosedError(expected("expression", at: state.currentLocation))
    }

    let stmt = try state.ast.insert(
      wellFormed: AssignStmt(
        left: lhs,
        right: rhs,
        origin: state.range(from: state.ast[lhs].origin!.lowerBound)))
    return AnyStmtID(stmt)
  }

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

  private static let nameTypeExpr = Apply(parseNameExpr(in:))

  private static let lambdaParameterListContents = (
    lambdaParameter.and(zeroOrMany(take(.comma).and(lambdaParameter).second))
      .map({ (_, tree) -> [LambdaTypeExpr.Parameter] in [tree.0] + tree.1 })
  )

  private static let lambdaParameter = Apply(parseLambdaParameter(in:))

  private static func parseLambdaParameter(
    in state: inout ParserState
  ) throws -> LambdaTypeExpr.Parameter? {
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
  }

  static let receiverEffect = translate([
    .inout: ReceiverEffect.inout,
    .sink : ReceiverEffect.sink,
  ])

  static let parameterTypeExpr = (
    maybe(passingConvention)
      .andCollapsingSoftFailures(expr)
      .map({ (state, tree) -> NodeID<ParameterTypeExpr> in
        try state.ast.insert(wellFormed: ParameterTypeExpr(
          convention: tree.0 ?? SourceRepresentable(value: .let),
          bareType: tree.1,
          origin: state.range(
            from: tree.0?.origin!.lowerBound ?? state.ast[tree.1].origin!.lowerBound)))
      })
  )

  static let passingConvention = translate([
    .let    : PassingConvention.let,
    .inout  : PassingConvention.inout,
    .set    : PassingConvention.set,
    .sink   : PassingConvention.sink,
    .yielded: PassingConvention.yielded,
  ])

  static let staticArgumentList = Apply(parseStaticArgumentList(in:))

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
    maybe(typeAttribute).and(expr)
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
          range: tree.0.origin.extended(upTo: state.currentIndex))
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
        guard let rhs = try expr.parse(&state) else {
          throw DiagnosedError(expected("type expression", at: state.currentLocation))
        }
        return SourceRepresentable(
          value: .equality(l: lhs, r: rhs),
          range: state.ast[lhs].origin!.extended(upTo: state.currentIndex))
      }

      // conformance-constraint
      if state.take(.colon) != nil {
        guard let traits = try traitComposition.parse(&state) else {
          throw DiagnosedError(expected("trait composition", at: state.currentLocation))
        }
        return SourceRepresentable(
          value: .conformance(l: lhs, traits: traits),
          range: state.ast[lhs].origin!.extended(upTo: state.currentIndex))
      }

      throw DiagnosedError(expected("constraint operator", at: state.currentLocation))
    })
  )

  static let valueConstraint = (
    valueAttribute.and(expr)
      .map({ (state, tree) -> SourceRepresentable<WhereClause.ConstraintExpr> in
        SourceRepresentable(
          value: .value(tree.1),
          range: tree.0.origin.extended(upTo: state.currentIndex))
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
              labels.append(String(state.lexer.source[label.origin]))
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
          value: Name(stem: String(state.lexer.source[head.origin]), labels: labels),
          range: head.origin)


      case .infix, .prefix, .postfix:
        // operator-entity-identifier
        let head = state.take()!

        if state.hasLeadingWhitespace {
          throw DiagnosedError(expected("operator", at: state.currentLocation))
        }
        guard let oper = state.takeOperator() else {
          throw DiagnosedError(expected("operator", at: state.currentLocation))
        }

        let stem = String(state.lexer.source[oper.origin!])
        let range = head.origin.extended(upTo: oper.origin!.upperBound)

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
          value: Name(stem: String(state.lexer.source[token.origin])),
          range: token.origin)
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
          range: tree.0.origin.extended(upTo: state.currentIndex))
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
        let value = String(state.lexer.source[token.origin].dropFirst().dropLast())
        return .string(SourceRepresentable(value: value, range: token.origin))
      })
  )

  static let integerAttributeArgument = (
    take(.int)
      .map({ (state, token) -> Attribute.Argument in
        if let value = Int(state.lexer.source[token.origin]) {
          return .integer(SourceRepresentable(value: value, range: token.origin))
        } else {
          throw DiagnosedError(.error("invalid integer literal", range: token.origin))
        }
      })
  )

  static let typeAttribute = attribute("@type")

  static let valueAttribute = attribute("@value")

  // MARK: Helpers

  /// Applies `parse` on `state`, propagating thrown errors, and returns `parse`'s result if it
  /// isn't `nil`. Otherwise, throw an error diagnosing that we expected `expectedConstruct`.
  private static func expect<T>(
    _ expectedConstruct: String,
    in state: inout ParserState,
    parsedWith parse: (inout ParserState) throws -> T?
  ) throws -> T {
    if let element = try parse(&state) {
      return element
    } else {
      throw DiagnosedError(expected(expectedConstruct, at: state.currentLocation))
    }
  }

  /// Creates a parse error describing failure to parse `subject` at `location`.
  private static func expected(
    _ subject: String,
    at location: SourceLocation,
    children: [Diagnostic] = []
  ) -> Diagnostic {
    .error("expected \(subject)", range: location ..< location, children: children)
  }

  private static func expected(
    _ closerDescription: String,
    matching opener: Token,
    in state: ParserState
  ) -> Diagnostic {
    expected(
      "'\(closerDescription)'",
      at: state.currentLocation,
      children: [
        .error(
          "to match this '\(state.lexer.source[opener.origin])'",
          range: opener.origin)
      ]
    )
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

/// The parsed component of a name expression.
struct NameExprComponent {

  /// The source range from which `self` was parsed.
  let origin: SourceRange

  /// The name of the component.
  let name: SourceRepresentable<Name>

  /// The static arguments of the component, if any.
  let arguments: [GenericArgument]

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
      if let value = T(rawValue: String(state.lexer.source[next.origin])) {
        _ = state.take()
        return SourceRepresentable(value: value, range: next.origin)
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
      return SourceRepresentable(value: translation, range: head.origin)
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

fileprivate extension OperatorNotation {

  /// Creates an instance from `token`'s kind, or returns `nil` if `token` does not represent an
  /// operator notation.
  init?(_ token: Token) {
    switch token.kind {
    case .infix:
      self = .infix
    case .prefix:
      self = .prefix
    case .postfix:
      self = .postfix
    default:
      return nil
    }
  }

}

fileprivate extension SourceRepresentable where Part == Identifier {

  init(token: Token, in source: SourceFile) {
    self.init(value: String(source[token.origin]), range: token.origin)
  }

}

fileprivate extension Diagnostic {

  static func diagnose(assignOperatorRequiresWhitespaces token: Token) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", range: token.origin)
  }

  static func diagnose(expected kind: Token.Kind, at location: SourceLocation) -> Diagnostic {
    .error("expected '\(kind)'", range: location ..< location)
  }

  static func diagnose(infixOperatorRequiresWhitespacesAt range: SourceRange?) -> Diagnostic {
    .error("infix operator requires whitespaces on both sides", range: range)
  }

  static func diagnose(separatedMutationMarkerAt range: SourceRange?) -> Diagnostic {
    .error("in-place mutation marker cannot be separated from its expression", range: range)
  }

  static func diagnose(separatedPrefixOperatorAt range: SourceRange?) -> Diagnostic {
    .error("prefix operator cannot be separated from its operand", range: range)
  }

  static func diagnose(unexpectedToken token: Token) -> Diagnostic {
    .error("unexpected token '\(token.kind)'", range: token.origin)
  }

  static func diagnose(unterminatedCommentEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated comment", range: endLocation ..< endLocation)
  }

  static func diagnose(unterminatedStringEndingAt endLocation: SourceLocation) -> Diagnostic {
    .error("unterminated string", range: endLocation ..< endLocation)
  }

}
