import Core
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

  /// Parses the declarations of `input` and inserts them into `ast[module]`, updating diagnostics
  /// as appropriate, returning the identity of parsed source file in `ast`.
  ///
  /// - Throws: Diagnostics if syntax errors were encountered.
  public static func parse(
    _ input: SourceFile,
    into module: NodeID<ModuleDecl>,
    in ast: inout AST,
    diagnostics: inout Diagnostics
  ) throws -> NodeID<TopLevelDeclSet> {

    // Temporarily stash the ast and diagnostics in the parser state, avoiding CoW costs
    var state = ParserState(ast: ast, lexer: Lexer(tokenizing: input), diagnostics: diagnostics)
    defer { diagnostics = state.diagnostics }
    diagnostics = Diagnostics()

    let translation = Self.parseSourceFile(in: &state)
    try state.diagnostics.throwOnError()

    // Make sure the entire input was consumed.
    assert(state.peek() == nil, "expected EOF")

    state.ast[module].addSourceFile(translation)
    ast = state.ast

    return translation
  }

  /// Parses an instance of `TopLevelDeclSet`.
  static func parseSourceFile(in state: inout ParserState) -> NodeID<TopLevelDeclSet> {
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
      } catch let error as Diagnostics {
        state.diagnostics.report(error.log)
        continue
      } catch let error {
        state.diagnostics.report(Diagnostic(level: .error, message: error.localizedDescription))
        continue
      }

      // Attempt to recover.
      _ = state.take()
      switch head.kind {
      case .unterminatedBlockComment:
        // Nothing to parse after an unterminated block comment.
        state.diagnostics.report(
          .diagnose(
            unterminatedCommentEndingAt: head.origin.last() ?? head.origin.first()))
        break

      case .unterminatedString:
        // Nothing to parse after an unterminated string.
        state.diagnostics.report(
          .diagnose(
            unterminatedStringEndingAt: head.origin.last() ?? head.origin.first()))
        break

      default:
        state.diagnostics.report(.diagnose(unexpectedToken: head))

        // Attempt to recover at the next new line.
        while let next = state.peek() {
          if state.hasNewline(before: next) { break }
          _ = state.take()
        }
      }
    }

    return state.insert(TopLevelDeclSet(decls: members))
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
    while let a = try parseDeclAttribute(in: &state) {
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
          state.diagnostics.report(
            .diagnose(
              memberModifier: member,
              appearsBeforeAccessModifier: access))
        }

        // Catch duplicate access modifiers.
        else if !accessModifiers.insert(access).inserted {
          state.diagnostics.report(.diagnose(duplicateAccessModifier: access))
        }

        // Look for the next modifier.
        continue
      }

      if let member = try Parser.memberModifier.parse(&state) {
        isPrologueEmpty = false

        // Catch member modifiers declared at non-type scope.
        if !state.isAtTypeScope {
          state.diagnostics.report(.diagnose(unexpectedMemberModifier: member))
        }

        // Catch duplicate member modifiers.
        else if !memberModifiers.insert(member).inserted {
          state.diagnostics.report(.diagnose(duplicateMemberModifier: member))
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
        if state.isAtTraitScope {
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
        if introducer == "value" && state.isAtTypeScope {
          // Note: associated values are parsed at any type scope to produce better diagnostics
          // when they are not at trait scope.
          return try parseAssociatedValueDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        }
        if introducer == "memberwise" && state.isAtTypeScope {
          return try parseMemberwiseInitDecl(withPrologue: prologue, in: &state)
            .map(AnyDeclID.init)
        }

      default:
        break
      }

      if prologue.isEmpty {
        return nil
      } else {
        throw Diagnostics(.diagnose(expected: "declaration", at: state.currentLocation))
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
    let opener = try state.expect("'{'", using: { $0.take(.lBrace) })

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
      } catch let error as Diagnostics {
        state.diagnostics.report(error.log)
        continue
      }

      // Nothing was consumed. Skip the next token or, if we reached EOF, diagnose a missing right
      // delimiter and exit.
      guard let head = state.take() else {
        state.diagnostics.report(
          .diagnose(
            expected: "'}'",
            at: state.currentLocation,
            children: [.error("to match this '{'", range: opener.origin)]
          ))
        break
      }

      // Diagnose the error.
      state.diagnostics.report(.diagnose(expected: "declaration", at: head.origin.first()))

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
    let parser =
      (take(.type).and(take(.name))
        .and(maybe(conformanceList))
        .and(maybe(whereClause))
        .and(maybe(take(.assign).and(expr).second)))
    guard let parts = try parser.parse(&state) else { return nil }

    // Associated type declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Associated type declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw Diagnostics(
        prologue.accessModifiers.map(
          Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(
          Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `AssociatedTypeDecl`.
    return state.insert(
      AssociatedTypeDecl(
        introducerRange: parts.0.0.0.0.origin,
        identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
        conformances: parts.0.0.1 ?? [],
        whereClause: parts.0.1,
        defaultValue: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `AssociatedValueDecl`.
  static func parseAssociatedValueDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<AssociatedValueDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(nameTokenWithValue: "value").and(take(.name))
        .and(maybe(whereClause))
        .and(maybe(take(.assign).and(expr).second)))
    guard let parts = try parser.parse(&state) else { return nil }

    // Associated value declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Associated value declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw Diagnostics(
        prologue.accessModifiers.map(
          Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(
          Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `AssociatedValueDecl`.
    return state.insert(
      AssociatedValueDecl(
        introducerRange: parts.0.0.0.origin,
        identifier: SourceRepresentable(token: parts.0.0.1, in: state.lexer.source),
        whereClause: parts.0.1,
        defaultValue: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `BindingDecl`.
  static func parseBindingDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<BindingDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (bindingPattern
        .and(maybe(take(.assign).and(expr).second)))
    guard let (pattern, initializer) = try parser.parse(&state) else { return nil }

    // Create a new `BindingDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    return state.insert(
      BindingDecl(
        attributes: prologue.attributes,
        accessModifier: prologue.accessModifiers.first,
        memberModifier: prologue.memberModifiers.first,
        pattern: pattern,
        initializer: initializer,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `ConformanceDecl`.
  static func parseConformanceDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ConformanceDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(.conformance).and(expr)
        .and(conformanceList)
        .and(maybe(whereClause))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .extensionBody) })))
    guard let parts = try parser.parse(&state) else { return nil }

    // Conformance declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Conformance declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(
          Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ConformanceDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      ConformanceDecl(
        accessModifier: prologue.accessModifiers.first,
        subject: parts.0.0.0.1,
        conformances: parts.0.0.1,
        whereClause: parts.0.1,
        members: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `ExtensionDecl`.
  static func parseExtensionDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ExtensionDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(.extension).and(expr)
        .and(maybe(whereClause))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .extensionBody) })))
    guard let parts = try parser.parse(&state) else { return nil }

    // Extension declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Extension declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw Diagnostics(
        prologue.accessModifiers.map(
          Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(
          Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ExtensionDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      ExtensionDecl(
        accessModifier: prologue.accessModifiers.first,
        subject: parts.0.0.1,
        whereClause: parts.0.1,
        members: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `FunctionDecl` or `MethodDecl`.
  static func parseFunctionOrMethodDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> AnyDeclID? {
    // Parse the signature of the function or method.
    guard let head = try parseFunctionDeclHead(in: &state) else { return nil }
    let signature = try state.expect("function signature", using: parseFunctionDeclSignature(in:))

    // Parse the body of the function or method.
    let body = try functionOrMethodDeclBody.parse(&state)

    // Apply the continuation corresponding to the body we just parsed.
    switch body {
    case .method(let impls):
      return try AnyDeclID(
        buildMethodDecl(
          prologue: prologue,
          head: head,
          signature: signature,
          impls: impls,
          in: &state))

    case .function(let body):
      return try AnyDeclID(
        buildFunctionDecl(
          prologue: prologue,
          head: head,
          signature: signature,
          body: body,
          in: &state))

    case nil:
      return try AnyDeclID(
        buildFunctionDecl(
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
    if state.isAtTypeScope && !prologue.isStatic {
      receiver = state.insert(
        synthesized: ParameterDecl(
          identifier: SourceRepresentable(value: "self"),
          origin: nil))
    } else {
      receiver = nil
    }

    // Create a new `FunctionDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    return state.insert(
      FunctionDecl(
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
      throw Diagnostics(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Method declarations cannot have a receiver effect.
    if let effect = signature.receiverEffect {
      throw Diagnostics(.diagnose(unexpectedEffect: effect))
    }

    // Method declarations cannot have captures.
    if let capture = head.captures.first {
      throw Diagnostics(.diagnose(unexpectedCapture: state.ast[state.ast[capture].pattern]))
    }

    // Create a new `MethodDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      MethodDecl(
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
  }

  /// Parses an instance of `ImportDecl`.
  static func parseImportDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ImportDecl>? {
    // Parse the parts of the declaration.
    let parser = (take(.import).and(take(.name)))
    guard let parts = try parser.parse(&state) else { return nil }

    // Import declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Import declarations shall not have modifiers.
    if !prologue.accessModifiers.isEmpty {
      throw Diagnostics(
        prologue.accessModifiers.map(
          Diagnostic.diagnose(unexpectedAccessModifier:)))
    }
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(
          Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `ImportDecl`.
    return state.insert(
      ImportDecl(
        introducerRange: parts.0.origin,
        identifier: SourceRepresentable(token: parts.1, in: state.lexer.source),
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `InitializerDecl`.
  static func parseInitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<InitializerDecl>? {
    // Parse the signature of the initializer.
    guard let introducer = state.take(.`init`) else { return nil }
    let genericClause = try genericClause.parse(&state)
    let parameters = try state.expect("function signature", using: parseParameterList(in:))

    // Parse the body of the initializer.
    let body = try initDeclBody.parse(&state)

    // Init declarations cannot be static.
    if let modifier = prologue.memberModifiers.first(where: { (m) in m.value == .static }) {
      throw Diagnostics(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Init declarations require an implicit receiver parameter.
    let receiver = state.insert(
      synthesized: ParameterDecl(
        identifier: SourceRepresentable(value: "self"),
        origin: nil))

    // Create a new `InitializerDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.isEmpty)
    return state.insert(
      InitializerDecl(
        introducer: SourceRepresentable(value: .`init`, range: introducer.origin),
        attributes: prologue.attributes,
        accessModifier: prologue.accessModifiers.first,
        genericClause: genericClause,
        parameters: parameters,
        receiver: receiver,
        body: body,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `InitializerDecl`.
  static func parseMemberwiseInitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<InitializerDecl>? {
    // Parse the parts of the declaration.
    let parser = (take(nameTokenWithValue: "memberwise").and(take(.`init`)))
    guard let parts = try parser.parse(&state) else { return nil }

    // Init declarations cannot be static.
    if let modifier = prologue.memberModifiers.first(where: { (m) in m.value == .static }) {
      throw Diagnostics(.diagnose(unexpectedMemberModifier: modifier))
    }

    // Init declarations require an implicit receiver parameter.
    let receiver = state.insert(
      synthesized: ParameterDecl(
        identifier: SourceRepresentable(value: "self"),
        origin: nil))

    // Create a new `InitializerDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      InitializerDecl(
        introducer: SourceRepresentable(value: .memberwiseInit, range: parts.0.origin),
        attributes: prologue.attributes,
        accessModifier: prologue.accessModifiers.first,
        genericClause: nil,
        parameters: [],
        receiver: receiver,
        body: nil,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `NamespaceDecl`.
  static func parseNamespaceDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<NamespaceDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(.namespace).and(take(.name))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .namespaceBody) })))
    guard let parts = try parser.parse(&state) else { return nil }

    // Namespace declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Namespace declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `NamespaceDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      NamespaceDecl(
        introducerRange: parts.0.0.origin,
        accessModifier: prologue.accessModifiers.first,
        identifier: SourceRepresentable(token: parts.0.1, in: state.lexer.source),
        members: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `OperatorDecl`.
  static func parseOperatorDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<OperatorDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(.operator).and(operatorNotation)
        .and(operatorIdentifier)
        .and(maybe(take(.colon).and(precedenceGroup).second)))
    guard let parts = try parser.parse(&state) else { return nil }

    // Operator declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Operator declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `OperatorDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      OperatorDecl(
        introducerRange: parts.0.0.0.origin,
        accessModifier: prologue.accessModifiers.first,
        notation: parts.0.0.1,
        name: parts.0.1,
        precedenceGroup: parts.1,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `SubscriptDecl` representing a property declaration.
  static func parsePropertyDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<SubscriptDecl>? {
    guard let (head, signature) = try propertyDeclHead.and(propertyDeclSignature).parse(&state)
    else { return nil }

    let isNonStatic = state.isAtTypeScope && !prologue.isStatic
    let impls = try state.expect(
      "'{'",
      using: { (s) in try parseSubscriptDeclBody(in: &s, asNonStaticMember: isNonStatic) })

    // Create a new `SubscriptDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    return state.insert(
      SubscriptDecl(
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
  }

  /// Parses an instance of `SubscriptDecl`.
  static func parseSubscriptDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<SubscriptDecl>? {
    // Parse the signature of the subscript.
    guard let head = try subscriptDeclHead.parse(&state) else { return nil }
    let signature = try state.expect(
      "subscript signature",
      using: parseSubscriptDeclSignature(in:))

    let isNonStatic = state.isAtTypeScope && !prologue.isStatic
    let impls = try state.expect(
      "'{'",
      using: { (s) in try parseSubscriptDeclBody(in: &s, asNonStaticMember: isNonStatic) })

    // Create a new `SubscriptDecl`.
    assert(prologue.accessModifiers.count <= 1)
    assert(prologue.memberModifiers.count <= 1)
    return state.insert(
      SubscriptDecl(
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
      } else {
        state.diagnostics.report(.diagnose(expected: .rBrace, at: state.currentLocation))
        break
      }
    }

    if let introducer = duplicateIntroducer {
      throw Diagnostics(.diagnose(duplicateImplementationIntroducer: introducer))
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
      receiver = state.insert(
        synthesized: ParameterDecl(
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
    return state.insert(
      SubscriptImplDecl(
        introducer: introducer,
        receiver: receiver,
        body: body,
        origin: origin))
  }

  /// Parses an instance of `TraitDecl`.
  static func parseTraitDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<TraitDecl>? {
    if state.take(.trait) == nil { return nil }

    // Parse the parts of the declaration.
    let name = try state.expect("identifier", using: { $0.take(.name) })
    let refinements = try conformanceList.parse(&state) ?? []
    var members = try state.expect(
      "trait body",
      using: { (s) in
        try parseTypeDeclBody(in: &s, wrappedIn: .traitBody)
      })

    // Trait declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Synthesize the `Self` parameter of the trait.
    let selfParameterDecl = state.insert(
      GenericParameterDecl(
        identifier: SourceRepresentable(value: "Self"),
        origin: nil))
    members.append(AnyDeclID(selfParameterDecl))

    // Create a new `TraitDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      TraitDecl(
        accessModifier: prologue.accessModifiers.first,
        identifier: SourceRepresentable(token: name, in: state.lexer.source),
        refinements: refinements,
        members: members,
        selfParameterDecl: selfParameterDecl,
        origin: state.range(from: prologue.startIndex)))
  }

  /// Parses an instance of `ProductTypeDecl`.
  static func parseProductTypeDecl(
    withPrologue prologue: DeclPrologue,
    in state: inout ParserState
  ) throws -> NodeID<ProductTypeDecl>? {
    // Parse the parts of the declaration.
    let parser =
      (take(.type).and(take(.name))
        .and(maybe(genericClause))
        .and(maybe(conformanceList))
        .and(Apply({ (s) in try parseTypeDeclBody(in: &s, wrappedIn: .productBody) })))
    guard let parts = try parser.parse(&state) else { return nil }

    // Product type declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Product type declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Retrieve or synthesize the type's memberwise initializer.
    var members = parts.1
    let memberwiseInit = findOrSynthesizeMemberwiseInitDecl(in: &members, updating: &state)

    // Create a new `ProductTypeDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      ProductTypeDecl(
        accessModifier: prologue.accessModifiers.first,
        identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
        genericClause: parts.0.0.1,
        conformances: parts.0.1 ?? [],
        members: members,
        memberwiseInit: memberwiseInit,
        origin: state.range(from: prologue.startIndex)))
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

    let receiver = state.insert(
      synthesized: ParameterDecl(
        identifier: SourceRepresentable(value: "self"),
        origin: nil))
    let m = state.insert(
      synthesized: InitializerDecl(
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
    let parser =
      (take(.typealias).and(take(.name))
        .and(maybe(genericClause))
        .and(take(.assign))
        .and(expr))
    guard let parts = try parser.parse(&state) else { return nil }

    // Type alias declarations shall not have attributes.
    if !prologue.attributes.isEmpty {
      throw Diagnostics(prologue.attributes.map(Diagnostic.diagnose(unexpectedAttribute:)))
    }

    // Type alias declarations shall not have member modifiers.
    if !prologue.memberModifiers.isEmpty {
      throw Diagnostics(
        prologue.memberModifiers.map(Diagnostic.diagnose(unexpectedMemberModifier:)))
    }

    // Create a new `TypeAliasDecl`.
    assert(prologue.accessModifiers.count <= 1)
    return state.insert(
      TypeAliasDecl(
        accessModifier: prologue.accessModifiers.first,
        identifier: SourceRepresentable(token: parts.0.0.0.1, in: state.lexer.source),
        genericClause: parts.0.0.1,
        body: .typeExpr(parts.1),
        origin: state.range(from: prologue.startIndex)))
  }

  private static func parseFunctionDeclHead(
    in state: inout ParserState
  ) throws -> FunctionDeclHead? {
    // Parse the function's introducer and identifier.
    guard let name = try parseFunctionDeclIntroducerAndIdentifier(in: &state) else { return nil }

    let genericClause = try genericClause.parse(&state)
    let captures = try captureList.parse(&state) ?? []

    return FunctionDeclHead(name: name, genericClause: genericClause, captures: captures)
  }

  static func parseFunctionDeclIntroducerAndIdentifier(
    in state: inout ParserState
  ) throws -> FunctionDeclName? {
    if let introducer = state.take(.fun) {
      let stem = try state.expect("identifier", using: { $0.take(.name) })
      return FunctionDeclName(
        introducerRange: introducer.origin,
        stem: SourceRepresentable(token: stem, in: state.lexer.source),
        notation: nil)
    }

    if let notation = try operatorNotation.parse(&state) {
      _ = try state.expect("'fun'", using: { $0.take(.fun) })
      let stem = try state.expect("operator", using: operatorIdentifier)
      return FunctionDeclName(
        introducerRange: notation.origin!,
        stem: stem,
        notation: notation)
    }

    return nil
  }

  static func parseFunctionDeclSignature(
    in state: inout ParserState
  ) throws -> FunctionDeclSignature? {
    guard let parameters = try parseParameterList(in: &state) else { return nil }

    let effect = try receiverEffect.parse(&state)

    let output: AnyExprID?
    if state.take(.arrow) != nil {
      output = try state.expect("type expression", using: parseExpr(in:))
    } else {
      output = nil
    }

    return FunctionDeclSignature(parameters: parameters, receiverEffect: effect, output: output)
  }

  private static let functionOrMethodDeclBody = TryCatch(
    trying:
      methodDeclBody
      .map({ (state, body) -> FunctionOrMethodDeclBody in .method(body) }),
    orCatchingAndApplying:
      functionDeclBody
      .map({ (state, body) -> FunctionOrMethodDeclBody in .function(body) })
  )

  static let functionDeclBody = inContext(
    .functionBody,
    apply: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (state, tree) -> FunctionDecl.Body in .expr(tree.0.1) }),
      orCatchingAndApplying:
        braceStmt
        .map({ (state, id) -> FunctionDecl.Body in .block(id) })
    ))

  static let methodDeclBody =
    (take(.lBrace).and(methodImplDecl+).and(take(.rBrace))
      .map({ (state, tree) -> [NodeID<MethodImplDecl>] in
        var introducers: Set<ImplIntroducer> = []
        var duplicateIntroducer: SourceRepresentable<ImplIntroducer>? = nil
        for implID in tree.0.1 {
          let introducer = state.ast[implID].introducer
          if !introducers.insert(introducer.value).inserted { duplicateIntroducer = introducer }
        }

        if let introducer = duplicateIntroducer {
          throw Diagnostics(.diagnose(duplicateImplementationIntroducer: introducer))
        } else {
          return tree.0.1
        }
      }))

  static let methodImplDecl =
    (methodIntroducer.and(maybe(methodImplBody))
      .map({ (state, tree) -> NodeID<MethodImplDecl> in
        let receiver = state.insert(
          ParameterDecl(
            identifier: SourceRepresentable(value: "self"),
            origin: nil))
        return state.insert(
          MethodImplDecl(
            introducer: tree.0,
            receiver: receiver,
            body: tree.1,
            origin: tree.0.origin!.extended(upTo: state.currentIndex)))
      }))

  static let methodImplBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> MethodImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying:
      braceStmt
      .map({ (state, id) -> MethodImplDecl.Body in .block(id) })
  )

  static let methodIntroducer = translate([
    .let: ImplIntroducer.let,
    .inout: ImplIntroducer.inout,
    .set: ImplIntroducer.set,
    .sink: ImplIntroducer.sink,
  ])

  static let initDeclBody = inContext(.functionBody, apply: braceStmt)

  static let operatorIdentifier =
    (Apply<ParserState, SourceRepresentable<Identifier>>({ (state) in
      state.takeOperator()
    }))

  static let operatorNotation = translate([
    .infix: OperatorNotation.infix,
    .prefix: OperatorNotation.prefix,
    .postfix: OperatorNotation.postfix,
  ])

  static let precedenceGroup = ContextualKeyword<PrecedenceGroup>()

  static let propertyDeclHead =
    (take(.property).and(take(.name))
      .map({ (state, tree) -> PropertyDeclHead in
        PropertyDeclHead(
          introducer: SourceRepresentable(value: .property, range: tree.0.origin),
          stem: SourceRepresentable(token: tree.1, in: state.lexer.source))
      }))

  static let propertyDeclSignature = (take(.colon).and(expr).second)

  static let subscriptDeclHead =
    (take(.subscript).and(maybe(take(.name))).and(maybe(genericClause)).and(maybe(captureList))
      .map({ (state, tree) -> SubscriptDeclHead in
        SubscriptDeclHead(
          introducer: SourceRepresentable(value: .subscript, range: tree.0.0.0.origin),
          stem: tree.0.0.1.map({ SourceRepresentable(token: $0, in: state.lexer.source) }),
          genericClause: tree.0.1,
          captures: tree.1 ?? [])
      }))

  static func parseSubscriptDeclSignature(
    in state: inout ParserState
  ) throws -> SubscriptDeclSignature? {
    guard let parameters = try parseParameterList(in: &state) else { return nil }

    _ = try state.expect("':'", using: { $0.take(.colon) })
    let output = try state.expect("type expression", using: parseExpr(in:))

    return SubscriptDeclSignature(parameters: parameters, output: output)
  }

  static let subscriptImplDecl = (subscriptIntroducer.and(maybe(subscriptImplDeclBody)))

  static let subscriptImplDeclBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> SubscriptImplDecl.Body in .expr(tree.0.1) }),
    orCatchingAndApplying:
      braceStmt
      .map({ (state, id) -> SubscriptImplDecl.Body in .block(id) })
  )

  static let subscriptIntroducer = translate([
    .let: ImplIntroducer.let,
    .inout: ImplIntroducer.inout,
    .set: ImplIntroducer.set,
    .sink: ImplIntroducer.sink,
  ])

  static let bindingDecl =
    (Apply<ParserState, NodeID<BindingDecl>>({ (state) -> NodeID<BindingDecl>? in
      switch state.peek()?.kind {
      case .let, .inout, .var, .sink:
        return try parseDeclPrologue(in: &state, then: parseBindingDecl(withPrologue:in:))
      default:
        return nil
      }
    }))

  static let parameterDecl =
    (parameterInterface
      .and(maybe(take(.colon).and(parameterTypeExpr)))
      .and(maybe(take(.assign).and(expr)))
      .map({ (state, tree) -> NodeID<ParameterDecl> in
        state.insert(
          ParameterDecl(
            label: tree.0.0.label,
            identifier: tree.0.0.name,
            annotation: tree.0.1?.1,
            defaultValue: tree.1?.1,
            origin: state.range(
              from: tree.0.0.label?.origin!.lowerBound ?? tree.0.0.name.origin!.lowerBound)))
      }))

  typealias ParameterInterface = (
    label: SourceRepresentable<Identifier>?,
    name: SourceRepresentable<Identifier>
  )

  static let parameterInterface =
    (Apply<ParserState, ParameterInterface>({ (state) in
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
            name: SourceRepresentable(token: nameCandidate, in: state.lexer.source)
          )
        } else {
          // case `label name`
          return (
            label: SourceRepresentable(token: labelCandidate, in: state.lexer.source),
            name: SourceRepresentable(token: nameCandidate, in: state.lexer.source)
          )
        }
      }

      // Assume the first token is the name.
      if labelCandidate.kind == .name {
        // case `<no-label> name`
        let name = SourceRepresentable(token: labelCandidate, in: state.lexer.source)
        return (label: name, name: name)
      }

      throw Diagnostics(
        .diagnose(expected: "parameter name", at: labelCandidate.origin.first()))
    }))

  static let memberModifier =
    (take(.static)
      .map({ (_, token) -> SourceRepresentable<MemberModifier> in
        SourceRepresentable(value: .static, range: token.origin)
      }))

  static let accessModifier =
    (take(.public)
      .map({ (_, token) -> SourceRepresentable<AccessModifier> in
        SourceRepresentable(value: .public, range: token.origin)
      }))

  static let captureList = inContext(
    .captureList,
    apply: (take(.lBrack)
      .and(bindingDecl.and(zeroOrMany(take(.comma).and(bindingDecl).second)))
      .and(take(.rBrack))
      .map({ (_, tree) -> [NodeID<BindingDecl>] in [tree.0.1.0] + tree.0.1.1 })))

  static let genericClause =
    (take(.lAngle).and(genericParameterListContents).and(maybe(whereClause)).and(take(.rAngle))
      .map({ (state, tree) -> SourceRepresentable<GenericClause> in
        return SourceRepresentable(
          value: GenericClause(parameters: tree.0.0.1, whereClause: tree.0.1),
          range: tree.0.0.0.origin.extended(upTo: state.currentIndex))
      }))

  static let genericParameterListContents =
    (genericParameter.and(zeroOrMany(take(.comma).and(genericParameter).second))
      .map({ (_, tree) -> [NodeID<GenericParameterDecl>] in [tree.0] + tree.1 }))

  static let genericParameter =
    (maybe(typeAttribute).andCollapsingSoftFailures(take(.name))
      .and(maybe(take(.colon).and(traitComposition)))
      .and(maybe(take(.assign).and(expr)))
      .map({ (state, tree) -> NodeID<GenericParameterDecl> in
        state.insert(
          GenericParameterDecl(
            identifier: SourceRepresentable(token: tree.0.0.1, in: state.lexer.source),
            conformances: tree.0.1?.1 ?? [],
            defaultValue: tree.1?.1,
            origin: state.range(
              from: tree.0.0.0?.origin.lowerBound ?? tree.0.0.1.origin.lowerBound)))
      }))

  static let conformanceList =
    (take(.colon).and(nameTypeExpr).and(zeroOrMany(take(.comma).and(nameTypeExpr).second))
      .map({ (state, tree) -> [NodeID<NameExpr>] in [tree.0.1] + tree.1 }))

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
      state.diagnostics.report(.diagnose(infixOperatorRequiresWhitespacesAt: infixOperator.origin))
    }

    let rhs = try state.expect("type expression", using: parseExpr(in:))

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

    let expr = state.insert(
      CastExpr(
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
        state.diagnostics.report(
          .diagnose(
            infixOperatorRequiresWhitespacesAt: operatorStem.origin))
      }

      // If we can't parse an operand, the tail is empty.
      guard let operand = try parsePrefixExpr(in: &state) else {
        state.restore(from: backup)
        return false
      }

      let `operator` = state.insert(
        NameExpr(
          name: SourceRepresentable(value: Name(stem: operatorStem.value, notation: .infix)),
          origin: operatorStem.origin))
      tail.append(SequenceExpr.TailElement(operator: `operator`, operand: operand))
    }

    // Nothing to transform if the tail is empty.
    if tail.isEmpty { return false }

    let expr = state.insert(
      SequenceExpr(
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
      let operand = try state.expect("expression", using: parsePostfixExpr(in:))

      // There must be no space before the next expression.
      if isSeparated {
        state.diagnostics.report(.diagnose(separatedPrefixOperatorAt: op.origin))
      }

      let callee = state.insert(
        NameExpr(
          domain: .expr(operand),
          name: SourceRepresentable(
            value: Name(stem: op.value, notation: .prefix),
            range: op.origin),
          origin: state.range(from: op.origin!.lowerBound)))

      let call = state.insert(
        FunctionCallExpr(
          callee: AnyExprID(callee),
          arguments: [],
          origin: state.ast[callee].origin))
      return AnyExprID(call)
    }

    // Attempt to parse an inout expression.
    if let op = state.take(.ampersand) {
      // Parse an operand.
      let isSeparated = state.hasLeadingWhitespace
      let operand = try state.expect("expression", using: parsePostfixExpr(in:))

      // There must be no space before the next expression.
      if isSeparated {
        state.diagnostics.report(.diagnose(separatedMutationMarkerAt: op.origin))
      }

      let expr = state.insert(
        InoutExpr(
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

      let callee = state.insert(
        NameExpr(
          domain: .expr(operand),
          name: SourceRepresentable(
            value: Name(stem: op.value, notation: .postfix),
            range: op.origin),
          origin: state.range(from: state.ast[operand].origin!.lowerBound)))

      let call = state.insert(
        FunctionCallExpr(
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
          let expr = state.insert(
            TupleMemberExpr(
              tuple: head,
              index: index,
              origin: state.range(from: headOrigin.lowerBound)))
          head = AnyExprID(expr)
          continue
        }

        if let component = try parseNameExprComponent(in: &state) {
          let expr = state.insert(
            NameExpr(
              domain: .expr(head),
              name: component.name,
              arguments: component.arguments,
              origin: state.range(from: headOrigin.lowerBound)))
          head = AnyExprID(expr)
          continue
        }

        throw Diagnostics(.diagnose(expected: "member name", at: state.currentLocation))
      }

      // Handle conformance lens expressions.
      if state.take(.twoColons) != nil {
        // Note: We're using the `parsePrimaryExpr(in:)` parser rather that `parseExpr(in:)` so
        // that `A::P.T` is parsed as `(A::P).T`.
        let lens = try state.expect("expression", using: parsePrimaryExpr(in:))
        let expr = state.insert(
          ConformanceLensTypeExpr(
            subject: head,
            lens: lens,
            origin: state.range(from: headOrigin.lowerBound)))
        head = AnyExprID(expr)
        continue
      }

      // Exit if there's a new line before the next token.
      guard let next = state.peek(), !state.hasNewline(before: next) else { break }

      // Handle function calls.
      if next.kind == .lParen {
        let arguments = try parseFunctionCallArgumentList(in: &state)!
        let expr = state.insert(
          FunctionCallExpr(
            callee: head,
            arguments: arguments,
            origin: state.range(from: headOrigin.lowerBound)))
        head = AnyExprID(expr)
        continue
      }

      // Handle subscript calls.
      if next.kind == .lBrack {
        let arguments = try parseSubscriptCallArgumentList(in: &state)!
        let expr = state.insert(
          SubscriptCallExpr(
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
      throw Diagnostics(.diagnose(expected: "name", at: state.ast[expr].origin!.first()))
    }
  }

  private static func parsePrimaryExpr(in state: inout ParserState) throws -> AnyExprID? {
    guard let head = state.peek() else { return nil }

    switch head.kind {
    case .bool:
      // Boolean literal.
      _ = state.take()
      let expr = state.insert(
        BooleanLiteralExpr(
          value: state.lexer.source[head.origin] == "true",
          origin: head.origin))
      return AnyExprID(expr)

    case .int:
      // Integer literal.
      _ = state.take()
      let expr = state.insert(
        IntegerLiteralExpr(
          value: state.lexer.source[head.origin].filter({ $0 != "_" }),
          origin: head.origin))
      return AnyExprID(expr)

    case .float:
      // Floating-point literal.
      _ = state.take()
      let expr = state.insert(
        FloatLiteralExpr(
          value: state.lexer.source[head.origin].filter({ $0 != "_" }),
          origin: head.origin))
      return AnyExprID(expr)

    case .string:
      // String literal.
      _ = state.take()
      let expr = state.insert(
        StringLiteralExpr(
          value: String(state.lexer.source[head.origin].dropFirst().dropLast()),
          origin: head.origin))
      return AnyExprID(expr)

    case .nil:
      // Nil literal.
      _ = state.take()
      let expr = state.insert(NilLiteralExpr(origin: head.origin))
      return AnyExprID(expr)

    case .under:
      // Wildcard expression.
      _ = state.take()
      let expr = state.insert(WildcardExpr(origin: head.origin))
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
    let traits = try state.expect("trait composition", using: traitComposition)
    let clause = try whereClause.parse(&state)

    return state.insert(
      ExistentialTypeExpr(
        traits: traits,
        whereClause: clause,
        origin: introducer.origin.extended(
          toCover: clause?.origin ?? state.ast[traits.last!].origin!)))
  }

  private static func parsePrimaryDeclRefExpr(
    in state: inout ParserState
  ) throws -> NodeID<NameExpr>? {
    // Parse the name component.
    let component = try state.expect("identifier", using: parseNameExprComponent(in:))

    return state.insert(
      NameExpr(
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
    let component = try state.expect("identifier", using: parseNameExprComponent(in:))

    return state.insert(
      NameExpr(
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

    // If the next token is a left angle bracket without any leading whitespace, parse a static
    // argument list.
    let arguments: [LabeledArgument]
    if !state.hasLeadingWhitespace && state.isNext(.lAngle) {
      arguments = try state.expect("static argument list", using: parseStaticArgumentList(in:))
    } else {
      arguments = []
    }

    return NameExprComponent(
      origin: name.origin!.extended(upTo: state.currentIndex),
      name: name,
      arguments: arguments)
  }

  private static func parseArgument(in state: inout ParserState) throws -> LabeledArgument? {
    let backup = state.backup()

    // Parse a labeled argument.
    if let label = state.take(if: { $0.isLabel }) {
      if state.take(.colon) != nil {
        if let value = try parseExpr(in: &state) {
          return LabeledArgument(
            label: SourceRepresentable(token: label, in: state.lexer.source),
            value: value)
        }
      }
    }

    // Backtrack and parse an unlabeled argument.
    state.restore(from: backup)
    if let value = try parseExpr(in: &state) {
      return LabeledArgument(label: nil, value: value)
    }

    return nil
  }

  private static func parseEntityName(
    in state: inout ParserState
  ) throws -> SourceRepresentable<Name>? {
    try parseFunctionEntityName(in: &state) ?? parseOperatorEntityName(in: &state)
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
        if !closeParenFound || labels.isEmpty {
          labels.removeAll()
          state.restore(from: backup)
        }
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
      throw Diagnostics(.diagnose(expected: "operator", at: state.currentLocation))
    }
    let identifier = try state.expect("operator", using: { $0.takeOperator() })

    return SourceRepresentable(
      value: Name(stem: identifier.value, notation: OperatorNotation(notation)!),
      range: state.range(from: identifier.origin!.lowerBound))
  }

  private static func parseLambdaExpr(in state: inout ParserState) throws -> NodeID<LambdaExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.fun) else { return nil }

    // Parse the parts of the expression.
    let explicitCaptures = try captureList.parse(&state)
    let signature = try state.expect("signature", using: parseFunctionDeclSignature(in:))
    let body = try state.expect("function body", using: lambdaBody)

    let decl = state.insert(
      FunctionDecl(
        introducerRange: introducer.origin,
        receiverEffect: signature.receiverEffect,
        explicitCaptures: explicitCaptures ?? [],
        parameters: signature.parameters,
        output: signature.output,
        body: body,
        isInExprContext: true,
        origin: state.range(from: introducer.origin.lowerBound)))
    return state.insert(
      LambdaExpr(decl: decl, origin: state.ast[decl].origin))
  }

  private static let lambdaBody = inContext(
    .functionBody,
    apply: TryCatch(
      trying: take(.lBrace).and(expr).and(take(.rBrace))
        .map({ (state, tree) -> FunctionDecl.Body in .expr(tree.0.1) }),
      orCatchingAndApplying:
        braceStmt
        .map({ (state, id) -> FunctionDecl.Body in .block(id) })
    ))

  private static func parseConditionalExpr(in state: inout ParserState) throws -> NodeID<CondExpr>?
  {
    // Parse the introducer.
    guard let introducer = state.take(.if) else { return nil }

    // Parse the parts of the expression.
    let condition = try state.expect("condition", using: conditionalClause)
    let body = try state.expect("body", using: conditionalExprBody)

    // Parse the 'else' clause, if any.
    let elseClause: CondExpr.Body?
    if state.take(.else) != nil {
      if let e = try parseConditionalExpr(in: &state) {
        elseClause = .expr(AnyExprID(e))
      } else {
        elseClause = try state.expect("body", using: conditionalExprBody)
      }
    } else {
      elseClause = nil
    }

    return state.insert(
      CondExpr(
        condition: condition,
        success: body,
        failure: elseClause,
        origin: state.range(from: introducer.origin.lowerBound)))
  }

  private static let conditionalExprBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> CondExpr.Body in .expr(tree.0.1) }),
    orCatchingAndApplying:
      braceStmt
      .map({ (state, id) -> CondExpr.Body in .block(id) })
  )

  private static func parseMatchExpr(in state: inout ParserState) throws -> NodeID<MatchExpr>? {
    // Parse the introducer.
    guard let introducer = state.take(.match) else { return nil }

    // Parse the parts of the expression.
    let subject = try state.expect("subject", using: parseExpr(in:))
    let cases = try state.expect(
      "match body",
      using: take(.lBrace).and(zeroOrMany(matchCase)).and(take(.rBrace)))

    return state.insert(
      MatchExpr(
        subject: subject,
        cases: cases.0.1,
        origin: state.range(from: introducer.origin.lowerBound)))
  }

  static let matchCase =
    (pattern.and(maybe(take(.where).and(expr))).and(matchCaseBody)
      .map({ (state, tree) -> NodeID<MatchCase> in
        state.insert(
          MatchCase(
            pattern: tree.0.0,
            condition: tree.0.1?.1,
            body: tree.1,
            origin: state.ast[tree.0.0].origin!.extended(upTo: state.currentIndex)))
      }))

  private static let matchCaseBody = TryCatch(
    trying: take(.lBrace).and(expr).and(take(.rBrace))
      .map({ (state, tree) -> MatchCase.Body in .expr(tree.0.1) }),
    orCatchingAndApplying:
      braceStmt
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
      output = try state.expect("expression", using: parseExpr(in:))
      body = try state.expect("function body", using: lambdaBody)
    } else {
      output = nil
      body = try .expr(state.expect("expression", using: parseExpr(in:)))
    }

    let decl = state.insert(
      FunctionDecl(
        introducerRange: introducer.origin,
        receiverEffect: effect,
        explicitCaptures: explicitCaptures,
        output: output,
        body: body,
        isInExprContext: true,
        origin: state.range(from: introducer.origin.lowerBound)))
    return state.insert(
      SpawnExpr(decl: decl, origin: state.ast[decl].origin))
  }

  private static func parseLambdaTypeOrTupleExpr(
    in state: inout ParserState
  ) throws -> AnyExprID? {
    // Expect a left parenthesis.
    guard
      let opener = state.peek(),
      opener.kind == .lParen
    else { return nil }

    // Assume we're parsing a lambda type expression until we reach the point where we should
    // consume a right arrow. Commit to that choice only if there's one.
    let backup = state.backup()

    // Parse the parameters or backtrack and parse a tuple expression.
    let parameters: [LambdaTypeExpr.Parameter]
    do {
      parameters = try parseLambdaParameterList(in: &state)!
    } catch {
      state.restore(from: backup)
      return try parseTupleOrParenthesizedExpr(in: &state)
    }

    // Parse the remainder of the type expression.
    let effect = try receiverEffect.parse(&state)

    guard state.take(.arrow) != nil else {
      // If we didn't parse any effect and the parameter list is empty, assume we parsed an empty
      // tuple. Otherwise, backtrack and parse a tuple expression.
      if (effect == nil) && parameters.isEmpty {
        let expr = state.insert(
          TupleExpr(
            elements: [],
            origin: state.range(from: opener.origin.lowerBound)))
        return AnyExprID(expr)
      }

      state.restore(from: backup)
      return try parseTupleOrParenthesizedExpr(in: &state)
    }

    let output = try state.expect("type expression", using: parseExpr(in:))

    let expr = state.insert(
      LambdaTypeExpr(
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
    if !state.isNext(.lParen) {
      let expr = state.insert(
        BufferLiteralExpr(
          elements: environement != nil ? [environement!] : [],
          origin: state.range(from: opener.origin.lowerBound)))
      return AnyExprID(expr)
    }

    // Parse the parameters or backtrack and parse a compound literal.
    let parameters: [LambdaTypeExpr.Parameter]
    do {
      parameters = try parseLambdaParameterList(in: &state)!
    } catch {
      state.restore(from: backup)
      return try parseCompoundLiteral(in: &state)
    }

    // Parse the remainder of the type expression.
    let effect = try receiverEffect.parse(&state)

    guard state.take(.arrow) != nil else {
      // Backtrack and parse a compound literal.
      state.restore(from: backup)
      return try parseCompoundLiteral(in: &state)
    }

    let output = try state.expect("type expression", using: parseExpr(in:))

    // Synthesize the environment as an empty tuple if we parsed `[]`.
    let e =
      environement
      ?? AnyExprID(state.insert(TupleTypeExpr(elements: [], origin: nil)))

    let expr = state.insert(
      LambdaTypeExpr(
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
    // Parse the elements.
    guard let elementList = try tupleExprElementList.parse(&state) else { return nil }

    // If there's only one element without any label and we didn't parse a trailing separator,
    // interpret the element's value as a parenthesized expression.
    if elementList.trailingSeparator == nil,
      let uniqueElement = elementList.elements.uniqueElement,
      uniqueElement.label == nil
    {
      return uniqueElement.value
    }

    let expr = state.insert(
      TupleExpr(
        elements: elementList.elements,
        origin: state.range(from: elementList.opener.origin.lowerBound)))
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

  private static func parseTupleTypeExpr(
    in state: inout ParserState
  ) throws -> NodeID<TupleTypeExpr>? {
    // Parse the elements.
    guard let elementList = try tupleTypeExprElementList.parse(&state) else { return nil }

    return state.insert(
      TupleTypeExpr(
        elements: elementList.elements,
        origin: state.range(from: elementList.opener.origin.lowerBound)))
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

  private static let bufferLiteral =
    (take(.lBrack).and(maybe(bufferComponentListContents)).and(take(.rBrack))
      .map({ (state, tree) -> NodeID<BufferLiteralExpr> in
        state.insert(
          BufferLiteralExpr(
            elements: tree.0.1 ?? [],
            origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  private static let bufferComponentListContents =
    (expr.and(zeroOrMany(take(.comma).and(expr).second))
      .map({ (state, tree) -> [AnyExprID] in [tree.0] + tree.1 }))

  private static let mapLiteral =
    (take(.lBrack).and(mapComponentListContents.or(mapComponentEmptyContents)).and(take(.rBrack))
      .map({ (state, tree) -> NodeID<MapLiteralExpr> in
        state.insert(
          MapLiteralExpr(
            elements: tree.0.1,
            origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  private static let mapComponentEmptyContents =
    (take(.colon)
      .map({ (_, _) -> [MapLiteralExpr.Element] in [] }))

  private static let mapComponentListContents =
    (mapComponent.and(zeroOrMany(take(.comma).and(mapComponent).second))
      .map({ (state, tree) -> [MapLiteralExpr.Element] in [tree.0] + tree.1 }))

  private static let mapComponent =
    (expr.and(take(.colon)).and(expr)
      .map({ (_, tree) -> MapLiteralExpr.Element in
        MapLiteralExpr.Element(key: tree.0.0, value: tree.1)
      }))

  private static let callArgument = Apply(parseArgument(in:))

  static let conditionalClause =
    (conditionalClauseItem.and(zeroOrMany(take(.comma).and(conditionalClauseItem).second))
      .map({ (_, tree) -> [ConditionItem] in [tree.0] + tree.1 }))

  static let conditionalClauseItem = Choose(
    bindingPattern.and(take(.assign)).and(expr)
      .map({ (state, tree) -> ConditionItem in
        let id = state.insert(
          BindingDecl(
            pattern: tree.0.0,
            initializer: tree.1,
            origin: state.ast[tree.0.0].origin!.extended(upTo: state.currentIndex)))
        return .decl(id)
      }),
    or:
      expr
      .map({ (_, id) -> ConditionItem in .expr(id) })
  )

  // MARK: Comma-separated lists

  private static let staticArgumentList = DelimitedCommaSeparatedList(
    openerKind: .lAngle,
    closerKind: .rAngle,
    closerDescription: ">",
    elementParser: Apply(parseArgument(in:)))

  private static func parseStaticArgumentList(
    in state: inout ParserState
  ) throws -> [LabeledArgument]? {
    try parseList(in: &state, with: staticArgumentList)
  }

  private static let functionCallArgumentList = DelimitedCommaSeparatedList(
    openerKind: .lParen,
    closerKind: .rParen,
    closerDescription: ")",
    elementParser: Apply(parseArgument(in:)))

  private static func parseFunctionCallArgumentList(
    in state: inout ParserState
  ) throws -> [LabeledArgument]? {
    try parseList(in: &state, with: functionCallArgumentList)
  }

  private static let subscriptCallArgumentList = DelimitedCommaSeparatedList(
    openerKind: .lBrack,
    closerKind: .rBrack,
    closerDescription: "]",
    elementParser: Apply(parseArgument(in:)))

  private static func parseSubscriptCallArgumentList(
    in state: inout ParserState
  ) throws -> [LabeledArgument]? {
    try parseList(in: &state, with: subscriptCallArgumentList)
  }

  private static let attributeArgumentList = DelimitedCommaSeparatedList(
    openerKind: .lParen,
    closerKind: .rParen,
    closerDescription: ")",
    elementParser: Apply(parseAttributeArgument(in:)))

  private static func parseAttributeArgumentList(
    in state: inout ParserState
  ) throws -> [Attribute.Argument]? {
    try parseList(in: &state, with: attributeArgumentList)
  }

  private static let parameterList = DelimitedCommaSeparatedList(
    openerKind: .lParen,
    closerKind: .rParen,
    closerDescription: ")",
    elementParser: parameterDecl)

  private static func parseParameterList(
    in state: inout ParserState
  ) throws -> [NodeID<ParameterDecl>]? {
    try parseList(in: &state, with: parameterList)
  }

  private static let lambdaParameterList = DelimitedCommaSeparatedList(
    openerKind: .lParen,
    closerKind: .rParen,
    closerDescription: ")",
    elementParser: Apply(parseLambdaParameter(in:)))

  private static func parseLambdaParameterList(
    in state: inout ParserState
  ) throws -> [LambdaTypeExpr.Parameter]? {
    try parseList(in: &state, with: lambdaParameterList)
  }

  private static func parseList<C: Combinator>(
    in state: inout ParserState,
    with parser: DelimitedCommaSeparatedList<C>
  ) throws -> [C.Element]? where C.Context == ParserState {
    guard let result = try parser.parse(&state) else { return nil }

    if let separator = result.trailingSeparator {
      state.diagnostics.report(.diagnose(unexpectedToken: separator))
    }

    return result.elements
  }

  private static let tupleExprElementList = DelimitedCommaSeparatedList(
    openerKind: .lParen,
    closerKind: .rParen,
    closerDescription: ")",
    elementParser: Apply(parseTupleExprElement(in:)))

  private static let tupleTypeExprElementList = DelimitedCommaSeparatedList(
    openerKind: .lBrace,
    closerKind: .rBrace,
    closerDescription: "}",
    elementParser: Apply(parseTupleTypeExprElement(in:)))

  // MARK: Patterns

  private static func anyPattern<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyPatternID>
  where Base.Context == ParserState, Base.Element: PatternID {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyPatternID.init(_:))
    })
  }

  static let pattern: Recursive<ParserState, AnyPatternID> = (Recursive(_pattern.parse(_:)))

  private static let _pattern =
    (oneOf([
      anyPattern(bindingPattern),
      anyPattern(exprPattern),
      anyPattern(tuplePattern),
      anyPattern(wildcardPattern),
    ]))

  static let bindingPattern =
    (bindingIntroducer
      .and(
        inContext(
          .bindingPattern,
          apply: oneOf([
            anyPattern(namePattern),
            anyPattern(tuplePattern),
            anyPattern(wildcardPattern),
          ]))
      )
      .and(maybe(take(.colon).and(expr)))
      .map({ (state, tree) -> NodeID<BindingPattern> in
        state.insert(
          BindingPattern(
            introducer: tree.0.0,
            subpattern: tree.0.1,
            annotation: tree.1?.1,
            origin: tree.0.0.origin!.extended(upTo: state.currentIndex)))
      }))

  static let bindingIntroducer =
    (Apply<ParserState, SourceRepresentable<BindingPattern.Introducer>>({ (state) in
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
        _ = try state.expect("'let'", using: { $0.take(.let) })
        introducer = .sinklet

      default:
        return nil
      }

      return SourceRepresentable(
        value: introducer,
        range: head.origin.extended(upTo: state.currentIndex))
    }))

  static let exprPattern =
    (Apply<ParserState, AnyPatternID>({ (state) -> AnyPatternID? in
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
      let id = state.insert(
        ExprPattern(
          expr: exprID,
          origin: state.ast[exprID].origin))
      return AnyPatternID(id)
    }))

  static let namePattern =
    (take(.name)
      .map({ (state, token) -> NodeID<NamePattern> in
        let declID = state.insert(
          VarDecl(
            identifier: SourceRepresentable(token: token, in: state.lexer.source)))
        return state.insert(NamePattern(decl: declID, origin: token.origin))
      }))

  static let tuplePattern =
    (take(.lParen).and(maybe(tuplePatternElementList)).and(take(.rParen))
      .map({ (state, tree) -> NodeID<TuplePattern> in
        state.insert(
          TuplePattern(
            elements: tree.0.1 ?? [],
            origin: tree.0.0.origin.extended(upTo: tree.1.origin.upperBound)))
      }))

  static let tuplePatternElementList =
    (tuplePatternElement.and(zeroOrMany(take(.comma).and(tuplePatternElement).second))
      .map({ (_, tree) -> [TuplePattern.Element] in [tree.0] + tree.1 }))

  static let tuplePatternElement =
    (Apply<ParserState, TuplePattern.Element>({ (state) in
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
    }))

  static let wildcardPattern =
    (take(.under)
      .map({ (state, token) -> NodeID<WildcardPattern> in
        state.insert(WildcardPattern(origin: token.origin))
      }))

  // MARK: Statements

  private static func anyStmt<Base: Combinator>(
    _ base: Base
  ) -> AnyCombinator<ParserState, AnyStmtID>
  where Base.Context == ParserState, Base.Element: StmtID {
    AnyCombinator(parse: { (state) in
      try base.parse(&state).map(AnyStmtID.init(_:))
    })
  }

  static let stmt: Recursive<ParserState, AnyStmtID> = (Recursive(_stmt.parse(_:)))

  static let _stmt =
    (oneOf([
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
    ]))

  static let braceStmt =
    (take(.lBrace)
      .and(zeroOrMany(take(.semi)))
      .and(zeroOrMany(stmt.and(zeroOrMany(take(.semi))).first))
      .and(take(.rBrace))
      .map({ (state, tree) -> NodeID<BraceStmt> in
        state.insert(
          BraceStmt(stmts: tree.0.1, origin: tree.0.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let discardStmt =
    (take(.under).and(take(.assign)).and(expr)
      .map({ (state, tree) -> NodeID<DiscardStmt> in
        state.insert(
          DiscardStmt(expr: tree.1, origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let doWhileStmt =
    (take(.do).and(loopBody).and(take(.while)).and(expr)
      .map({ (state, tree) -> NodeID<DoWhileStmt> in
        state.insert(
          DoWhileStmt(
            body: tree.0.0.1, condition: tree.1,
            origin: tree.0.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let whileStmt =
    (take(.while).and(conditionalClause).and(loopBody)
      .map({ (state, tree) -> NodeID<WhileStmt> in
        state.insert(
          WhileStmt(
            condition: tree.0.1, body: tree.1,
            origin: tree.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let forStmt =
    (take(.for).and(bindingPattern).and(forRange).and(maybe(forFilter)).and(loopBody)
      .map({ (state, tree) -> NodeID<ForStmt> in
        let decl = state.insert(
          BindingDecl(
            pattern: tree.0.0.0.1,
            initializer: nil,
            origin: state.ast[tree.0.0.0.1].origin))
        return state.insert(
          ForStmt(
            binding: decl, domain: tree.0.0.1, filter: tree.0.1, body: tree.1,
            origin: tree.0.0.0.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let forRange = (take(.in).and(expr).second)

  static let forFilter = (take(.where).and(expr).second)

  static let loopBody = inContext(.loopBody, apply: braceStmt)

  static let returnStmt =
    (take(.return).and(maybe(onSameLine(expr)))
      .map({ (state, tree) -> NodeID<ReturnStmt> in
        state.insert(
          ReturnStmt(
            value: tree.1,
            origin: tree.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let yieldStmt =
    (take(.yield).and(onSameLine(expr))
      .map({ (state, tree) -> NodeID<YieldStmt> in
        state.insert(
          YieldStmt(
            value: tree.1,
            origin: tree.0.origin.extended(upTo: state.currentIndex)))
      }))

  static let breakStmt =
    (take(.break)
      .map({ (state, token) -> NodeID<BreakStmt> in
        state.insert(BreakStmt(origin: token.origin))
      }))

  static let continueStmt =
    (take(.break)
      .map({ (state, token) -> NodeID<ContinueStmt> in
        state.insert(ContinueStmt(origin: token.origin))
      }))

  static let bindingStmt =
    (Apply<ParserState, AnyStmtID>({ (state) -> AnyStmtID? in
      let backup = state.backup()
      do {
        if let element = try conditionalBindingStmt.parse(&state) { return AnyStmtID(element) }
      } catch {}
      state.restore(from: backup)

      if let decl = try bindingDecl.parse(&state) {
        let id = state.insert(
          DeclStmt(
            decl: AnyDeclID(decl),
            origin: state.ast[decl].origin))
        return AnyStmtID(id)
      } else {
        return nil
      }
    }))

  static let conditionalBindingStmt =
    (bindingDecl.and(take(.else)).and(conditionalBindingFallback)
      .map({ (state, tree) -> NodeID<CondBindingStmt> in
        let bindingRange = state.ast[tree.0.0].origin!

        if state.ast[tree.0.0].initializer == nil {
          throw Diagnostics(
            .error(
              "conditional binding requires an initializer",
              range: bindingRange.extended(upTo: bindingRange.lowerBound)))
        }

        return state.insert(
          CondBindingStmt(
            binding: tree.0.0,
            fallback: tree.1,
            origin: bindingRange.extended(upTo: state.currentIndex)))
      }))

  static let conditionalBindingFallback =
    (conditionalBindingFallbackStmt.or(conditionalBindingFallbackExpr))

  static let conditionalBindingFallbackExpr =
    (expr.map({ (_, id) -> CondBindingStmt.Fallback in .expr(id) }))

  static let conditionalBindingFallbackStmt =
    (oneOf([
      anyStmt(breakStmt),
      anyStmt(continueStmt),
      anyStmt(returnStmt),
      anyStmt(braceStmt),
    ])
    .map({ (_, id) -> CondBindingStmt.Fallback in .exit(id) }))

  static let declStmt =
    (Apply(parseDecl)
      .map({ (state, decl) -> NodeID<DeclStmt> in
        state.insert(DeclStmt(decl: decl, origin: state.ast[decl].origin))
      }))

  static let exprStmt = Apply(parseExprOrAssignStmt(in:))

  private static func parseExprOrAssignStmt(in state: inout ParserState) throws -> AnyStmtID? {
    guard let lhs = try expr.parse(&state) else { return nil }

    // Return an expression statement unless the next token is `=`.
    guard let assign = state.take(.assign) else {
      let stmt = state.insert(
        ExprStmt(expr: lhs, origin: state.ast[lhs].origin))
      return AnyStmtID(stmt)
    }

    if !state.hasLeadingAndTrailingWhitespaces(assign) {
      state.diagnostics.report(.diagnose(assignOperatorRequiresWhitespaces: assign))
    }

    let rhs = try state.expect("expression", using: parsePrefixExpr(in:))

    let stmt = state.insert(
      AssignStmt(
        left: lhs,
        right: rhs,
        origin: state.range(from: state.ast[lhs].origin!.lowerBound)))
    return AnyStmtID(stmt)
  }

  // MARK: Type expressions

  private static let nameTypeExpr = Apply(parseNameExpr(in:))

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

  static let parameterTypeExpr =
    (maybe(passingConvention)
      .andCollapsingSoftFailures(expr)
      .map({ (state, tree) -> NodeID<ParameterTypeExpr> in
        state.insert(
          ParameterTypeExpr(
            convention: tree.0 ?? SourceRepresentable(value: .let),
            bareType: tree.1,
            origin: state.range(
              from: tree.0?.origin!.lowerBound ?? state.ast[tree.1].origin!.lowerBound)))
      }))

  static let receiverEffect = accessEffect

  static let passingConvention = accessEffect

  static let accessEffect = translate([
    .let: AccessEffect.let,
    .inout: AccessEffect.inout,
    .set: AccessEffect.set,
    .sink: AccessEffect.sink,
    .yielded: AccessEffect.yielded,
  ])

  static let whereClause =
    (take(.where).and(whereClauseConstraintList)
      .map({ (state, tree) -> SourceRepresentable<WhereClause> in
        SourceRepresentable(
          value: WhereClause(constraints: tree.1),
          range: tree.0.origin.extended(upTo: state.currentIndex))
      }))

  static let whereClauseConstraintList =
    (whereClauseConstraint.and(zeroOrMany(take(.comma).and(whereClauseConstraint).second))
      .map({ (state, tree) -> [SourceRepresentable<WhereClause.ConstraintExpr>] in
        [tree.0] + tree.1
      }))

  static let whereClauseConstraint = (typeConstraint.or(valueConstraint))

  static let typeConstraint =
    (Apply<ParserState, SourceRepresentable<WhereClause.ConstraintExpr>>({ (state) in
      guard let lhs = try parseNameExpr(in: &state) else { return nil }

      // equality-constraint
      if state.take(.equal) != nil {
        let rhs = try state.expect("type expression", using: parseExpr(in:))
        return SourceRepresentable(
          value: .equality(l: lhs, r: rhs),
          range: state.ast[lhs].origin!.extended(upTo: state.currentIndex))
      }

      // conformance-constraint
      if state.take(.colon) != nil {
        let traits = try state.expect("trait composition", using: traitComposition)
        return SourceRepresentable(
          value: .conformance(l: lhs, traits: traits),
          range: state.ast[lhs].origin!.extended(upTo: state.currentIndex))
      }

      throw Diagnostics(.diagnose(expected: "constraint operator", at: state.currentLocation))
    }))

  static let valueConstraint =
    (valueAttribute.and(expr)
      .map({ (state, tree) -> SourceRepresentable<WhereClause.ConstraintExpr> in
        SourceRepresentable(
          value: .value(tree.1),
          range: tree.0.origin.extended(upTo: state.currentIndex))
      }))

  static let traitComposition =
    (nameTypeExpr.and(zeroOrMany(take(.ampersand).and(nameTypeExpr).second))
      .map({ (state, tree) -> TraitComposition in [tree.0] + tree.1 }))

  // MARK: Attributes

  static func parseDeclAttribute(
    in state: inout ParserState
  ) throws -> SourceRepresentable<Attribute>? {
    guard let introducer = state.take(.attribute) else { return nil }
    let arguments = try parseAttributeArgumentList(in: &state) ?? []

    return SourceRepresentable(
      value: Attribute(
        name: SourceRepresentable(token: introducer, in: state.lexer.source),
        arguments: arguments),
      range: state.range(from: introducer.origin.lowerBound))
  }

  private static func parseAttributeArgument(
    in state: inout ParserState
  ) throws -> Attribute.Argument? {
    if let token = state.take(.int) {
      if let value = Int(state.lexer.source[token.origin]) {
        return .integer(SourceRepresentable(value: value, range: token.origin))
      } else {
        throw Diagnostics(.error("invalid integer literal", range: token.origin))
      }
    }

    if let token = state.take(.string) {
      let value = String(state.lexer.source[token.origin].dropFirst().dropLast())
      return .string(SourceRepresentable(value: value, range: token.origin))
    }

    return nil
  }

  static let typeAttribute = attribute("@type")

  static let valueAttribute = attribute("@value")

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
  let receiverEffect: SourceRepresentable<AccessEffect>?

  /// The return type annotation of the declaration, if any.
  let output: AnyExprID?

}

/// The body of a function or method declaration.
enum FunctionOrMethodDeclBody {

  case function(FunctionDecl.Body)

  case method([NodeID<MethodImplDecl>])

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
  let output: AnyExprID

}

/// The parsed component of a name expression.
struct NameExprComponent {

  /// The source range from which `self` was parsed.
  let origin: SourceRange

  /// The name of the component.
  let name: SourceRepresentable<Name>

  /// The static arguments of the component, if any.
  let arguments: [LabeledArgument]

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

/// A combinator that parses comma-separated lists of elements delimited by tokens on both ends.
struct DelimitedCommaSeparatedList<E: Combinator>: Combinator where E.Context == ParserState {

  typealias Context = ParserState

  /// The result of a comma-separated list parser.
  struct Element {

    /// The left delimiter of the list.
    let opener: Token

    /// The right delimiter of the list, if parsed.
    let closer: Token?

    /// The trailing comma, if parsed.
    let trailingSeparator: Token?

    /// The elements of the list.
    let elements: [E.Element]

  }

  /// The kind of the left delimiter.
  let openerKind: Token.Kind

  /// The kind of the right delimiter.
  let closerKind: Token.Kind

  /// The description of the right delimiter (for diagnostics).
  let closerDescription: String

  /// The parser recognizing the list's elements.
  let elementParser: E

  func parse(_ state: inout ParserState) throws -> Element? {
    // Parse the opening angle.
    guard let opener = state.take(openerKind) else { return nil }

    // Parse the elements.
    var elements: [E.Element] = []
    var trailingSeparator: Token? = nil
    var closer: Token? = nil

    while true {
      // Parse one element.
      if let element = try elementParser.parse(&state) {
        elements.append(element)

        // Look for a separator.
        trailingSeparator = nil
        if let t = state.take(.comma) {
          trailingSeparator = t
          continue
        }
      }

      // If we get here, we either parsed an element not followed by a separator (1), or we got a
      // soft failure and didn't consume anything from the stream (2). In both case, we should
      // expect the right delimiter.
      if let t = state.take(closerKind) {
        closer = t
        break
      }

      // If we got here by (2) but didn't parse any element yet, diagnose a missing delimiter and
      // exit the loop.
      if elements.isEmpty {
        state.diagnostics.report(
          .diagnose(expected: closerDescription, matching: opener, in: state))
        break
      }

      // If we got here by (1), diagnose a missing separator and try to parse the next element
      // unless we reached EOF. Otherwise, diagnose a missing expression and exit.
      if trailingSeparator == nil {
        if let head = state.peek() {
          state.diagnostics.report(
            .diagnose(expected: "',' separator", at: head.origin.first()))
          continue
        } else {
          state.diagnostics.report(
            .diagnose(expected: closerDescription, matching: opener, in: state))
          break
        }
      } else {
        state.diagnostics.report(
          .diagnose(expected: "expression", at: state.currentLocation))
        break
      }
    }

    return Element(
      opener: opener,
      closer: closer,
      trailingSeparator: trailingSeparator,
      elements: elements)
  }

}

/// Creates a combinator that parses tokens with the specified kind.
private func take(_ kind: Token.Kind) -> TakeKind {
  TakeKind(kind: kind)
}

/// Creates a combinator that parses name tokens with the specified value.
private func take(nameTokenWithValue value: String) -> Apply<ParserState, Token> {
  Apply({ (state) in state.take(nameTokenWithValue: value) })
}

/// Creates a combinator that parses attribute tokens with the specified name.
private func attribute(_ name: String) -> Apply<ParserState, Token> {
  Apply({ (state) in state.take(attribute: name) })
}

/// Creates a combinator that translates token kinds to instances of type.
private func translate<T>(
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
private func inContext<Base: Combinator>(
  _ context: ParserState.Context,
  apply base: Base
) -> WrapInContext<Base> {
  WrapInContext(context: context, base: base)
}

/// Creates a combinator that applies `base` only if its input is not preceeded by whitespaces.
private func withoutLeadingWhitespace<Base: Combinator>(
  _ base: Base
) -> Apply<ParserState, Base.Element>
where Base.Context == ParserState {
  Apply({ (state) in try state.hasLeadingWhitespace ? nil : base.parse(&state) })
}

/// Creates a combinator that applies `base` only if its input is not preceeded by newlines.
private func onSameLine<Base: Combinator>(
  _ base: Base
) -> Apply<ParserState, Base.Element>
where Base.Context == ParserState {
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

extension OperatorNotation {

  /// Creates an instance from `token`'s kind, or returns `nil` if `token` does not represent an
  /// operator notation.
  fileprivate init?(_ token: Token) {
    switch token.kind {
    case .infix: self = .infix
    case .prefix: self = .prefix
    case .postfix: self = .postfix
    default: return nil
    }
  }

}

extension SourceRepresentable where Part == Identifier {

  fileprivate init(token: Token, in source: SourceFile) {
    self.init(value: String(source[token.origin]), range: token.origin)
  }

}

extension AST {
  public mutating func insert<S: Sequence>(
    _ sources: S, asModule newModule: String, diagnostics: inout Diagnostics
  ) throws -> NodeID<ModuleDecl>
  where S.Element == SourceFile {
    let newModule = self.insert(synthesized: ModuleDecl(name: newModule))

    for f in sources {
      do {
        _ = try Parser.parse(f, into: newModule, in: &self, diagnostics: &diagnostics)
      } catch _ as Diagnostics {}  // These have been logged, let's keep parsing other files.
    }
    try diagnostics.throwOnError()
    return newModule
  }
}
