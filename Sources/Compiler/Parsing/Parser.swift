import Foundation

/// The parser for Val source code.
public struct Parser {

  /// The internal state of a parser.
  fileprivate struct State {

    /// A Boolean value that indicates whether the parser encountered an error.
    var hasError = false

    /// The diagnostics of the the parse errors.
    var diags: [Diag] = []

    /// The declaration space in which new declarations are being parsed.
    var declSpace: DeclSpace?

    /// The parser's lexer.
    var lexer: Lexer

    /// The lookahead buffer.
    var lookahead: Token?

    /// The flags of the parser's state.
    var flags = Flags.isParsingTopLevel

    /// The counter that is used to generate discriminators for anonymous functions.
    var discriminator = 0

    /// Returns the next element in the token stream, without consuming it.
    mutating func peek() -> Token? {
      // Return the token in the lookahead buffer, if available.
      if let token = lookahead {
        return token
      }

      // Attempt to pull a new element from the lexer.
      guard let token = lexer.next() else { return nil }
      lookahead = token
      return token
    }

    /// Consumes the next element from the token stream.
    mutating func take() -> Token? {
      if let token = lookahead {
        lookahead = nil
        return token
      } else {
        return lexer.next()
      }
    }

    /// Consumes the next element from the token stream, if it has the specified kind.
    mutating func take(_ kind: Token.Kind) -> Token? {
      return peek()?.kind == kind
        ? take()
        : nil
    }

    /// Consumes the next element from the token stream, if it satisfies the specified predicate.
    mutating func take(if predicate: (Token) -> Bool) -> Token? {
      guard let token = peek() else { return nil }
      return predicate(token)
        ? take()
        : nil
    }

    /// Consumes an operator from the token stream.
    ///
    /// If the next token in the stream is an angle bracket, it is interpreter as an operator and
    /// merged with any attached operator.
    ///
    /// - Parameter includingAssign: A flag that indicates whether `=` should be consumed.
    mutating func takeOperator(includingAssign: Bool = true) -> Ident? {
      guard let head = peek() else { return nil }

      switch head.kind {
      case .oper:
        _ = take()
        return ident(head)

      case .assign where includingAssign:
        _ = take()
        return ident(head)

      case .lAngle, .rAngle:
        _ = take()

        // Merge the leading angle bracket with attached operators.
        var upper = head.range.upperBound
        while let next = peek(), next.isOperator && (upper == next.range.lowerBound) {
          upper = next.range.upperBound
        }

        let range = head.range.lowerBound ..< upper
        return Ident(name: String(lexer.source[range]), range: range)

      default:
        return nil
      }
    }

    /// Consumes the stream token as long as the specified predicate is satisfied.
    mutating func skip(while predicate: (Token) -> Bool) {
      while let token = peek(), predicate(token) {
        _ = take()
      }
    }

    /// Creates a save point to restore the state.
    func save() -> State { self }

    /// Restores the state to the specified backup.
    mutating func restore(_ backup: State) {
      self = backup
    }

    /// Creates an identifier from the specified token.
    func ident(_ token: Token) -> Ident {
      let name = lexer.source[token.range]
      return Ident(name: String(name), range: token.range)
    }

    /// Returns a unique function discriminator.
    mutating func nextDiscriminator() -> Int {
      discriminator += 1
      return discriminator
    }

    /// Returns a source range suitable to report an error at the current stream position.
    mutating func errorRange() -> SourceRange {
      return peek()?.range ?? (lexer.location ..< lexer.location)
    }

  }

  /// The flags of a parser's state.
  fileprivate struct Flags {

    private var rawValue: UInt8

    static func + (lhs: Flags, rhs: Flags) -> Flags {
      return Flags(rawValue: lhs.rawValue | rhs.rawValue)
    }

    static func & (lhs: Flags, rhs: Flags) -> Bool {
      return (lhs.rawValue & rhs.rawValue) != 0
    }

    static let isParsingTopLevel  = Flags(rawValue: 1 << 0)
    static let isParsingNamespace = Flags(rawValue: 1 << 1)
    static let isParsingProdBody  = Flags(rawValue: 1 << 2)
    static let isParsingViewBody  = Flags(rawValue: 1 << 3)
    static let isParsingExtnBody  = Flags(rawValue: 1 << 4)
    static let isParsingFunBody   = Flags(rawValue: 1 << 5)
    static let isParsingLoopBody  = Flags(rawValue: 1 << 6)

    static let isParsingTypeBody  = isParsingProdBody + isParsingViewBody + isParsingExtnBody

  }

  /// The result of an attempt to parse with backtracking.
  fileprivate enum AttemptResult<T> {

    case success(T)

    case failure

  }

  /// The AST context in which the parser operates.
  public let context: Compiler

  /// An alias to the context's unresolved type.
  private var unresolved: UnresolvedType { context.unresolvedType }

  /// Creates a parser.
  ///
  /// - Parameter context: The AST context in which source files will be parsed.
  public init(context: Compiler) {
    self.context = context
  }

  /// Parses the contents of the file at the specified URL.
  public func parse(contentsOf url: URL) throws -> (unit: FileUnit, hasError: Bool) {
    let unit = FileUnit(url: url)
    var state = try State(declSpace: unit, lexer: Lexer(contentsOf: url))

    while true {
      // Skip leading semicolons and exit the loop once we've reached the end of the stream
      state.skip(while: { $0.kind == .semi })
      guard state.peek() != nil else { break }

      // Parse a declaration.
      do {
        guard let decl = try parseDecl(state: &state) else {
          throw ParseError(Diag("expected declaration", anchor: state.errorRange()))
        }
        unit.decls.append(decl)
        if let decl = decl as? FunDecl, decl.ident == nil {
          state.diags.append(Diag(
            "anonymous function can never be called", anchor: decl.introRange))
        }
      } catch let error as ParseError {
        state.diags.append(error.diag)
        state.hasError = true
        state.skip(while: { ($0.kind != .semi) && !$0.mayBeginDecl })
      } catch {
        fatalError("unreachable")
      }
    }

    // Report diagnostics.
    for diag in state.diags {
      DiagDispatcher.instance.report(diag)
    }

    return (unit, state.hasError)
  }

  /// Parses a declaration.
  ///
  ///     decl ::= decl-modifier* (binding-decl | fun-decl | type-decl | extn-decl | oper-decl)
  private func parseDecl(state: inout State) throws -> Decl? {
    // Consume a list of declaration modifiers.
    var kinds: Set<DeclModifier.Kind> = []
    var modifiers: [DeclModifier] = []

    loop:while let token = state.peek() {
      var m: DeclModifier
      switch token.kind {
      case .pub       : m = DeclModifier(kind: .pub)
      case .mod       : m = DeclModifier(kind: .mod)
      case .consuming : m = DeclModifier(kind: .consuming)
      case .mut       : m = DeclModifier(kind: .mut)
      case .infix     : m = DeclModifier(kind: .infix)
      case .prefix    : m = DeclModifier(kind: .prefix)
      case .postfix   : m = DeclModifier(kind: .postfix)
      case .static    : m = DeclModifier(kind: .static)
      default         : break loop
      }
      m.range = token.range
      _ = state.take()

      guard kinds.insert(m.kind).inserted else {
        state.diags.append(Diag(
          .warning, "ignoring duplicate modifier \(m.kind)", anchor: m.range))
        continue
      }

      // 'pub' and 'mod' are mutually exclusive.
      guard ((m.kind != .pub) || !kinds.contains(.mod)),
            ((m.kind != .mod) || !kinds.contains(.pub))
      else {
          state.diags.append(Diag(
            "'pub' and 'mod' modifiers are mutually exclusive", anchor: token.range))
        state.hasError = true
        continue
      }

      // 'infix', 'prefix' and 'postfix' are mutually exclusive.
      guard ((m.kind != .infix)   || !kinds.contains(.prefix) && !kinds.contains(.postfix)),
            ((m.kind != .prefix)  || !kinds.contains(.infix)  && !kinds.contains(.postfix)),
            ((m.kind != .postfix) || !kinds.contains(.infix)  && !kinds.contains(.prefix))
      else {
        state.diags.append(Diag(
          "'infix', 'prefix' and 'postfix' modifiers are mutually exclusive", anchor: token.range))
        state.hasError = true
        continue
      }

      modifiers.append(m)
    }

    // Parse a declaration.
    let head = state.peek()
    switch head?.kind {
    case .let, .var:
      return parseBindingDecl(state: &state, parsedModifiers: modifiers)

    case .fun, .new, .del:
      return parseFunDecl(state: &state, parsedModifiers: modifiers, requiringParamSign: true)

    case .type:
      return parseTypeDecl(state: &state, parsedModifiers: modifiers)

    case .view:
      if !(state.flags & .isParsingTopLevel) {
        state.diags.append(Diag(
          "view declarations can only appear at top level", anchor: head?.range))
        state.hasError = true
      }
      return parseTypeDecl(state: &state, parsedModifiers: modifiers)

    case .extension:
      if !(state.flags & .isParsingTopLevel) {
        state.diags.append(Diag(
          "extension declarations can only appear at top level", anchor: head?.range))
        state.hasError = true
      }
      return parseTypeExtn(state: &state, parsedModifiers: modifiers)

    case .namespace:
      if !(state.flags & (.isParsingTopLevel + .isParsingNamespace)) {
        state.diags.append(Diag(
          "namespace declarations can only appear at top level or in another namespace",
          anchor: head?.range))
        state.hasError = true
      }
      return parseNamespaceDecl(state: &state, parsedModifiers: modifiers)

    default:
      // We must commit to a failure, unless we didn't parse any modifier.
      if let modifier = modifiers.last {
        throw ParseError(
          Diag("expected declaration after \(modifier.kind)", anchor: state.errorRange()))
      } else {
        return nil
      }
    }
  }

  /// Parses a pattern binding declaration.
  ///
  ///     binding-decl ::= ('let' | 'var') NAME (':' sign)? ('=' expr)
  ///
  /// The next token is expected to be 'let' or 'var'.
  private func parseBindingDecl(
    state: inout State, parsedModifiers: [DeclModifier]
  ) -> PatternBindingDecl {
    let introducer = state.take(if: { $0.kind == .let || $0.kind == .var })!
    let lowerLoc = parsedModifiers.first?.range?.lowerBound ?? introducer.range.lowerBound
    var upperLoc = introducer.range.upperBound

    // Filter out modifiers that are not applicable in this context.
    let modifiers = parsedModifiers.filter({ (modifier: DeclModifier) -> Bool in
      switch modifier.kind {
      case .pub, .mod:
        guard !(state.flags & .isParsingViewBody) else {
          state.diags.append(Diag(
            "'\(modifier.kind)' cannot be applied to property requirement",
            anchor: modifier.range))
          state.hasError = true
          return false
        }
        return true

      case .static:
        guard state.flags & .isParsingTypeBody else {
          state.diags.append(Diag(
            "'static' is only allowed inside of a type", anchor: modifier.range))
          state.hasError = true
          return false
        }
        return true

      default:
        state.diags.append(Diag(
          "'\(modifier.kind)' cannot be applied to this declaration", anchor: modifier.range))
        state.hasError = true
        return false
      }
    })

    // Create the declaration.
    let decl = PatternBindingDecl(
      modifiers: modifiers,
      isMutable: introducer.kind == .var,
      pattern: WildcardPattern(type: unresolved, range: state.errorRange()),
      sign: nil,
      initializer: nil)
    decl.introRange = introducer.range
    decl.parentDeclSpace = state.declSpace

    if let pattern = parsePattern(state: &state) {
      decl.pattern = pattern
      upperLoc = pattern.range!.upperBound

      // Associate each introduced variable declaration to the new pattern binding declaration.
      for name in decl.pattern.namedPatterns {
        name.decl.isMutable = decl.isMutable
        name.decl.patternBindingDecl = decl
        decl.varDecls.append(name.decl)
      }
    } else {
      state.diags.append(Diag("expected pattern", anchor: state.errorRange()))
      state.hasError = true
    }

    if state.take(.colon) != nil {
      // We've consumed a colon, so we're committed to parse a type signature.
      if let sign = parseSign(state: &state) {
        decl.sign = sign
        upperLoc = sign.range!.upperBound
      } else {
        state.diags.append(Diag("expected type signature after ':'", anchor: state.errorRange()))
        state.hasError = true
      }
    }

    if let assign = state.take(.assign) {
      // We've consumed an assignment operator, so we're committed to parse an initializer.
      if let expr = parseExpr(state: &state) {
        decl.initializer = expr
        upperLoc = expr.range!.upperBound
      } else {
        state.diags.append(Diag("expected expression after '='", anchor: state.errorRange()))
        state.hasError = true
      }

      if state.flags & .isParsingViewBody {
        state.diags.append(Diag(
          "property requirement cannot have an initializer", anchor: assign.range))
        state.hasError = true
      }
    }

    decl.range = lowerLoc ..< upperLoc
    return decl
  }

  /// Parses a function declaration.
  ///
  ///     fun-decl ::= 'fun' fun-ident? generic-clause? capture-list? fun-interface brace-stmt?
  ///     fun-ident ::= operator | NAME
  ///     fun-interface ::= param-decl-list ('->' sign)?
  ///
  /// The next token is expected to be 'fun', 'new' or 'del'.
  private func parseFunDecl(
    state: inout State,
    parsedModifiers: [DeclModifier],
    requiringParamSign: Bool
  ) -> BaseFunDecl {
    let introducer = state.take(if: { $0.kind == .fun || $0.kind == .new || $0.kind == .del })!
    let lowerLoc = parsedModifiers.first?.range?.lowerBound ?? introducer.range.lowerBound
    var upperLoc = introducer.range.upperBound

    // Filter out the modifiers that are not applicable in this context.
    let modifiers = parsedModifiers.filter({ (modifier: DeclModifier) -> Bool in
      switch modifier.kind {
      case .pub, .mod:
        guard introducer.kind != .del else {
          state.diags.append(Diag(
            "'\(modifier.kind)' cannot be applied to destructor", anchor: modifier.range))
          state.hasError = true
          return false
        }
        guard !(state.flags & .isParsingViewBody) else {
          state.diags.append(Diag(
            "'\(modifier.kind)' cannot be applied to function requirement",
            anchor: modifier.range))
          state.hasError = true
          return false
        }
        return true

      case .consuming, .mut, .infix, .prefix, .postfix:
        guard introducer.kind == .fun,
              state.flags & .isParsingTypeBody,
              !parsedModifiers.contains(where: { $0.kind == .static })
        else {
          state.diags.append(Diag(
            "'\(modifier.kind) can only be applied to non-static member functions",
            anchor: modifier.range))
          state.hasError = true
          return false
        }

        if modifier.kind == .mut {
          guard !parsedModifiers.contains(where: { $0.kind == .consuming }) else {
            state.diags.append(Diag(
              "consuming member function cannot be mutating", anchor: modifier.range))
            state.hasError = true
            return false
          }
        }

        return true

      case .static where introducer.kind == .fun:
        guard state.flags & .isParsingTypeBody else {
          state.diags.append(Diag(
            "'static' is only allowed inside of a type", anchor: modifier.range))
          state.hasError = true
          return false
        }
        return true

      default:
        state.diags.append(Diag(
          "'\(modifier.kind)' cannot be applied to this declaration", anchor: modifier.range))
        state.hasError = true
        return false
      }
    })

    // Create the declaration.
    let decl: BaseFunDecl
    var isOperator = false

    switch introducer.kind {
    case .fun:
      decl = FunDecl(modifiers: modifiers, type: unresolved)
      decl.introRange = introducer.range

      if state.flags & .isParsingTypeBody {
        decl.props.insert(.isMember)
      }

      // The identifier of the function can be a name, or any non-cast operator.
      if let name = state.take(.name) {
        decl.ident = state.ident(name)
      } else if let oper = state.takeOperator() {
        decl.ident = oper
        isOperator = true

        if introducer.kind != .fun
            || !(state.flags & .isParsingTypeBody)
            || modifiers.contains(where: { $0.kind == .static })
        {
          state.diags.append(Diag(
            "operator '\(oper.name)' must be declared as a non-static member function",
            anchor: oper.range))
          state.hasError = true
        }
      } else {
        // The function is anonymous.
        decl.discriminator = state.nextDiscriminator()
      }

    case .new:
      decl = CtorDecl(modifiers: modifiers, type: unresolved)
      decl.introRange = introducer.range

      if !(state.flags & .isParsingTypeBody) {
        state.diags.append(Diag(
          "constructors are only allowed inside of a type", anchor: introducer.range))
        state.hasError = true
      }
      if let name = state.take(.name) {
        state.diags.append(Diag(
          "constructors cannot have a name", anchor: name.range))
        state.hasError = true
      }

    case .del:
      fatalError("not implemented")

    default:
      fatalError("unreachable")
    }

    decl.parentDeclSpace = state.declSpace
    state.declSpace = decl
    defer { state.declSpace = decl.parentDeclSpace }

    // Parse the generic clause.
    decl.genericClause = parseGenericClause(state: &state)

    // Parse the capture list.
    if let opener = state.peek(), opener.kind == .lBrack {
      decl.explicitCaptures = parseCaptureList(state: &state)!
      if state.flags & .isParsingTypeBody {
        state.diags.append(Diag(
          "capture lists are only allowed on free functions", anchor: opener.range))
        state.hasError = true
      }
    }

    // Parse the parameter list.
    if let (opener, params, closer) = list(
      state: &state,
      delimiters: (.lParen, .rParen),
      parser: { parseParamDecl(state: &$0, requiringSign: requiringParamSign) })
    {
      decl.params = params
      upperLoc = (closer?.range ?? params.last?.range ?? opener.range).upperBound

      if isOperator {
        switch params.count {
        case 0 where !modifiers.contains(where: { $0.kind == .prefix || $0.kind == .postfix }):
          state.diags.append(Diag(
            "unary operator declaration requires 'prefix' or 'postfix' modifier",
            anchor: introducer.range))
          state.hasError = true

        case 1:
          if let m = modifiers.first(where: { $0.kind == .prefix || $0.kind == .postfix }) {
            state.diags.append(Diag("unary operator must have 0 parameter", anchor: m.range))
            state.hasError = true
          } else if let m = modifiers.first(where: { $0.kind == .infix }) {
            state.diags.append(Diag(
              .warning,
              "extraneous modifier; binary operators are always infix",
              anchor: m.range))
          }

        default:
          state.diags.append(Diag(
            "operators must have 0 or 1 parameters", anchor: params[1].range))
          state.hasError = true
        }
      }
    } else {
      // We're missing the opening parenthesis of the parameter list. We'll pretend the user forgot
      // the parameters and try to recover before the arrow of the return type signature, or before
      // the opening brace of the function's body.
      state.diags.append(Diag("expected parameter list", anchor: state.errorRange()))
      state.hasError = true
      state.skip(while: {
        !$0.isOf(kind: [.semi, .lBrace]) && !$0.mayBeginDecl && !$0.mayBeginCtrlStmt
      })
    }

    if state.take(.arrow) != nil {
      // We've consumed an arrow operator, so we're committed to parse a type signature.
      if let sign = parseSign(state: &state) {
        decl.retSign = sign
        upperLoc = sign.range!.upperBound
      } else {
        state.diags.append(Diag(
          "expected return type signature after '->'", anchor: state.errorRange()))
        state.hasError = true
        state.skip(while: {
          !$0.isOf(kind: [.semi, .rBrace]) && !$0.mayBeginDecl && !$0.mayBeginCtrlStmt
        })
      }
    }

    let oldFlags = state.flags
    state.flags = .isParsingFunBody
    if let body = parseBraceStmt(state: &state) {
      decl.body = body
      upperLoc = body.range!.upperBound
    }
    state.flags = oldFlags

    // Function declarations must have a body, unless they are defined within a view.
    if decl.body == nil && !(state.flags & .isParsingViewBody) {
      state.diags.append(Diag("expected function body", anchor: state.errorRange()))
      state.hasError = true
    }

    decl.range = lowerLoc ..< upperLoc
    return decl
  }

  /// Parses a capture list.
  ///
  ///     capture-list ::= '[' capture-decl (',' capture-decl)* ','? ']'
  private func parseCaptureList(state: inout State) -> [CaptureDecl]? {
    guard state.take(.lBrack) != nil else { return nil }

    var list: [CaptureDecl] = []
    while let decl = parseCaptureDecl(state: &state) {
      list.append(decl)
      guard state.take(.comma) != nil else { break }
    }

    if state.take(.rBrack) == nil {
      state.diags.append(Diag("expected ']' delimiter", anchor: state.errorRange()))
      state.hasError = true
    }

    return list
  }

  /// Parses a capture declaration.
  ///
  ///     capture-decl ::= ('local' | 'mut' | 'consuming') NAME '=' expr
  private func parseCaptureDecl(state: inout State) -> CaptureDecl? {
    let policy: PassingPolicy
    switch state.peek()?.kind {
    case .local     : policy = .local
    case .mut       : policy = .inout
    case .consuming : policy = .consuming
    default:
      return nil
    }
    let introducer = state.take()!

    let ident: Ident
    if let name = state.take(.name) {
      ident = state.ident(name)
    } else {
      state.diags.append(Diag("expected identifier", anchor: state.errorRange()))
      state.hasError = true
      ident = Ident(name: "", range: state.errorRange())
    }

    if state.take(.assign) == nil {
      state.diags.append(Diag("expected '=' after capture identifier", anchor: state.errorRange()))
      state.hasError = true
    }

    let value: Expr
    if let expr = parseExpr(state: &state) {
      value = expr
    } else {
      value = ErrorExpr(type: context.errorType, range: introducer.range)
      state.diags.append(Diag("expected expression after '='", anchor: state.errorRange()))
      state.hasError = true
    }

    let decl = CaptureDecl(
      policy: policy,
      ident: ident,
      value: value,
      type: unresolved,
      range: introducer.range.lowerBound ..< ident.range!.upperBound)
    decl.parentDeclSpace = state.declSpace

    return decl
  }

  /// Parses a parameter declaration.
  ///
  ///     param-decl ::= (label | '_')? NAME ':' sign
  private func parseParamDecl(state: inout State, requiringSign: Bool) -> FunParamDecl? {
    // We assume that the first token corresponds to the parameter label. If the following token is
    // a name, then we can use it as the name. Owtherwise, we can use the first token for both the
    // label and name.
    guard let label = state.take(if: { $0.kind == .under || $0.isLabel }) else { return nil }
    let lowerLoc = label.range.lowerBound
    var upperLoc = label.range.upperBound

    // Create the declaration.
    let decl = FunParamDecl(name: "", policy: .local, type: unresolved)
    decl.parentDeclSpace = state.declSpace

    if let name = state.take(.name) {
      // We parsed a label *and* a name.
      decl.name = String(state.lexer.source[name.range])
      upperLoc = name.range.upperBound

      // Further, if the token for the label is '_', the parameter is anonymous.
      if label.kind != .under {
        decl.label = String(state.lexer.source[label.range])
      }

      // Warn against identical extraneous parameter names.
      if decl.name == decl.label {
        state.diags.append(Diag(.warning, "extraneous parameter name", anchor: name.range))
      }
    } else {
      // The next token is a colon: the label should be used as the name.
      decl.name = String(state.lexer.source[label.range])
      decl.label = decl.name

      // Require that the parameter name be a valid identifier.
      if label.kind != .name {
        state.diags.append(Diag(
          "'\(decl.name)' is not a valid parameter name", anchor: label.range))
        state.hasError = true
      }
    }

    // Parse the type signature of the parameter.
    if state.take(.colon) != nil, let sign = parseFunParamSign(state: &state) {
      decl.sign = sign
      decl.policy = sign.policy
      upperLoc = sign.range!.upperBound
    } else if requiringSign {
      state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
      state.hasError = true
    }

    decl.range = lowerLoc ..< upperLoc
    return decl
  }

  /// Parses a type declaration.
  ///
  /// The next token is expected to be 'type' or 'view'.
  private func parseTypeDecl(state: inout State, parsedModifiers: [DeclModifier]) -> TypeDecl {
    // All type declarations but views have similar prefixes. Thus, we must parse the "head" of the
    // declaration before we can instantiate a new declaration node. Unfortunately, that means that
    // we'll have to rebind the declaration space of the generic clause.
    let head = parseTypeDeclHead(state: &state)
    let lowerLoc = parsedModifiers.first?.range?.lowerBound ?? head.introducer.range.lowerBound
    var upperLoc = head.introducer.range.upperBound

    // Filter out the modifiers that are not applicable in this context.
    for modifier in parsedModifiers {
      switch modifier.kind {
      case .pub, .mod:
        continue

      default:
        state.diags.append(Diag(
          "'\(modifier.kind)' cannot be applied to this declaration", anchor: modifier.range))
        state.hasError = true
      }
    }

    // If the introducer was 'view', then we know what we're parsing already.
    if head.introducer.kind == .view {
      let decl = ViewTypeDecl(name: head.ident.name, type: unresolved)
      decl.parentDeclSpace = state.declSpace
      decl.inheritances = head.inheritances
      decl.type = context.viewType(decl: decl).kind

      if let clause = head.genericClause {
        state.diags.append(Diag(
          "view type declaration cannot have a generic clause, "
            + "declare abstract type requirements instead",
          anchor: clause.range))
        state.hasError = true
      }

      // Parse the members of the declaration.
      guard state.peek()?.kind == .lBrace else {
        state.diags.append(Diag("expected view body", anchor: state.errorRange()))
        state.hasError = true
        return decl
      }

      let before = (state.flags, state.declSpace)
      state.flags = .isParsingViewBody
      state.declSpace = decl
      defer { (state.flags, state.declSpace) = before }

      let body = parseDeclBody(state: &state)
      decl.members = body.members
      if let closer = body.closer {
        upperLoc = closer.range.upperBound
      }

      for case let member as TypeDecl in decl.members where !(member is AbstractTypeDecl) {
        state.diags.append(Diag(
          "type '\(member.name)' cannot be nested in view '\(decl.name)'", anchor: member.range))
        state.hasError = true
      }

      decl.range = lowerLoc ..< upperLoc
      return decl
    }

    // If the introducer was 'type', the next token determines what we're parsing.
    switch state.peek()?.kind {
    case .lBrace:
      // We're parsing a product type declaration.
      let decl = ProductTypeDecl(name: head.ident.name, type: unresolved)
      decl.parentDeclSpace = state.declSpace
      decl.inheritances = head.inheritances
      decl.type = context.productType(decl: decl).kind

      // Update the parent space of all type parameters of the generic clause.
      if let clause = head.genericClause {
        decl.genericClause = clause
        for param in clause.params {
          param.parentDeclSpace = decl
        }
      }

      // Parse the members of the declaration.
      let before = (state.flags, state.declSpace)
      state.flags = .isParsingProdBody
      state.declSpace = decl
      defer { (state.flags, state.declSpace) = before }

      let body = parseDeclBody(state: &state)
      decl.members = body.members
      if let closer = body.closer {
        upperLoc = closer.range.upperBound
      }

      for case let member as AbstractTypeDecl in decl.members {
        state.diags.append(Diag(
          "abstract types are only allowed inside of a view", anchor: member.range))
        state.hasError = true
      }

      decl.range = lowerLoc ..< upperLoc
      return decl

    case .assign:
      // We're parsing a type alias declaration.
      _ = state.take()

      let sign = parseSign(state: &state) ?? { () -> Sign in
        state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
        state.hasError = true
        return ErrorSign(type: context.errorType, range: state.errorRange())
      }()
      upperLoc = sign.range!.upperBound

      let decl = AliasTypeDecl(name: head.ident.name, aliasedSign: sign, type: unresolved)
      decl.parentDeclSpace = state.declSpace
      decl.inheritances = head.inheritances

      // Update the parent space of all type parameters of the generic clause.
      if let clause = head.genericClause {
        decl.genericClause = clause
        for param in clause.params {
          param.parentDeclSpace = decl
        }
      }

      decl.range = lowerLoc ..< upperLoc
      return decl

    default:
      // We're most likely parsing an asbtract type declaration.
      let decl = AbstractTypeDecl(name: head.ident.name, type: unresolved)
      decl.parentDeclSpace = state.declSpace
      decl.inheritances = head.inheritances
      decl.type = context.genericParamType(decl: decl).kind
      upperLoc = head.ident.range!.upperBound

      if let clause = head.genericClause {
        state.diags.append(Diag(
          "abstract type declaration cannot have a generic clause", anchor: clause.range))
        state.hasError = true
      }

      if state.take(.where) != nil {
        // We've consumed 'where', so we're committed to parse a type requirements.
        var mustParse = false
        while let typeReq = parseTypeReq(state: &state) {
          mustParse = false
          decl.typeReqs.append(typeReq)

          // If we consume a comma, then we're committed to parse another requirement.
          if state.take(.comma) != nil {
            mustParse = true
          } else {
            break
          }
        }

        if mustParse {
          state.diags.append(Diag("expected type requirement", anchor: state.errorRange()))
          state.hasError = true
          state.skip(while: { $0.kind == .comma })
        } else {
          upperLoc = decl.typeReqs.last!.range!.upperBound
        }
      }

      decl.range = lowerLoc ..< upperLoc
      return decl
    }
  }

  /// Parses the head of a type or view declaration.
  ///
  ///     type-decl-head ::= 'type' NAME generic-clause? inheritance-list?
  ///     view-decl-head ::= 'view' NAME inheritance-list?
  ///
  /// The next token is expected to be 'type' or 'view'.
  private func parseTypeDeclHead(state: inout State) -> TypeDeclHead {
    let introducer = state.take(if: { $0.isOf(kind: [.type, .view]) })!

    let ident: Ident
    if let name = state.take(.name) {
      ident = state.ident(name)
    } else {
      state.diags.append(Diag("expected type identifier", anchor: state.errorRange()))
      state.hasError = true
      ident = Ident(name: "", range: state.errorRange())
    }

    switch ident.name {
    case "Any", "Kind", "Nothing", "Unit":
      state.diags.append(Diag(
        "'\(ident.name)' is a reserved type identifier", anchor: ident.range))
      state.hasError = true
    default:
      break
    }

    let genericClause = parseGenericClause(state: &state)
    let inheritances = state.peek()?.kind == .colon
      ? parseInheritanceList(state: &state)
      : []

    return TypeDeclHead(
      introducer: introducer,
      ident: ident,
      genericClause: genericClause,
      inheritances: inheritances)
  }

  /// Parses an inheritance list.
  ///
  ///     inheritance-list ::= ':' ident-sign (',' ident-sign)*
  ///
  /// The next token is expected to be ':'.
  private func parseInheritanceList(state: inout State) -> [IdentSign] {
    _ = state.take(.colon)!
    var inheritances: [IdentSign] = []

    // We've consumed a colon, so we're committed to parse an inheritance list.
    var mustParse = true
    while let sign = parseSign(state: &state) {
      mustParse = false

      // The signature must designate an identifier. We can't inherit from a structural type.
      if let identSign = sign as? IdentSign {
        inheritances.append(identSign)
      } else {
        state.diags.append(Diag("expected type identifier", anchor: sign.range))
        state.hasError = true
      }

      // If we consume a comma, then we're committed to parse another signature.
      if state.take(.comma) != nil {
        mustParse = true
      } else {
        break
      }
    }

    if mustParse {
      state.diags.append(Diag("expected type identifier", anchor: state.errorRange()))
      state.hasError = true
      state.skip(while: { $0.kind == .comma })
    }

    return inheritances
  }

  /// Parses the body of a product type or view declaration.
  ///
  ///     decl-body ::= '{' (decl ';'*)* ';'* '}'
  ///
  /// The next token is expected to be '{'. The caller is expected to set the appropriate
  /// parser flags (e.g., `isParsingProdBody`).
  private func parseDeclBody(state: inout State) -> (members: [Decl], closer: Token?) {
    _ = state.take(.lBrace)!

    var members: [Decl] = []
    while state.peek() != nil {
      // Skip leading semicolons and exit the loop once we've found a closing brace.
      state.skip(while: { $0.kind == .semi })
      if let closer = state.take(.rBrace) {
        return (members, closer)
      }

      // Parse a member.
      do {
        guard let member = try parseDecl(state: &state) else {
          throw ParseError(Diag("expected declaration", anchor: state.errorRange()))
        }
        members.append(member)
        if let decl = member as? FunDecl, decl.ident == nil {
          state.diags.append(Diag(
            .warning, "anonymous member function can never be called", anchor: decl.introRange))
        }
      } catch let error as ParseError {
        state.diags.append(error.diag)
        state.hasError = true
        state.skip(while: { !$0.isOf(kind: [.semi, .rBrace]) && !$0.mayBeginDecl })
      } catch {
        fatalError("unreachable")
      }
    }

    return (members, nil)
  }

  /// Parses a type extension.
  ///
  ///     type-extn-decl ::= 'extn' compound-ident-sign decl-body
  ///
  /// The next token is expected to be 'extension'.
  private func parseTypeExtn(state: inout State, parsedModifiers: [DeclModifier]) -> TypeExtnDecl {
    let introducer = state.take(.extension)!
    let lowerLoc = parsedModifiers.first?.range?.lowerBound ?? introducer.range.lowerBound
    var upperLoc = introducer.range.upperBound

    for modifier in parsedModifiers {
      state.diags.append(Diag(
        "'\(modifier.kind)' cannot be applied to this declaration", anchor: modifier.range))
      state.hasError = true
    }

    let identSign: IdentSign
    switch parseSign(state: &state) {
    case let i as IdentSign:
      identSign = i

    case .some(let sign):
      identSign = BareIdentSign(ident: Ident(name: "", range: sign.range), type: context.errorType)

    case nil:
      state.diags.append(Diag("expected type identifier", anchor: state.errorRange()))
      state.hasError = true
      identSign = BareIdentSign(
        ident: Ident(name: "", range: state.errorRange()),
        type: context.errorType)
    }

    let inheritances = state.peek()?.kind == .colon
      ? parseInheritanceList(state: &state)
      : []

    // Create the declaration.
    let decl = TypeExtnDecl(extendedIdent: identSign, members: [])
    decl.parentDeclSpace = state.declSpace
    decl.inheritances = inheritances

    // Parse the members of the declaration.
    let before = (state.flags, state.declSpace)
    state.flags = .isParsingExtnBody
    state.declSpace = decl
    defer { (state.flags, state.declSpace) = before }

    let body = parseDeclBody(state: &state)
    decl.members = body.members
    if let closer = body.closer {
      upperLoc = closer.range.upperBound
    }

    decl.range = lowerLoc ..< upperLoc
    return decl
  }

  /// Parses a namespace.
  ///
  ///     namespace-decl ::= 'namespace' ident decl-body
  ///
  /// The next token is expected to be 'namespace'.
  private func parseNamespaceDecl(
    state: inout State,
    parsedModifiers: [DeclModifier]
  ) -> NamespaceDecl {
    let introducer = state.take(.namespace)!
    let lowerLoc = parsedModifiers.first?.range?.lowerBound ?? introducer.range.lowerBound
    var upperLoc = introducer.range.upperBound

    for modifier in parsedModifiers {
      state.diags.append(Diag(
        "'\(modifier.kind)' cannot be applied to this declaration", anchor: modifier.range))
      state.hasError = true
    }

    // Parse the name of the declaration.
    let ident: Ident
    if let name = state.take(.name) {
      ident = state.ident(name)
    } else {
      state.diags.append(Diag("expected namespace identifier", anchor: state.errorRange()))
      state.hasError = true
      ident = Ident(name: "", range: state.errorRange())
    }

    switch ident.name {
    case "Any", "Unit", "Nothing":
      state.diags.append(Diag(
        "'\(ident.name)' is a reserved type identifier", anchor: ident.range))
      state.hasError = true
    default:
      break
    }

    // Create the declaration.
    let decl = NamespaceDecl(name: ident.name, decls: [], context: context)
    decl.parentDeclSpace = state.declSpace

    // Parse the members of the declaration.
    let before = (state.flags, state.declSpace)
    state.flags = .isParsingNamespace
    state.declSpace = decl
    defer { (state.flags, state.declSpace) = before }

    let body = parseDeclBody(state: &state)
    decl.decls = body.members
    if let closer = body.closer {
      upperLoc = closer.range.upperBound
    }

    decl.range = lowerLoc ..< upperLoc
    return decl
  }

  /// Parses a generic clause.
  ///
  ///     generic-clause ::= '<' generic-param-list type-req-clause? '>'
  ///     generic-param-list ::= NAME (',' NAME)* ','?
  ///     type-req-clause ::= 'where' type-req (',' type-req)* ','?
  private func parseGenericClause(state: inout State) -> GenericClause? {
    guard let opener = state.take(.lAngle) else { return nil }

    let clause = GenericClause(params: [], typeReqs: [])

    // Parse a list of generic parameter names.
    while let token = state.take(.name) {
      let decl = GenericParamDecl(name: String(state.lexer.source[token.range]), type: unresolved)
      decl.range = token.range
      decl.parentDeclSpace = state.declSpace
      decl.type = context.genericParamType(decl: decl).kind
      clause.params.append(decl)

      guard state.take(.comma) != nil else { break }
    }

    // Parse a type requirement clause.
    if state.take(.where) != nil {
      while let typeReq = parseTypeReq(state: &state) {
        clause.typeReqs.append(typeReq)
        guard state.take(.comma) != nil else { break }
      }
    }

    let closer = state.take(.rAngle)
    if closer == nil {
      state.diags.append(Diag("expected '>' delimiter", anchor: state.errorRange()))
      state.hasError = true
    }

    let upperLoc = closer?.range.upperBound
      ?? clause.typeReqs.last?.range?.upperBound
      ?? clause.params.last?.range?.upperBound
      ?? opener.range.upperBound
    clause.range = opener.range.lowerBound ..< upperLoc
    return clause
  }

  /// Parses a type requirement.
  ///
  ///     type-req ::= same-req | conf-req
  ///     same-req ::= compound-ident-sign '==' sign
  ///     conf-req ::= compound-ident-sign ':' sign
  private func parseTypeReq(state: inout State) -> TypeReq? {
    guard let lhs = parseCompoundIdentSign(state: &state) else { return nil }

    // Handle failures to parse a type identifier.
    let identSign = (lhs as? IdentSign) ?? BareIdentSign(
      ident: Ident(name: "", range: lhs.range),
      type: context.errorType)

    // Identify the type of requirement we're parsing.
    let kind: TypeReq.Kind

    let oper = state.peek()
    switch oper?.kind {
    case .oper where state.lexer.source[oper!.range] == "==":
      _ = state.take()
      kind = .equality

    case .colon:
      _ = state.take()
      kind = .conformance

    default:
      state.diags.append(Diag(
        "expected '==' or ':' to specify a type requirement", anchor: state.errorRange()))
      state.hasError = true
      kind = .equality
    }

    // Parse the right operand of the requirement.
    let rhs = parseSign(state: &state) ?? { () -> Sign in
      state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
      state.hasError = true
      return ErrorSign(type: context.errorType, range: state.errorRange())
    }()

    return TypeReq(kind: kind, lhs: identSign, rhs: rhs, range: lhs.range! ..< rhs.range!)
  }

  /// Parses a control statement.
  private func parseCtrlStmt(state: inout State) -> Stmt? {
    switch state.peek()?.kind {
    case .break:
      return parseBreakStmt(state: &state)
    case .continue:
      return parseContinueStmt(state: &state)
    case .if:
      return parseIfStmt(state: &state)
    case .return:
      return parseRetStmt(state: &state)
    case .for:
      fatalError("not implemented")
    case .while:
      fatalError("not implemented")
    case .lBrace:
      return parseBraceStmt(state: &state)
    default:
      return nil
    }
  }

  /// Parses a break statement.
  ///
  ///     break-stmt ::= 'break' NAME?
  private func parseBreakStmt(state: inout State) -> /*Break*/Stmt? {
    guard let opener = state.take(.break) else { return nil }

    if !(state.flags & .isParsingLoopBody) {
      state.diags.append(Diag("'break' is only allowed inside of a loop", anchor: opener.range))
      state.hasError = true
    }

    fatalError("not implemented")
  }

  /// Parses a continue statement.
  ///
  ///     break-stmt ::= 'continue' NAME?
  private func parseContinueStmt(state: inout State) -> /*Continue*/Stmt? {
    guard let opener = state.take(.continue) else { return nil }

    if !(state.flags & .isParsingLoopBody) {
      state.diags.append(Diag("'continue' is only allowed inside of a loop", anchor: opener.range))
      state.hasError = true
    }

    fatalError("not implemented")
  }

  /// Parses a conditional statement.
  ///
  ///     if-stmt ::= 'if' expr brace-stmt ('else' brace-stmt | if-stmt)?
  private func parseIfStmt(state: inout State) -> IfStmt? {
    guard let opener = state.take(.if) else { return nil }

    let condition: Expr
    if let c = parseExpr(state: &state) {
      condition = c
    } else {
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      condition = ErrorExpr(type: context.errorType, range: state.errorRange())
    }

    let thenBody: BraceStmt
    if let b = parseBraceStmt(state: &state) {
      thenBody = b
    } else {
      state.diags.append(Diag("expected '{' after condition", anchor: state.errorRange()))
      state.hasError = true
      thenBody = BraceStmt(statements: [], range: state.errorRange())
    }

    let elseBody: Stmt?
    let endLocation: SourceRange.Bound
    if state.take(.else) != nil {
      if let b = parseIfStmt(state: &state) {
        elseBody = b
      } else if let b = parseBraceStmt(state: &state) {
        elseBody = b
      } else {
        state.diags.append(Diag("expected '{' after 'else'", anchor: state.errorRange()))
        state.hasError = true
        elseBody = BraceStmt(statements: [], range: state.errorRange())
      }
      endLocation = elseBody!.range!.upperBound
    } else {
      elseBody = nil
      endLocation = thenBody.range!.upperBound
    }

    return IfStmt(
      condition: condition,
      thenBody: thenBody,
      elseBody: elseBody,
      range: opener.range.lowerBound ..< endLocation)
  }

  /// Parses a return statement.
  ///
  ///     return-stmt ::= 'return' expr?
  private func parseRetStmt(state: inout State) -> RetStmt? {
    guard let opener = state.take(.return) else { return nil }
    let stmt = RetStmt(value: nil, range: opener.range)

    if let expr = parseExpr(state: &state) {
      stmt.value = expr
      stmt.range = opener.range ..< expr.range!
    }

    if state.flags & .isParsingFunBody {
      stmt.funDecl = state.declSpace?.spacesUpToRoot.first(as: BaseFunDecl.self)
    } else {
      state.diags.append(Diag(
        "'return' is only allowed inside of a function", anchor: opener.range))
      state.hasError = true
    }

    return stmt
  }

  /// Parses a brace statement.
  ///
  ///     brace-stmt ::= '{' (stmt ';'*)* ';'* '}'
  private func parseBraceStmt(state: inout State) -> BraceStmt? {
    guard let opener = state.take(.lBrace) else { return nil }

    let scope = BraceStmt(statements: [], range: opener.range)
    scope.parentDeclSpace = state.declSpace
    state.declSpace = scope
    defer { state.declSpace = scope.parentDeclSpace }

    loop:while true {
      // Skip leading semicolons and exit the loop once we've found a closing brace.
      state.skip(while: { $0.kind == .semi })
      if let closer = state.take(.rBrace) {
        scope.range = opener.range ..< closer.range
        return scope
      }

      guard let head = state.peek() else {
        state.diags.append(Diag("expected '}' delimiter", anchor: state.errorRange()))
        state.hasError = true
        return scope
      }

      do {
        if head.mayBeginDecl {
          guard let decl = try parseDecl(state: &state) else {
            throw ParseError(Diag("expected declaration", anchor: state.errorRange()))
          }
          scope.stmts.append(decl)
          if let decl = decl as? FunDecl, decl.ident == nil {
            state.diags.append(Diag(
              .warning, "anonymous function can never be called", anchor: decl.introRange))
          }
        } else if head.mayBeginCtrlStmt {
          guard let stmt = parseCtrlStmt(state: &state) else {
            throw ParseError(Diag("expected statement", anchor: state.errorRange()))
          }
          scope.stmts.append(stmt)
        } else {
          guard let expr = parseExpr(state: &state) else {
            throw ParseError(Diag("expected expression", anchor: state.errorRange()))
          }
          scope.stmts.append(expr)

          // If a match or conditional expression is parsed at the top-level of a brace statement,
          // then it should be considered as a statement rather than an expression.
          if let match = expr as? MatchExpr {
            match.isSubexpr = false
          }
        }
      } catch let error as ParseError {
        state.diags.append(error.diag)
        state.hasError = true
        state.skip(while: {
          !$0.isOf(kind: [.semi, .rBrace]) && !$0.mayBeginDecl && !$0.mayBeginCtrlStmt
        })
      } catch {
        fatalError("unreachable")
      }
    }
  }

  /// Parses an expression.
  ///
  ///     expr ::= prefix-expr binary*
  ///     binary ::= (operator | NAME) prefix-expr
  private func parseExpr(state: inout State) -> Expr? {
    guard var head = parsePrefixExpr(state: &state) else { return nil }
    var tree = InfixTree.leaf(.expr(head))

    // Parse a sequence of binary suffixes.
    var upperLoc = head.range!.upperBound
    while true {
      let oper: Ident
      let group: PrecedenceGroup?

      if let ident = state.takeOperator() {
        // The operator is a standard non-cast infix operator,.
        oper = ident
        group = PrecedenceGroup(for: ident.name) ?? {
          state.diags.append(Diag("unknown infix operator '\(oper.name)'", anchor: oper.range))
          return nil
        }()
      } else if let name = state.peek(), name.kind == .name {
        // The next token might be an infix identifier. However, we require that it be on the same
        // line as the left operand, to to avoid any ambiguity with statements that may begin with
        // a name on the next line.
        let gap = state.lexer.source[upperLoc ..< name.range.lowerBound]
        guard !gap.contains(where: { $0.isNewline }) else { break }

        _ = state.take()
        oper = state.ident(name)
        group = .identifier
      } else if let token = state.take(.cast) {
        // The right operand of a cast operator is a type signature, not an expression.
        guard let rhs = parseSign(state: &state) else {
          state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
          state.hasError = true
          return ErrorExpr(type: context.errorType, range: head.range)
        }

        // Append the new operator/operand to the tree.
        upperLoc = rhs.range!.upperBound
        tree.append(oper: state.ident(token), group: .casting, rhs: .sign(rhs))
        continue
      } else {
        // No binary suffix; we're done.
        break
      }

      // Parse the right operand.
      guard let rhs = parsePrefixExpr(state: &state) else {
        state.diags.append(Diag("expected right operand", anchor: state.errorRange()))
        state.hasError = true
        return ErrorExpr(type: context.errorType, range: head.range)
      }

      // Check that spacing around the operator is consistent.
      let lGap = upperLoc < oper.range!.lowerBound
      let rGap = upperLoc < oper.range!.lowerBound
      if lGap != rGap {
        state.diags.append(Diag("inconsistent spacing around infix operator", anchor: oper.range))
        state.hasError = true
      }

      // Append the new operator/operand to the tree.
      if let group = group {
        upperLoc = rhs.range!.upperBound
        tree.append(oper: oper, group: group, rhs: .expr(rhs))
      } else {
        head = ErrorExpr(type: context.errorType, range: head.range! ..< rhs.range!)
        tree = InfixTree.leaf(.expr(head))
      }
    }

    return tree.flattened()
  }

  /// Parses compound expression, optionally preceeded by a prefix operator.
  ///
  ///     prefix-expr ::= prefix-oper? compound-expr
  private func parsePrefixExpr(state: inout State) -> Expr? {
    // Assign and cast operators are never infix.
    guard let oper = state.takeOperator(includingAssign: false) else {
      // No prefix operator; just parse a compound expression.
      return parseCompoundExpr(state: &state)
    }

    // Parse the operand.
    guard let value = parseCompoundExpr(state: &state) else {
      state.diags.append(Diag("expected operand expression", anchor: state.errorRange()))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: oper.range)
    }

    // Prefix operators cannot be separated from their operand.
    if oper.range!.upperBound != value.range!.lowerBound {
      state.diags.append(Diag(
        "prefix operator cannot be separated from its operand", anchor: oper.range))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: oper.range! ..< value.range!)
    }

    // If the operator is '&', then return an AddrOfExpr; otherwise, return a prefix call.
    if oper.name == "&" {
      return AddrOfExpr(value: value, type: unresolved, range: oper.range! ..< value.range!)
    } else {
      let fun = UnresolvedMemberExpr(base: value, ident: oper, type: unresolved, range: oper.range)
      return CallExpr.prefix(fun: fun, type: unresolved, range: oper.range! ..< value.range!)
    }
  }

  /// Parses a compound expression.
  ///
  ///     compound-expr ::= primary-expr (call-args | subscript | member)*
  ///     call-args ::= list['(', call-arg, ')']
  ///     subscript ::= list['[', call-arg, ']']
  ///     member ::= '.' (label | oper | INTEGER)
  private func parseCompoundExpr(state: inout State) -> Expr? {
    guard var base = parsePrimaryExpr(state: &state) else { return nil }
    let lowerLoc = base.range!.lowerBound

    // Parse a sequence of primary suffixes.
    while let token = state.peek() {
      switch token.kind {
      case .lParen:
        // We require call suffixes to start on the same line, to avoid any ambiguity with
        // statements that may begin with a left parenthesis on the next line.
        let gap = state.lexer.source[base.range!.upperBound ..< token.range.lowerBound]
        guard !gap.contains(where: { $0.isNewline }) else { return base }

        // Parse an argument list.
        let (opener, elems, closer) = list(
          state: &state, delimiters: (.lParen, .rParen), parser: parseTupleExprElem(state:))!

        // Create a call expression.
        base = CallExpr(
          fun: base,
          args: elems,
          notation: .standard,
          type: unresolved,
          range: lowerLoc ..< (closer?.range ?? elems.last?.range ?? opener.range).upperBound)

      case .lBrack:
        fatalError("not implemented")

      case .dot:
        // Consume the dot token.
        _ = state.take()

        // Parse the member identifier or index.
        if let name = state.take(if: { $0.isLabel }) {
          let expr = UnresolvedMemberExpr(base: base, ident: state.ident(name), type: unresolved)
          expr.range = lowerLoc ..< expr.ident.range!.upperBound
          base = expr
        } else if let oper = state.takeOperator() {
          let expr = UnresolvedMemberExpr(base: base, ident: oper, type: unresolved)
          expr.range = lowerLoc ..< oper.range!.upperBound
          base = expr
        } else if let index = state.take(.int) {
          let value = state.lexer.source[index.range]

          guard !value.contains(where: { !$0.isDigit }), let i = Int(value) else {
            state.diags.append(Diag("'\(value)' is not a valid tuple index", anchor: index.range))
            state.hasError = true
            return ErrorExpr(type: context.errorType, range: lowerLoc ..< index.range.upperBound)
          }

          let expr = TupleMemberExpr(base: base, memberIndex: i, type: unresolved)
          expr.range = lowerLoc ..< index.range.upperBound
          base = expr
        } else {
          state.diags.append(Diag("expected member identifier", anchor: state.errorRange()))
          state.hasError = true
          return ErrorExpr(type: context.errorType, range: lowerLoc ..< token.range.upperBound)
        }

      case _ where token.isOperator:
        // Attempt to consume a postfix.
        let backup = state.save()
        guard let oper = state.takeOperator(includingAssign: false) else {
          return base
        }

        // Postfix operators must be attached to the expression, and followed by a whitespace.
        let lGap = base.range!.upperBound != oper.range!.lowerBound
        let rGap = (oper.range!.upperBound.index == state.lexer.source.endIndex)
          || state.lexer.source[oper.range!.upperBound.index].isWhitespace
        guard !lGap && rGap else {
          state.restore(backup)
          return base
        }

        let fun = UnresolvedMemberExpr(
          base: base, ident: oper, type: unresolved, range: oper.range)
        base = CallExpr.postfix(
          fun: fun, type: unresolved, range: lowerLoc ..< oper.range!.upperBound)

      default:
        return base
      }
    }

    return base
  }

  /// Parses a primary expression.
  private func parsePrimaryExpr(state: inout State) -> Expr? {
    switch state.peek()?.kind {
    case .bool:
      return parseBoolLiteral(state: &state)
    case .int:
      return parseIntLiteral(state: &state)
    case .float:
      return parseFloatLiteral(state: &state)
    case .string:
      return parseStringLiteral(state: &state)
    case .name:
      return parseDeclRefExpr(state: &state)
    case .dot:
      fatalError("not implemented")
    case .fun:
      return parseLambdaExpr(state: &state)
    case .async:
      return parseAsyncExpr(state: &state)
    case .await:
      return parseAwaitExpr(state: &state)
    case .match:
      return parseMatchExpr(state: &state)
    case .under:
      return parseWildcardExpr(state: &state)
    case .lParen:
      // We might be parsing a tuple expression or a kind reference prefixed by a parenthesized
      // signature. Assume the latter first.
      if let expr = attempt(state: &state, { (state) -> AttemptResult<Expr> in
        let opener = state.take()!
        guard
          let sign = parseSign(state: &state),
          state.take(.rParen) != nil,
          state.take(.twoColons) != nil
        else { return .failure }

        // We've parsed a parenthesized signature followed by '::'. we should commit to parsing a
        // qualified declaration reference.
        guard let ident = parseIdent(state: &state) else {
          state.diags.append(Diag("expected identifier", anchor: state.errorRange()))
          state.hasError = true
          return .success(ErrorExpr(type: context.errorType, range: sign.range))
        }

        return .success(UnresolvedQualDeclRefExpr(
          namespace: sign, ident: ident, type: unresolved, range: opener.range ..< ident.range!))
      }) {
        return expr
      } else {
        return parseTupleExpr(state: &state)
      }

    default:
      return nil
    }
  }

  /// Parses a Boolean literal.
  private func parseBoolLiteral(state: inout State) -> Expr? {
    guard let token = state.take(.bool) else { return nil }
    let value = state.lexer.source[token.range] == "true"
    return BoolLiteralExpr(value: value, type: unresolved, range: token.range)
  }

  /// Parses an integer number literal.
  private func parseIntLiteral(state: inout State) -> Expr? {
    guard let token = state.take(.int) else { return nil }

    let literal = state.lexer.source[token.range]
    guard let value = Int(literal) else {
      state.diags.append(Diag("invalid integer literal '\(literal)'", anchor: token.range))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: token.range)
    }

    return IntLiteralExpr(value: value, type: unresolved, range: token.range)
  }

  /// Parses a floating-point number literal.
  private func parseFloatLiteral(state: inout State) -> Expr? {
    guard let token = state.take(.float) else { return nil }

    let literal = state.lexer.source[token.range]
    guard let value = Double(literal) else {
      state.diags.append(Diag("invalid floating point literal '\(literal)'", anchor: token.range))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: token.range)
    }

    return FloatLiteralExpr(value: value, type: unresolved, range: token.range)
  }

  /// Parses a string literal.
  ///
  ///     string-literal ::= STRING+
  private func parseStringLiteral(state: inout State) -> Expr? {
    guard let first = state.take(.string) else { return nil }

    var tokens = [first]
    while let token = state.take(.string) {
      tokens.append(token)
    }

    let value = tokens.isEmpty
      ? String(state.lexer.source[tokens[0].range])
      : tokens.reduce(into: "", { $0 += String(state.lexer.source[$1.range]) })
    return StringLiteralExpr(
      value: value, type: unresolved, range: tokens.first!.range ..< tokens.last!.range)
  }

  /// Parses a declaration reference.
  ///
  ///     decl-ref ::= (sign '::')? ident
  private func parseDeclRefExpr(state: inout State) -> Expr? {
    if let sign = parsePrimarySign(state: &state) {
      // We parsed a type signature. If the next token is '::', then the whole signature is a
      // namespace qualifying some declaration reference. Otherwise, the declaration reference
      // might have been parsed with the signature.
      if state.take(.twoColons) == nil {
        switch sign {
        case let sign as BareIdentSign:
          return UnresolvedDeclRefExpr(ident: sign.ident, type: unresolved)

        case let sign as CompoundIdentSign where sign.lastComponent is BareIdentSign:
          let namespace = CompoundIdentSign.create(sign.components.dropLast())
          let last = sign.lastComponent as! BareIdentSign
          return UnresolvedQualDeclRefExpr(
            namespace: namespace, ident: last.ident, type: unresolved, range: sign.range)

        default:
          state.diags.append(Diag(
            "expected '::' after type signature", anchor: state.errorRange()))
          state.hasError = true
          return ErrorExpr(type: context.errorType, range: sign.range)
        }
      }

      // We've just parse the qualifying namespace. We're expecting a name or an operator.
      guard let ident = parseIdent(state: &state) else {
        state.diags.append(Diag("expected identifier", anchor: state.errorRange()))
        state.hasError = true
        return ErrorExpr(type: context.errorType, range: sign.range)
      }

      return UnresolvedQualDeclRefExpr(
        namespace: sign, ident: ident, type: unresolved, range: sign.range! ..< ident.range!)
    } else {
      // We didn't parse a type signature. We're expecting a name.
      guard let name = state.take(.name) else { return nil }
      return UnresolvedDeclRefExpr(ident: state.ident(name), type: unresolved)
    }
  }

  /// Parses an identifier.
  private func parseIdent(state: inout State) -> Ident? {
    if let name = state.take(.name) {
      return state.ident(name)
    } else {
      return state.takeOperator()
    }
  }

  /// Parses a lambda expression.
  ///
  ///     lambda-expr ::= 'fun' capture-list? fun-interface brace-stmt
  private func parseLambdaExpr(state: inout State) -> Expr? {
    guard state.peek()?.kind == .fun else { return nil }

    // Parse a standard function declaration. The result must be a FunDecl since we peeked `fun`.
    let decl = parseFunDecl(
      state: &state, parsedModifiers: [], requiringParamSign: false) as! FunDecl

    // Check that the parsed declaration is valid for a lambda.
    if let ident = decl.ident {
      state.diags.append(Diag("lambda functions must be anonymous", anchor: ident.range))
      state.hasError = true
    }
    if let clause = decl.genericClause {
      state.diags.append(Diag("lambda functions cannot be generic", anchor: clause.range))
      state.hasError = true
    }

    return LambdaExpr(decl: decl, type: unresolved, range: decl.range)
  }

  /// Parses an async expression.
  ///
  ///     async-expr ::= 'async' capture-list? ('->' sign)? (expr | brace-stmt)
  private func parseAsyncExpr(state: inout State) -> Expr? {
    guard let introducer = state.take(.async) else { return nil }

    // Create the implicit function declaration representing the body of the async expression.
    let decl = FunDecl(type: unresolved)
    decl.introRange = introducer.range
    decl.discriminator = state.nextDiscriminator()

    decl.parentDeclSpace = state.declSpace
    state.declSpace = decl
    defer { state.declSpace = decl.parentDeclSpace }

    // Parse the capture list.
    if let opener = state.peek(), opener.kind == .lBrack {
      decl.explicitCaptures = parseCaptureList(state: &state)!
      if state.flags & .isParsingTypeBody {
        state.diags.append(Diag(
          "capture lists are only allowed on free functions", anchor: opener.range))
        state.hasError = true
      }
    }

    // Parse an explicit type annotation.
    if state.take(.arrow) != nil {
      if let sign = parseSign(state: &state) {
        decl.retSign = sign
      } else {
        state.diags.append(Diag("expected type signature after '->'", anchor: state.errorRange()))
        state.hasError = true
      }
    }

    // The body of the async expression can be either a brace statement (just like the body of a
    // function) or a single expression, in which case we synthesize a return statement.
    let oldFlags = state.flags
    state.flags = .isParsingFunBody
    if let body = parseBraceStmt(state: &state) {
      decl.body = body
      decl.range = body.range
    } else if let value = parseExpr(state: &state) {
      let ret = RetStmt(value: value, range: value.range)
      ret.funDecl = decl

      decl.body = BraceStmt(statements: [ret], range: value.range)
      decl.body!.parentDeclSpace = decl
      decl.range = value.range
    }
    state.flags = oldFlags

    if decl.body == nil {
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: introducer.range)
    }

    return AsyncExpr(body: decl, type: unresolved, range: introducer.range ..< decl.range!)
  }

  /// Parses an await expression.
  ///
  ///     await-expr ::= 'await' expr
  private func parseAwaitExpr(state: inout State) -> Expr? {
    guard let introducer = state.take(.await) else { return nil }

    guard let value = parseExpr(state: &state) else {
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: introducer.range)
    }

    return AwaitExpr(value: value, type: unresolved, range: introducer.range ..< value.range!)
  }

  /// Parses a match expression.
  ///
  ///     match-expr ::= 'match' expr '{' match-case* '}'
  private func parseMatchExpr(state: inout State) -> Expr? {
    guard let introducer = state.take(.match) else { return nil }

    let subject = parseExpr(state: &state) ?? { () -> Expr in
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      return ErrorExpr(type: context.errorType, range: state.errorRange())
    }()

    let expr = MatchExpr(
      isSubexpr: true, subject: subject, cases: [], type: unresolved, range: introducer.range)

    guard state.take(.lBrace) != nil else {
      state.diags.append(Diag(
        "expected '{' after match subject expression", anchor: state.errorRange()))
      state.hasError = true
      return expr
    }

    while let case_ = parseMatchCase(state: &state) {
      expr.cases.append(case_)
    }

    if let closer = state.take(.rBrace) {
      expr.range = introducer.range.lowerBound ..< closer.range.upperBound
    } else {
      state.diags.append(Diag("expected '}' delimiter", anchor: state.errorRange()))
      state.hasError = true
    }

    return expr
  }

  /// Parses a case in a match expression.
  ///
  ///     match-case ::= 'case' pattern ('where' expr)? brace-stmt
  private func parseMatchCase(state: inout State) -> MatchCaseStmt? {
    guard let introducer = state.take(.case) else { return nil }

    let pattern = parsePattern(state: &state) ?? { () -> Pattern in
      state.diags.append(Diag("expected pattern", anchor: state.errorRange()))
      state.hasError = true
      return WildcardPattern(type: context.errorType, range: state.errorRange())
    }()

    let condition: Expr?
    if state.take(.where) != nil {
      condition = parseExpr(state: &state) ?? {
        state.diags.append(Diag("expected condition", anchor: state.errorRange()))
        state.hasError = true
        return nil
      }()
    } else {
      condition = nil
    }

    let body = parseBraceStmt(state: &state) ?? { () -> BraceStmt in
      state.diags.append(Diag("expected '{' after case pattern", anchor: state.errorRange()))
      state.hasError = true
      return BraceStmt(statements: [], range: state.errorRange())
    }()

    let stmt = MatchCaseStmt(pattern: pattern, condition: condition, body: body)
    stmt.range = introducer.range.lowerBound ..< body.range!.upperBound
    stmt.parentDeclSpace = state.declSpace
    body.parentDeclSpace = stmt
    return stmt
  }

  /// Parses a tuple expression.
  ///
  ///     tuple-expr ::= list['(', tuple-expr-elem, ')']
  private func parseTupleExpr(state: inout State) -> Expr? {
    guard let (opener, elems, closer) = list(
            state: &state, delimiters: (.lParen, .rParen), parser: parseTupleExprElem(state:))
    else { return nil }

    let expr = TupleExpr(elems: elems, type: unresolved)
    expr.range = opener.range ..< (closer?.range ?? elems.last?.range ?? opener.range)
    return expr
  }

  /// Parses a single element in a tuple expression.
  ///
  ///     tuple-expr-elem ::= (label ':')? expr
  private func parseTupleExprElem(state: inout State) -> TupleElem? {
    guard let opener = state.peek() else { return nil }
    let backup = state.save()
    let label: String?

    // Attempt to consume a label.
    if let labelToken = state.take(if: { $0.isLabel }), state.take(.colon) != nil {
      // We successfully parsed a label; we must parse an expression.
      label = String(state.lexer.source[labelToken.range])
    } else {
      // We failed to parse a label; backtrack.
      state.restore(backup)
      label = nil
    }

    let value: Expr
    if let expr = parseExpr(state: &state) {
      value = expr
    } else if label != nil {
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      value = ErrorExpr(type: context.errorType, range: state.errorRange())
    } else {
      // We don't have to commit to a failure, since we didn't consume anything.
      return nil
    }

    var elem = TupleElem(label: label, value: value)
    elem.range = opener.range.lowerBound ..< value.range!.upperBound
    return elem
  }

  /// Parses a wildcard expression.
  private func parseWildcardExpr(state: inout State) -> Expr? {
    guard let token = state.take(.under) else { return nil }
    return WildcardExpr(type: unresolved, range: token.range)
  }

  /// Parses a pattern.
  private func parsePattern(state: inout State) -> Pattern? {
    switch state.peek()?.kind {
    case .name:
      return parseNamedPattern(state: &state)
    case .var, .let:
      return parseBindingPattern(state: &state)
    case .lParen:
      return parseTuplePattern(state: &state)
    case .under:
      return parseWildcardPattern(state: &state)
    default:
      return nil
    }
  }

  /// Parses a named pattern.
  private func parseNamedPattern(state: inout State) -> Pattern? {
    guard let token = state.take(.name) else { return nil }

    // Create a variable declaration for the pattern.
    let decl = VarDecl(ident: state.ident(token), type: unresolved)
    decl.range = token.range
    decl.parentDeclSpace = state.declSpace

    // Create the pattern.
    return NamedPattern(decl: decl, type: unresolved, range: token.range)
  }

  /// Parses a binding pattern.
  ///
  ///     binding-pattern ::= ('let' | 'var') pattern (':' sign)?
  private func parseBindingPattern(state: inout State) -> Pattern? {
    guard let introducer = state.take(if: { $0.isOf(kind: [.let, .var]) }) else { return nil }

    // Commit to parse a pattern.
    guard let subpattern = parsePattern(state: &state) else {
      state.diags.append(Diag("expected pattern", anchor: state.errorRange()))
      state.hasError = true
      return WildcardPattern(type: unresolved, range: introducer.range)
    }

    let pattern = BindingPattern(
      isMutable: introducer.kind == .var,
      subpattern: subpattern,
      sign: nil,
      type: unresolved)
    pattern.range = introducer.range.lowerBound ..< subpattern.range!.upperBound
    pattern.introRange = introducer.range

    // Attempt to parse a type signature.
    if state.take(.colon) != nil {
      if let sign = parseSign(state: &state) {
        pattern.sign = sign
      } else {
        // We must commit to a failure.
        state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
        state.hasError = true
      }
    }

    return pattern
  }

  /// Parses a tuple pattern.
  ///
  ///     tuple-pattern ::= list['(', tuple-pattern-elem, ')']
  private func parseTuplePattern(state: inout State) -> Pattern? {
    guard let (opener, elems, closer) = list(
            state: &state, delimiters: (.lParen, .rParen), parser: parseTuplePatternElem(state:))
    else { return nil }

    let pattern = TuplePattern(elems: elems, type: unresolved)
    pattern.range = opener.range ..< (closer?.range ?? elems.last?.range ?? opener.range)
    return pattern
  }

  /// Parses a single element in a tuple pattern.
  ///
  ///     tuple-pattern-elem ::= (label ':')? expr
  private func parseTuplePatternElem(state: inout State) -> TuplePattern.Elem? {
    guard let opener = state.peek() else { return nil }
    let backup = state.save()
    let label: String?

    // Attempt to consume a label.
    if let labelToken = state.take(if: { $0.isLabel }), state.take(.colon) != nil {
      // We successfully parsed a label; we must parse a subpattern.
      label = String(state.lexer.source[labelToken.range])
    } else {
      // We failed to parse a label; backtrack.
      state.restore(backup)
      label = nil
    }

    let subpattern: Pattern
    if let pat = parsePattern(state: &state) {
      subpattern = pat
    } else if label != nil {
      state.diags.append(Diag("expected expression", anchor: state.errorRange()))
      state.hasError = true
      subpattern = WildcardPattern(type: context.errorType, range: state.errorRange())
    } else {
      // We don't have to commit to a failure, since we didn't consume anything.
      return nil
    }

    var elem = TuplePattern.Elem(label: label, pattern: subpattern)
    elem.range = opener.range.lowerBound ..< subpattern.range!.upperBound
    return elem
  }

  /// Parses a wildcard pattern.
  private func parseWildcardPattern(state: inout State) -> Pattern? {
    guard let token = state.take(.under) else { return nil }
    return WildcardPattern(type: unresolved, range: token.range)
  }

  /// Parses a type signature.
  ///
  ///     sign ::= type-modifier* (async-sign | fun-sign)
  ///     fun-sign ::= (sign -> sign) | ('volatile' tuple-sign '->' sign)
  ///
  /// This parser produces an instance of `FunParamSign` only if the parsed signature is prefixed
  /// by a parameter type modifier. Otherwise, it returns a raw signature.
  ///
  /// Use `parseFunParamSign` instead if you are expecteding to parse a parameter type signature.
  private func parseSign(state: inout State) -> Sign? {
    guard let opener = state.peek() else { return nil }
    var lastModifier: Token?

    // Parse type modifiers.
    var modifiers: [Token.Kind: Token] = [:]
    while let token = state.take(if: { $0.isOf(kind: [.mut, .consuming, .volatile]) }) {
      guard modifiers[token.kind] == nil else {
        state.diags.append(Diag(
          .warning, "ignoring duplicate modifier \(token.kind)", anchor: token.range))
        continue
      }

      // To avoid ambuiguities, 'volatile' must immediately preceed a function signature whose
      // domain is enclosed in parentheses.
      if token.kind == .volatile {
        if state.peek()?.kind != .lParen {
          state.diags.append(Diag(
            .warning,
            "volatile function signature requires parentheses",
            anchor: state.errorRange()))
        }
        break
      }

      modifiers[token.kind] = token
      lastModifier = token
    }

    // Parse an async signature.
    guard var base = parseAsyncSign(state: &state) else {
      // We must commit to a failure, unless we didn't parse any modifier.
      if let m = lastModifier {
        state.diags.append(Diag(
          "expected type signature after \(m.kind)", anchor: state.errorRange()))
        state.hasError = true
        return ErrorSign(type: context.errorType, range: opener.range ..< m.range)
      } else {
        return nil
      }
    }

    // If the next token is an arrow, we're parsing a function signature.
    if let arrow = state.take(.arrow) {
      guard let retSign = parseSign(state: &state) else {
        state.diags.append(Diag("expected type signature after '->'", anchor: state.errorRange()))
        state.hasError = true
        return ErrorSign(type : context.errorType, range: opener.range ..< arrow.range)
      }

      // If we parsed a tuple signature on the LHS of the arrow, we have to transform each element
      // into a parameter signature, reusing the element's label. Otherwise, the whole signature
      // that is a interpreted as a single parameter.
      var params: [FunParamSign] = []
      if let tuple = base as? TupleSign {
        params = tuple.elems.map({ (elem) -> FunParamSign in
          let sign = FunParamSign(
            label: elem.label, rawSign: elem.sign, type: unresolved, range: elem.range)
          if let s = elem.sign as? FunParamSign {
            sign.policy = s.policy
            sign.rawSign = s.rawSign
          }
          return sign
        })
      } else {
        params = [FunParamSign(rawSign: base, type: unresolved, range: base.range)]
      }

      let sign = FunSign(params: params, retSign: retSign, type: unresolved)
      sign.isVolatile = modifiers[.volatile] != nil
      sign.range = opener.range ..< retSign.range!
      base = sign
    } else if let m = modifiers[.volatile] {
      // If we're not parsing a function signature, then 'volatile' is illegal.
      state.diags.append(Diag(
        "'volatile' is only allowed on function signatures", anchor: m.range))
      state.hasError = true
      return ErrorSign(type: context.errorType, range: base.range)
    }

    // If we parsed a 'mut' or 'consuming' modifier, the signature denotes a function parameter.
    if modifiers[.mut] != nil || modifiers[.consuming] != nil {
      let policy: PassingPolicy
      if modifiers[.consuming] == nil {
        policy = .inout
      } else if modifiers[.mut] == nil {
        policy = .consuming
      } else {
        state.diags.append(Diag(
          "consuming parameter cannot be mutating", anchor: modifiers[.mut]!.range))
        policy = .consuming
      }
      base = FunParamSign(policy: policy, rawSign: base, type: unresolved, range: base.range)
    }

    return base
  }

  /// Parses a parameter signature.
  ///
  ///     fun-param-sign ::= 'mut'? 'consuming'? sign
  private func parseFunParamSign(state: inout State) -> FunParamSign? {
    guard let sign = parseSign(state: &state) else { return nil }
    if let s = sign as? FunParamSign {
      return s
    } else {
      return FunParamSign(rawSign: sign, type: sign.type, range: sign.range)
    }
  }

  /// Parses a maxterm signature, optionally prefix by an async modifier.
  ///
  ///     async-sign ::= 'async'? maxterm-sign
  private func parseAsyncSign(state: inout State) -> Sign? {
    // Attempt to consume an asnyc modifier.
    if let modifier = state.take(.async) {
      // Commit to parse the base of the signature.
      guard let base = parseSign(state: &state) else {
        state.diags.append(Diag(
          "expected type signature after 'async'", anchor: state.errorRange()))
        state.hasError = true
        return ErrorSign(type: context.errorType, range: modifier.range)
      }

      let sign = AsyncSign( base: base, type: unresolved)
      sign.modifierRange = modifier.range
      sign.range = modifier.range.lowerBound ..< base.range!.upperBound
      return sign
    }

    // Parse a maxterm signature.
    return parseMaxtermSign(state: &state)
  }

  /// Parses a maxterm signature, i.e., a disjunction of signatures.
  ///
  ///     maxterm-sign ::= minterm-sign ('|' minterm-sign)*
  private func parseMaxtermSign(state: inout State) -> Sign? {
    let items = parseTermSign(state: &state, oper: "|", element: parseMintermSign(state:))
    switch items.count {
    case 0:
      return nil

    case 1:
      return items[0]

    default:
      let sign = UnionSign(elems: items, type: unresolved)
      sign.range = items.first!.range!.lowerBound ..< items.last!.range!.upperBound
      return sign
    }
  }

  /// Parses a minterm signature, i.e., a conjunction of signatures.
  ///
  ///     minterm-sign ::= primary-sign ('&' primary-sign)*
  private func parseMintermSign(state: inout State) -> Sign? {
    let items = parseTermSign(state: &state, oper: "&", element: parsePrimarySign(state:))
    switch items.count {
    case 0:
      return nil

    case 1:
      return items[0]

    default:
      let sign = ViewCompSign(views: items, type: unresolved)
      sign.range = items.first!.range!.lowerBound ..< items.last!.range!.upperBound
      return sign
    }
  }

  /// Parses a minterm or a maxterm signature.
  private func parseTermSign(
    state: inout State,
    oper: String,
    element: (inout State) -> Sign?
  ) -> [Sign] {
    guard let first = element(&state) else { return [] }
    var items = [first]

    // Attempt to parse an operator and its operand.
    while let token = state.peek(),
          (token.kind == .oper) && (state.lexer.source[token.range] == oper)
    {
      _ = state.take()

      guard let sign = element(&state) else {
        state.diags.append(Diag("expected type signature", anchor: state.errorRange()))
        state.hasError = true
        items = [ErrorSign(type: context.errorType, range: items[0].range! ..< items.last!.range!)]
        continue
      }

      items.append(sign)
    }

    return items
  }

  /// Parses a primary signature.
  ///
  ///     primary-sign ::= compound-ident-sign | tuple-sign
  private func parsePrimarySign(state: inout State) -> Sign? {
    switch state.peek()?.kind {
    case .name:
      return parseCompoundIdentSign(state: &state)
    case .lParen:
      return parseTupleSign(state: &state)
    default:
      return nil
    }
  }

  /// Parses a compound type identifier.
  ///
  ///     compound-ident-sign ::= ident-comp-sign ('::' ident-comp-sign)*
  private func parseCompoundIdentSign(state: inout State) -> Sign? {
    guard let first = parseIdentCompSign(state: &state) else { return nil }
    var comps = [first]

    while let sep = state.peek(), sep.kind == .twoColons {
      _ = state.take()
      guard let sign = parseIdentCompSign(state: &state) else {
        state.diags.append(Diag("expected type identifier", anchor: state.errorRange()))
        state.hasError = true
        return ErrorSign(type: context.errorType, range: comps[0].range! ..< comps.last!.range!)
      }
      comps.append(sign)
    }

    return CompoundIdentSign.create(comps)
  }

  /// Parses an unqualified type identifier.
  ///
  ///     ident-comp-sign ::= NAME generic-args?
  ///     generic-args ::= list['<', sign, '>']
  private func parseIdentCompSign(state: inout State) -> IdentCompSign? {
    guard let token = state.take(.name) else { return nil }
    let ident = state.ident(token)

    // Attempt to parse a list of generic arguments.
    if let (opener, args, closer) = list(
        state: &state, delimiters: (.lAngle, .rAngle), parser: { parseSign(state: &$0) })
    {
      // Return a specialized identifier.
      let sign = SpecializedIdentSign(ident: ident, args: args, type : unresolved)
      sign.range = opener.range ..< (closer?.range ?? args.last?.range ?? opener.range)
      return sign
    }

    // Return a bare identifier.
    return BareIdentSign(ident: ident, type : unresolved)
  }

  /// Parses a tuple signature.
  ///
  ///     tuple-sign ::= list['(', tuple-sign-elem, ')']
  private func parseTupleSign(state: inout State) -> Sign? {
    guard let (opener, elems, closer) = list(
      state: &state, delimiters: (.lParen, .rParen), parser: tupleSignElem(state:))
    else { return nil }

    let sign = TupleSign(elems: elems, type: unresolved)
    sign.range = opener.range ..< (closer?.range ?? elems.last?.range ?? opener.range)
    return sign
  }

  /// Parses a single element in a tuple signature.
  ///
  ///     tuple-sign-elem ::= (label ':')? sign
  private func tupleSignElem(state: inout State) -> TupleSign.Elem? {
    guard let opener = state.peek() else { return nil }
    let backup = state.save()
    let label: String?

    // Attempt to consume a label.
    if let labelToken = state.take(if: { $0.isLabel }), state.take(.colon) != nil {
      // We successfully parsed a label; we must parse a signature.
      label = String(state.lexer.source[labelToken.range])
    } else {
      // We failed to parse a label; backtrack.
      state.restore(backup)
      label = nil
    }

    let sign: Sign
    if let s = parseSign(state: &state) {
      sign = s
    } else {
      // We must commit to a failure, unless we didn't parse a label.
      if label != nil {
        state.diags.append(Diag("expected expression", anchor: state.errorRange()))
        state.hasError = true
        sign = ErrorSign(type: context.errorType, range: state.errorRange())
      } else {
        return nil
      }
    }

    var elem = TupleSign.Elem(label: label, sign: sign)
    elem.range = opener.range.lowerBound ..< sign.range!.upperBound
    return elem
  }

  /// Parses a comma-separated list of elements, delimited by the specified tokens.
  ///
  /// - Parameters:
  ///   - state: The parser's state.
  ///   - delimiters: The kind of the opening and closing delimiter tokens.
  ///   - parser: A closure that accepts the parser's state and parses a single element.
  private func list<T>(
    state: inout State,
    delimiters: (left: Token.Kind, right: Token.Kind),
    parser: (inout State) -> T?
  ) -> (opener: Token, elems: [T], closer: Token?)? {
    // Consume the left delimiter.
    guard let opener = state.take(delimiters.left) else { return nil }

    // Consume the list's elements.
    var elems: [T] = []
    while let elem = parser(&state) {
      elems.append(elem)
      guard state.take(.comma) != nil else { break }
    }

    // Consume the right delimiter.
    var closer = state.take(delimiters.right)
    if closer == nil {
      if let token = state.take(.comma) {
        state.diags.append(Diag("unexpected ',' separator", anchor: token.range))
        state.hasError = true
      } else {
        state.diags.append(Diag(
          "expected '\(delimiters.right)' delimiter", anchor: state.errorRange()))
        state.hasError = true
      }

      // Recover at a delimiter.
      state.skip(while: { !$0.isOf(kind: [delimiters.right, .rBrace, .semi]) })
      closer = state.take(delimiters.right)
    }

    return (opener, elems, closer)
  }

  /// Attempts to execute the given closure and backtracks if it fails.
  ///
  /// - Parameters:
  ///   - state: The parser's state.
  ///   - action: A closure that performs some action with the parser's state.
  private func attempt<T>(state: inout State, _ action: (inout State) -> AttemptResult<T>) -> T? {
    // Save the current parser state.
    let backup = state.save()

    switch action(&state) {
    case .success(let result):
      return result

    case .failure:
      // If the action failed, restore the saved state.
      state.restore(backup)
      return nil
    }
  }

}

/// The head of a type declaration.
fileprivate struct TypeDeclHead {

  /// The token introducing the declation.
  let introducer: Token

  /// The declaration's identifier.
  let ident: Ident

  /// The generic clause of the declaration.
  let genericClause: GenericClause?

  /// The inheritances of the declaration.
  let inheritances: [Sign]

}

/// A helper data structure to parse infix expressions.
///
/// Infix expressions are parsed as a sequence of operators and operands. This data structure is
/// used to insert every element of the sequence in a tree that encodes operator precedrence and
/// associativity.
fileprivate enum InfixTree {

  /// An expression or a type signature operand.
  enum Operand {
    case expr(Expr)
    case sign(Sign)
  }

  /// A single expression.
  case leaf(Operand)

  /// An infix call.
  indirect case node(oper: Ident, group: PrecedenceGroup, left: InfixTree, right: InfixTree)

  /// Appends the specified operator and operand to the tree.
  mutating func append(oper: Ident, group: PrecedenceGroup, rhs: Operand) {
    switch self {
    case .leaf:
      self = .node(oper: oper, group: group, left: self, right: .leaf(rhs))

    case .node(let loper, let lgroup, let left, var right):
      if (lgroup.weight < group.weight)
          || (lgroup.weight == group.weight && group.associativity == .right)
      {
        // Either the new operator binds more strongly, or it has the same weight and it is
        // right-associative.
        right.append(oper: oper, group: group, rhs: rhs)
        self = .node(oper: loper, group: lgroup, left: left, right: right)
      } else {
        // Either the new operator binds more weakly, or it has the same weight and it is
        // left-associative.
        self = .node(oper: oper, group: group, left: self, right: .leaf(rhs))
      }
    }
  }

  /// Flattens the tree as a single expression, transforming each node into an infix call.
  func flattened() -> Expr {
    switch self {
    case .leaf(.expr(let expr)):
      return expr

    case .node(let oper, _, let left, let right):
      let lhs = left.flattened()
      let unresolved = lhs.type.context.unresolvedType

      switch oper.name {
      case "=":
        let rhs = right.flattened()
        let expr = AssignExpr(lvalue: lhs, rvalue: rhs)
        expr.range = lhs.range!.lowerBound ..< rhs.range!.upperBound
        return expr

      case "as":
        guard case .leaf(.sign(let rhs)) = right else { fatalError("unreachable") }
        let expr = StaticCastExpr(value: lhs, sign: rhs, type: unresolved)
        expr.range = lhs.range!.lowerBound ..< rhs.range!.upperBound
        return expr

      case "as!":
        guard case .leaf(.sign(let rhs)) = right else { fatalError("unreachable") }
        let expr = RuntimeCastExpr(value: lhs, sign: rhs, type: unresolved)
        expr.range = lhs.range!.lowerBound ..< rhs.range!.upperBound
        return expr

      case "as!!":
        guard case .leaf(.sign(let rhs)) = right else { fatalError("unreachable") }
        let expr = PointerCastExpr(value: lhs, sign: rhs, type: unresolved)
        expr.range = lhs.range!.lowerBound ..< rhs.range!.upperBound
        return expr

      default:
        let rhs = right.flattened()
        let fun = UnresolvedMemberExpr(base: lhs, ident: oper, type: unresolved, range: oper.range)
        let expr = CallExpr.infix(fun: fun, operand: rhs, type: unresolved)
        expr.range = lhs.range!.lowerBound ..< rhs.range!.upperBound
        return expr
      }

    case .leaf(.sign):
      preconditionFailure("cannot convert type signature to expression")
    }
  }

}

/// A parse error.
fileprivate struct ParseError: Error {

  public init(_ diag: Diag) {
    self.diag = diag
  }

  public let diag: Diag

}

fileprivate func ..< (lhs: SourceRange, rhs: SourceRange) -> SourceRange {
  return lhs.lowerBound ..< rhs.upperBound
}

fileprivate extension String {

  subscript(range: SourceRange) -> Substring {
    return self[range.lowerBound.index ..< range.upperBound.index]
  }

}
