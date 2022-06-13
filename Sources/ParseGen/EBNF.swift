import CitronLexerModule
import Utils

enum EBNF {
  typealias Error = EBNFError

  struct Token: EBNFNode {
    typealias ID = EBNFParser.CitronTokenCode

    init(_ id: ID, _ content: String, at position: SourceRegion) {
      self.id = id
      self.text = content
      self.position_ = .init(position)
    }

    let id: ID
    let text: String
    let position_: Incidental<SourceRegion>

    var position: SourceRegion { position_.value }

    func dumped(level: Int) -> String { text }
  }

  struct Symbol: EBNFNode {
    init(_ t: Token) {
      precondition(t.id == .SYMBOL_NAME || t.id == .LHS)
      self.init(t.text, at: t.position)
    }
    init(_ content: String, at position: SourceRegion) {
      self.text = content
      self.position_ = .init(position)
    }

    let text: String
    let position_: Incidental<SourceRegion>
    var position: SourceRegion { position_.value }

    func dumped(level: Int) -> String { text }
  }

  typealias DefinitionList = [Definition]
  struct Definition: EBNFNode {
    enum Kind { case plain, token, oneOf, regexp, noNewline, noWhitespace }
    let kind: Kind
    let lhs: Symbol
    let alternatives: AlternativeList
  }

  typealias AlternativeList = [Alternative]
  typealias Alternative = TermList
  typealias TermList = [Term]

  enum Term: EBNFNode {
    case group(AlternativeList)
    case symbol(Symbol)
    case literal(String, position: Incidental<SourceRegion>)
    case regexp(String, position: Incidental<SourceRegion>)
    indirect case quantified(Term, Character, position: Incidental<SourceRegion>)
  }
}

extension EBNF.Token: CustomStringConvertible {
  var description: String {
    "Token(.\(id), \(String(reflecting: text)), at: \(String(reflecting: position)))"
  }
}

extension EBNF.Symbol: CustomStringConvertible {
  var description: String {
    "Symbol(\(String(reflecting: text)), at: \(String(reflecting: position)))"
  }
}


/// An EBNFNode node.
protocol EBNFNode: Hashable {
  /// The region of source parsed as this node.
  var position: SourceRegion { get }

  /// A string representation in the original syntax.
  func dumped(level: Int)-> String
}

extension EBNFNode {
  var dump: String { dumped(level: 0) }
}

extension Array: EBNFNode where Element: EBNFNode {
  var position: SourceRegion {
    first != nil ? first!.position...last!.position : .empty
  }

  func dumped(level: Int) -> String {
    self.lazy.map { $0.dumped(level: level + 1) }
      .joined(separator: Self.dumpSeparator(level: level))
  }

  static func dumpSeparator(level: Int) -> String {
    return Element.self == EBNF.Definition.self ? "\n\n"
      : Element.self == EBNF.Alternative.self ? (level == 0 ? "\n  " : " | ")
      : " "
  }
}

extension Optional: EBNFNode where Wrapped: EBNFNode {
  var position: SourceRegion {
    self?.position ?? .empty
  }
  func dumped(level: Int) -> String { self?.dumped(level: level + 1) ?? "" }
}

extension EBNF.Definition {
  var position: SourceRegion { lhs.position...alternatives.position }
  func dumped(level: Int) -> String {
    let k = [.oneOf: " (one of)", .token: " (token)", .regexp: " (regexp)"][kind]

    return """
    \(position): note: rule
    \(lhs.dump) ::=\(k ?? "")
      \(alternatives.dump)
    """
  }
}

extension EBNF.Term {
  var position: SourceRegion {
    switch self {
    case .group(let g): return g.position
    case .symbol(let s): return s.position
    case .regexp(_, let p): return p.value
    case .literal(_, let p): return p.value
    case .quantified(_, _, let p): return p.value
    }
  }
  func dumped(level: Int) -> String {
    switch self {
    case .group(let g): return "( \(g.dumped(level: level + 1)) )"
    case .symbol(let s): return s.dumped(level: level)
    case .literal(let s, _):
      return "'\(s.replacingOccurrences(of: "'", with: "\\'"))'"
    case .regexp(let s, _): return "/\(s)/"
    case .quantified(let t, let q, _): return t.dumped(level: level + 1) + String(q)
    }
  }
}

