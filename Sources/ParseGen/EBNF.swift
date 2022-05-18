import CitronLexerModule

enum EBNF {
  struct Token: EBNFNode, Hashable {
    typealias ID = EBNFParser.CitronTokenCode

    init(_ id: ID, _ content: Substring, at position: SourceRegion) {
      self.id = id
      self.text = content
      self.position = position
    }

    let id: ID
    let text: Substring
    let position: SourceRegion

    var dump: String { String(text) }
  }

  typealias RuleList = [Rule]
  struct Rule: EBNFNode {
    enum Kind { case plain, token, oneOf, regexp }
    let kind: Kind
    let lhs: Symbol
    let rhs: AltList
  }

  typealias AltList = [Alt]
  typealias Alt = TermList
  typealias TermList = [Term]
  typealias Symbol = Token
  
  enum Term: EBNFNode {
    case group(AltList)
    case symbol(Symbol)
    case literal(String, position: SourceRegion)
    case regexp(Substring, position: SourceRegion)
    indirect case quantified(Term, Character, position: SourceRegion)
  }
}

extension EBNF.Token: CustomStringConvertible {
  var description: String {
    "Token(.\(id), \(String(reflecting: text)), at: \(String(reflecting: position)))"
  }
}


/// An EBNFNode node.
protocol EBNFNode {
  /// The region of source parsed as this node.
  var position: SourceRegion { get }

  /// A string representation in the original syntax.
  var dump: String { get }
}


extension Array: EBNFNode where Element: EBNFNode {
  var position: SourceRegion {
    first != nil ? first!.position...last!.position : .empty
  }
  var dump: String {
    self.lazy.map { $0.dump }.joined(separator: Self.dumpSeparator)
  }
  static var dumpSeparator: String {
    return Element.self == EBNF.Rule.self ? "\n\n"
      : Element.self == EBNF.Alt.self ? "\n  | "
      : " "
  }
}

extension Optional: EBNFNode where Wrapped: EBNFNode {
  var position: SourceRegion {
    self?.position ?? .empty
  }
  var dump: String { self?.dump ?? "" }
}

extension EBNF.Rule {
  var position: SourceRegion { lhs.position...rhs.position }
  var dump: String {
    """
    \(position): note: rule
    \(lhs.dump) ::=\(rhs.count > 1 ? "\n    " : " ")\(rhs.dump)
    """
  }
}

extension EBNF.Term {
  var position: SourceRegion {
    switch self {
    case .group(let g): return g.position
    case .symbol(let s): return s.position
    case .regexp(_, let p): return p
    case .literal(_, let p): return p
    case .quantified(_, _, let p): return p
    }
  }
  var dump: String {
    switch self {
    case .group(let g): return "( \(g.dump) )"
    case .symbol(let s): return s.dump
    case .literal(let s, _):
      return "'\(s.replacingOccurrences(of: "'", with: "\\'"))'"
    case .regexp(let s, _): return "/\(s)/"
    case .quantified(let t, let q, _): return t.dump + String(q)
    }
  }
}
