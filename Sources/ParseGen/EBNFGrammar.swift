extension EBNF {
  struct Grammar {
    typealias Token = EBNF.Token
    typealias Error = EBNF.Error
    typealias Alternative = EBNF.Alternative
    typealias AlternativeList = EBNF.AlternativeList
    typealias Term = EBNF.Term
    typealias Symbol = String
    let definitions: [Symbol: Definition]
    let start: Token
  }
}

extension EBNF.Grammar {
  init(_ definitions: [EBNF.Definition], start: Symbol) throws {
    try self.definitions = Dictionary(
      definitions.lazy.map {(key: $0.lhs.text, value: $0)}
    ) { a, b in
      throw Error(
        "Duplicate symbol definition", at: b.position,
        notes: [.init(message: "First definition", site: a.position)])
    }

    guard let d = self.definitions[start] else {
      throw Error(
        "Start symbol \(start) not defined\n\(self.definitions)",
        at: definitions.position)
    }
    self.start = d.lhs

    try validate()
  }

  func validate() throws {
    for d in definitions.values {
      try validate(d.alternatives)
    }

    func validate(_ x: EBNF.Token) throws {
      if definitions[x.text] == nil { throw Error("undefined symbol", at: x.position) }
    }

    func validate(_ x: EBNF.AlternativeList) throws {
      for rhs in x { try validate(rhs) }
    }

    func validate(_ x: Alternative) throws {
      for rhs in x { try validate(rhs) }
    }

    func validate(_ x: Term) throws {
      switch x {
      case .group(let g): try validate(g)
      case .symbol(let s): try validate(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): try validate(t)
      }
    }
  }

}
