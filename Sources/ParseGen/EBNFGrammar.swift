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
    var errors: Set<Error> = []

    for d in definitions.values {
      validate(d.alternatives)
    }

    func validate(_ x: EBNF.Token) {
      if definitions[x.text] == nil {
        errors.insert(Error("undefined symbol '\(x.text)'", at: x.position))
      }
    }

    func validate(_ x: EBNF.AlternativeList) {
      for rhs in x { validate(rhs) }
    }

    func validate(_ x: Alternative) {
      for rhs in x { validate(rhs) }
    }

    func validate(_ x: Term) {
      switch x {
      case .group(let g): validate(g)
      case .symbol(let s): validate(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): validate(t)
      }
    }
    if !errors.isEmpty { throw errors.sorted { $0.site.span.lowerBound < $1.site.span.lowerBound } }
  }
}
