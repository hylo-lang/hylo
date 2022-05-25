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
    var reachable: Set<Symbol> = []

    for d in definitions.values {
      checkDefined(d.alternatives)
    }

    reach(start)

    for d in definitions.values {
      if !reachable.contains(d.lhs.text) {
        errors.insert(
          Error("Symbol '\(d.lhs.text)' is unreachable from start symbol '\(start.text)'",
                at: d.lhs.position))
      }
    }

    func reach(_ x: Token) {
      if reachable.insert(x.text).inserted {
        reach(definitions[x.text]!.alternatives)
      }
    }

    func reach(_ x: EBNF.AlternativeList) {
      for a in x {
        for t in a { reach(t) }
      }
    }

    func reach(_ x: Term) {
      switch x {
      case .group(let g): reach(g)
      case .symbol(let s): reach(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): reach(t)
      }
    }

    func checkDefined(_ x: EBNF.Token) {
      if definitions[x.text] == nil {
        errors.insert(Error("undefined symbol '\(x.text)'", at: x.position))
      }
    }

    func checkDefined(_ x: EBNF.AlternativeList) {
      for a in x {
        for t in a { checkDefined(t) }
      }
    }

    func checkDefined(_ x: Term) {
      switch x {
      case .group(let g): checkDefined(g)
      case .symbol(let s): checkDefined(s)
      case .regexp(_, _): do {}
      case .literal(_, _): do {}
      case .quantified(let t, _, _): checkDefined(t)
      }
    }
    if !errors.isEmpty { throw errors.sorted { $0.site.span.lowerBound < $1.site.span.lowerBound } }
  }
}
