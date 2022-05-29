import Marpa
import CitronLexerModule

struct Parser {
  let grammar: Marpa.Grammar
  let recognizer: Marpa.Recognizer
  let unrecognizedToken: Marpa.Symbol
  let scanner: Scanner<Marpa.Symbol>
  let symbolName: [Marpa.Symbol: String]
  let ruleLocation: [Marpa.Rule: SourceRegion]

  init(
    grammar: Marpa.Grammar,
    unrecognizedToken: Marpa.Symbol,
    scanner: Scanner<Marpa.Symbol>,
    symbolName: [Marpa.Symbol: String],
    ruleLocation: [Marpa.Rule: SourceRegion]
  ) {
    (self.grammar, self.unrecognizedToken, self.scanner, self.symbolName, self.ruleLocation)
      = (grammar, unrecognizedToken, scanner, symbolName, ruleLocation)
    recognizer = Recognizer(grammar)
  }

  func dumpGrammar() {
    for r in grammar.rules {
      print("\(ruleLocation[r]!): note:", description(r))
    }
  }

  func description(_ r: Marpa.Rule, dotPosition: Int? = nil) -> String {
    let lhsName = symbolName[grammar.lhs(r)]!
    let rhsNames = grammar.rhs(r).lazy.map { s in symbolName[s]! }
    guard let n = dotPosition else {
      return "\(lhsName) -> \(rhsNames.joined(separator: " "))"
    }
    let dottedRHS = rhsNames.prefix(n) + ["•"] + rhsNames.dropFirst(n)
    return "\(lhsName) -> \(dottedRHS.joined(separator: " "))"
  }

  func progressReport(
    text: Substring,
    startingAt startPosition: SourcePosition = .init(line: 1, column: 1),
    inFile sourceFile: String
  ) -> [EBNFError.Note] {
    let diagnosticOffset: SourcePosition.Offset
      = (line: startPosition.line - 1, column: startPosition.column - 1)

    let tokens = scanner.tokens(
      in: String(text), fromFile: specPath, unrecognizedToken: unrecognizedToken)

    var r: [EBNFError.Note] = []

    for (e, (t, s, position)) in tokens.enumerated() {
      r.append(
        EBNFError.Note(
          message: "------------------- token \(e): '\(s)' (\(symbolName[t]!)) -------------------",
          site: position + diagnosticOffset))

      r.append(
        contentsOf: recognizer.progress(at: EarleySet(id: UInt32(e)))
          .lazy.map { rule, origin, n in
            EBNFError.Note(
              message: "\(description(rule, dotPosition: n)) (\(origin.id))",
              site: ruleLocation[rule]!)
          })
    }
    return r
  }

  struct Tree {
    let step: Evaluation.Step
    let children: [Tree]

    init(_ e: Evaluation) {
      var stack: [UInt32: Tree] = [:]
      for step in e {
        let children: [Tree] = step.rule.map { r in r.input.map { i in stack[i]! } } ?? []
        stack[step.output] = Tree(step: step, children: children)
      }
      self = stack[0]!
    }

    init(step: Evaluation.Step, children: [Tree]) {
      self.step = step
      self.children = children
    }
  }

  func recognize(
    _ text: Substring,
    startingAt diagnosticOffset: SourcePosition.Offset = (line: 0, column: 0),
    inFile sourceFile: String
  ) -> EBNFErrorLog {
    var errors: EBNFErrorLog = []

    let tokens = scanner.tokens(
      in: String(text), fromFile: specPath, unrecognizedToken: unrecognizedToken)

    recognizer.startInput()

    var lastPosition: SourceRegion = .empty
    for (t, s, p) in tokens {
      lastPosition = p + diagnosticOffset

      guard let err = recognizer.read(t) else {
        recognizer.advanceEarleme()
        continue
      }

      switch err {
      case .unexpectedToken:
        let expected = recognizer.expectedTerminals.lazy.map { t in symbolName[t]! }
          .joined(separator: ", ")

        errors.insert(
          EBNFError(
            "\(err) \(symbolName[t]!): '\(s)'", at: lastPosition,
            notes: [.init(message: "expected one of: " + expected, site: lastPosition)]))

      default:
        errors.insert(EBNFError("\(err)", at: lastPosition))
      }
      break
    }
    if !errors.isEmpty { return errors }

    guard let b = Bocage(recognizer) else {
      errors.insert(EBNFError("No parse", at: lastPosition))
      return errors
    }

    if b.isAmbiguous {
      let tokenRegions = tokens.map { _, _, p in p + diagnosticOffset }

      func error(_ s: Evaluation.Step) -> EBNF.Error {
        EBNF.Error(
          "Ambiguity: \(s.symbol != nil ? symbolName[s.symbol!.0]! : description(s.rule!.0))",
          at: tokenRegions[Int(s.sourceRange.lowerBound.id)]
            ... tokenRegions[Int(s.sourceRange.upperBound.id)])
      }

      var commonSteps = Set<Evaluation.Step>()
      var ambiguousSteps = Set<Evaluation.Step>()
      for e in Order(b, highRankOnly: false) {
        if !ambiguousSteps.isEmpty { break }
        if commonSteps.isEmpty && ambiguousSteps.isEmpty {
          commonSteps = Set(e)
          continue
        }

        let e1 = Array(e)
        ambiguousSteps.formUnion(commonSteps.symmetricDifference(e1))
        commonSteps.formIntersection(e1)
      }
      errors.formUnion(ambiguousSteps.lazy.map { s in error(s) })

      if ambiguousSteps.isEmpty {
        errors.insert(
          .init(
            "No parse (https://github.com/jeffreykegler/libmarpa/issues/115)",
            at: tokenRegions.first!...tokenRegions.last!))
      }
    }
    return errors
  }
}
