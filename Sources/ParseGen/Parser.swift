import Marpa
import CitronLexerModule

struct Parser {
  let grammar: Marpa.Grammar
  let recognizer: Marpa.Recognizer
  let unrecognizedToken: Marpa.Symbol
  let scanner: Scanner<Marpa.Symbol>
  let symbolName: (Marpa.Symbol) -> String
  let ruleLocation: (Marpa.Rule) -> SourceRegion

  init(
    grammar: Marpa.Grammar,
    unrecognizedToken: Marpa.Symbol,
    scanner: Scanner<Marpa.Symbol>,
    symbolName: @escaping (Marpa.Symbol) -> String,
    ruleLocation: @escaping (Marpa.Rule) -> SourceRegion
  ) {
    (self.grammar, self.unrecognizedToken, self.scanner, self.symbolName, self.ruleLocation)
      = (grammar, unrecognizedToken, scanner, symbolName, ruleLocation)
    recognizer = Recognizer(grammar)
  }

  func dumpGrammar() {
    for r in grammar.rules {
      print("\(ruleLocation(r)): note:", description(r))
    }
  }

  func description(_ r: Marpa.Rule, dotPosition: Int? = nil) -> String {
    let lhsName = symbolName(grammar.lhs(r))
    let rhsNames = grammar.rhs(r).lazy.map { s in symbolName(s) }
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
      in: String(text), fromFile: sourceFile, unrecognizedToken: unrecognizedToken)

    var r: [EBNFError.Note] = []

    for (e, (t, s, position)) in tokens.enumerated() {
      r.append(
        EBNFError.Note(
          message: "------------------- token \(e): '\(s)' (\(symbolName(t))) -------------------",
          site: position + diagnosticOffset))

      r.append(
        contentsOf: recognizer.progress(at: EarleySet(id: UInt32(e)))
          .lazy.map { rule, origin, n in
            EBNFError.Note(
              message: "\(description(rule, dotPosition: n)) (\(origin.id))",
              site: ruleLocation(rule))
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
      in: String(text), fromFile: sourceFile, unrecognizedToken: unrecognizedToken)

    recognizer.startInput()

    var esRegions: [SourceRegion] = []
    for (t, s, p) in tokens {
      esRegions.append(p + diagnosticOffset)

      guard let err = recognizer.read(t) else {
        recognizer.advanceEarleme()
        continue
      }

      switch err {
      case .unexpectedToken:
        let expected = recognizer.expectedTerminals.lazy.map { t in symbolName(t) }
          .joined(separator: ", ")

        errors.insert(
          EBNFError(
            "\(err) \(symbolName(t)): '\(s)'", at: esRegions.last!,
            notes: [.init(message: "expected one of: " + expected, site: esRegions.last!)]))

      default:
        errors.insert(EBNFError("\(err)", at: esRegions.last!))
      }
      break
    }
    if !errors.isEmpty { return errors }

    guard let b = Bocage(recognizer) else {
      errors.insert(EBNFError("No parse", at: esRegions.last!))
      return errors
    }

    // Deal with the final earley set.
    let l = esRegions.last!
    esRegions.append(SourceRegion(fileName: l.fileName, l.span.upperBound..<l.span.upperBound))
    let inputRegion = esRegions.first!...esRegions.last!
    var trees: [Tree] = []
    for (n, e) in Order(b, highRankOnly: false).enumerated() {
      trees.append(Tree(e))
      if n == 1 { break }
    }
    var notes: [EBNFError.Note] = []
    for t in trees {
      appendNotes(showing: t, to: &notes)
    }

    if trees.count != 1 {
      errors.insert(
        .init(trees.count > 1 ? "Ambiguous parse" : "No parse", at: inputRegion, notes: notes))
    }

    func appendNotes(
      showing t: Tree,
      to notes: inout [EBNFError.Note],
      depth: Int = 0
    ) {
      let s = t.step, l = s.sourceRange
      let indent = repeatElement("  ", count: depth).joined()
      let description = s.symbol != nil
        ? symbolName(s.symbol!.0) + (s.symbol!.tokenValue == nil ? " (null)" : "")
        : "(" + description(s.rule!.0)
      let location: SourceRegion
      let start = esRegions[Int(l.lowerBound.id)]
      if l.isEmpty {
        location = SourceRegion(fileName: sourceFile, start.span.lowerBound..<start.span.lowerBound)
      }
      else {
        location = start...esRegions[Int(l.upperBound.id) - 1]
      }
      notes.append(.init(message: "\t\(indent)\(description)", site: location))

      for child in t.children {
        appendNotes(showing: child, to: &notes, depth: depth + 1)
      }
      if s.rule != nil { notes[notes.count - 1].message += ")" }
    }

    return errors
  }
}
