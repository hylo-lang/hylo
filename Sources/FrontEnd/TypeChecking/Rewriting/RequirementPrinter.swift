import Utils

/// An object creating textual descriptions of requirement rewriting systems.
struct RequirementPrinter {

  /// The program defining the symbols represented by the terms.
  let program: TypedProgram

  /// A factory for making unique type names.
  private var disambiguator = DistinctNameGenerator<AnyDeclID>()

  /// Creates an instance for creating textual descriptions of symbols defined in `program`.
  init(program: TypedProgram) {
    self.program = program
  }

  /// Returns a textual description of `rule`.
  mutating func describe(_ rule: RequirementRule) -> String {
    let l = describe(rule.lhs)
    let r = describe(rule.rhs)
    return "\(l) => \(r)"
  }

  /// Returns a textual description of `term`.
  mutating func describe(_ term: RequirementTerm) -> String {
    var result: [String] = []
    for s in term.symbols {
      switch s {
      case .associatedType(let d):
        let t = program.traitDeclaring(d)!.name
        let a = program[d].baseName
        result.append("[::\(t.value).\(a)]")

      case .parameterType(let d):
        let p = disambiguator.name(program[d].baseName, keyedBy: AnyDeclID(d))
        result.append(p)

      case .trait(let d):
        let p = program[d].baseName
        result.append("[\(p)]")

      case .concrete(let t):
        result.append("[concrete: \(t)]")
      }
    }
    return result.isEmpty ? "ε" : result.joined(separator: ".")
  }

}

extension TypedProgram {

  /// Returns a debug description of `m`.
  func describe(_ m: RequirementSystem) -> String {
    var o = ""
    var p = RequirementPrinter(program: self)
    for r in m.rules.indices where !m.rules[r].isSimplified {
      o += "\(r): \(p.describe(m.rules[r]))\n"
    }
    return o
  }

  /// Returns a debug description of `r`.
  func describe(_ r: RequirementRule) -> String {
    var p = RequirementPrinter(program: self)
    return p.describe(r)
  }

  /// Returns a debug description of `t`.
  func describe(_ t: RequirementTerm) -> String {
    var p = RequirementPrinter(program: self)
    return p.describe(t)
  }

}
