/// A solution returned by a constraint solver.
struct Solution {

  /// The score of a solution.
  struct Score: Comparable {

    let errorCount: Int

    let penalities: Int

    static let worst = Score(errorCount: Int.max, penalities: Int.max)

    static func < (l: Self, r: Self) -> Bool {
      l.errorCount == r.errorCount
        ? l.penalities < r.penalities
        : l.errorCount < r.errorCount
    }

  }

  /// The assumptions made by the constraint solver.
  var assumptions: [TypeVariable: Type]

  /// The penalities of the solution.
  var penalities: Int

  /// The errors associated with the solution.
  var errors: [TypeError]

  /// The score of the solution.
  var score: Score { Score(errorCount: errors.count, penalities: penalities) }

  /// Reifies the given type, substituting each free variable by its corresponding binding.
  func reify(_ type: Type) -> Type {
    switch type {
    case .associated,
         .builtin,
         .error,
         .existential,
         .genericSizeParam,
         .genericTypeParam,
         .module,
         .product,
         .trait,
         .typeAlias:
      return type

    case .boundGeneric(let type):
      return .boundGeneric(BoundGenericType(
        reify(type.base),
        arguments: type.arguments.map({ a in
          switch a {
          case .type(let type):
            return .type(reify(type))
          case .size:
            fatalError("not implemented")
          }
        })))

    case .conformanceLens(let type):
      return .conformanceLens(ConformanceLensType(
        wrapped: reify(type.wrapped),
        focus: type.focus))

    case .subscript(let type):
      return .subscript(SubscriptType(
        isProperty: type.isProperty,
        capabilities: type.capabilities,
        inputs: type.inputs.map({ parameter in
          SubscriptType.Parameter(
            label: parameter.label,
            type: reify(parameter.type))
        }),
        output: reify(type.output)))

    case .tuple(let type):
      return .tuple(TupleType(
        type.elements.map({ element in
          TupleType.Element(
            label: element.label,
            type: reify(element.type))
        })))

    case .union(let type):
      return .union(UnionType(type.elements.map(reify)))

    case .variable(let t):
      return assumptions[t] ?? type
    }
  }
}
