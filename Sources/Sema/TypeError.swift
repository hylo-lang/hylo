/// A type error encountered by a constraint solver.
enum TypeError {

  case conflictingTypes(Constraint)

  case conflictingLabels(Constraint)

  case nonExistentProperty(Constraint)

  case nonConformingType(Constraint)

  case ambiguousConstraint(Constraint)

}
