/// How a name expression is being used.
enum NameUse: Hashable {

  /// The name is used as the callee of an arbitrary function call.
  case function

  /// The name is used as the callee of a constructor call.
  case constructor

  /// The name is used as the callee of a subscript call.
  case `subscript`

  /// The name is used as an unapplied reference to a declaration.
  case unapplied

}
