/// How a name expression is being used.
enum NameUse: Hashable {

  /// The name is used as the callee of an arbitrary function call.
  ///
  /// `labels` contains the labels of the run-time arguments passed to the callee.
  case function(labels: [String?])

  /// The name is used as the callee of a constructor call.
  ///
  /// `labels` contains the labels of the run-time arguments passed to the callee.
  case constructor(labels: [String?])

  /// The name is used as the callee of a subscript call.
  ///
  /// `labels` contains the labels of the run-time arguments passed to the callee.
  case `subscript`(labels: [String?])

  /// The name is used as an unapplied reference to a declaration.
  case unapplied

  /// `true` iff `self` is `.constructor`.
  var isConstructor: Bool {
    if case .constructor = self {
      return true
    } else {
      return false
    }
  }

  /// Returns the associated labels if `self` denotes a use as a callee; otherwise returns `nil`.
  var labels: [String?]? {
    switch self {
    case .function(let l), .constructor(let l), .subscript(let l):
      return l
    case .unapplied:
      return nil
    }
  }

}
