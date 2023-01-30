/// A CXX expression.
protocol CXXExpr: CXXNode {

  /// The precedence of the expression.
  /// This is used to determine the evaluation order of C++ expressions.
  /// Expressions with smaller precedence values are evaluated in front of expressions with higher precendence values.
  /// Following the conventions from https://en.cppreference.com/w/cpp/language/operator_precedence
  var precedence: Int { get }

  /// `true` if this expression needs to be evaluated from left to right.
  /// Determines the evaluation order for expressions of the same precedence value.
  var isLeftToRight: Bool { get }

}
