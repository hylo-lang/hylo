import Core

/// A C++ prefix operator call expression.
struct CXXPrefixExpr: CXXExpr {

  enum Operator {
    // operator                   example     precedence  associativity
    case prefixIncrement  //      ++a         3           <--
    case prefixDecrement  //      --a         3           <--
    case unaryPlus  //            +a          3           <--
    case unaryMinus  //           -a          3           <--
    case logicalNot  //           !           3           <--
    case bitwiseNot  //           ~           3           <--
    case dereference  //          *a          3           <--
    case addressOf  //            &a          3           <--
    case sizeOf  //               sizeof      3           <--
    case coAwait  //              co_await    3           <--
    case throwOp  //              throw       16          <--
    case coYield  //              co_yield    16          <--
  }

  /// The operator that needs to be called.
  let oper: Operator

  /// The base expression of the operator call.
  let base: CXXExpr

  var precedence: Int {
    switch oper {
    case .prefixIncrement, .prefixDecrement, .unaryPlus, .unaryMinus, .logicalNot, .bitwiseNot,
      .dereference, .addressOf, .sizeOf, .coAwait:
      return 3
    case .throwOp, .coYield:
      return 16
    }
  }
  var isLeftToRight: Bool {
    return false
  }

}
