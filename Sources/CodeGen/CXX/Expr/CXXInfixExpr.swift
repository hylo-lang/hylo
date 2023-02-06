import Core

/// A C++ infix operator call expression.
struct CXXInfixExpr: CXXExpr {

  enum Operator {
    // operator                   example     precedence  associativity
    case scopeResolution  //      ::          1           -->
    case dotAccess  //            .           2           -->
    case ptrAccess  //            ->          2           -->
    case dotPtrToMember  //       .*          4           -->
    case ptrToMember  //          ->*         4           -->
    case multiplication  //       a*b         5           -->
    case division  //             a/b         5           -->
    case remainder  //            a%b         5           -->
    case addition  //             a+b         6           -->
    case subtraction  //          a-b         6           -->
    case leftShift  //            a<<b        7           -->
    case rightShift  //           a>>b        7           -->
    case spaceship  //            a<=>b       8           -->
    case lessThan  //             a<b         9           -->
    case lessEqual  //            a<=b        9           -->
    case greaterThan  //          a>b         9           -->
    case greaterEqual  //         a>=b        9           -->
    case equality  //             a==b        10          -->
    case inequality  //           a==b        10          -->
    case bitwiseAnd  //           a&b         11          -->
    case bitwiseXor  //           a^b         12          -->
    case bitwiseOr  //            a|b         13          -->
    case logicalAnd  //           a&&b        14          -->
    case logicalOr  //            a||b        15          -->
    case assignment  //           a=b         16          <--
    case addAssignment  //        a+=b        16          <--
    case subAssignment  //        a-=b        16          <--
    case mulAssignment  //        a*=b        16          <--
    case divAssignment  //        a/=b        16          <--
    case remAssignment  //        a%=b        16          <--
    case shiftLeftAssignment  //  a<<=b       16          <--
    case shiftRightAssignment  // a>>=b       16          <--
    case bitwiseAndAssignment  // a&=b        16          <--
    case bitwiseXorAssignment  // a^=b        16          <--
    case bitwiseOrAssignment  //  a|=b        16          <--
    case comma  //                a,b         17          -->
  }

  /// The operator that needs to be called.
  let oper: Operator

  /// The left-hand-side argument of the operator call.
  let lhs: CXXExpr

  /// The right-hand-side argument of the operator call.
  let rhs: CXXExpr

  var precedence: Int {
    switch oper {
    case .scopeResolution:
      return 1
    case .dotAccess, .ptrAccess:
      return 2
    case .dotPtrToMember, .ptrToMember:
      return 4
    case .multiplication, .division, .remainder:
      return 5
    case .addition, .subtraction:
      return 6
    case .leftShift, .rightShift:
      return 7
    case .spaceship:
      return 8
    case .lessThan, .lessEqual, .greaterThan, .greaterEqual:
      return 9
    case .equality, .inequality:
      return 10
    case .bitwiseAnd:
      return 11
    case .bitwiseXor:
      return 12
    case .bitwiseOr:
      return 13
    case .logicalAnd:
      return 14
    case .logicalOr:
      return 15
    case .assignment, .addAssignment, .subAssignment, .mulAssignment, .divAssignment,
      .remAssignment, .shiftLeftAssignment, .shiftRightAssignment, .bitwiseAndAssignment,
      .bitwiseXorAssignment, .bitwiseOrAssignment:
      return 16
    case .comma:
      return 17
    }
  }
  var isLeftToRight: Bool {
    switch oper {
    case .scopeResolution, .dotAccess, .ptrAccess, .dotPtrToMember, .ptrToMember, .multiplication,
      .division, .remainder, .addition, .subtraction, .leftShift, .rightShift, .spaceship,
      .lessThan, .lessEqual, .greaterThan, .greaterEqual, .equality, .inequality, .bitwiseAnd,
      .bitwiseXor, .bitwiseOr, .logicalAnd, .logicalOr, .comma:
      return true
    default:
      return false
    }
  }

}
