extension Token {
  var citronKind: ValParser.CitronTokenCode {
    switch kind {
    // Errors
    case .invalid: return .INVALID
    case .unterminatedString: return .UNTERMINATEDSTRING
    case .unterminatedBlockComment: return .UNTERMINATEDBLOCKCOMMENT

    // Scalar literals
    case .bool: return .BOOL
    case .int: return .INT
    case .float: return .FLOAT
    case .string: return .STRING

    // Identifiers
    case .name: return .NAME
    case .under: return .UNDER

    // Keywords
    case .async: return .ASYNC
    case .await: return .AWAIT
    case .break: return .BREAK
    case .case: return .CASE
    case .consuming: return .CONSUMING
    case .continue: return .CONTINUE
    case .del: return .DEL
    case .else: return .ELSE
    case .extension: return .EXTENSION
    case .for: return .FOR
    case .fun: return .FUN
    case .if: return .IF
    case .in: return .IN
    case .infix: return .INFIX
    case .let: return .LET
    case .local: return .LOCAL
    case .match: return .MATCH
    case .mod: return .MOD
    case .mut: return .MUT
    case .namespace: return .NAMESPACE
    case .new: return .NEW
    case .nil: return .NIL
    case .postfix: return .POSTFIX
    case .prefix: return .PREFIX
    case .pub: return .PUB
    case .return: return .RETURN
    case .static: return .STATIC
    case .type: return .TYPE
    case .var: return .VAR
    case .view: return .VIEW
    case .volatile: return .VOLATILE
    case .where: return .WHERE
    case .while: return .WHILE

    // Operators
    case .oper: return .OPER
    case .cast: return .CAST
    case .arrow: return .ARROW
    case .assign: return .ASSIGN

    // Punctuation
    case .comma: return .COMMA
    case .semi: return .SEMI
    case .dot: return .DOT
    case .colon: return .COLON
    case .twoColons: return .TWOCOLONS

    // Delimiters
    case .lParen: return .LPAREN
    case .rParen: return .RPAREN
    case .lBrace: return .LBRACE
    case .rBrace: return .RBRACE
    case .lBrack: return .LBRACK
    case .rBrack: return .RBRACK
    case .lAngle: return .LANGLE
    case .rAngle: return .RANGLE
    }
  }
}
