import Core
import FrontEnd
import XCTest

final class LexerTests: XCTestCase {

  func testBool() {
    let input: SourceFile = "true false"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.bool, "true"),
        TokenSpecification(.bool, "false"),
      ],
      in: input)
  }

  func testDecimalInteger() {
    let input: SourceFile = "0 001 42 00 1_234 1_2__34__ -1 -a"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0"),
        TokenSpecification(.int, "001"),
        TokenSpecification(.int, "42"),
        TokenSpecification(.int, "00"),
        TokenSpecification(.int, "1_234"),
        TokenSpecification(.int, "1_2__34__"),
        TokenSpecification(.int, "-1"),
        TokenSpecification(.oper, "-"),
        TokenSpecification(.name, "a"),
      ],
      in: input)
  }

  func testHexadecimalInteger() {
    let input: SourceFile = "0x0123 0xabcdef 0x__0_a_ 0xg 0x -0x1"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0x0123"),
        TokenSpecification(.int, "0xabcdef"),
        TokenSpecification(.int, "0x__0_a_"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "xg"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "x"),
        TokenSpecification(.int, "-0x1"),
      ],
      in: input)
  }

  func testOctalInteger() {
    let input: SourceFile = "0o0123 0o__0_6_ 0o8 0o -0o1"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0o0123"),
        TokenSpecification(.int, "0o__0_6_"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "o8"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "o"),
        TokenSpecification(.int, "-0o1"),
      ],
      in: input)
  }

  func testBinaryInteger() {
    let input: SourceFile = "0b01 0b__0_1_ 0b8 0b -0b1"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0b01"),
        TokenSpecification(.int, "0b__0_1_"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "b8"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.name, "b"),
        TokenSpecification(.int, "-0b1"),
      ],
      in: input)
  }

  func testFloatingPointSequences() {
    let input: SourceFile = "0.0 001.00 0.1_2__34__ 1e1_000 1.12e+123 3.45E-6 1. 1e -1e2"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "0"),
        TokenSpecification(.int, "001"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "00"),
        TokenSpecification(.int, "0"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "1_2__34__"),
        TokenSpecification(.int, "1"),
        TokenSpecification(.exponent, "e1_000"),
        TokenSpecification(.int, "1"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "12"),
        TokenSpecification(.exponent, "e+123"),
        TokenSpecification(.int, "3"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "45"),
        TokenSpecification(.exponent, "E-6"),
        TokenSpecification(.int, "1"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.int, "1"),
        TokenSpecification(.invalid, "e"),
        TokenSpecification(.int, "-1"),
        TokenSpecification(.exponent, "e2"),
      ],
      in: input)
  }

  func testString() {
    let input: SourceFile = #""" "a 0+ " "a\nb" "a\"" "abc "#
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.string, #""""#),
        TokenSpecification(.string, #""a 0+ ""#),
        TokenSpecification(.string, #""a\nb""#),
        TokenSpecification(.string, #""a\"""#),
        TokenSpecification(.unterminatedString, #""abc "#),
      ],
      in: input)
  }

  func testKeywords() {
    let input: SourceFile = """
      any break catch conformance continue do else extension for fun if import in infix init inout
      let match namespace nil operator postfix prefix property private public remote return set
      sink some spawn static subscript trait try type typealias var where while yield yielded
      """

    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.`any`, "any"),
        TokenSpecification(.`break`, "break"),
        TokenSpecification(.`catch`, "catch"),
        TokenSpecification(.`conformance`, "conformance"),
        TokenSpecification(.`continue`, "continue"),
        TokenSpecification(.`do`, "do"),
        TokenSpecification(.`else`, "else"),
        TokenSpecification(.`extension`, "extension"),
        TokenSpecification(.`for`, "for"),
        TokenSpecification(.`fun`, "fun"),
        TokenSpecification(.`if`, "if"),
        TokenSpecification(.`import`, "import"),
        TokenSpecification(.`in`, "in"),
        TokenSpecification(.`infix`, "infix"),
        TokenSpecification(.`init`, "init"),
        TokenSpecification(.`inout`, "inout"),
        TokenSpecification(.`let`, "let"),
        TokenSpecification(.`match`, "match"),
        TokenSpecification(.`namespace`, "namespace"),
        TokenSpecification(.`nil`, "nil"),
        TokenSpecification(.`operator`, "operator"),
        TokenSpecification(.`postfix`, "postfix"),
        TokenSpecification(.`prefix`, "prefix"),
        TokenSpecification(.`property`, "property"),
        TokenSpecification(.`private`, "private"),
        TokenSpecification(.`public`, "public"),
        TokenSpecification(.`remote`, "remote"),
        TokenSpecification(.`return`, "return"),
        TokenSpecification(.`set`, "set"),
        TokenSpecification(.`sink`, "sink"),
        TokenSpecification(.`some`, "some"),
        TokenSpecification(.`spawn`, "spawn"),
        TokenSpecification(.`static`, "static"),
        TokenSpecification(.`subscript`, "subscript"),
        TokenSpecification(.`trait`, "trait"),
        TokenSpecification(.`try`, "try"),
        TokenSpecification(.`type`, "type"),
        TokenSpecification(.`typealias`, "typealias"),
        TokenSpecification(.`var`, "var"),
        TokenSpecification(.`where`, "where"),
        TokenSpecification(.`while`, "while"),
        TokenSpecification(.`yield`, "yield"),
        TokenSpecification(.`yielded`, "yielded"),
      ],
      in: input)
  }

  func testIdentifiers() {
    let input: SourceFile = "foo éléphant _bar _1_2_3 _"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name, "foo"),
        TokenSpecification(.name, "éléphant"),
        TokenSpecification(.name, "_bar"),
        TokenSpecification(.name, "_1_2_3"),
        TokenSpecification(.under, "_"),
      ],
      in: input)
  }

  func testBackquotedIdentifier() {
    let input: SourceFile = "`type` `foo` `a_b_` `12`"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name, "type"),
        TokenSpecification(.name, "foo"),
        TokenSpecification(.name, "a_b_"),
        TokenSpecification(.invalid, "`"),
        TokenSpecification(.int, "12"),
        TokenSpecification(.invalid, "`"),
      ],
      in: input)
  }

  func testAttributes() {
    let input: SourceFile = "@implicitcopy @_foo @2"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.attribute, "@implicitcopy"),
        TokenSpecification(.attribute, "@_foo"),
        TokenSpecification(.invalid, "@"),
        TokenSpecification(.int, "2"),
      ],
      in: input)
  }

  func testPragmas() {
    let input: SourceFile = "#file #_foo #2"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.pragmaLiteral, "#file"),
        TokenSpecification(.pragmaLiteral, "#_foo"),
        TokenSpecification(.invalid, "#"),
        TokenSpecification(.int, "2"),
      ],
      in: input)
  }

  func testOperators() {
    let input: SourceFile = "= -> * / % +- == != ~> >! <? >> &|^ ... ..< | &"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.assign, "="),
        TokenSpecification(.arrow, "->"),
        TokenSpecification(.oper, "*"),
        TokenSpecification(.oper, "/"),
        TokenSpecification(.oper, "%"),
        TokenSpecification(.oper, "+-"),
        TokenSpecification(.equal, "=="),
        TokenSpecification(.oper, "!="),
        TokenSpecification(.oper, "~>"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.oper, "!"),
        TokenSpecification(.lAngle, "<"),
        TokenSpecification(.oper, "?"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.oper, "&|^"),
        TokenSpecification(.oper, "..."),
        TokenSpecification(.oper, "..<"),
        TokenSpecification(.pipe, "|"),
        TokenSpecification(.ampersand, "&"),
      ],
      in: input)
  }

  func testCastOperators() {
    let input: SourceFile = "is as as!"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.cast, "is"),
        TokenSpecification(.cast, "as"),
        TokenSpecification(.cast, "as!"),
      ],
      in: input)
  }

  func testPunctuation() {
    let input: SourceFile = ",;.: ::"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.comma, ","),
        TokenSpecification(.semi, ";"),
        TokenSpecification(.dot, "."),
        TokenSpecification(.colon, ":"),
        TokenSpecification(.twoColons, "::"),
      ],
      in: input)
  }

  func testDelimiters() {
    let input: SourceFile = "()[]{}<>"
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.lParen, "("),
        TokenSpecification(.rParen, ")"),
        TokenSpecification(.lBrack, "["),
        TokenSpecification(.rBrack, "]"),
        TokenSpecification(.lBrace, "{"),
        TokenSpecification(.rBrace, "}"),
        TokenSpecification(.lAngle, "<"),
        TokenSpecification(.rAngle, ">"),
      ],
      in: input)
  }

  func testComments() {
    let input: SourceFile = """
      // line comment
      // line comment with block start /*
      /* Block comment
       * Second line
       */
      /**** /* Nested block */ */
      a = /* not c, but */ b
      /**** /* Unterminated */
      """

    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name, "a"),
        TokenSpecification(.assign, "="),
        TokenSpecification(.name, "b"),
        TokenSpecification(.unterminatedBlockComment, "/**** /* Unterminated */"),
      ],
      in: input)
  }

  func testInvalid() {
    let input: SourceFile = "\0"
    assert(
      tokenize(input),
      matches: [TokenSpecification(.invalid, "\0")],
      in: input)
  }

  private func tokenize(_ input: SourceFile) -> [Token] {
    let lexer = Lexer(tokenizing: input)
    return Array(lexer)
  }

  private func assert(
    _ tokens: [Token],
    matches specs: [TokenSpecification],
    in sourceCode: SourceFile,
    file: StaticString = #filePath,
    line: UInt = #line
  ) {
    // The list of tokens should have the same lenght as that of the specifications.
    XCTAssert(
      tokens.count == specs.count,
      "expected \(specs.count) token(s), found \(tokens.count)",
      file: file,
      line: line)

    for (token, spec) in zip(tokens, specs) {
      XCTAssert(
        token.kind == spec.kind,
        "token has kind '\(token.kind)', not '\(spec.kind)'",
        file: spec.file,
        line: spec.line)

      let value = sourceCode[token.site]
      XCTAssert(
        value == spec.value,
        "token has value '\(value)', not '\(spec.value)'",
        file: spec.file,
        line: spec.line)
    }
  }

}

private struct TokenSpecification {

  let kind: Token.Kind

  let value: String

  let file: StaticString

  let line: UInt

  init(_ kind: Token.Kind, _ value: String, file: StaticString = #filePath, line: UInt = #line) {
    self.kind = kind
    self.value = value
    self.file = file
    self.line = line
  }

}
