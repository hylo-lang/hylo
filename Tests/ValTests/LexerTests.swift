import XCTest
import Compiler

final class LexerTests: XCTestCase {

  func testBool() {
    let input = SourceFile(contents: "true false")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.bool, "true"),
        TokenSpecification(.bool, "false"),
      ],
      in: input)
  }

  func testDecimalInteger() {
    let input = SourceFile(contents: "0 001 42 00 1_234 1_2__34__")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int, "0"),
        TokenSpecification(.int, "001"),
        TokenSpecification(.int, "42"),
        TokenSpecification(.int, "00"),
        TokenSpecification(.int, "1_234"),
        TokenSpecification(.int, "1_2__34__"),
      ],
      in: input)
  }

  func testHexadecimalInteger() {
    let input = SourceFile(contents: "0x0123 0xabcdef 0x__0_a_ 0xg 0x")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int , "0x0123"),
        TokenSpecification(.int , "0xabcdef"),
        TokenSpecification(.int , "0x__0_a_"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "xg"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "x"),
      ],
      in: input)
  }

  func testOctalInteger() {
    let input = SourceFile(contents: "0o0123 0o__0_6_ 0o8 0o")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int , "0o0123"),
        TokenSpecification(.int , "0o__0_6_"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "o8"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "o"),
      ],
      in: input)
  }

  func testBinaryInteger() {
    let input = SourceFile(contents: "0b01 0b__0_1_ 0b8 0b")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.int , "0b01"),
        TokenSpecification(.int , "0b__0_1_"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "b8"),
        TokenSpecification(.int , "0"),
        TokenSpecification(.name, "b"),
      ],
      in: input)
  }

  func testFloatingPoint() {
    let input = SourceFile(contents: "0.0 001.00 0.1_2__34__ 1e1_000 1.12e+123 3.45E-6 1. 1e")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.float, "0.0"),
        TokenSpecification(.float, "001.00"),
        TokenSpecification(.float, "0.1_2__34__"),
        TokenSpecification(.float, "1e1_000"),
        TokenSpecification(.float, "1.12e+123"),
        TokenSpecification(.float, "3.45E-6"),
        TokenSpecification(.int  , "1"),
        TokenSpecification(.dot  , "."),
        TokenSpecification(.int  , "1"),
        TokenSpecification(.name , "e"),
      ],
      in: input)
  }

  func testString() {
    let input = SourceFile(contents: #""" "a 0+ " "a\nb" "a\"" "abc "#)
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
    let input = SourceFile(contents: """
    async await break catch conformance continue deinit else extension for fun if import in
    indirect infix init inout let match namespace nil operator postfix prefix property public
    return set sink some static subscript trait try type typealias var where while yield yielded
    """)

    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.`async`       , "async"),
        TokenSpecification(.`await`       , "await"),
        TokenSpecification(.`break`       , "break"),
        TokenSpecification(.`catch`       , "catch"),
        TokenSpecification(.`conformance` , "conformance"),
        TokenSpecification(.`continue`    , "continue"),
        TokenSpecification(.`deinit`      , "deinit"),
        TokenSpecification(.`else`        , "else"),
        TokenSpecification(.`extension`   , "extension"),
        TokenSpecification(.`for`         , "for"),
        TokenSpecification(.`fun`         , "fun"),
        TokenSpecification(.`if`          , "if"),
        TokenSpecification(.`import`      , "import"),
        TokenSpecification(.`in`          , "in"),
        TokenSpecification(.`indirect`    , "indirect"),
        TokenSpecification(.`infix`       , "infix"),
        TokenSpecification(.`init`        , "init"),
        TokenSpecification(.`inout`       , "inout"),
        TokenSpecification(.`let`         , "let"),
        TokenSpecification(.`match`       , "match"),
        TokenSpecification(.`namespace`   , "namespace"),
        TokenSpecification(.`nil`         , "nil"),
        TokenSpecification(.`operator`    , "operator"),
        TokenSpecification(.`postfix`     , "postfix"),
        TokenSpecification(.`prefix`      , "prefix"),
        TokenSpecification(.`property`    , "property"),
        TokenSpecification(.`public`      , "public"),
        TokenSpecification(.`return`      , "return"),
        TokenSpecification(.`set`         , "set"),
        TokenSpecification(.`sink`        , "sink"),
        TokenSpecification(.`some`        , "some"),
        TokenSpecification(.`static`      , "static"),
        TokenSpecification(.`subscript`   , "subscript"),
        TokenSpecification(.`trait`       , "trait"),
        TokenSpecification(.`try`         , "try"),
        TokenSpecification(.`type`        , "type"),
        TokenSpecification(.`typealias`   , "typealias"),
        TokenSpecification(.`var`         , "var"),
        TokenSpecification(.`where`       , "where"),
        TokenSpecification(.`while`       , "while"),
        TokenSpecification(.`yield`       , "yield"),
        TokenSpecification(.`yielded`     , "yielded"),
      ],
      in: input)
  }

  func testIdentifiers() {
    let input = SourceFile(contents: "foo éléphant _bar _1_2_3 _")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name  , "foo"),
        TokenSpecification(.name  , "éléphant"),
        TokenSpecification(.name  , "_bar"),
        TokenSpecification(.name  , "_1_2_3"),
        TokenSpecification(.under , "_"),
      ],
      in: input)
  }

  func testBackquotedIdentifier() {
    let input = SourceFile(contents: "`type` `foo` `a_b_` `12`")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name   , "type"),
        TokenSpecification(.name   , "foo"),
        TokenSpecification(.name   , "a_b_"),
        TokenSpecification(.invalid, "`"),
        TokenSpecification(.int    , "12"),
        TokenSpecification(.invalid, "`"),
      ],
      in: input)
  }

  func testOperators() {
    let input = SourceFile(contents: "= -> * / % +- == != ~> >! <? >> &|^")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.assign, "="),
        TokenSpecification(.arrow , "->"),
        TokenSpecification(.oper  , "*"),
        TokenSpecification(.oper  , "/"),
        TokenSpecification(.oper  , "%"),
        TokenSpecification(.oper  , "+-"),
        TokenSpecification(.oper  , "=="),
        TokenSpecification(.oper  , "!="),
        TokenSpecification(.oper  , "~>"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.oper  , "!"),
        TokenSpecification(.lAngle, "<"),
        TokenSpecification(.oper  , "?"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.rAngle, ">"),
        TokenSpecification(.oper  , "&|^"),
      ],
      in: input)
  }

  func testCastOperators() {
    let input = SourceFile(contents: "is as as!")
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
    let input = SourceFile(contents: ",;.: ::")
    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.comma    , ","),
        TokenSpecification(.semi     , ";"),
        TokenSpecification(.dot      , "."),
        TokenSpecification(.colon    , ":"),
        TokenSpecification(.twoColons, "::"),
      ],
      in: input)
  }

  func testDelimiters() {
    let input = SourceFile(contents: "()[]{}<>")
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
    let input = SourceFile(contents: """
    // line comment
    // line comment with block start /*
    /* Block comment
     * Second line
     */
    /**** /* Nested block */ */
    a = /* not c, but */ b
    /**** /* Unterminated */
    """)

    assert(
      tokenize(input),
      matches: [
        TokenSpecification(.name   , "a"),
        TokenSpecification(.assign , "="),
        TokenSpecification(.name   , "b"),
        TokenSpecification(.unterminatedBlockComment, "/**** /* Unterminated */"),
      ],
      in: input)
  }

  func testInvalid() {
    let input = SourceFile(contents: "\0")
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
    in source: SourceFile,
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

      let value = source[token.range]
      XCTAssert(
        value == spec.value,
        "token has value '\(value)', not '\(spec.value)'",
        file: spec.file,
        line: spec.line)
    }
  }

}

fileprivate struct TokenSpecification {

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
