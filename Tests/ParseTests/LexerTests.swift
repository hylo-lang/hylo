import XCTest

import Basic
import Parse

final class LexerTests: XCTestCase {

  private var manager: SourceManager?

  override func setUp() {
    manager = SourceManager()
  }

  override func tearDown() {
    manager = nil
  }

  func testBool() {
    let input = "true false"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.bool, "true"),
        TokenSpec(.bool, "false"),
      ],
      in: input)
  }

  func testDecimalInteger() {
    let input = "0 001 42 00 1_234 1_2__34__"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.int, "0"),
        TokenSpec(.int, "001"),
        TokenSpec(.int, "42"),
        TokenSpec(.int, "00"),
        TokenSpec(.int, "1_234"),
        TokenSpec(.int, "1_2__34__"),
      ],
      in: input)
  }

  func testHexadecimalInteger() {
    let input = "0x0123 0xabcdef 0x__0_a_ 0xg 0x"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.int, "0x0123"),
        TokenSpec(.int, "0xabcdef"),
        TokenSpec(.int, "0x__0_a_"),
        TokenSpec(.int, "0x"),
        TokenSpec(.name, "g"),
        TokenSpec(.int, "0x"),
      ],
      in: input)
  }

  func testOctalInteger() {
    let input = "0o0123 0o__0_6_ 0o8 0o"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.int, "0o0123"),
        TokenSpec(.int, "0o__0_6_"),
        TokenSpec(.int, "0o"),
        TokenSpec(.int, "8"),
        TokenSpec(.int, "0o"),
      ],
      in: input)
  }

  func testBinaryInteger() {
    let input = "0b01 0b__0_1_ 0b8 0b"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.int, "0b01"),
        TokenSpec(.int, "0b__0_1_"),
        TokenSpec(.int, "0b"),
        TokenSpec(.int, "8"),
        TokenSpec(.int, "0b"),
      ],
      in: input)
  }

  func testFloatingPoint() {
    let input = "0.0 001.00 0.1_2__34__ 1e1_000 1.12e+123 3.45E-6 1. 1e"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.float, "0.0"),
        TokenSpec(.float, "001.00"),
        TokenSpec(.float, "0.1_2__34__"),
        TokenSpec(.float, "1e1_000"),
        TokenSpec(.float, "1.12e+123"),
        TokenSpec(.float, "3.45E-6"),
        TokenSpec(.int  , "1"),
        TokenSpec(.dot  , "."),
        TokenSpec(.int  , "1"),
        TokenSpec(.name , "e"),
      ],
      in: input)
  }

  func testString() {
    let input = #""" "a 0+ " "a\nb" "a\"" "abc "#
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.string, #""""#),
        TokenSpec(.string, #""a 0+ ""#),
        TokenSpec(.string, #""a\nb""#),
        TokenSpec(.string, #""a\"""#),
        TokenSpec(.unterminatedString, #""abc "#),
      ],
      in: input)
  }

  func testMultilineString() {
    let input = "\"a\nb\" \"a\nbc "
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.string, "\"a\nb\""),
        TokenSpec(.unterminatedString, "\"a\nbc "),
      ],
      in: input)
  }

  func testKeywords() {
    let input = """
    async await break case consuming continue del else extn for fun if in infix local match mod mut
    new nil postfix prefix pub return static type let var view volatile where while
    """

    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.async    , "async"),
        TokenSpec(.await    , "await"),
        TokenSpec(.break    , "break"),
        TokenSpec(.case     , "case"),
        TokenSpec(.consuming, "consuming"),
        TokenSpec(.continue , "continue"),
        TokenSpec(.del      , "del"),
        TokenSpec(.else     , "else"),
        TokenSpec(.extn     , "extn"),
        TokenSpec(.for      , "for"),
        TokenSpec(.fun      , "fun"),
        TokenSpec(.if       , "if"),
        TokenSpec(.in       , "in"),
        TokenSpec(.infix    , "infix"),
        TokenSpec(.local    , "local"),
        TokenSpec(.match    , "match"),
        TokenSpec(.mod      , "mod"),
        TokenSpec(.mut      , "mut"),
        TokenSpec(.new      , "new"),
        TokenSpec(.nil      , "nil"),
        TokenSpec(.postfix  , "postfix"),
        TokenSpec(.prefix   , "prefix"),
        TokenSpec(.pub      , "pub"),
        TokenSpec(.return   , "return"),
        TokenSpec(.static   , "static"),
        TokenSpec(.type     , "type"),
        TokenSpec(.let      , "let"),
        TokenSpec(.var      , "var"),
        TokenSpec(.view     , "view"),
        TokenSpec(.volatile , "volatile"),
        TokenSpec(.where    , "where"),
        TokenSpec(.while    , "while"),
      ],
      in: input)
  }

  func testIdentifiers() {
    let input = "foo éléphant _bar _1_2_3 _"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.name     , "foo"),
        TokenSpec(.name     , "éléphant"),
        TokenSpec(.name     , "_bar"),
        TokenSpec(.name     , "_1_2_3"),
        TokenSpec(.under    , "_"),
      ],
      in: input)
  }

  func testBackquotedIdentifier() {
    let input = "`type` `foo` `a_b_` `12`a"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.name     , "type"),
        TokenSpec(.name     , "foo"),
        TokenSpec(.name     , "a_b_"),
        TokenSpec(.invalid  , "`"),
        TokenSpec(.int      , "12"),
        TokenSpec(.invalid  , "`"),
        TokenSpec(.name     , "a"),
      ],
      in: input)
  }

  func testOperators() {
    let input = "= -> * / % +- == != ~> >! <? >> &|^"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.assign   , "="),
        TokenSpec(.arrow    , "->"),
        TokenSpec(.oper     , "*"),
        TokenSpec(.oper     , "/"),
        TokenSpec(.oper     , "%"),
        TokenSpec(.oper     , "+-"),
        TokenSpec(.oper     , "=="),
        TokenSpec(.oper     , "!="),
        TokenSpec(.oper     , "~>"),
        TokenSpec(.rAngle   , ">"),
        TokenSpec(.oper     , "!"),
        TokenSpec(.lAngle   , "<"),
        TokenSpec(.oper     , "?"),
        TokenSpec(.rAngle   , ">"),
        TokenSpec(.rAngle   , ">"),
        TokenSpec(.oper     , "&|^"),
      ],
      in: input)
  }

  func testCastOperators() {
    let input = "is as as!"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.cast     , "is"),
        TokenSpec(.cast     , "as"),
        TokenSpec(.cast     , "as!"),
      ],
      in: input)
  }

  func testPunctuation() {
    let input = ",;.: ::"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.comma    , ","),
        TokenSpec(.semi     , ";"),
        TokenSpec(.dot      , "."),
        TokenSpec(.colon    , ":"),
        TokenSpec(.twoColons, "::"),
      ],
      in: input)
  }

  func testDelimiters() {
    let input = "()[]{}<>"
    assert(
      that: tokenize(input),
      match: [
        TokenSpec(.lParen, "("),
        TokenSpec(.rParen, ")"),
        TokenSpec(.lBrack, "["),
        TokenSpec(.rBrack, "]"),
        TokenSpec(.lBrace, "{"),
        TokenSpec(.rBrace, "}"),
        TokenSpec(.lAngle, "<"),
        TokenSpec(.rAngle, ">"),
      ],
      in: input)
  }

  func testComments() {
    let input = """
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
      that: tokenize(input),
      match: [
        TokenSpec(.name   , "a"),
        TokenSpec(.assign , "="),
        TokenSpec(.name   , "b"),
        TokenSpec(.unterminatedBlockComment, "/**** /* Unterminated */"),
      ],
      in: input)
  }

  func testInvalid() {
    let input = "\0"
    assert(
      that: tokenize(input),
      match: [TokenSpec(.invalid, "\0")],
      in: input)
  }

  private func tokenize(_ input: String) -> [Token] {
    let file = manager!.load(string: input)
    let lexer = Lexer(source: file)
    return Array(lexer)
  }

  private func assert(
    that tokens : [Token],
    match specs : [TokenSpec],
    in source   : String,
    file        : StaticString = #filePath,
    line        : UInt = #line
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
      XCTAssert(
        source[token.range] == spec.value,
        "token has value '\(source[token.range])', not '\(spec.value)'",
        file: spec.file,
        line: spec.line)
    }
  }

}

fileprivate struct TokenSpec {

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
