import ArgumentParser
import Foundation
import SwiftDiagnostics
import SwiftOperators
import SwiftParser
import SwiftParserDiagnostics
import SwiftSyntax

/// Mapping from (presumed) XCTest type names to a list of test method
/// names.
typealias TestCatalog = [String: [String]]

/// A command-line tool that generates XCTest cases for a list of annotated ".hylo"
/// files as part of our build process.
@main
struct GenerateHyloFileTests: ParsableCommand {

  @Option(
    name: [.customShort("o")],
    help: ArgumentHelp("Write output to <file>.", valueName: "output-swift-file"),
    transform: URL.init(fileURLWithPath:))
  var main: URL

  @Argument(
    help: "Paths of files to be scraped for invocable XCTests.",
    transform: URL.init(fileURLWithPath:))
  var sourcesToScrape: [URL]

  /// Returns a mapping from (presumed) XCTest type names in
  /// sourcesToScrape to an array of names of test methods.
  ///
  /// Scraping is primitive.  Any nullary method defined in these
  /// files whose name begins with `test` will be included, even if it
  /// is a struct method or in an extension on `Array`.  Use of
  /// constructs that complicate the syntax, including conditional
  /// compilation and Swift macros, could cause problems.  In
  /// particular, if you need to disable a test for particular
  /// platforms, conditionally compile-out the contents of its body,
  /// not the test itself.
  func discoveredTests() throws -> TestCatalog {
    try sourcesToScrape.map(discoveredTests(in:)).reduce(into: TestCatalog()) {
      $0.merge($1, uniquingKeysWith: +)
    }
  }

  func discoveredTests(in f: URL) throws -> TestCatalog {
    let tree = try parseAndEmitDiagnostics(source: String(contentsOf: f))
    let scraper = TestScraper()
    scraper.walk(tree)
    return scraper.result
  }

  func allTestsExtension(testCaseName: String, testNames: [String]) -> String {
    """
    private extension \(testCaseName) {

      static var allTests: AllTests<\(testCaseName)> {
        return [
    \(testNames.map { "      (\(String(reflecting: $0)), \($0))," }.joined(separator: "\n"))
        ]
      }

    }
    """
  }

  func run() throws {

    let tests = try discoveredTests()

    let output =
      """
      import XCTest
      #if canImport(Darwin)
        import Darwin
        // Portability to MacOS
        private typealias XCTestCaseClosure = (XCTestCase) throws -> Void
        private typealias XCTestCaseEntry = (testCaseClass: XCTestCase.Type, allTests: [(String, XCTestCaseClosure)])
      #elseif canImport(Glibc)
        import Glibc
      #elseif canImport(CRT)
        import CRT
      #endif

      private typealias AllTests<T> = [(String, (T) -> () throws -> Void)]

      private extension Array {

        func eraseTestTypes<T>() -> XCTestCaseEntry where Self == AllTests<T> {
          guard let t = T.self as? XCTestCase.Type else {
            // Skip any methods scraped from non-XCTestCase types.
            return (testCaseClass: XCTestCase.self, allTests: [])
          }
          return (
            testCaseClass: t,
            allTests: self.map { name_f in
              (name_f.0, { (x: XCTestCase) throws in try name_f.1(x as! T)() })
          })
        }

      }

      \(tests.map(allTestsExtension).joined(separator: "\n\n"))

      private let allTestCases: [XCTestCaseEntry] = [
        \(tests.keys.map {"\($0).allTests.eraseTestTypes()"}.joined(separator: ",\n  "))
      ]

      func main() {
        #if !os(Windows)
          // ignore SIGPIPE which is sent when writing to closed file descriptors.
          _ = signal(SIGPIPE, SIG_IGN)
        #endif

        atexit({
          fflush(stdout)
          fflush(stderr)
        })
        #if !os(macOS) || canImport(SwiftXCTest)
          XCTMain(allTestCases)
        #endif
      }
      """

    try output.write(to: main, atomically: true, encoding: .utf8)
  }

}

struct ParseFailure: Error {
  let diagnostics: [Diagnostic]
}

/// Parses the given source code and returns a valid `SourceFileSyntax` node.
///
/// This helper function automatically folds sequence expressions using the given operator table,
/// ignoring errors so that formatting can do something reasonable in the presence of unrecognized
/// operators.
///
/// - Throws: If an unrecoverable error occurs when formatting the code.
func parseAndEmitDiagnostics(
  source: String,
  operatorTable: OperatorTable = .standardOperators
) throws -> SourceFileSyntax {
  let sourceFile =
    operatorTable.foldAll(Parser.parse(source: source)) { _ in }.as(SourceFileSyntax.self)!

  let diagnostics = ParseDiagnosticsGenerator.diagnostics(for: sourceFile)
  if !diagnostics.isEmpty {
    throw ParseFailure(diagnostics: diagnostics)
  }

  return sourceFile
}

class TestScraper: SyntaxVisitor {

  init() { super.init(viewMode: .all) }

  var scope: [(name: String, possibleClass: Bool)] = []
  var result: TestCatalog = [:]

  private func enterScope(name: String, possibleClass: Bool, modifiers: DeclModifierListSyntax) -> SyntaxVisitorContinueKind {
    scope.append((name, possibleClass))
    return modifiers.contains(where: { $0.name.text == "private" }) ? .skipChildren
      : .visitChildren
  }

  private func leaveScope() { scope.removeLast() }

  override func visit(_ n: ClassDeclSyntax) -> SyntaxVisitorContinueKind {
    enterScope(name: n.name.text, possibleClass: true, modifiers: n.modifiers)
  }

  override func visitPost(_: ClassDeclSyntax) {
    leaveScope()
  }

  override func visit(_ n: StructDeclSyntax) -> SyntaxVisitorContinueKind {
    enterScope(name: n.name.text, possibleClass: false, modifiers: n.modifiers)
  }

  override func visitPost(_: StructDeclSyntax) {
    leaveScope()
  }

  override func visit(_ n: EnumDeclSyntax) -> SyntaxVisitorContinueKind {
    enterScope(name: n.name.text, possibleClass: false, modifiers: n.modifiers)
  }

  override func visitPost(_: EnumDeclSyntax) {
    scope.removeLast()
  }

  override func visit(_ n: ExtensionDeclSyntax) -> SyntaxVisitorContinueKind {
    enterScope(
      name: n.extendedType.trimmedDescription, possibleClass: true, modifiers: n.modifiers)
  }

  override func visitPost(_: ExtensionDeclSyntax) {
    scope.removeLast()
  }

  override func visit(_ n: FunctionDeclSyntax) -> SyntaxVisitorContinueKind {
    // Bail if we can't possibly be in a class, or if the method is static or private.
    if scope.isEmpty || !scope.last!.possibleClass
         || n.modifiers.contains(where: { ["static", "private"].contains($0.name.text) })
    {
      return .skipChildren
    }

    let baseName = n.name.text
    if baseName.starts(with: "test")
         && n.signature.parameterClause.parameters.isEmpty
         && n.genericParameterClause == nil
         && n.genericWhereClause == nil
    {
      result[scope.lazy.map(\.name).joined(separator: "."), default: []].append(baseName)
    }

    return .skipChildren
  }
}
