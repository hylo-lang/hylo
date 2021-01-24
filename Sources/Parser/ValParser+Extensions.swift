import Antlr4
import Basic

extension ValParser {

  public convenience init(sourceFile: SourceFile) throws {
    let input = ANTLRInputStream(Array(sourceFile), sourceFile.count)
    let lexer = ValLexer(input)
    let tokens = CommonTokenStream(lexer)
    try self.init(tokens)
  }

}
