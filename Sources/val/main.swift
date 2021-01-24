import Foundation

import AST
import Basic
import Parser
import Sema

// Create a new AST context.
let context = AST.Context(sourceManager: SourceManager())
context.isCompilingStdLib = true
context.diagnosticConsumer = Terminal(sourceManager: context.sourceManager)

// Create a new compilation unit.
context.stdlib = Module(id: "Val", context: context)
context.modules["Val"] = context.stdlib

// Parse a source file.
let sourceFile = try context.sourceManager.load(contentsOf: CommandLine.arguments[1])
let parser = try ValParser(sourceFile: sourceFile)
let parseTree = try parser.file()
let transformer = ParseTreeTransformer(
  sourceFile: sourceFile,
  module: context.stdlib!,
  context: context)

_ = parseTree.accept(transformer)

// Run the semantic analysis.
let sema: [AST.Pass] = [
  NameBinder(context: context),
  TypeChecker(context: context),
]
for pass in sema {
  try pass.run(on: context.stdlib!)
}

//NodePrinter(context: context).print()
