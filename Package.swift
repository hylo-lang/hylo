// swift-tools-version:5.3
import PackageDescription

let package = Package(
  name: "Val",
  products: [
    .executable(name: "val", targets: ["val"]),
  ],
  dependencies: [
    .package(name: "Antlr4", path: "antlr4/runtime/Swift"),
  ],
  targets: [
    .target(name: "val", dependencies: ["Driver", "Basic"]),
    .target(
      name: "AST",
      dependencies: ["Basic", "Parser"],
      resources: [.copy("Builtins.json")]),
    .target(name: "Basic"),
    .target(name: "Driver", dependencies: ["AST", "Basic", "Eval", "Parser", "Sema", "VIL"]),
    .target(
      name: "Parser",
      dependencies: ["Antlr4", "Basic"],
      exclude: ["Val.g4", "Val.interp", "Val.tokens", "ValLexer.interp", "ValLexer.tokens"]),
    .target(name: "Eval", dependencies: ["AST", "Basic", "VIL"]),
    .target(name: "Sema", dependencies: ["AST", "Basic"]),
    .target(name: "VIL", dependencies: ["AST", "Basic"]),

    .testTarget(
      name: "ValTests",
      dependencies: ["Basic", "Driver"],
      resources: [.copy("TestCases")]),
  ])
