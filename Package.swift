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
    .target(name: "val", dependencies: ["AST", "Basic", "Parser", "Sema"]),

    .target(
      name: "AST",
      dependencies: ["Basic", "Parser"],
      resources: [
        .copy("Builtin.json"),
      ]),

    .target(name: "Basic"),

    .target(
      name: "Parser",
      dependencies: ["Antlr4", "Basic"],
      exclude: ["Val.g4", "Val.interp", "Val.tokens", "ValLexer.interp", "ValLexer.tokens"]),

    .target(name: "Sema", dependencies: ["AST", "Basic"]),
  ])
