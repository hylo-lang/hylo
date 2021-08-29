// swift-tools-version:5.3
import PackageDescription

let OrderedCollections = Target.Dependency.product(name: "OrderedCollections", package: "swift-collections")
let DequeModule = Target.Dependency.product(name: "DequeModule", package: "swift-collections")

let package = Package(
  name: "Val",
  products: [
    .executable(name: "val", targets: ["val"]),
  ],
  dependencies: [
    .package(name: "swift-argument-parser", url: "https://github.com/apple/swift-argument-parser.git", from: "0.4.0"),
    .package(url: "https://github.com/apple/swift-collections", from: "0.0.1"),
  ],
  targets: [
    // The compiler's executable target.
    .target(
      name: "val",
      dependencies: [
        "Basic", "Driver", "Eval",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ]),

    // Targets related to the compiler's internal library.
    .target(
      name: "AST",
      dependencies: ["Basic", OrderedCollections],
      resources: [.copy("Builtins.json")]),
    .target(name: "Basic"),
    .target(name: "Driver", dependencies: ["AST", "Basic", "Parse", "Sema", "VIL"]),
    .target(name: "Parse", dependencies: ["AST", "Basic"]),
    .target(
      name: "Eval",
      dependencies: ["AST", "Basic", "VIL", DequeModule]),
    .target(name: "Sema", dependencies: ["AST", "Basic"]),
    .target(name: "VIL", dependencies: ["AST", "Basic"]),

    // Test targets.
    .testTarget(name: "ASTTests", dependencies: ["AST", "Basic"]),
    .testTarget(name: "ParseTests", dependencies: ["AST", "Parse", "Basic"]),
    .testTarget(
      name: "ValTests",
      dependencies: ["Basic", "Driver", "Eval"],
      resources: [.copy("TestCases")]),
  ])
