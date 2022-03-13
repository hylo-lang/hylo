// swift-tools-version:5.3
import PackageDescription

let OrderedCollections = Target.Dependency.product(
  name: "OrderedCollections", package: "swift-collections")
let DequeModule = Target.Dependency.product(
  name: "DequeModule", package: "swift-collections")
let CitronParser = Target.Dependency.product(
  name: "CitronParserModule", package: "citron")

let package = Package(
  name: "Val",

  products: [
    .executable(name: "val", targets: ["CLI"]),
  ],

  dependencies: [
    .package(
      name: "swift-argument-parser",
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "0.4.0"),
    .package(
      url: "https://github.com/apple/swift-collections",
      from: "0.0.1"),
    .package(url: "https://github.com/roop/citron.git", .branch("master"))
  ],

  targets: [
    // The compiler's executable target.
    .target(
      name: "CLI",
      dependencies: [
        "Compiler", "Driver", "Eval",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ]),

    // Targets related to the compiler's internal library.
    .target(
      name: "Compiler",
      dependencies: ["Utils", OrderedCollections, CitronParser],
      exclude: ["Parsing/Parser.citron"],
      resources: [.copy("Builtins.json")]
    ),
    .target(name: "Utils"),
    .target(name: "Driver", dependencies: ["Compiler", "Utils", "ValLibrary"]),
    .target(name: "Eval", dependencies: ["Compiler", DequeModule]),

    // Val sources.
    .target(name: "ValLibrary", path: "Library", resources: [.copy("Public")]),

    // Test targets.
    .testTarget(name: "ParseTests", dependencies: ["Compiler", DequeModule]),
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler", "Driver", "Eval"],
      resources: [.copy("TestCases")]),
  ])
