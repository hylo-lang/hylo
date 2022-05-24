// swift-tools-version:5.6
import PackageDescription

let CitronParser
  = Target.Dependency.product(name: "CitronParserModule", package: "citron")
let CitronLexer
  = Target.Dependency.product(  name: "CitronLexerModule", package: "citron")

let package = Package(
  name: "Val",

  products: [
    .executable(name: "val", targets: ["CLI"]),
  ],

  dependencies: [
    .package(
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "0.4.0"),

    .package(
      url: "https://github.com/loftware/Zip2Collection.git",
      from: "0.1.0"
    ),

    .package(url: "https://github.com/dabrahams/citron.git", branch: "master"),
    .package(url: "https://github.com/dabrahams/SwiftMarpa.git", from: "0.9.0"),
  ],

  targets: [
    // The compiler's executable target.
    .executableTarget(
      name: "CLI",
      dependencies: [
        "Compiler",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ]),

    // Targets related to the compiler's internal library.
    .target(
      name: "Compiler",
      dependencies: ["Utils"]),

    .target(
      name: "Utils",
      dependencies: [
        .product(name: "LoftDataStructures_Zip2Collection", package: "Zip2Collection")]
    ),

    .target(
      name: "ParseGen",
      dependencies: ["Utils", CitronParser, CitronLexer],
      exclude: ["README.md"],
      plugins: [ .plugin(name: "CitronParserGenerator", package: "citron") ]
    ),

    // Test targets.
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler"]),

    .testTarget(
      name: "ParseGenTests",
      dependencies: ["ParseGen", "Utils"]),
  ])
