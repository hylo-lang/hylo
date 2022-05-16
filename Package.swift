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

    .package(url: "https://github.com/dabrahams/citron.git", branch: "main"),
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
    .target(name: "Utils"),

    .target(name: "ParseGen", exclude: ["README.md"]),

    // Test targets.
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler"]),

    .testTarget(
      name: "ParseGenTests",
      dependencies: [
        "ParseGen",
        .product(name: "LoftDataStructures_Zip2Collection", package: "Zip2Collection")]),
  ])
