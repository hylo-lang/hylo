// swift-tools-version:5.5
import PackageDescription

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
      url: "https://github.com/loftware/Zip2Collection.git",
      from: "0.1.0"
    ),
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
