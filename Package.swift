// swift-tools-version:5.3
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
      url: "https://github.com/apple/swift-collections",
      from: "0.0.1"),
  ],

  targets: [
    // The compiler's executable target.
    .target(
      name: "CLI",
      dependencies: [
        "Compiler",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ]),

    // Targets related to the compiler's internal library.
    .target(
      name: "Compiler",
      dependencies: ["Utils"],
      resources: [.copy("Builtins.json")]),
    .target(name: "Utils"),

    // Val sources.
    .target(name: "ValLibrary", path: "Library", resources: [.copy("Public")]),

    // Test targets.
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler"],
      resources: [.copy("TestCases")]),
  ])
