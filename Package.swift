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
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
  ],

  targets: [
    // The compiler's executable target.
    .target(
      name: "CLI",
      dependencies: [
        "Compiler", "Library",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ]),

    // Targets related to the compiler's internal library.
    .target(
      name: "Compiler",
      dependencies: [
        "Utils",
        .product(name: "Collections", package: "swift-collections"),
      ]),
    .target(name: "Utils"),

    // Test targets.
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler", "Library"]),
    .testTarget(
      name: "CXXTests",
      dependencies: ["Compiler", "Library"]),

    .target(
      name: "Library",
      dependencies: ["Compiler"])
  ])
