// swift-tools-version:5.6
import PackageDescription

let package = Package(
  name: "Val",

  platforms: [
    .macOS(.v12),
  ],

  products: [
    .executable(name: "valc", targets: ["CLI"]),
  ],

  dependencies: [
    .package(
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "0.4.0"),
    .package(
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
    .package(
      url: "https://github.com/kyouko-taiga/LLVMSwift.git",
      branch: "master"),
  ],

  targets: [
    // The compiler's executable target.
    .executableTarget(
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
        "ParserCombinators",
        .product(name: "Collections", package: "swift-collections"),
        .product(name: "LLVM", package: "LLVMSwift"),
      ]),

      .target(name: "Utils"),

      .target(name: "ParserCombinators"),

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
