// swift-tools-version:5.6
import PackageDescription

/// Settings to be passed to swiftc for all targets.
let allTargetsSwiftSettings: [SwiftSetting] = [
  .unsafeFlags(["-warnings-as-errors"])
]

let package = Package(
  name: "Val",

  platforms: [
    .macOS(.v12)
  ],

  products: [
    .executable(name: "valc", targets: ["CLI"])
  ],

  dependencies: [
    .package(
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "0.4.0"),
    .package(
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
    .package(
      url: "https://github.com/val-lang/Durian.git",
      from: "1.2.0"),
    .package(
      url: "https://github.com/attaswift/BigInt.git",
      from: "5.3.0"),
  ],

  targets: [
    // The compiler's executable target.
    .executableTarget(
      name: "CLI",
      dependencies: [
        "Compiler",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ],
      swiftSettings: allTargetsSwiftSettings),

    // Targets related to the compiler's internal library.
    .target(
      name: "Compiler",
      dependencies: [
        "Utils",
        "ValModule",
        .product(name: "Collections", package: "swift-collections"),
        .product(name: "Durian", package: "Durian"),
        .product(name: "BigInt", package: "BigInt"),
      ],
      exclude: ["CXX/README.md"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "ValModule",
      path: "Library",
      resources: [.copy("Core")],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "Utils",
      dependencies: [.product(name: "BigInt", package: "BigInt")],
      swiftSettings: allTargetsSwiftSettings
    ),

    // Test targets.
    .testTarget(
      name: "ValTests",
      dependencies: ["Compiler"],
      resources: [.copy("TestCases")],
      swiftSettings: allTargetsSwiftSettings),
  ])
