// swift-tools-version:5.7
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
      from: "1.1.4"),
    .package(
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
    .package(
      url: "https://github.com/val-lang/Durian.git",
      from: "1.2.0"),
    .package(
      url: "https://github.com/attaswift/BigInt.git",
      from: "5.3.0"),
    .package(
      url: "https://github.com/val-lang/Swifty-LLVM",
      branch: "main"),
    .package(
      url: "https://github.com/val-lang/swift-format",
      branch: "main"),
  ],

  targets: [
    // The compiler's executable target.
    .executableTarget(
      name: "CLI",
      dependencies: [
        "ValCommand"
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "ValCommand",
      dependencies: [
        "FrontEnd",
        "IR",
        "CodeGenLLVM",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ],
      swiftSettings: allTargetsSwiftSettings),

    // Targets related to the compiler's internal library.
    .target(
      name: "FrontEnd",
      dependencies: [
        "Utils",
        "Core",
        "ValModule",
        .product(name: "Collections", package: "swift-collections"),
        .product(name: "Durian", package: "Durian"),
        .product(name: "BigInt", package: "BigInt"),
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "Core",
      dependencies: [
        "Utils",
        .product(name: "Collections", package: "swift-collections"),
        .product(name: "LLVM", package: "Swifty-LLVM"),
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "IR",
      dependencies: ["Utils", "Core", "FrontEnd"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "CodeGenLLVM",
      dependencies: [
        "Core",
        "IR",
        "Utils",
        .product(name: "LLVM", package: "Swifty-LLVM"),
      ],
      path: "Sources/CodeGen/LLVM",
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "Utils",
      dependencies: [.product(name: "BigInt", package: "BigInt")],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "TestUtils",
      dependencies: ["Core", "Utils"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "ValModule",
      path: "Library",
      resources: [.copy("Val")],
      swiftSettings: allTargetsSwiftSettings),

    .plugin(
      name: "TestGeneratorPlugin", capability: .buildTool(),
      dependencies: [.target(name: "GenerateValFileTests")]),

    .executableTarget(
      name: "GenerateValFileTests",
      dependencies: [
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        "Utils",
      ],
      swiftSettings: allTargetsSwiftSettings),

    // Test targets.
    .testTarget(
      name: "UtilsTests",
      dependencies: ["Utils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "CoreTests",
      dependencies: ["Core", "FrontEnd", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "ValTests",
      dependencies: ["Core", "FrontEnd", "IR", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),

    .testTarget(
      name: "ValCommandTests",
      dependencies: ["ValCommand"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "EndToEndTests",
      dependencies: ["ValCommand", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),
  ])
