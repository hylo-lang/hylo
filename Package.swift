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

  ],

  dependencies: [
    .package(
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "1.1.4"),
    .package(
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
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

    .target(
      name: "Core",
      dependencies: [
        "Utils",
        .product(name: "LLVM", package: "Swifty-LLVM"),
        .product(name: "Collections", package: "swift-collections"),
      ],
      swiftSettings: allTargetsSwiftSettings),


    .target(
      name: "Utils",
      dependencies: [.product(name: "BigInt", package: "BigInt")],
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
        .product(name: "ArgumentParser", package: "swift-argument-parser")
      ],
      swiftSettings: allTargetsSwiftSettings),

    // Test targets.
    .testTarget(
      name: "EndToEndTests",
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),
  ])
