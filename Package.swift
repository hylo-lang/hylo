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
        "ValCommand"
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "ValCommand",
      dependencies: [
        "FrontEnd",
        "IR",
        "CodeGenCXX",
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
        "Utils"
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "IR",
      dependencies: ["Utils", "Core", "FrontEnd"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "CodeGenCXX",
      dependencies: ["FrontEnd", "Utils"],
      path: "Sources/CodeGen/CXX",
      exclude: ["README.md"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "ValModule",
      path: "Library",
      resources: [.copy("Core"), .copy("CXX")],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "Utils",
      dependencies: [.product(name: "BigInt", package: "BigInt")],
      swiftSettings: allTargetsSwiftSettings),

    // Test targets.
    .target(
      name: "TestSupport",
      dependencies: ["Utils", "Core", "FrontEnd"],
      path: "Tests/TestSupport",
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "UtilsTests",
      dependencies: ["Utils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "ValTests",
      dependencies: ["FrontEnd", "Core", "CodeGenCXX", "IR", "TestSupport"],
      resources: [.copy("TestCases")],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "ValCommandTests",
      dependencies: ["ValCommand", "TestSupport"],
      resources: [.copy("Inputs")],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "EndToEndTests",
      dependencies: ["ValCommand", "TestSupport"],
      resources: [.copy("TestCases")],
      swiftSettings: allTargetsSwiftSettings),
  ])
