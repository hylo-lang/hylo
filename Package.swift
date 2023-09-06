// swift-tools-version:5.7
import PackageDescription

/// Settings to be passed to swiftc for all targets.
let allTargetsSwiftSettings: [SwiftSetting] = [
  .unsafeFlags(["-warnings-as-errors"])
]

let package = Package(
  name: "Hylo",

  platforms: [
    .macOS(.v13)
  ],

  products: [
    .executable(name: "hc", targets: ["hc"])
  ],

  dependencies: [
    .package(
      url: "https://github.com/apple/swift-argument-parser.git",
      from: "1.1.4"),
    .package(
      url: "https://github.com/apple/swift-collections.git",
      from: "1.0.0"),
    .package(
      url: "https://github.com/hylo-lang/Durian.git",
      from: "1.2.0"),
    .package(
      url: "https://github.com/attaswift/BigInt.git",
      from: "5.3.0"),
    .package(
      url: "https://github.com/hylo-lang/Swifty-LLVM",
      branch: "main"),
    .package(
      url: "https://github.com/apple/swift-format",
      from: "508.0.1"),
    .package(url: "https://github.com/apple/swift-docc-plugin.git", from: "1.1.0"),
    .package(
      url: "https://github.com/SwiftPackageIndex/SPIManifest.git",
      from: "0.12.0"),
  ],

  targets: [
    // The compiler's executable target.
    .executableTarget(
      name: "hc",
      dependencies: [
        "Driver"
      ],
      swiftSettings: allTargetsSwiftSettings),

    .executableTarget(
      name: "hylo-demangle",
      dependencies: [
        "IR"
      ],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "Driver",
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
        "HyloModule",
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
      dependencies: ["Core", "Driver", "Utils"],
      swiftSettings: allTargetsSwiftSettings),

    .target(
      name: "HyloModule",
      path: "Library",
      resources: [.copy("Hylo")],
      swiftSettings: allTargetsSwiftSettings),

    .plugin(
      name: "TestGeneratorPlugin", capability: .buildTool(),
      dependencies: [.target(name: "GenerateHyloFileTests")]),

    .executableTarget(
      name: "GenerateHyloFileTests",
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
      name: "DriverTests",
      dependencies: ["Driver"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "ManglingTests",
      dependencies: ["Core", "FrontEnd", "IR", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "HyloTests",
      dependencies: ["Core", "FrontEnd", "IR", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),

    .testTarget(
      name: "EndToEndTests",
      dependencies: ["Driver", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),

    .testTarget(
      name: "LibraryTests",
      dependencies: ["Driver", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings,
      plugins: ["TestGeneratorPlugin"]),
  ])
