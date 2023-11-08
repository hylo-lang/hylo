// swift-tools-version:5.7
import PackageDescription

#if os(Windows)
let onWindows = true
#else
let onWindows = false
#endif

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
    .executable(name: "hc", targets: ["hc"]),
    .executable(name: "hylo-demangle", targets: ["hylo-demangle"]),
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
    .package(url: "https://github.com/SomeRandomiOSDev/CBORCoding.git", from: "1.0.0"),
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
        "StandardLibrary",
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
      ],
      swiftSettings: allTargetsSwiftSettings),

    // Targets related to the compiler's internal library.
    .target(
      name: "FrontEnd",
      dependencies: [
        "Utils",
        "Core",
        "StandardLibrary",
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
      name: "StandardLibrary",
      dependencies: [.product(name: "CBORCoding", package: "CBORCoding")],
      path: "StandardLibrary",
      resources: [.copy("Sources")],
      swiftSettings: allTargetsSwiftSettings,
      plugins: [] // ["StandardLibraryBuilderPlugin"]
    ),

    .plugin(
      name: "TestGeneratorPlugin", capability: .buildTool(),
      // Workaround for SPM bug; see PortableBuildToolPlugin.swift
      dependencies: onWindows ? [] : ["GenerateHyloFileTests"]),

    .executableTarget(
      name: "GenerateHyloFileTests",
      dependencies: [
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        "Utils",
      ],
      swiftSettings: allTargetsSwiftSettings + [ .unsafeFlags(["-parse-as-library"]) ]),

    .plugin(
      name: "StandardLibraryBuilderPlugin", capability: .buildTool(),
      dependencies: onWindows ? [] : ["BuildStandardLibrary"]),

    .executableTarget(
      name: "BuildStandardLibrary",
      dependencies: [
        .product(name: "ArgumentParser", package: "swift-argument-parser"),
        .product(name: "CBORCoding", package: "CBORCoding"),
        "Utils", "FrontEnd"
      ],
      swiftSettings: allTargetsSwiftSettings + [ .unsafeFlags(["-parse-as-library"]) ]),

    // Test targets.
    .testTarget(
      name: "UtilsTests",
      dependencies: ["Utils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "DriverTests",
      dependencies: ["Driver", "TestUtils"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "ManglingTests",
      dependencies: ["Core", "FrontEnd", "IR", "TestUtils", "StandardLibrary"],
      swiftSettings: allTargetsSwiftSettings),

    .testTarget(
      name: "HyloTests",
      dependencies: ["Core", "FrontEnd", "IR", "TestUtils", "StandardLibrary"],
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
