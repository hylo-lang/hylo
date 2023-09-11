import Foundation

public let standardLibraryRoot = URL(fileURLWithPath: #filePath).deletingLastPathComponent()
public let standardLibraryBundleRoot = Bundle.module.url(forResource: "SourceGeneration", withExtension: nil)!
