import Foundation

private let libraryRoot = Bundle.module.resourceURL!

/// The root URL of Hylo's standard library.
public let standardLibrarySourceRoot: URL = libraryRoot.appendingPathComponent("Sources")

/// The root URL of Hylo's core library.
public let coreLibrarySourceRoot: URL = standardLibrarySourceRoot.appendingPathComponent("Core")
