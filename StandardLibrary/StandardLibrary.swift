import Foundation

fileprivate let libraryRoot = Bundle.module.resourceURL!

/// The root URL of Hylo's standard library.
public let standardLibrarySourceRoot = libraryRoot.appendingPathComponent("Sources")

/// The root URL of Hylo's core library.
public let coreLibrarySourceRoot = standardLibrarySourceRoot.appendingPathComponent("Core")

