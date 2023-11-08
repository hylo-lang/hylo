import Foundation
import Utils
import Core
import FrontEnd

private let libraryRoot = Bundle.module.resourceURL!

/// The root URL of Hylo's standard library.
private let standardLibrarySourceRoot: URL = libraryRoot.appendingPathComponent("Sources")

/// The root URL of Hylo's core library.
private let coreLibrarySourceRoot: URL = standardLibrarySourceRoot.appendingPathComponent("Core")

public let standardLibraryModule = Lazy { AST(libraryRoot: standardLibrarySourceRoot) }
public let coreLibraryModule = Lazy { AST(libraryRoot: coreLibrarySourceRoot) }
