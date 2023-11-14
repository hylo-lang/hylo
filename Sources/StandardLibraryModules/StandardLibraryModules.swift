import Core
import FrontEnd
import StandardLibrary
import Utils

/// The standard library module.
public let standardLibraryModule = Lazy { AST(libraryRoot: standardLibrarySourceRoot) }

/// The core library module.
public let coreLibraryModule = Lazy { AST(libraryRoot: coreLibrarySourceRoot) }
