import Core
import FrontEnd
import StandardLibrary
import Utils

/// The standard library module.
public let standardLibraryModule
  = LazyThrowing { try AST(libraryRoot: standardLibrarySourceRoot) }

/// The core library module.
public let coreLibraryModule
  = LazyThrowing { try AST(libraryRoot: coreLibrarySourceRoot) }
