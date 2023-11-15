import CBORCoding
import Core
import Foundation
import FrontEnd
import StandardLibrary
import Utils

private let standardLibraries = LazyThrowing<[AST]> { 
  let d = try Data(contentsOf: Bundle.module.resourceURL!, options: .alwaysMapped)
  return try CBORDecoder().forAST.decode(from: d)
}

/// The standard library module.
public let standardLibraryModule = LazyThrowing { try standardLibraries[][0] }

/// The core library module.
public let coreLibraryModule = LazyThrowing { try standardLibraries[][1] }
