import Core
import Foundation
import Utils

fileprivate let libraryRoot = Bundle.module.resourceURL!

/// The root URL of Hylo's standard library.
private let standardLibrarySourceRoot = libraryRoot.appendingPathComponent("Sources")

/// The root URL of Hylo's core library.
private let freestandingLibrarySourceRoot = standardLibrarySourceRoot.appendingPathComponent("Core")

extension Utils.Host {

  public static let standardLibraryAST = AST(
    libraryRoot: standardLibrarySourceRoot,
    for: CompilerConfiguration([]))

  public static let freestandingLibraryAST = AST(
    libraryRoot: freestandingLibrarySourceRoot,
    for: CompilerConfiguration(["freestanding"]))

}
