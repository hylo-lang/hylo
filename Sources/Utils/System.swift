import Foundation

/// Recursively calls `action` with the files in `directory`.
///
/// - Parameters:
///   - action: A closure that is called with the URL of each file and returns whether the
///     recursive visit of `directory` should continue.
/// - Returns: `true` unless `action` returned `false`.
@discardableResult
public func withFiles(in directory: URL, _ action: (URL) throws -> Bool) rethrows -> Bool {
  let enumerator = FileManager.default.enumerator(
    at: directory,
    includingPropertiesForKeys: [.isRegularFileKey],
    options: [.skipsHiddenFiles, .skipsPackageDescendants])!

  for case let url as URL in enumerator {
    guard try action(url) else { return false }
  }
  return true
}

/// Given a collection of file and directory paths as specified on the valc command line, returns the paths of
/// the actual source files to process.
///
/// Paths of files in `sourcePaths` are unconditionally treated as Val source files. Paths of directories are
/// recursively searched for `.val` files, which are considered Val `sourceFiles`; all others are
/// treated as non-source files and are ignored.
public func sourceFilePaths<S: Collection>(in sourcePaths: S) -> [URL]
where S.Element == URL {
  let explicitSourceFiles = sourcePaths.filter { !$0.hasDirectoryPath }
  let sourceDirectories = sourcePaths.filter { $0.hasDirectoryPath }

  // Recursively search the directory paths, adding .val files to `sourceFiles`
  var sourceFiles = explicitSourceFiles
  for d in sourceDirectories {
    withFiles(in: d) { f in
      if f.pathExtension == "val" { sourceFiles.append(f) }
      return true
    }
  }
  return sourceFiles
}
