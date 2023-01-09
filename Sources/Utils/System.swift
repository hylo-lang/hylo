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

/// Given a collection of source paths as specified on the valc command line, returns the paths of
/// the actual source files to process along with the paths of any files that should be skipped.
///
/// Paths of files are unconditionally treated as Val `sourceFiles`. Paths of directories are
/// recursively searched for `.val` files, which are considered Val `sourceFiles`; all others are
/// treated as `nonSourceFiles` and should be skipped.
public func sourceAndNonSourceFilesFromCommandPaths<S: Collection>(
  _ commandPaths: S
) -> (sourceFiles: [URL], nonSourceFiles: [URL])
where S.Element == URL {
  let explicitSourceFiles = commandPaths.filter { !$0.hasDirectoryPath }
  let sourceDirectories = commandPaths.filter { $0.hasDirectoryPath }

  // Recursively search the directory paths.
  var nonSourceFiles: [URL] = []
  var sourceFiles = explicitSourceFiles
  for d in sourceDirectories {
    withFiles(in: d) { f in
      if f.pathExtension == "val" { sourceFiles.append(f) } else { nonSourceFiles.append(f) }
      return true
    }
  }
  return (sourceFiles, nonSourceFiles)
}
