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
