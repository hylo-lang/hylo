extension Diagnostic {

  /// Returns whether `l` should be logged before `r`.
  public static func isLoggedBefore(_ l: Diagnostic, _ r: Diagnostic) -> Bool {
    let lhs = l.site
    let rhs = r.site

    if lhs.file == rhs.file {
      return lhs.first() < rhs.first()
    } else {
      return lhs.file.url.fileSystemPath.lexicographicallyPrecedes(rhs.file.url.path)
    }
  }

}
