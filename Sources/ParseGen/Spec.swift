/// The path to the specification file
public let specPath
  = "/" + (#filePath.split(separator: "/").dropLast(3)
             + ["spec", "spec.md"]).joined(separator: "/")
