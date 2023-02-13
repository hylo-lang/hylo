/// A transformation that can be run on code.
public typealias CodeTransform = (String) -> String

/// Returns an identity code transform.
public func identity() -> CodeTransform {
  return { code in code }
}
