/// Returns `lhs!` if `lhs` is non-`nil`; evaluates `rhs()`, which never returns, otherwise.
public func ?? <T>(lhs: T?, rhs: @autoclosure () -> Never) -> T {
  if let lhs = lhs {
    return lhs
  } else {
    rhs()
  }
}