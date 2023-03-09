/// A sequence of property offsets identifying the part of an object (i.e., a sub-object).
///
/// Given a path `p`, `p[i]` denotes the index of a stored property in the object identified by the
/// path `p[..<i]`. An empty path represents the identity.
///
/// For example, relative to a type `T = {foo: A, {bar: B, ham: C}}`:
/// - `[0]` denotes `foo`,
/// - `[1, 0]` denotes `bar`, and
/// - `[]` denotes `self`.
public typealias PartPath = [Int]
