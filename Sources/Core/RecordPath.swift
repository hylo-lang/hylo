/// A path relative to some record type, identifying a field or sub-field of that record, or the empty path denoting the identity.
///
/// Given a path `p`, `p[i]` denotes the index of a field in the object identified by the path
/// `p[..<i]`. An empty path can be used relative to any type, including non-records.
///
/// For example, relative to a type `T = {foo: A, bar: {baz: B, ham: C}}`:
/// - `[0]` denotes `foo`,
/// - `[1, 0]` denotes `bar.baz`, and
/// - `[]` denotes the whole `T` record.
public typealias RecordPath = [Int]
