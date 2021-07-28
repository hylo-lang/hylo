/// The result of a name lookup in a declaration space.
///
/// This is essentially a wrapper around two collections of type and value declarations.
public struct LookupResult: Collection {

  /// The type declarations that were found.
  public var types: [TypeDecl]

  /// The value declarations that were found.
  public var values: [ValueDecl]

  public init(types: [TypeDecl] = [], values: [ValueDecl] = []) {
    self.types = types
    self.values = values
  }

  /// Filters the lookup result with the given predicate.
  ///
  /// - Parameter isIncluded: A closure that accepts either a type or  avalue declarations and
  ///   returns whether is should be included in the returned set.
  public func filter(_ isIncluded: (TypeOrValueDecl) throws -> Bool) rethrows -> LookupResult {
    return LookupResult(
      types: try types.filter(isIncluded),
      values: try values.filter(isIncluded))
  }

  /// Appends the declarations of another lookup result.
  ///
  /// - Parameter other: Another lookup result.
  public mutating func append(contentsOf other: LookupResult) {
    types.append(contentsOf: other.types)
    values.append(contentsOf: other.values)
  }

  public var startIndex: Int { 0 }

  public var endIndex: Int {
    return types.count + values.count
  }

  public func index(after position: Int) -> Int {
    return position + 1
  }

  public subscript(position: Int) -> TypeOrValueDecl {
    if position < types.count {
      return types[position]
    } else {
      return values[position - types.count]
    }
  }

}
