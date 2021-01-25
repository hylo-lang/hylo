/// A concrete or abstract construct that contains declarations.
///
/// Nodes conforming to this protocol represent a declaration space, delimiting the visibility of
/// the named identities (i.e., types and values) declared within it.
///
/// Spaces do not necessarily match concrete, lexical scopes in the program source. They may simply
/// denote a collection of nodes grouped under a single abstract entity, such as a module.
/// Nonetheless, they always form a topological hierarchy.
public protocol DeclSpace: Node {

  /// The enclosing space.
  var parentDeclSpace: DeclSpace? { get set }

  /// Returns whether the space is topologically nested in another.
  ///
  /// - Parameter ancestor: Another declaration space.
  func isDescendant(of ancestor: DeclSpace) -> Bool

  /// The type and value declarations directly enclosed in this space.
  var localTypeAndValueDecls: [TypeOrValueDecl] { get }

  /// Looks up for declarations that match the give unqualified name.
  ///
  /// This implements a core part of Val's name resolution.
  ///
  /// The default implementation (defined in `NameLookup.swift`) walks declaration space from the
  /// current one and looks for symbols that are named after `unqualifiedName`, adding all matches
  /// to the result set. Because type and function symbols can overloaded, the search always moves
  /// up to the module space. Nonetheless, all non-overlodable symbols are filtered out once one
  /// has been found. Therefore, the result set always contain all possible overloads and at most
  /// one "valid" non-overloadable declarations.
  ///
  /// The sequence of spaces visible from a specific source location is not necessarily strictly
  /// ordered. In particular, two extensions of a type are at the same "level" in the hierarchy,
  /// meaning that a (non-overloadable) symbol declared in one does not shadow the same symbol
  /// declared in the other. This situations consistutes an error, that is must be caught during
  /// the semantic analyis
  ///
  /// - Parameters:
  ///   - unqualifiedName: The name to search.
  ///   - context: The AST context in which the search is carried out.
  func lookup(_ unqualifiedName: String, in context: Context) -> LookupResult

}

extension DeclSpace {

  public func isDescendant(of ancestor: DeclSpace) -> Bool {
    var parent = parentDeclSpace
    while parent != nil {
      if parent === ancestor {
        return true
      }
      parent = parent!.parentDeclSpace
    }

    return false
  }

}
