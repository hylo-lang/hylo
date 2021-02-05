/// A concrete or abstract construct that contains declarations.
///
/// Nodes conforming to this protocol represent a declaration space, delimiting the visibility of
/// the named identities (i.e., types and values) declared within it.
///
/// Spaces do not necessarily match concrete, lexical scopes in the program source. They may simply
/// denote a collection of nodes grouped under a single abstract entity, such as a module.
/// Nonetheless, they always form a topological hierarchy.
public protocol DeclSpace: Node {

  /// The innermost parent in which this declaration space resides.
  ///
  /// This property is always be defined, except for modules and unbound extensions. Thus, it forms
  /// a linked list from the space to the module in which it resides.
  var parentDeclSpace: DeclSpace? { get }

  /// Looks up for declarations that match the given unqualified name.
  ///
  /// This implements a core part of Val's name resolution.
  ///
  /// The default implementation (defined in `NameLookup.swift`) walks declaration space from the
  /// current one and looks for symbols that are named after `unqualifiedName`, adding all matches
  /// to the result set. Because type and function symbols can overloaded, the search always moves
  /// up to the module space. Nonetheless, all non-overlodable symbols are filtered out once one
  /// has been found. Therefore, the result set always contain all possible overloads and at most
  /// one "valid" non-overloadable declaration.
  ///
  /// The sequence of spaces visible from a specific source location is not necessarily strictly
  /// ordered. In particular, two extensions of a type are at the same "level" in the hierarchy,
  /// meaning that a (non-overloadable) symbol declared in one does not shadow the same symbol
  /// declared in the other. This situations consistutes an error, that must be caught during the
  /// semantic analyis
  ///
  /// - Parameters:
  ///   - name: The name to search.
  ///   - context: The AST context in which the search is carried out.
  func lookup(unqualified name: String, in context: Context) -> LookupResult

  /// Looks up for declarations that match the given name, directly enclosed in this space.
  func lookup(qualified name: String) -> LookupResult

}

extension DeclSpace {

  /// The module in which this declaration space resides.
  public var rootDeclSpace: Module {
    return (self as? Module) ?? parentDeclSpace!.rootDeclSpace
  }

  /// Returns whether the space is topologically nested in another.
  ///
  /// - Parameter ancestor: Another declaration space.
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

  /// A sequence containing this space and all its ancestors, from closest to farthest.
  public var spacesUpToRoot: AnySequence<DeclSpace> {
    return AnySequence({ () -> AnyIterator<DeclSpace> in
      var current: DeclSpace? = self
      return AnyIterator({
        guard let s = current else { return nil }
        current = s.parentDeclSpace
        return s
      })
    })
  }

  /// The innermost space that has its own generic parameters, starting from this one.
  public var innermostGenericSpace: GenericDeclSpace? {
    var current: DeclSpace? = self
    while let s = current {
      if let gds = s as? GenericDeclSpace, gds.hasOwnGenericParams {
        return gds
      }
      current = s.parentDeclSpace
    }
    return nil
  }

}

/// An iterable declaration space.
///
/// This is a simple, lightweight protocol that provides a default implementation for
/// `lookup(qualfified:)` (defined in `NameLookup.swift`).
protocol IterableDeclSpace: DeclSpace {

  associatedtype DeclSequence: Sequence where DeclSequence.Element == Decl

  /// All the declarations directly enclosed in this space.
  var decls: DeclSequence { get }

}

/// A declaration space that provides generic type parameters.
public protocol GenericDeclSpace: DeclSpace {

  /// A flag that indicates whether the space has generic parameters of its own.
  var hasOwnGenericParams: Bool { get }

  /// The generic enviroment of the declaration space.
  var genericEnv: GenericEnv? { get set }

  /// Prepares the generic environment.
  func prepareGenericEnv() -> GenericEnv?

}
