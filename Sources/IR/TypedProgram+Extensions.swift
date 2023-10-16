import Core
import FrontEnd

extension TypedProgram {

  /// If `f` refers to a trait member, returns the declaration of that member along with the trait
  /// in which it is defined. Otherwise, returns `nil`.
  func traitMember(referredBy f: Function.ID) -> (declaration: AnyDeclID, trait: TraitType)? {
    switch f.value {
    case .lowered(let d):
      guard let t = traitDefining(d) else { return nil }
      return (declaration: d, trait: t)

    default:
      return nil
    }
  }

  /// If `f` refers to the member `d` of trait `c`, returns `(d, c)` if `d` is a requirement, or
  /// `(r, c)` if `d` is a default implementation of a requirement `r`. Otherwise, returns `nil`.
  func requirementDeclaring(
    memberReferredBy f: Function.ID
  ) -> (decl: AnyDeclID, trait: TraitType)? {
    switch f.value {
    case .lowered(let d):
      return requirementDeclaring(d)
    default:
      return nil
    }
  }

}
