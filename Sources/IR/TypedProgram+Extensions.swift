import Core
import FrontEnd

extension TypedProgram {

  /// If `f` refers to a trait member, returns the declaration of that member along with the trait
  /// in which it is defined. Otherwise, returns `nil`.
  func traitMember(
    referredBy f: Function.ID
  ) -> (declaration: AnyDeclID, trait: TraitType)? {
    switch f.value {
    case .lowered(let d):
      guard let t = trait(defining: d) else { return nil }
      return (declaration: d, trait: TraitType(t, ast: ast))

    default:
      return nil
    }
  }

}
