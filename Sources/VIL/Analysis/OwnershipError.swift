import struct Basic.Diag
import struct Basic.SourceRange

/// An ownership error.
enum OwnershipError: Error {

  case moveOfActiveLoans

  case moveOfInoutedValue

  case moveOfProjectedValue

  case overlappingMutableAccesses

  case useAfterFree

  case useOfConsumedValue

  case useOfPartialValue

  case useOfUninitializedValue

  case writeAccessToInoutedValue

  case writeAccessToProjectedValue

  func diag(anchor: SourceRange?) -> Diag {
    switch self {
    default:
      return Diag("\(self)", anchor: anchor)
    }
  }

}
