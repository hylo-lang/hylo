import Foundation
import BigInt

// MARK: - R1CS Serialization

extension R1CS {
  /// Writes the R1CS to a binary file in the standard .r1cs format.
  ///
  /// - Parameter url: The file URL to write to.
  /// - Throws: Any I/O errors that occur during writing.
  public func serialize(to url: URL) throws {
    var data = Data()
    
    // Magic number "r1cs"
    data.append(contentsOf: [0x72, 0x31, 0x63, 0x73])
    
    // Version 1
    data.append(uint32: 1)
    
    // Number of sections (3: header, constraints, wire2label map)
    data.append(uint32: 3)
    
    // Write header section
    let headerData = serializeHeader()
    data.append(uint32: 0x01)  // Section type: Header
    data.append(uint64: UInt64(headerData.count))
    data.append(headerData)
    
    // Write constraints section
    let constraintsData = serializeConstraints()
    data.append(uint32: 0x02)  // Section type: Constraints
    data.append(uint64: UInt64(constraintsData.count))
    data.append(constraintsData)
    
    // Write wire2label map section
    let mapData = serializeWire2LabelMap()
    data.append(uint32: 0x03)  // Section type: Wire2LabelId Map
    data.append(uint64: UInt64(mapData.count))
    data.append(mapData)
    
    try data.write(to: url)
  }
  
  /// Serializes the header section.
  private func serializeHeader() -> Data {
    var data = Data()
    
    // Field size in bytes
    let fieldSizeBytes = ((prime.bitWidth + 7) / 8)
    let fieldSize = UInt32((fieldSizeBytes + 7) / 8 * 8)  // Round up to multiple of 8
    data.append(uint32: fieldSize)
    
    // Prime (padded to field size)
    data.append(bigUInt: prime, byteCount: Int(fieldSize))
    
    // Number of wires
    data.append(uint32: wireCount)
    
    // Number of public outputs
    data.append(uint32: publicOutputCount)
    
    // Number of public inputs
    data.append(uint32: publicInputCount)
    
    // Number of private inputs
    data.append(uint32: privateInputCount)
    
    // Number of labels
    data.append(uint64: labelCount)
    
    // Number of constraints
    data.append(uint32: UInt32(constraints.count))
    
    return data
  }
  
  /// Serializes the constraints section.
  private func serializeConstraints() -> Data {
    var data = Data()
    
    let fieldSizeBytes = ((prime.bitWidth + 7) / 8)
    let fieldSize = (fieldSizeBytes + 7) / 8 * 8  // Round up to multiple of 8
    
    for constraint in constraints {
      // Serialize A
      data.append(serializeLinearCombination(constraint.a, fieldSize: fieldSize))
      
      // Serialize B
      data.append(serializeLinearCombination(constraint.b, fieldSize: fieldSize))
      
      // Serialize C
      data.append(serializeLinearCombination(constraint.c, fieldSize: fieldSize))
    }
    
    return data
  }
  
  /// Serializes a linear combination.
  private func serializeLinearCombination(_ lc: LinearCombination, fieldSize: Int) -> Data {
    var data = Data()
    
    // Sort terms by wire ID for serialization
    let sortedTerms = lc.terms.sorted { $0.wire < $1.wire }
    
    // Number of non-zero factors
    data.append(uint32: UInt32(sortedTerms.count))
    
    // Each factor
    for term in sortedTerms {
      data.append(uint32: term.wire.raw)
      data.append(bigUInt: term.coefficient, byteCount: fieldSize)
    }
    
    return data
  }
  
  /// Serializes the wire to label ID mapping.
  private func serializeWire2LabelMap() -> Data {
    var data = Data()
    
    for labelId in wireToLabelMap {
      data.append(uint64: labelId.rawValue)
    }
    
    return data
  }
}

// MARK: - Data Extension for Binary Serialization

extension Data {
  /// Appends a UInt32 in little-endian format.
  mutating func append(uint32 value: UInt32) {
    var v = value.littleEndian
    Swift.withUnsafeBytes(of: &v) { append(contentsOf: $0) }
  }
  
  /// Appends a UInt64 in little-endian format.
  mutating func append(uint64 value: UInt64) {
    var v = value.littleEndian
    Swift.withUnsafeBytes(of: &v) { append(contentsOf: $0) }
  }
  
  /// Appends a BigUInt in little-endian format, padded to the specified byte count.
  mutating func append(bigUInt value: BigUInt, byteCount: Int) {
    var bytes = [UInt8](repeating: 0, count: byteCount)
    let valueBytes = value.serialize()  // Big-endian
    
    // Convert to little-endian and pad
    let copyCount = Swift.min(valueBytes.count, byteCount)
    for i in 0..<copyCount {
      bytes[i] = valueBytes[valueBytes.count - 1 - i]
    }
    
    append(contentsOf: bytes)
  }
}
