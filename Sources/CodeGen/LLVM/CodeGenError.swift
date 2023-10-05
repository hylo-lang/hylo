import Core

/// Traps indicating that `t` is not representable in LLVM.
func notLLVMRepresentable<T: TypeProtocol>(
  _ t: T, file: StaticString = #filePath, line: UInt = #line
) -> Never {
  preconditionFailure("'\(t)' is not representable in LLVM", file: (file), line: line)
}
