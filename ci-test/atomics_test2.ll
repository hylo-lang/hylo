define dso_local void @f() {
entry:
  %n = alloca i64, align 8
  store i64 0, ptr %n, align 4
  store atomic i64 1, ptr %n release, align 4
  ret void
}

declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

define dso_local i32 @main() {
entry:
  %retval = alloca i32, align 4
  store i32 0, ptr %retval, align 4
  ret i32 0
}
