namespace Val {

  // Native functions used in core library
  inline int64_t native_zeroinitializer_i64() { return 0; }
  inline bool native_zeroinitializer_i1() { return false; }
  inline double native_zeroinitializer_double() { return 0.0; }
  inline float native_zeroinitializer_float() { return 0.0f; }
  inline int32_t native_zeroinitializer_i32() { return 0; }
  inline int8_t native_zeroinitializer_i8() { return 0; }
  inline float native_fptrunc_double_float(double value) { return static_cast<float>(value); }
  inline int32_t native_trunc_i64_i32(int value) { return static_cast<int32_t>(value); }
  inline int8_t native_trunc_i64_i8(int value) { return static_cast<int8_t>(value); }

}
