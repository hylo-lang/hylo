namespace Val {

  // Native functions used in core library
  inline int64_t native_zeroinitializer_int64_t() { return 0; }
  inline bool native_zeroinitializer_bool() { return false; }
  inline double native_zeroinitializer_double() { return 0.0; }
  inline float native_zeroinitializer_float() { return 0.0f; }
  inline int32_t native_zeroinitializer_int32_t() { return 0; }
  inline int8_t native_zeroinitializer_int8_t() { return 0; }
  inline float native_fptrunc_float(double value) { return static_cast<float>(value); }
  inline int32_t native_trunc_int32_t(int value) { return static_cast<int32_t>(value); }
  inline int8_t native_trunc_int8_t(int value) { return static_cast<int8_t>(value); }

}
