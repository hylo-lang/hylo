#ifndef ValBuiltin
#define ValBuiltin

#include <cstdint>
#include <cstdio>
#include <exception>

namespace Val {
namespace Builtin {

// MARK: System

[[noreturn]] void terminate() {
  std::terminate();
}

// MARK: 64-bit integer

int64_t i64_copy(int64_t const* const x) {
  return *x;
}

int64_t i64_add(int64_t const* const x, int64_t const* const y) {
  return *x + *y;
}

void i64_print(int64_t const* const x) {
  printf("%lli\n", *x);
}

} // Builtin
} // Val

#endif
