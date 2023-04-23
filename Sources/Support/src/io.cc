#include <cstdint>
#include <iostream>
#include <string>

extern "C" {

void _valcore_print_utf8(int64_t count, void* base) {
  std::string s;
  s.assign(static_cast<char*>(base), count);
  std::cout << s << std::endl;
}

}
