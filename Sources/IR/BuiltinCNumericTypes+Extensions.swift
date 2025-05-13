import FrontEnd

extension BuiltinCNumericType {
  // todo fetch these based on the target
  public var size: Int {
    switch self {
    case .cChar, .cUChar, .cSChar:
      return 1
    case .cShort, .cUShort:
      return 2
    case .cInt, .cUInt:
      return 4
    case .cLong, .cULong:
      return 8
    case .cLongLong, .cULongLong:
      return 8
    case .cFloat:
      return 4
    case .cDouble:
      return 8
    case .cLongDouble:
      return 16
    }
  }

  public var bitwidth: Int {
    return size * 8
  }

  public var alignment: Int {
    switch self {
    case .cChar, .cUChar, .cSChar:
      return 1
    case .cShort, .cUShort:
      return 2
    case .cInt, .cUInt:
      return 4
    case .cLong, .cULong:
      return 8
    case .cLongLong, .cULongLong:
      return 8
    case .cFloat:
      return 4
    case .cDouble:
      return 8
    case .cLongDouble:
      return 16
    }
  }
  public var signedInteger: Bool {
    switch self {
    case .cChar, .cSChar, .cShort, .cInt, .cLong, .cLongLong:
      return true
    case .cUChar, .cUShort, .cUInt, .cULong, .cULongLong:
      return false
    case .cFloat, .cDouble, .cLongDouble:
      return false
    }
  }
}

// #include <stdalign.h>
// #include <stdio.h>
// int main() {
//     printf("Type                \tAlign. \tSize\n");
//     printf("char                \t%lu \t%lu\n", alignof(char), sizeof(char));
//     printf("unsigned char       \t%lu \t%lu\n", alignof(unsigned char), sizeof(unsigned char));
//     printf("short               \t%lu \t%lu\n", alignof(short), sizeof(short));
//     printf("unsigned short      \t%lu \t%lu\n", alignof(unsigned short), sizeof(unsigned short));
//     printf("int                 \t%lu \t%lu\n", alignof(int), sizeof(int));
//     printf("unsigned int        \t%lu \t%lu\n", alignof(unsigned int), sizeof(unsigned int));
//     printf("long                \t%lu \t%lu\n", alignof(long), sizeof(long));
//     printf("unsigned long       \t%lu \t%lu\n", alignof(unsigned long), sizeof(unsigned long));
//     printf("long long           \t%lu \t%lu\n", alignof(long long), sizeof(long long));
//     printf("unsigned long long  \t%lu \t%lu\n", alignof(unsigned long long), sizeof(unsigned long long));
//     printf("float               \t%lu \t%lu\n", alignof(float), sizeof(float));
//     printf("double              \t%lu \t%lu\n", alignof(double), sizeof(double));
//     printf("long double         \t%lu \t%lu\n", alignof(long double), sizeof(long double));

//     return 0;
// }

// char                    1       1
// unsigned char           1       1
// short                   2       2
// unsigned short          2       2
// int                     4       4
// unsigned int            4       4
// long                    8       8
// unsigned long           8       8
// long long               8       8
// unsigned long long      8       8
// float                   4       4
// double                  8       8
// long double             16      16
