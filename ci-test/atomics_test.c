#include <stdint.h>
#include <stdio.h>

int main() {
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
    printf("i8 atomic free: %d\n", __atomic_always_lock_free(sizeof(i8), &i8));
    printf("i16 atomic free: %d\n", __atomic_always_lock_free(sizeof(i16), &i16));
    printf("i32 atomic free: %d\n", __atomic_always_lock_free(sizeof(i32), &i32));
    printf("i64 atomic free: %d\n", __atomic_always_lock_free(sizeof(i64), &i64));

    __atomic_store_1(&i8, 0, 0);
    __atomic_store_2(&i16, 0, 0);
    __atomic_store_4(&i32, 0, 0);
    __atomic_store_8(&i64, 0, 0);

    return 0;
}
