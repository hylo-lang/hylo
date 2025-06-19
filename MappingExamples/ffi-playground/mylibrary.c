#include <stdio.h>
#include <stdlib.h>

char fst(char x, char y) {
    return x;
}
char snd(char x, char y) {
    return y;
}

unsigned char hello(unsigned char c) {
    return c * 2;
}

// a complicated C hashing function with a lot of parameters of different numeric types
long long hash(long a, char b, int c, short d, long long e, unsigned long f, signed char g) {
    return a * 31 + b * 7 + c * 3 + d * 2 + e * 5 + f * 11 - g * 13;
}


double using_float(double x, double y, double z, double p1, double p2, double p3, double p4, double p5, double p6, double p7, double p8, double p9, double p10) {
    return x + y + z + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10;
}

void print_float(double x) {
    printf("Float value: %f\n", x);
}