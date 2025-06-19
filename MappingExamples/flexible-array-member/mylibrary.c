#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


/// A structure with a flexible array member of 32-bit int elements
typedef struct {
    // The nummber of elements in the flexible array member
    int64_t count;

    // Something that would lead to paddings
    uint8_t random;

    // Flexible array member
    int32_t data[];
} FAMStruct;

FAMStruct* create_fam(int64_t count) {
    FAMStruct* s = (FAMStruct*) calloc(1, sizeof(FAMStruct) + count * sizeof(int32_t));
    s->count = count;
    return s;
}

void free_fam(FAMStruct* s) {
    free(s);
}

int32_t read_at(FAMStruct* s, int64_t index) {
    if (index < 0 || index >= s->count) {
        puts("Index out of bounds\n");
        return -1; // Error value
    }
    return s->data[index];
}

int32_t write_at(FAMStruct* s, int64_t index, int32_t value) {
    if (index < 0 || index >= s->count) {
        puts("Index out of bounds\n");
        return -1; // Error value
    }
    s->data[index] = value;
    return 0; // Success
}
int64_t get_count(FAMStruct* s) {
    return s->count;
}