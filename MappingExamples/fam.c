typedef struct{
    int x;
    int y;
    int rest[];
} FAM;

int f(FAM *p, int i) {
    return p->rest[i];
}
