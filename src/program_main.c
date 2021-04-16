#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

static void error(char *msg) {
    fprintf(stderr, "Error: %s\n", msg);
    exit(1);
}

void div_by_zero_error(void) {
    error("Division by zero");
}

void bounds_error(void) {
    error("List index out of bounds");
}

void case_error(void) {
    error("Inexhaustive cases");
}

void print_int(int64_t n) {
    printf("%"PRId64"\n", n);
}

int64_t read_int(void) {
    int64_t n;
    if (scanf("%"SCNd64, &n) != 1)
        error("Failed to read integer");
    return n;
}

extern void hagane_main(void);

int main() {
    hagane_main();
}
