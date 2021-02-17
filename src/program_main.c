#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

void print(int64_t n) {
    printf("%"PRId64"\n", n);
}

int64_t read(void) {
    int64_t n;
    if (scanf("%"SCNd64, &n) != 1) {
        fprintf(stderr, "Error: Failed to read integer\n");
        exit(1);
    }
    return n;
}

extern void hagane_main(void);

int main() {
    hagane_main();
}
