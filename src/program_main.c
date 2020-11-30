#include <stdio.h>
#include <inttypes.h>

void print(int64_t n) {
    printf("%"PRId64"\n", n);
}

extern void hagane_main(void);

int main() {
    hagane_main();
}
