#include <stdio.h>
#include <stdint.h>

void print(int64_t n) {
    printf("%d\n", n);
}

extern void hagane_main(void);

int main() {
    hagane_main();
}
