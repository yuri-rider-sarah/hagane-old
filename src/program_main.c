#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

static void *s_malloc(size_t n) {
    void *p = malloc(n);
    if (p == NULL)
        error("Out of memory");
    return p;
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

void print_byte(int64_t c) {
    if (c < 0 || c > 255)
        error("Not a byte");
    putchar(c);
}

int64_t read_byte(void) {
    int c = getchar();
    if (c == EOF) {
        if (ferror(stdin))
            error("Failed to read byte");
        return -1;
    }
    return c;
}

static int argc;
static char **argv;

uint64_t get_argc(void) {
    return argc;
}

uint64_t get_argv_len(uint64_t i) {
    if (i >= argc)
        bounds_error();
    return strlen(argv[i]);
}

uint64_t get_argv_byte(uint64_t i, uint64_t j) {
    if (i >= argc || j >= strlen(argv[i]))
        bounds_error();
    return argv[i][j];
}

extern void hagane_main(void);

int main(int argc_, char **argv_) {
    argc = argc_;
    argv = argv_;
    hagane_main();
}
