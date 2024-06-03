#include <unistd.h>


int main(int argc, char** argv) {
    return 0;
}
#if 0
int argc_get() {
    asm volatile (
        ".intel_syntax noprefix\n"
        "mov eax, esp\n"
        ".att_syntax"
    );
}

char** argv_get() {
    asm volatile (
        ".intel_syntax noprefix\n"
        "lea rax, [rsp + 4]\n"
        ".att_syntax"
    );
}
#endif

void _start() {
    int res = main(1, 0);

    _exit(0);
}
