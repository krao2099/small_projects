#include <stdio.h>

void func() {
    int x = 5;
}

int main () {
    int x = 0;
    func();
    printf("%d\n", x);
    return 0;
}