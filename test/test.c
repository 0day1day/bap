#include <stdio.h>
#include <stdint.h>
//#include "test.h"

extern int _test();

int main() {
    int result = 3;

    printf("Starting with result = %d\n", result);
    result = _test();
    printf("Done with result = %d\n", result);
    
    return 0;
}
