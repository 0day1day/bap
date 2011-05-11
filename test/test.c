#include <stdio.h>
#include <stdint.h>
//#include "test.h"

extern int _test(uint8_t r);


int main() {
    uint8_t result = 3;

    printf("Starting with result = %d\n", result);
    result = _test(result);
    printf("Done with result = %d\n", result);
    
    return 0;
}
