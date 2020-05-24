#include <stdio.h>
#include <stdint.h>

// Adapted from https://rosettacode.org/wiki/Linear_congruential_generator#C

dynamic_var(uint32_t, random_state, 0x1337);

int random_uint8() {
  dynamic_ref(uint32_t, random_state)
    = (dynamic_ref(uint32_t, random_state) * 1103515245 + 12345) & 0xFFFFFFFF;
  return dynamic_ref(uint32_t, random_state) & 0xFF;
}

int main(int argc, char** argv) {
  printf("Outside bindings, random number 1: %d\n", random_uint8());
  dynamic_bind(uint32_t, random_state, 0x1337) {
    printf("Inside first binding, random number 1: %d\n", random_uint8());
    printf("Inside first binding, random number 2: %d\n", random_uint8());
    printf("Inside first binding, random number 3: %d\n", random_uint8());
  }
  printf("Outside bindings, random number 2: %d\n", random_uint8());
  dynamic_bind(uint32_t, random_state, 0x1337) {
    printf("Inside second binding, random number 1: %d\n", random_uint8());
    printf("Inside second binding, random number 2: %d\n", random_uint8());
    printf("Inside second binding, random number 3: %d\n", random_uint8());
  }
  printf("Outside bindings, random number 3: %d\n", random_uint8());
}
