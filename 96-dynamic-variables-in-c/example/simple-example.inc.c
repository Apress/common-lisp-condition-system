#include <stdio.h>

dynamic_var(int, x, 5);

int get_x() {
  return dynamic_ref(int, x);
}

void rebind() {
  dynamic_bind(int, x, 42) {
    printf("before assignment: %d\n", get_x());
    dynamic_ref(int, x) = 24;
    printf("after assignment: %d\n", get_x());
  }
}

int main(int argc, char** argv) {
  printf("toplevel binding: %d\n", get_x());
  rebind();
  printf("toplevel binding: %d\n", get_x());
  return 0;
}
