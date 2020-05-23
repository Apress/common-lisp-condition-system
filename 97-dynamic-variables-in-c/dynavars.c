#include <stdio.h>

#define dynamic_name(type, name, postfix)               \
  __dynamic_ ## type ## _ ## name ## _ ## postfix ## __ \

#define dynamic_var_aux(type, name, cleanup) \
  void cleanup (type * arg) { name = *arg; } \

#define dynamic_var(type, name, value)               \
  type name = value;                                 \
  dynamic_var_aux(type,                              \
                  name,                              \
                  dynamic_name(type, name, cleanup)) \

#define dynamic_bind_aux(type, name, value, save, pop, var)   \
  for(int var = 1; var;)                                      \
    for(type save __attribute__((cleanup(pop))) = name; var;) \
      for(name = value; var; var = 0)                         \

#define dynamic_bind(type, name, value)                \
  dynamic_bind_aux(type,                               \
                   name,                               \
                   value,                              \
                   dynamic_name(type, name, save),     \
                   dynamic_name(type, name, cleanup),  \
                   dynamic_name(type, name, continue)) \

dynamic_var(int, x, 5);

int get_x() {
  return x;
}

void rebind() {
  dynamic_bind(int, x, 42) {
    printf("before assignment: %d\n", get_x());
    x = 24;
    printf("after assignment: %d\n", get_x());
  }
}

int main(int argc, char** argv) {
  printf("toplevel binding: %d\n", get_x());
  rebind();
  printf("toplevel binding: %d\n", get_x());
  return 0;
}
