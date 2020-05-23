#include <stdio.h>

#define dynamic_name_aux(type, name, postfix)           \
  __dynamic_ ## type ## _ ## name ## _ ## postfix ## __ \

#define dynamic_struct_name(type, name) \
  dynamic_name_aux(type, name, struct)  \

#define dynamic_cleanup_name(type, name) \
  dynamic_name_aux(type, name, cleanup)  \

#define dynamic_save_name(type, name) \
  dynamic_name_aux(type, name, save)  \

#define dynamic_continue_name(type, name) \
  dynamic_name_aux(type, name, continue)  \

#define dynamic_struct_aux(type, name, struct_name) \
  typedef struct {                                  \
    type data;                                      \
    type * target;                                  \
  } struct_name                                     \

#define dynamic_struct_def(type, name)                \
  dynamic_struct_aux(type,                            \
                     name,                            \
                     dynamic_struct_name(type, name)) \

#define dynamic_cleanup_aux(type, name, struct, cleanup) \
  void cleanup (struct * arg) {                          \
    *((*arg).target) = (*arg).data;                      \
  }                                                      \

#define dynamic_cleanup_def(type, name)                 \
  dynamic_cleanup_aux(type,                             \
                      name,                             \
                      dynamic_struct_name(type, name),  \
                      dynamic_cleanup_name(type, name)) \

#define dynamic_var(type, name, value) \
  dynamic_struct_def(type, name);      \
  dynamic_cleanup_def(type, name);     \
  type name = value                    \

#define dynamic_bind_aux(type, name, value, stype, save, pop, var)      \
  for(int var = 1; var;)                                                \
    for(stype save __attribute__((cleanup(pop))) = {name, &name}; var;) \
      for(name = value; var; var = 0)                                   \

#define dynamic_bind(type, name, value)               \
  dynamic_bind_aux(type,                              \
                   name,                              \
                   value,                             \
                   dynamic_struct_name(type, name),   \
                   dynamic_save_name(type, name),     \
                   dynamic_cleanup_name(type, name),  \
                   dynamic_continue_name(type, name)) \

dynamic_var(int, x, 5);

int get_x() {
  return x;
}

void rebind() {
  dynamic_bind(int, x, 5) {
    printf("before assignment: %d\n", get_x());
    x = 222;
    printf("after assignment: %d\n", get_x());
  }
}

int main(int argc, char** argv) {
  printf("toplevel binding: %d\n", get_x());
  rebind();
  printf("toplevel binding: %d\n", get_x());
  return 0;
}
