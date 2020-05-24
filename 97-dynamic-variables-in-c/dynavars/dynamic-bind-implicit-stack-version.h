#ifndef DYNAMIC_BIND_IMPLICIT_STACK_VERSION_H
#define DYNAMIC_BIND_IMPLICIT_STACK_VERSION_H

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

#define dynamic_ref(type, name) \
  name                          \

#endif /* DYNAMIC_BIND_IMPLICIT_STACK_VERSION_H */
