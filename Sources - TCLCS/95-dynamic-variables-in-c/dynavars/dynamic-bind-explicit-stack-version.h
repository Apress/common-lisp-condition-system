#ifndef DYNAMIC_BIND_EXPLICIT_STACK_VERSION_H
#define DYNAMIC_BIND_EXPLICIT_STACK_VERSION_H

// The name of the (VALUE, PREVIOUS) record of this type and name.
#define dynamic_record(type,name) __ ## type ## _ ## name ## _record

// The name of the stack of dynamic bindings for this type and name.  Each
// stack is a linked list of records.  The final record is the global one,
// all new records are allocated on the stack by uses of dynamic_bind.
#define dynamic_stack(type,name) __ ## type ## _ ## name ## _stack

// The name of the cleanup function that is invoked by GCC whenever control
// is transferred outside of a dynamic_bind block.
#define dynamic_pop(type,name) __ ## type ## _ ## name ## _pop

#define dynamic_bind_aux(initform, record, stack, pop)      \
  for(struct record __record __attribute__((cleanup (pop))) \
        = {initform, 1, stack};                             \
      stack->validp && (stack = &__record);                 \
      stack->validp = 0)                                    \

#define dynamic_bind(type, name, initform)     \
  dynamic_bind_aux(initform,                   \
                   dynamic_record(type, name), \
                   dynamic_stack(type, name),  \
                   dynamic_pop(type,name))     \

#define dynamic_var_aux(type, initform, record, stack, pop) \
  struct record {                                           \
    type value;                                             \
    int validp;                                             \
    struct record* previous;                                \
  } record = {initform, 1, 0};                              \
  struct record* stack = &record;                           \
  void pop(struct record* x) {stack = stack->previous;}     \

#define dynamic_var(type, name, initform)    \
  dynamic_var_aux(type,                      \
                  initform,                  \
                  dynamic_record(type,name), \
                  dynamic_stack(type,name),  \
                  dynamic_pop(type,name))    \

// In this example, we need a macro to access the current value of the
// dynamic variable.
#define dynamic_ref(type, name)   \
  dynamic_stack(type,name)->value \

#endif /* DYNAMIC_BIND_EXPLICIT_STACK_VERSION_H */
