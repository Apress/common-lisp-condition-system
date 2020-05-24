#ifndef DYNAMIC_BIND_MEMCPY_VERSION_H
#define DYNAMIC_BIND_MEMCPY_VERSION_H

#include <string.h>

/*
 * This uses nested for()s, which more or less work like LET. The
 * pattern to have macros that have what would be a &body is to say:
 *
 * #define with_foo() \
 *     for (int run = 1; run; run = 0)
 *
 * FOR AS LET AND PROGN
 *
 * You could nest multiple for()s to either do things or define new
 * lexical variables, like:
 *
 *    for (int run = 1; run; )
 *      for (int somevar = 42; run; )
 *        for (dosomething(); run; run = 0)
 *          <<body>>
 *
 * The pattern is to have the outermost for() have a loop control
 * variable 'run', which is a flag to indicate whether to run the
 * loop. All for()s must then test for 'run' and the innermost for()
 * must have 'run = 0' as its step form to make the loop run exactly
 * once.
 *
 * UNWIND-PROTECT
 *
 * The second trick is to use __attribute__((cleanup (cleanup_fun)))
 * to do some work after the body is exited. This is a bit like
 * UNWIND-PROTECT, but without support for non-local exits.
 *
 * For example to create a foo object via some 'alloc_foo' function
 * and upon exit free it again by calling 'free_foo', you first need
 * define a helper function 'cleanup_foo' since the cleanup function
 * is passed a pointer to the local variable it is attached to.
 *
 *    static void cleanup_foo (foo_t **p)
 *    {
 *      free_foo (*p);
 *    }
 *
 * We could now integrate this into our for() chain to allocate a foo
 * before the body is run and deallocate it when body is left.
 *
 *    #define with_foo(var)                             \
 *      for (int run = 1; run; )                        \
 *        for (foo_t *var                               \
 *              __attribute__((cleanup(cleanup_foo)))   \
 *              = alloc_foo ();                         \
 *             run;
 *             run = 0)
 *
 * This macro could now be used naturally:
 *
 *    void blah (void)
 *    {
 *      with_foo (x)
 *       {
 *         dosomething (x);
 *       }
 *    }
 *
 * DYNAMIC BINDING
 *
 * The C compiler is clever enough to inline the memcpy(3) and use
 * 'mov' instructions. The dynbind_frame is just like any other local
 * variable and the components, which are constant thoughout the
 * lifetime will not hit the stack or use registers.
 *
 */

/* -- Implementation ------------------------------------------------------- */

struct dynbind_frame {
  void *addr;      /* pointer to lvalue bound */
  const void *old; /* pointer to saved value */
  size_t sz;       /* size of value */
};

static void dynbind_cleanup (const struct dynbind_frame *frame)
{
  memcpy (frame->addr, frame->old, frame->sz);
}

/* The macro dynbind() is capable of working with arbitrary lvalues,
 * not just variable names. For this example, however, we will only
 * use its capability to work with variable names.
 * The code contains five nested for()s, left not indented for
 * readability.
 */

#define dynbind(lvalue, val)                                    \
  /* loop control variable */                                   \
  for (int dynbind_run = 1;                                     \
       dynbind_run; )                                           \
  /* get hold of address of the lvalue. This is the only place  \
     where the lvalue is evaluated. */                          \
  for (__typeof__(lvalue) * const dynbind_addr = &(lvalue);     \
       dynbind_run; )                                           \
  /* save the old value */                                      \
  for (const __typeof__(lvalue) dynbind_old = *dynbind_addr;    \
       dynbind_run; )                                           \
  /* setup the frame for dynbind_cleanup to pick up. */         \
  for (const struct dynbind_frame dynbind_save                  \
         __attribute__ ((cleanup (dynbind_cleanup)))            \
         = { dynbind_addr,                                      \
             &dynbind_old,                                      \
             sizeof (lvalue) };                                 \
       dynbind_run; )                                           \
  /* install the new value into what `lvalue` is */             \
  for (*dynbind_addr = (val);                                   \
       dynbind_run;                                             \
       dynbind_run = 0) /* all done */                          \

#define dynamic_var(type, name, value) \
  type name = value                    \

#define dynamic_bind(type, name, value) \
  dynbind(name, value)                  \

#define dynamic_ref(type, name) \
  name                          \

#endif /* DYNAMIC_BIND_MEMCPY_VERSION_H */
