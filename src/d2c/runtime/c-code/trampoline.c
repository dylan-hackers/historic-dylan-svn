#include <stdio.h>
#include <stdlib.h>
#include <runtime.h>

#include "../gc/gc.h"

/* A trampoline is a full lexical closure masquerading as a simple
 * function pointer.  When capturing a closure that will be used as a
 * callback, we dynamically build executable code that prepends the
 * closure information as the first argument of the function call
 * and jumps to the callback-closure's entry point.
 */


heapptr_t make_trampoline(void *func, descriptor_t closure,
			  int nargs, char *signature)
{
  fprintf(stderr, "make_trampoline not supported on this architecture\n");
  fflush(stderr);
  abort();
  return 0;
}
