#include "asm_program.h"
#include <errno.h>

/* http://sourceware.org/binutils/docs/bfd/Symbols.html#Symbols */


static void make_assertions()
{
  // Assertions needed for the camlidl interface
}


asymbol ** asmir_get_symbols(asm_program_t *prog, long *num)
{
  static int do_asserts = 1;
  long storage_needed, res;
  asymbol **symbol_table;

  if (do_asserts) {
    make_assertions();
    do_asserts = 0;
  }
  
  storage_needed = bfd_get_symtab_upper_bound(prog->abfd);
  if (storage_needed <= 0) goto fail;

  symbol_table = bfd_alloc(prog->abfd, storage_needed);
  if (!symbol_table) goto fail;

  res = bfd_canonicalize_symtab(prog->abfd, symbol_table);
  if (res <= 0) goto fail;

  *num = res;
  return symbol_table;

  fail:
  *num = 0;
  return NULL;

}
