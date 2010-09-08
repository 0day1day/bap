#include "asm_program.h"

asection* bfd_sections( bfd *abfd) {
  return abfd->sections;
}

asection* bfd_next_section( asection *s) {
  return s->next;
}


//        bfd_get_section_contents(abfd, section, data, 0, datasize);

bfd* asmir_get_bfd(asm_program_t *p) {
  return p->abfd;
}
