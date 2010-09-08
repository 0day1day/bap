#include <string.h>

#include <bfd.h>
// maybe all the header files should be moved to 
// libasmir/include? - ethan
#include "./traces/readtrace.h"
#include "irtoir.h"
#include "asm_program.h"
#include "irtoir-internal.h"

#include "jumpbuf.h"

bap_blocks_t * asmir_asmprogram_to_bap(asm_program_t *prog) {
  vector<bap_block_t *> *res = new vector<bap_block_t *>();
  // eww, references

  jmp_buf_set = 1;
  if (setjmp(vex_error) != 0) {
    /* There was an exception */
    jmp_buf_set = 0;
    cerr << "There was an exception" << endl;
    return NULL;
  }

  *res = generate_vex_ir(prog);
  generate_bap_ir(prog, *res);
  jmp_buf_set = 0;
  return res;
}


bap_blocks_t *asmir_asmprogram_range_to_bap(asm_program_t *prog, 
					address_t start,
					address_t end)
{
  vector<bap_block_t *> *res = new vector<bap_block_t *>();
  // eww, references

  jmp_buf_set = 1;
  if (setjmp(vex_error) != 0) {
    /* There was an exception */
    jmp_buf_set = 0;
    return NULL;
  }
  
  *res = generate_vex_ir(prog, start, end);
  generate_bap_ir(prog, *res);
  jmp_buf_set = 0;
  return res;
}

int asmir_bap_blocks_size(bap_blocks_t *bs) {
  return bs->size();

}

bap_block_t * asmir_bap_blocks_get(bap_blocks_t *bs, int i) {
  return bs->at(i);
}

void destroy_bap_block(bap_block_t *b) {
  // FIXME: inst seems to be a pointer into the asm_program_t,
  // so we don't need to free it?
  // FIXME: stuff in vex_ir seems to be allocated in VEX's own heap?
  // If so, should provide a way to free that memory too?
  //free(b->vex_ir);
  for (vector<Stmt*>::iterator j = b->bap_ir->begin();
       j != b->bap_ir->end(); j++) {
    Stmt::destroy(*j);
  }
  delete b->bap_ir;
  delete b;
}

void destroy_bap_blocks(bap_blocks_t *bs) {
  for (vector<bap_block_t *>::iterator i = bs->begin(); i != bs->end(); i++) {
    destroy_bap_block(*i);
  }
  delete bs;
}

/* Check for an error (i.e., a NULL ptr */
bool asmir_bap_blocks_error(bap_blocks_t *bs) {

  if (bs == NULL) {
    return TRUE;
  }
  else {
    return FALSE;
  }
}


address_t asmir_bap_block_address(bap_block_t *b)
{
  return b->inst;
}

int asmir_bap_block_size(bap_block_t *b) {
  return b->bap_ir->size();
}

Stmt * asmir_bap_block_get(bap_block_t *b, int i) {
  return b->bap_ir->at(i);
}


// hmm, this isn't in bfd.h, but is in the documentation...
extern "C" {
  extern void *bfd_alloc (bfd *, bfd_size_type);
}

asm_program_t*
byte_insn_to_asmp(bfd_architecture arch, address_t addr, unsigned char *bb_bytes, unsigned int len)
{
  asm_program_t *prog = asmir_new_asmp_for_arch(arch);
  unsigned char *bytes = (unsigned char*)bfd_alloc(prog->abfd, len);
  // copy the string, because the old one is freed when we return
  memcpy(bytes, bb_bytes, len);
  section_t *sec = (section_t*)bfd_alloc(prog->abfd, sizeof(section_t));
  
  sec->start_addr = addr;
  sec->datasize = len;
  sec->end_addr = addr+len;
  sec->data = bytes;
  sec->is_code = 1;
  sec->next = NULL;
  prog->segs = sec;

  struct disassemble_info *disasm_info = &prog->disasm_info;
  disasm_info->buffer = bytes;
  disasm_info->buffer_vma = addr;
  disasm_info->buffer_length = len;
  disasm_info->section = NULL;
      
  return prog;
}


// moved from ir_program.cpp
bap_block_t* asmir_addr_to_bap(asm_program_t *p, address_t addr, address_t *next)
{
  translate_init();
  bap_block_t * bap_block = generate_vex_ir(p, addr);
  generate_bap_ir_block(p, bap_block);
  if (next)
    *next = addr + asmir_get_instr_length(p, addr);
  return bap_block;
}

bap_blocks_t * asmir_bap_from_trace_file(char * filename, bool atts) 
{
  bap_blocks_t * b = read_trace_from_file(string(filename), 0, false, atts);
  return b;
}

