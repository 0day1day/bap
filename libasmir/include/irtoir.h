/*
 Owned and copyright BitBlaze, 2007. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*/

#ifndef __IRTOIR_H
#define __IRTOIR_H

typedef struct bap_block_s bap_block_t;

typedef  unsigned long long int   ULong;
typedef  ULong     Addr64;

#include "pin_frame.h"
#include "asm_program.h"

#ifdef __cplusplus

#include <vector>

#include "stmt.h"

//
// A struct that encapsulates the stages of translation of 1 asm instruction.
// inst is the original instruction.
// bap_ir is inst translated into a block of Vine IR from its VEX IR translation.
//
struct bap_block_s
{
  bfd_vma         inst;
  vector<Stmt *>  *bap_ir;
};

// Take a bap block that has gone through VEX translation and translate it
// to Vine IR.
// *DEPRECIATED*
void generate_bap_ir_block( asm_program_t *prog, bap_block_t *block );

//
// Take a vector of bap blocks that have gone through VEX translation
// and translate them to Vine IR
//
// \param vblocks Vector of bap blocks with valid VEX IR translations
// \return Vector of bap blocks with the bap_ir field filled in
//
vector<bap_block_t *> generate_bap_ir( asm_program_t *prog, vector<bap_block_t *> vblocks );

extern "C" {
  typedef struct vector<bap_block_t *> bap_blocks_t;

#else
  struct bap_block_s
  {
    bfd_vma         inst;
    struct vector_Stmt  *bap_ir;
  };


  typedef struct vector_bap_block_t bap_blocks_t;
#endif  // __cplusplus

  extern void asmir_set_print_warning(bool value);
  extern bool asmir_get_print_warning();

  extern void asmir_set_use_simple_segments(bool value);

  /* extern bap_blocks_t * asmir_asmprogram_to_bap(asm_program_t *prog); */
  /* extern bap_blocks_t * asmir_asmprogram_range_to_bap(asm_program_t *prog, address_t start, address_t end); */
  extern asm_program_t* byte_insn_to_asmp(bfd_architecture arch, address_t addr, unsigned char *bb_bytes, unsigned int len);

  extern int asmir_bap_blocks_size(bap_blocks_t *bs);
  extern bap_block_t * asmir_bap_blocks_get(bap_blocks_t *bs, int i);
  extern long asmir_bap_blocks_error(bap_blocks_t *bs);
  extern long asmir_bap_block_error(bap_block_t *bs);
  extern void destroy_bap_blocks(bap_blocks_t *bs);
  extern void destroy_bap_block(bap_block_t *bs);
  extern int asmir_bap_block_size(bap_block_t *b);
  extern address_t asmir_bap_block_address(bap_block_t *b);
  extern Stmt * asmir_bap_block_get(bap_block_t *b, int i);
  extern const char* asm_string_from_block(bap_block_t *b);
  extern char* string_blockinsn(asm_program_t *prog, bap_block_t *block);
  extern bap_blocks_t * asmir_bap_from_trace_file(char *filename, uint64_t offset, uint64_t numisns, long atts, long pintrace);
  extern trace_frames_t * asmir_frames_from_trace_file(char *filename, uint64_t offset, uint64_t numisns);
  extern void asmir_frames_destroy(trace_frames_t *tfs);
  extern int asmir_frames_length(trace_frames_t *tfs);
  extern trace_frame_t * asmir_frames_get(trace_frames_t *tfs, int index);
  extern pintrace::FrameType asmir_frame_type(trace_frame_t *tf);
  extern int asmir_frame_tid(trace_frame_t *tf);
  extern uint8_t * asmir_frame_get_insn_bytes(trace_frame_t *tf, uint64_t *addrout, int *len);

  extern const char* asmir_frame_get_loadmod_info(trace_frame_t *tf, uint64_t *lowout, uint64_t *highout);
  extern void asmir_frame_get_syscall_info(trace_frame_t *tf, int *callno, uint64_t *addr, int *tid);
  extern void asmir_frame_get_except_info(trace_frame_t *tf, int *exceptno, int *tid, uint64_t *from_addr, uint64_t *to_addr);
  extern conc_map_vec * asmir_frame_get_operands(trace_frame_t *tf);
  extern void asmir_frame_destroy_operands(conc_map_vec *cv);
  extern int asmir_frame_operands_length(conc_map_vec *cv);
  extern ConcPair* asmir_frame_get_operand(conc_map_vec *cv, int num);
#ifdef __cplusplus
}
#endif

#endif

