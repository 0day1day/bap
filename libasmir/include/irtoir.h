/*
 Owned and copyright BitBlaze, 2007. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*/

#ifndef __IRTOIR_H
#define __IRTOIR_H

typedef struct bap_block_s bap_block_t;


#include "asm_program.h"
#include "vexmem.h"

//
// VEX headers (inside Valgrind/VEX/pub)
//
#ifdef __cplusplus
extern "C"
{
#endif

#include "libvex.h"

#ifdef __cplusplus
}

#include <vector>

#include "stmt.h"

//
// A struct that encapsulates the stages of translation of 1 asm instruction.
// inst is the original instruction.
// vex_ir is inst translated into a block of VEX IR.
// bap_ir is inst translated into a block of Vine IR from its VEX IR translation.
//
struct bap_block_s
{
  bfd_vma         inst;
  IRSB            *vex_ir;
  vector<Stmt *>  *bap_ir;
};




//======================================================================
// 
// Translation functions. These should not be used directly unless you
// understand exactly how they work and have a specific need for them.
// If you just wanna translate a program, see generate_vex_ir and 
// generate_bap_ir at the bottom. They wrap these functions and
// provide an easier interface for translation. 
//
//======================================================================

extern "C" {
//
// Initializes VEX. This function must be called before translate_insn
// can be used. 
// vexir.c
void translate_init();

//
// Translates 1 asm instruction (in byte form) into a block of VEX IR
//
// \param insn_start Pointer to bytes of instruction
// \param insn_addr Address of the instruction in its own address space
// \return An IRSB containing the VEX IR translation of the given instruction
// vexir.c
IRSB *translate_insn( VexArch guest, unsigned char *insn_start, unsigned int insn_addr );
}


//======================================================================
// 
// Utility functions that wrap the raw translation functions.
// These are what should be used to do the actual translation.
// See print-ir.cpp in ../../apps for examples of how to use these.
//
//======================================================================


// Take an instrs and translate it into a VEX IR block
// and store it in a bap block
//bap_block_t* generate_vex_ir(VexArch guest, Instruction *inst);
bap_block_t* generate_vex_ir(asm_program_t *prog, address_t inst);

//
// Take a vector of instrs and translate it into VEX IR blocks
// and store them in a vector of bap blocks
//
// \param instrs the list of instructions to translate
// \return A vector of bap blocks. Only the inst and vex_ir fields in
//         the bap block are valid at this point.
//

//vector<bap_block_t *> generate_vex_ir(asm_program_t *prog, const vector<Instruction *> &instrs);

//
// Take a disassembled function and translate it into VEX IR blocks
// and store them in a vector of bap blocks
//
// \param func A disassembled function
// \return A vector of bap blocks. Only the inst and vex_ir fields in
//         the bap block are valid at this point.
//
//vector<bap_block_t *> generate_vex_ir(asm_program_t *prog, asm_function_t *func);

//
// Take a disassembled program and translate it into VEX IR blocks
// and store them in a vector of bap blocks
//
// \param prog A disassembled program
// \return A vector of bap blocks. Only the inst and vex_ir fields in
//         the bap block are valid at this point.
//
vector<bap_block_t *> generate_vex_ir( asm_program_t *prog );

// Same as generate_vex_ir, but only for an address range
vector<bap_block_t *> generate_vex_ir(asm_program_t *prog,
					address_t start,
					address_t end);

// Take a bap block that has gone through VEX translation and translate it
// to Vine IR.
void generate_bap_ir_block( asm_program_t *prog, bap_block_t *block );

//
// Take a vector of bap blocks that have gone through VEX translation
// and translate them to Vine IR
//
// \param vblocks Vector of bap blocks with valid VEX IR translations
// \return Vector of bap blocks with the bap_ir field filled in
//
vector<bap_block_t *> generate_bap_ir( asm_program_t *prog, vector<bap_block_t *> vblocks );


VexArch vexarch_of_bfdarch(bfd_architecture arch);

// Get global variables declaractions for this arch
//vector<VarDecl *> get_reg_decls(VexArch arch);


extern "C" {
  typedef struct vector<bap_block_t *> bap_blocks_t;

#else
  struct bap_block_s
  {
    bfd_vma         inst;
    IRSB            *vex_ir;
    struct vector_Stmt  *bap_ir;
  };


  typedef struct vector_bap_block_t bap_blocks_t;
#endif  // __cplusplus

  /*
  /// Enable/disable eflags thunks code. 
  extern void set_use_eflags_thunks(bool value);
  extern bool get_use_eflags_thunks();
  /// If 1, calls/returns translated as Call/Return.
  /// If 0, calls/returns translated as Jmp
  extern void set_call_return_translation(int value);
  extern Stmt** gen_eflags_helpers_c();
  */

  extern bap_blocks_t * asmir_asmprogram_to_bap(asm_program_t *prog);
  extern bap_blocks_t * asmir_asmprogram_range_to_bap(asm_program_t *prog, address_t start, address_t end);
  extern asm_program_t* byte_insn_to_asmp(bfd_architecture arch, address_t addr, unsigned char *bb_bytes, unsigned int len);
  extern bap_block_t* asmir_addr_to_bap(asm_program_t *p, address_t addr);

  extern int asmir_bap_blocks_size(bap_blocks_t *bs);
  extern bap_block_t * asmir_bap_blocks_get(bap_blocks_t *bs, int i);
  extern void destroy_bap_blocks(bap_blocks_t *bs);
  extern void destroy_bap_block(bap_block_t *bs);
  extern int asmir_bap_block_size(bap_block_t *b);
  extern address_t asmir_bap_block_address(bap_block_t *b);
  extern Stmt * asmir_bap_block_get(bap_block_t *b, int i);
  extern char* string_blockinsn(asm_program_t *prog, bap_block_t *block);

#ifdef __cplusplus
}
#endif

#endif

