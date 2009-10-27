/*
 Owned and copyright BitBlaze, 2007. All rights reserved.
 Do not copy, disclose, or distribute without explicit written
 permission.
*/

#ifndef __IRTOIR_H
#define __IRTOIR_H

typedef struct vine_block_s vine_block_t;


//
// VEX headers (inside Valgrind/VEX/pub)
//
#ifdef __cplusplus
extern "C"
{
#endif

#include "asm_program.h"
#include "vexmem.h"

#include "libvex.h"

#ifdef __cplusplus
}

#include <vector>

#include "stmt.h"

//
// A struct that encapsulates the stages of translation of 1 asm instruction.
// inst is the original instruction.
// vex_ir is inst translated into a block of VEX IR.
// vine_ir is inst translated into a block of Vine IR from its VEX IR translation.
//
struct vine_block_s
{
  bfd_vma         inst;
  IRSB            *vex_ir;
  vector<Stmt *>  *vine_ir;
};




//======================================================================
// 
// Translation functions. These should not be used directly unless you
// understand exactly how they work and have a specific need for them.
// If you just wanna translate a program, see generate_vex_ir and 
// generate_vine_ir at the bottom. They wrap these functions and
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
// and store it in a vine block
//vine_block_t* generate_vex_ir(VexArch guest, Instruction *inst);
vine_block_t* generate_vex_ir(asm_program_t *prog, address_t inst);

//
// Take a vector of instrs and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param instrs the list of instructions to translate
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//

//vector<vine_block_t *> generate_vex_ir(asm_program_t *prog, const vector<Instruction *> &instrs);

//
// Take a disassembled function and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param func A disassembled function
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//
//vector<vine_block_t *> generate_vex_ir(asm_program_t *prog, asm_function_t *func);

//
// Take a disassembled program and translate it into VEX IR blocks
// and store them in a vector of vine blocks
//
// \param prog A disassembled program
// \return A vector of vine blocks. Only the inst and vex_ir fields in
//         the vine block are valid at this point.
//
vector<vine_block_t *> generate_vex_ir( asm_program_t *prog );

// Same as generate_vex_ir, but only for an address range
vector<vine_block_t *> generate_vex_ir(asm_program_t *prog,
					address_t start,
					address_t end);

// Take a vine block that has gone through VEX translation and translate it
// to Vine IR.
void generate_vine_ir_block( asm_program_t *prog, vine_block_t *block );

//
// Take a vector of vine blocks that have gone through VEX translation
// and translate them to Vine IR
//
// \param vblocks Vector of vine blocks with valid VEX IR translations
// \return Vector of vine blocks with the vine_ir field filled in
//
vector<vine_block_t *> generate_vine_ir( asm_program_t *prog, vector<vine_block_t *> vblocks );


VexArch vexarch_of_bfdarch(bfd_architecture arch);

// Get global variables declaractions for this arch
//vector<VarDecl *> get_reg_decls(VexArch arch);


extern "C" {
  typedef struct vector<vine_block_t *> vine_blocks_t;

#else
  struct vine_block_s
  {
    bfd_vma         inst;
    IRSB            *vex_ir;
    struct vector_Stmt  *vine_ir;
  };


  typedef struct vector_vine_block_t vine_blocks_t;
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

  extern vine_blocks_t * asmir_asmprogram_to_vine(asm_program_t *prog);
  extern vine_blocks_t * asmir_asmprogram_range_to_vine(asm_program_t *prog, address_t start, address_t end);
  extern asm_program_t* byte_insn_to_asmp(bfd_architecture arch, address_t addr, unsigned char *bb_bytes, unsigned int len);
  extern vine_block_t* asmir_addr_to_vine(asm_program_t *p, address_t addr);

  extern int asmir_vine_blocks_size(vine_blocks_t *bs);
  extern vine_block_t * asmir_vine_blocks_get(vine_blocks_t *bs, int i);
  extern void destroy_vine_blocks(vine_blocks_t *bs);
  extern void destroy_vine_block(vine_block_t *bs);
  extern int asmir_vine_block_size(vine_block_t *b);
  extern address_t asmir_vine_block_address(vine_block_t *b);
  extern Stmt * asmir_vine_block_get(vine_block_t *b, int i);
  extern char* string_blockinsn(asm_program_t *prog, vine_block_t *block);

#ifdef __cplusplus
}
#endif

#endif

