#ifndef _REG_MAPPING_H
#define _REG_MAPPING_H

#include <stmt.h>

uint32_t regid_to_full(uint32_t id);
string regid_to_fullname(uint32_t id);
reg_t regid_to_type(uint32_t id);
string register_name(uint32_t id);

Move* write_reg(uint32_t id, Exp* val, int offset=-1);

// returns a 32 bit wide unsigned int,
Exp* read_reg(uint32_t id);

// returns a 32 bit wide unsigned int,
// but only 8 bits of it (in lowest 8 bits of the expression)
// offset 0 is least significant byte
Exp* read_reg(uint32_t id, int offset);

#endif
