#ifndef _PIN_MISC_H
#define _PIN_MISC_H

#include "pin.H"
#include <stdint.h>

namespace pintrace {

const uint32_t MAX_BYTES_PER_PIN_REG = 32;
const uint32_t MAX_WORDS_PER_PIN_REG = (MAX_BYTES_PER_PIN_REG/2);
const uint32_t MAX_DWORDS_PER_PIN_REG = (MAX_WORDS_PER_PIN_REG/2);
const uint32_t MAX_QWORDS_PER_PIN_REG = (MAX_DWORDS_PER_PIN_REG/2);
const uint32_t MAX_FLOATS_PER_PIN_REG = (MAX_BYTES_PER_PIN_REG/sizeof(float));
const uint32_t MAX_DOUBLES_PER_PIN_REG = (MAX_BYTES_PER_PIN_REG/sizeof(double));


union PIN_REGISTER
{
  uint8_t  byte[MAX_BYTES_PER_PIN_REG];
  uint16_t word[MAX_WORDS_PER_PIN_REG];
  uint32_t dword[MAX_DWORDS_PER_PIN_REG];
  uint64_t qword[MAX_QWORDS_PER_PIN_REG];

  int8_t   s_byte[MAX_BYTES_PER_PIN_REG];
  int16_t  s_word[MAX_WORDS_PER_PIN_REG];
  int32_t  s_dword[MAX_DWORDS_PER_PIN_REG];
  int64_t  s_qword[MAX_QWORDS_PER_PIN_REG];

  float  flt[MAX_FLOATS_PER_PIN_REG];
  double  dbl[MAX_DOUBLES_PER_PIN_REG];

};

typedef enum {
  NONE = 0,
  REGISTER = 1,
  MEM = 2,
} RegMemEnum_t;

typedef struct RegMem_s {
  RegMemEnum_t type;
  uint32_t size;   // In bits
} RegMem_t;

static const RegMem_t INVALIDREGMEM = { NONE, 8 };// Size of temporary buffers

#define BUFSIZE 128

#define MAX_INSN_BYTES 15

#define MAX_VALUES_COUNT 0x300

#define MAX_CACHEMASK_BTYES 2

// TODO: Make these values something proper.
#define MAX_FRAME_MEMSIZE sizeof(StdFrame)
#define MAX_FRAME_DISKSIZE 1024

  /** Total syscall args */
#define MAX_SYSCALL_ARGS 9
  /** Number to record on this platform */
#ifdef _WIN32
#define PLAT_SYSCALL_ARGS 9
#else
#define PLAT_SYSCALL_ARGS 6
#endif

#define REG_BASE 0x1
#define MEM_BASE 0x50
#define REGTYPE_LAST (MEM_BASE-1)
#define MEMTYPE_LAST (MEM_BASE+0x50)
// Value specifier type.
#define VT_NONE     0x0
#define VT_REG8     (REG_BASE+0x0)
#define VT_REG16    (REG_BASE+0x1)
#define VT_REG32    (REG_BASE+0x2)
#define VT_REG64    (REG_BASE+0x3)
#define VT_REG128   (REG_BASE+0x4)
#define VT_MEM8     (MEM_BASE+0x0)
#define VT_MEM16    (MEM_BASE+0x1)
#define VT_MEM32    (MEM_BASE+0x2)
#define VT_MEM64    (MEM_BASE+0x3)
#define VT_MEM128   (MEM_BASE+0x4)
#define VT_MEM256   (MEM_BASE+0x5)

uint32_t GetTypeOfReg(REG r);
bool valid_regmem_type(pintrace::RegMem_t rm);

}

#endif
