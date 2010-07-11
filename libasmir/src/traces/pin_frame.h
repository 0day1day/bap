
// -*- c++ -*-

#pragma once

#include <iostream>
#include <stdint.h>
#include "trace_vXX.h"

// As defined by Intel.
#define MAX_INSN_BYTES 15

#define MAX_VALUES_COUNT 15

#define MAX_CACHEMASK_BTYES 2

// TODO: Make these values something proper.
#define MAX_FRAME_MEMSIZE sizeof(StdFrame)
#define MAX_FRAME_DISKSIZE 1024

#define MAX_SYSCALL_ARGS 5

/***************** Syscalls ***************/
// FIXME: use the ones from /usr/include/asm/unistd.h
     
#define __NR_read		  3
#define __NR_open		  5
#define __NR_execve		 11

/********************************************/

enum FrameType {

   // Invalid frame type.
   FRM_NONE = 0,

   // Keyframe. Supplies all register values and invalidates cache.
   FRM_KEY = 1,

   // "Regular" frame. Sufficient to handle majority of x86 instructions.
   FRM_STD = 2,

   // Frame indicating a module was loaded.
   FRM_LOADMOD = 3,

   // Frame containing information about a system call.
   FRM_SYSCALL = 4,

   // Frame taint information
   FRM_TAINT = 5,

};

namespace pintrace { // Use namespace to avoid conflict

   /**
    * Frame: Base struct for all frame objects.
    */
   struct Frame {

      FrameType type;

      Frame(FrameType ty) : type(ty) {}

      //
      // Serializes the frame and outputs to the ostream. 'sz' is the size
      // of the "child" frame, i.e. the size of all data that comes after
      // this frame's data.
      //
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);

      virtual std::istream &unserializePart(std::istream &in) = 0;

      //
      // The main unserialization function. Returns a new Frame object.
      // If 'noskip' is false, then the serialized data representing the
      // frame will merely be skipped and not constructed in an object;
      // NULL will be returned instead. This can be used to quickly skip
      // to the next frame.
      //
      static Frame *unserialize(std::istream &in, bool noskip = true);

   };

   /*
    * StdFrame: Standard frame, used to log majority of instructions.
    *
    * Packed format:
    *   4 bytes -> Address (addr)
    *   4 bytes -> Thread ID (tid)
    *   1 byte  -> Lengths (packed values_count and insn_length)
    *   N bytes -> Raw bytes (rawbytes) where N == insn_length * sizeof(char)
    *   N bytes -> Cache mask, where N = ceil(values_count / 8.0)
    *   N bytes -> Values where N = values_count * 4
    *
    *
    * Additional notes:
    *   Although StdFrame objects have an insn_length field, this field
    *   will not always reflect the actual length of the instruction. If
    *   the value of this field is set to 0, then the instruction length
    *   should be inferred by parsing the raw instruction bytes as an x86
    *   instruction, i.e. the length of the instruction is implicit in its
    *   x86 encoding. The rawbytes field will always be padded with extra
    *   garbage bytes after the instruction bytes to make up
    *   MAX_INSN_BYTES bytes.
    *
    * RULES FOR STORING VALUES:
    *   The values array can hold up to MAX_VALUES_COUNT "values". Each
    *   value corresponds to the concrete value of a particular register
    *   or memory location read by the instruction. Note that we only need
    *   to store values that were read, and not those that were written.
    *   The order of the values is as follows:
    *
    *     For each operand (both explicit and implicit), ordered based on
    *     Pin's operand ordering scheme:
    *        If the operand is a register to be read:
    *           If a segment register is specified:
    *              Store the value of the segment register.
    *           EndIf
    *           Store the value of the register to be read.
    *        Else if the operand is a memory location:
    *           If a segment register was specified:
    *              Store the value of the segment register.
    *           EndIf
    *           If a base register is specified:
    *              Store the value of the base register.
    *           EndIf
    *           If an index register is specified:
    *              Store the value of the index register.
    *           EndIf
    *        EndIf
    *     EndFor
    *     If this instruction is a memory read:
    *        Store the value of the memory location being read.
    *     EndIf
    *     If this instruction has any subsequent memory reads:
    *        Store the values of the memory locations being red.
    *     EndIf
    *
    *   i.e. the values array will start with the values of all registers
    *   that will be read by the processor, whether to use the value in an
    *   operation or to use the value to calculate an effective
    *   address. The registers are stored in the order of the operands
    *   they occur in. Finally, if the instruction reads from memory, the
    *   value of the memory location will be stored as well.
    *
    * VALUE CACHING:
    *   The packed StdFrame representation uses a data cache to reduce the
    *   amount of values that need to be stored in the trace. In order to
    *   denote which values can be found in the cache and which are stored
    *   in the current frame, a cache mask is used. The cache mask uses 1
    *   bit per value that needs to be stored in the frame; if the bit is
    *   NOT set, then the value in the cache is invalid and the right
    *   value is found in the frame. Else, the trace reader should
    *   retrieve the value from the cache, i.e. 1 for in cache, 0 for not
    *   in cache.  The cache mask is always long enough such that there
    *   are sufficient bits to represent every needed value in the
    *   trace. It is thus values_count bits long, rounded up to a multiple
    *   of 8 in order to fit in an integral number of bytes. The mapping
    *   between value and cache map bit is as follows: the first value
    *   maps to the least significant bit of the first byte, and so on.
    *
    *
    */
   struct StdFrame : public Frame {

      uint32_t addr;
      uint32_t tid;
      uint8_t insn_length;         // Must be <= MAX_INSN_BYTES
      uint8_t values_count;          // Must be <= MAX_VALUES_COUNT
      char rawbytes[MAX_INSN_BYTES];
      char cachemask[MAX_CACHEMASK_BTYES];
      uint32_t values[MAX_VALUES_COUNT];
      uint32_t types[MAX_VALUES_COUNT];
      uint32_t locs[MAX_VALUES_COUNT];
      uint32_t tainted[MAX_VALUES_COUNT];

      StdFrame() : Frame(FRM_STD) {}
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);
      virtual std::istream &unserializePart(std::istream &in);

      void clearCache();
 
      conc_map_vec * getOperands();

      // TODO: Removing the bounds checking in the functions below might
      // lead to some speedups. Test and see if the risk is worth it.

      // Note: x & 7 === x % 8
      bool isCached(uint32_t pos)
      {
         if (pos < MAX_VALUES_COUNT)
            return (cachemask[pos >> 3] >> (pos & 7)) & 1;
         else
            return false;
      }

      void setCached(uint32_t pos)
      {
         if (pos < MAX_VALUES_COUNT)
            cachemask[pos >> 3] |= (1 << (pos & 7));
      }

      void unsetCached(uint32_t pos)
      {
         if (pos < MAX_VALUES_COUNT)
            cachemask[pos >> 3] &= ~(1 << (pos & 7));
      }

   };

   /*
    * KeyFrame: Keyframe, used to resynchronize the execution trace.
    *
    * Packed format:
    *    8 bytes -> Position (pos)
    *   48 bytes -> Values of all registers.
    *
    * Additional notes:
    *   The keyframe should be inserted once every few thousand (or
    *   hundred, or million) frames. It is used to resynchronize the
    *   trace. The keyframe contains the values of all registers at that
    *   current point in the trace, which are used to populate the
    *   register value cache. At the same time, when a keyframe is
    *   encountered, the memory cache should be cleared, i.e. all frames
    *   following the keyframe cannot refer to memory values cached prior
    *   to the keyframe.
    *
    *   The keyframe also contains positional information, so readers will
    *   know where the keyframe is located relative to the stream. The
    *   'pos' attribute holds the count of the number of instruction
    *   frames that are in the trace prior to the keyframe. Note that
    *   special frames such as SyscallFrames and LoadModuleFrames are not
    *   part of the instruction count. 'pos' can thus be used to determine
    *   how many instructions have been executed just before the keyframe
    *   was added.
    *
    */
   struct KeyFrame : public Frame {

      uint64_t pos;

      uint32_t eax;
      uint32_t ebx;
      uint32_t ecx;
      uint32_t edx;
      uint32_t esi;
      uint32_t edi;
      uint32_t esp;
      uint32_t ebp;
      uint32_t eflags;

      uint16_t cs;
      uint16_t ds;
      uint16_t ss;
      uint16_t es;
      uint16_t fs;
      uint16_t gs;

      // XXX: Additional registers?

      KeyFrame() : Frame(FRM_KEY) {}
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);
      virtual std::istream &unserializePart(std::istream &in);

      void setAll(uint32_t eax, uint32_t ebx, uint32_t ecx, uint32_t edx,
                  uint32_t esi, uint32_t edi, uint32_t esp, uint32_t ebp,
                  uint32_t eflags,
                  uint16_t cs, uint16_t ds, uint16_t ss,
                  uint16_t es, uint16_t fs, uint16_t gs);

   };

   /*
    * LoadModuleFrame : Records information about a loaded module.
    *
    * Packed format:
    *   48 bytes -> Values of all registers.
    *
    * Additional notes:
    *
    */
   struct LoadModuleFrame : public Frame {

      uint32_t low_addr;
      uint32_t high_addr;
      uint32_t start_addr;
      uint32_t load_offset;
      char name[64];
      
      LoadModuleFrame() : Frame(FRM_LOADMOD) {}
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);
      virtual std::istream &unserializePart(std::istream &in);
      
   };

   /*
    * SyscallFrame : A system call.
    *
    * Packed format:
    *   48 bytes -> Values of all registers.
    *
    * Additional notes:
    *
    */
   struct SyscallFrame : public Frame {

      uint32_t addr;
      uint32_t tid;
      uint32_t callno;
      uint32_t args[MAX_SYSCALL_ARGS];
      
      SyscallFrame() : Frame(FRM_SYSCALL) {}
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);
      virtual std::istream &unserializePart(std::istream &in);
      conc_map_vec * getOperands();
      
   };

   struct TaintFrame : public Frame {

      uint32_t id;
      uint32_t length;
      uint32_t addr;
      
      TaintFrame() : Frame(FRM_TAINT) {}
      virtual std::ostream &serialize(std::ostream &out, uint16_t sz = 0);
      virtual std::istream &unserializePart(std::istream &in);
      conc_map_vec * getOperands();
      
   };


}; // END of namespace
