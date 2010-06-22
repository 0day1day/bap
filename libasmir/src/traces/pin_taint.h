// -*- c++ -*-

#pragma once

#include <iostream>
#include <fstream>
#include <map>
#include "pin_frame.h"
#include "pin.H"


// TODO: we need a type for the mapping to variables/registers
typedef uint32_t var;
// We only consider 32-bit memory addresses
// #define address uint32_t
// And enable 2^32 different taint tags
// special values: 
//  * 0        -> untainted
//  * ffffffff -> mixed taint
//  * other n  -> nth input byte
#define MIXED_TAINT 0xFFFFFFFF
#define NOTAINT 0
typedef uint32_t t;

#define MASK1 0x000000FF
#define MASK2 0x0000FF00
#define MASK3 0x00FF0000
#define MASK4 0xFF000000

typedef std::map<var,t> context;

// Value specifier type.
#define VT_NONE     0x0
#define VT_REG32    0x1
#define VT_REG16    0x2
#define VT_REG8     0x3
#define VT_MEM32    0x11
#define VT_MEM16    0x12
#define VT_MEM8     0x13

// Some bit masks
#define LOW8   0xff
#define HIGH8  0xff000000
#define LOW16  0xffff
#define HIGH16 0xffff0000

struct ValSpecRec {
   uint32_t type;               // Type of value specifier.
   uint32_t loc;                // Location of this value.
   uint32_t value;              // Actual value.
   uint32_t isWrite;              // Taint status of the value
   uint32_t taint;
};

namespace pintrace { // We will use namespace to avoid collision

   // Tracking the taint during program flow
   class TaintTracker {

   public:

      TaintTracker();

      // A function to introduce taint in the contexts
      bool taintStart(uint32_t callno, uint32_t * args);
      void taintIntroduction(uint32_t bytes, uint32_t * args);
      // A function to propagate taint
      void taintPropagation(ValSpecRec values[MAX_VALUES_COUNT],
                            uint32_t count);
      // A function to apply taint policies
      bool taintChecking();

      // Helpers
      // A function to check whether the instruction arguments are tainted
      uint32_t getReadTaint(ValSpecRec values[MAX_VALUES_COUNT],
                            uint32_t count);
      bool hasTaint(ValSpecRec values[MAX_VALUES_COUNT],
                    uint32_t count);
   private:

      // A flag to denote a syscall in progress
      bool readSys;
      // The taint source (producing taint tags)
      uint32_t source;
      // a context defining a map from registers to taint
      context delta;
      // We can use a byte-centric approach, each byte maps to taint
      // a context defining a map from memory locations to taint
      context memory;

      void addTaintToWritten(ValSpecRec values[MAX_VALUES_COUNT],
                             uint32_t tag,
                             uint32_t count);
      
      bool isReg(uint32_t type);

      uint32_t getRegTaint(uint32_t reg_int);

      uint32_t getMemTaint(uint32_t addr, uint32_t type);

      uint32_t combineTaint(uint32_t oldtag, uint32_t newtag);

      uint32_t exists(context ctx, uint32_t elem);

      uint32_t getTaint(context ctx, uint32_t elem);

      uint32_t getSize(uint32_t type);

      bool isValid(uint32_t type);

      void setTaint(context &ctx, uint32_t key, uint32_t tag);

      void printRegs();
      
      void printMem();

   };

}; // End of namespace
