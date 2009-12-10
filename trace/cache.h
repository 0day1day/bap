// -*- c++ -*-

#pragma once

#include <stdint.h>
#include <string.h>

#include "pin.H"

/** An emulated 8-bit register */
typedef unsigned char reg8;

/** An emulated 16-bit register. */
union reg16 {
   reg8 parts[2];
   unsigned short full;
};

/** An emulated 32-bit register. */
union reg32 {
   reg16 parts[2];
   unsigned int full;
};

struct RegCache {

   reg32 eax;
   reg32 ebx;
   reg32 ecx;
   reg32 edx;
   reg32 esi;
   reg32 edi;
   reg32 esp;
   reg32 ebp;
   reg32 eflags;

   reg16 cs;
   reg16 ds;
   reg16 ss;
   reg16 es;
   reg16 fs;
   reg16 gs;

   // Catch-all for all other registers.
   reg32 catchall;

   // Mapping between register value holders and Pin register
   // names. Provide a way to resolve from a Pin register name into a
   // reference to the associated cache value.
private:
   reg8 *_mapping[REG_LAST];

public:

   RegCache()
   {
      for(int i = 0; i < REG_LAST; i++)
         _mapping[i] = (reg8 *) &catchall;

      _mapping[REG_EAX] = (reg8 *) &eax;
      // TODO: Remaining mappings.

   }

   // Access the elements of the cache given a Pin register name.
   reg32& elem32(REG r) {}
   reg16& elem16(REG r) {}
   reg8& elem8(REG r) {}

};

#define MEMCACHE_SIZE 1024
#define MEMCACHE_MASK 0x3ff

struct MemCache {
   uint32_t cache[MEMCACHE_SIZE];
   
   MemCache() 
   { memset((void *) cache, 0, MEMCACHE_SIZE * sizeof(uint32_t)); }

   // Access the cache entry for a given address.
   uint32_t& elem(uint32_t addr)
   { return &(cache[addr & MEMCACHE_MASK]); }

};
