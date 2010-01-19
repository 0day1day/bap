#include "pin.H"

#include <iostream>
#include <fstream>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include "frame.h"
#include "trace.h"
#include "cache.h"

#include "frame.cpp"
#include "trace.cpp"

#define BUFFER_SIZE 1024

#define USE_CACHING

KNOB<string> KnobOut(KNOB_MODE_WRITEONCE, "pintool",
                     "out", "out.bpt",
                     "Trace file to output to.");


// Value specifier type.
#define VT_NONE     0x0
#define VT_REG32    0x1
#define VT_REG16    0x2
#define VT_REG8     0x3
#define VT_MEM32    0x11
#define VT_MEM16    0x12
#define VT_MEM8     0x13

struct ValSpecRec {
   uint32_t type;               // Type of value specifier.
   uint32_t loc;                // Location of this value.
   uint32_t value;              // Actual value.
};

struct FrameBuf {
   uint32_t addr;
   uint32_t tid;
   uint32_t insn_length;

   // The raw instruction bytes will be stored as 16 bytes placed over 4
   // integers. The conversion is equivalent to the casting of a char[16]
   // to a uint32_t[4].
   // NOTE: This assumes that MAX_INSN_BYTES == 16!!!
   uint32_t rawbytes0;
   uint32_t rawbytes1;
   uint32_t rawbytes2;
   uint32_t rawbytes3;

   uint32_t values_count;
   ValSpecRec valspecs[MAX_VALUES_COUNT];

};

TraceWriter *g_tw;

FrameBuf g_buffer[BUFFER_SIZE];
uint32_t g_bufidx;

// Caches.
RegCache g_regcache;
MemCache g_memcache;

//
// Returns true if the buffer index with count added to it exceeds the
// maximum size of the buffer.
//
ADDRINT CheckBuffer(UINT32 count)
{
   return (g_bufidx + count) >= BUFFER_SIZE;
}

//
// Writes all instructions stored in the buffer to disk, and resets the
// buffer index to 0.
//
VOID FlushBuffer()
{

   for(uint32_t i = 0; i < g_bufidx; i++) {
      
      // TODO: Implement caching.

      StdFrame f;
      f.addr = g_buffer[i].addr;
      f.tid = g_buffer[i].tid;
      f.insn_length = g_buffer[i].insn_length;

      if (f.insn_length > MAX_INSN_BYTES) {
         LOG("Error! f.insn_length is too long!\n");
      }

      memcpy((char *) f.rawbytes, (char *) &(g_buffer[i].rawbytes0), f.insn_length);

      f.clearCache();

      uint32_t newcnt = 0;

      // Go through each value and remove the ones that are cached.

      for (uint32_t j = 0; j < g_buffer[i].values_count; j++) {

         ValSpecRec &v = g_buffer[i].valspecs[j];

         switch (v.type) {
         case VT_REG32:
#ifdef USE_CACHING
            if (g_regcache.elem32((REG) v.loc).full == v.value) {
               f.setCached(j);
            } else {
               g_regcache.elem32((REG) v.loc).full = v.value;
#endif
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         case VT_REG16:
#ifdef USE_CACHING
            if (((uint32_t) g_regcache.elem16((REG) v.loc).full) == v.value) {
               f.setCached(j);
            } else {
#endif
               g_regcache.elem16((REG) v.loc).full = (uint16_t) v.value;
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         case VT_REG8:
#ifdef USE_CACHING
            if (((uint32_t) g_regcache.elem8((REG) v.loc)) != v.value) {
               f.setCached(j);
            } else {
               g_regcache.elem8((REG) v.loc) = (uint8_t) v.value;
#endif
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         case VT_MEM32:
#ifdef USE_CACHING
            if (g_memcache.elem32(v.loc) == v.value) {
               f.setCached(j);
               LOG(hexstr(f.addr) + ": " +
                   "cached: " + hexstr(v.loc) + 
                   " has value " + hexstr(g_memcache.elem32(v.loc)) +
                   "\n");
            } else {
               LOG(hexstr(f.addr) + ": " +
                   "not cached: " + hexstr(v.loc) + 
                   " has value " + hexstr(v.value) +
                   ", cache is " + hexstr(g_memcache.elem32(v.loc)) +
                   "\n");

               g_memcache.elem32(v.loc) = v.value;
#endif
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         case VT_MEM16:
#ifdef USE_CACHING
            if (((uint32_t) g_memcache.elem16(v.loc)) == v.value) {
               f.setCached(j);
            } else {
               g_memcache.elem16(v.loc) = (uint16_t) v.value;
#endif
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         case VT_MEM8:
#ifdef USE_CACHING
            if (((uint32_t) g_memcache.elem8(v.loc)) == v.value) {
               f.setCached(j);
            } else {
               g_memcache.elem8(v.loc) = (uint8_t) v.value;
#endif
               f.values[newcnt] = v.value;
               newcnt++;
#ifdef USE_CACHING
            }
#endif
            break;

         default:
            // TODO: Figure out what to do here.

            f.values[newcnt] = v.value;
            newcnt++;
            
         }

      }

      f.values_count = newcnt;

      g_tw->add(f);

   }

   g_bufidx = 0;

}

VOID AppendBuffer(ADDRINT addr,
                  THREADID tid,
                  UINT32 insn_length,

                  UINT32 rawbytes0,
                  UINT32 rawbytes1,
                  UINT32 rawbytes2,
                  UINT32 rawbytes3,

                  // NOTE: We assume here that MAX_VALUES_COUNT == 15.
                  UINT32 valspec0_type,
                  UINT32 valspec0_loc,
                  UINT32 valspec0_spec,

                  UINT32 valspec1_type,
                  UINT32 valspec1_loc,
                  UINT32 valspec1_spec,

                  UINT32 valspec2_type,
                  UINT32 valspec2_loc,
                  UINT32 valspec2_spec,

                  UINT32 valspec3_type,
                  UINT32 valspec3_loc,
                  UINT32 valspec3_spec,

                  UINT32 valspec4_type,
                  UINT32 valspec4_loc,
                  UINT32 valspec4_spec,

                  UINT32 valspec5_type,
                  UINT32 valspec5_loc,
                  UINT32 valspec5_spec,

                  UINT32 valspec6_type,
                  UINT32 valspec6_loc,
                  UINT32 valspec6_spec,

                  UINT32 valspec7_type,
                  UINT32 valspec7_loc,
                  UINT32 valspec7_spec,

                  UINT32 valspec8_type,
                  UINT32 valspec8_loc,
                  UINT32 valspec8_spec,

                  UINT32 valspec9_type,
                  UINT32 valspec9_loc,
                  UINT32 valspec9_spec,

                  UINT32 valspec10_type,
                  UINT32 valspec10_loc,
                  UINT32 valspec10_spec,

                  UINT32 valspec11_type,
                  UINT32 valspec11_loc,
                  UINT32 valspec11_spec,

                  UINT32 valspec12_type,
                  UINT32 valspec12_loc,
                  UINT32 valspec12_spec,

                  UINT32 valspec13_type,
                  UINT32 valspec13_loc,
                  UINT32 valspec13_spec,

                  UINT32 valspec14_type,
                  UINT32 valspec14_loc,
                  UINT32 valspec14_spec,

                  // This is the last argument because often we don't know
                  // how many values there will be until after we've built
                  // up the preceding arguments in the argument list.
                  UINT32 values_count
                  )
{

   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   g_buffer[g_bufidx].values_count = values_count;

#define BUILD_VALSPEC(i)                                                \
   if (i >= values_count) goto _exit;                                   \
   g_buffer[g_bufidx].valspecs[i].type = valspec##i##_type;             \
   g_buffer[g_bufidx].valspecs[i].loc = valspec##i##_loc;               \
   switch(valspec##i##_type) {                                          \
   case VT_REG32:                                                       \
   case VT_REG16:                                                       \
   case VT_REG8:                                                        \
      g_buffer[g_bufidx].valspecs[i].value = valspec##i##_spec;         \
      break;                                                            \
   case VT_MEM32:                                                       \
   case VT_MEM16:                                                       \
   case VT_MEM8:                                                        \
      if (valspec##i##_spec == 1) {                                     \
         g_buffer[g_bufidx].valspecs[i].value =                         \
            (uint32_t) *((uint8_t *) valspec##i##_loc);                 \
         g_buffer[g_bufidx].valspecs[i].type = VT_MEM8;                 \
      } else if (valspec##i##_spec == 2) {                              \
         g_buffer[g_bufidx].valspecs[i].value =                         \
            (uint32_t) *((uint16_t *) valspec##i##_loc);                \
         g_buffer[g_bufidx].valspecs[i].type = VT_MEM16;                \
      } else if (valspec##i##_spec == 4) {                              \
         g_buffer[g_bufidx].valspecs[i].value =                         \
            *((uint32_t *) valspec##i##_loc);                           \
         g_buffer[g_bufidx].valspecs[i].type = VT_MEM32;                \
      }                                                                 \
      break;                                                            \
   }
   
   BUILD_VALSPEC(0);
   BUILD_VALSPEC(1);
   BUILD_VALSPEC(2);
   BUILD_VALSPEC(3);
   BUILD_VALSPEC(4);
   BUILD_VALSPEC(5);
   BUILD_VALSPEC(6);
   BUILD_VALSPEC(7);
   BUILD_VALSPEC(8);
   BUILD_VALSPEC(9);
   BUILD_VALSPEC(10);
   BUILD_VALSPEC(11);
   BUILD_VALSPEC(12);
   BUILD_VALSPEC(13);
   BUILD_VALSPEC(14);
#undef BUILD_VALSPEC

 _exit:

   g_bufidx++;
   return;

}

VOID InstrBlock(BBL bbl)
{

   uint32_t icount = 0;

   for (INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins)) {
      // Add instrumentation call to insert instruction into buffer.

      // The argument list to be passed into the instruction analysis call.
      IARGLIST arglist = IARGLIST_Alloc();

      // The first few arguments to AppendBuffer.
      IARGLIST_AddArguments(arglist,
                            IARG_ADDRINT, INS_Address(ins), 
                            IARG_THREAD_ID,
                            IARG_UINT32, INS_Size(ins),
                            IARG_END);

      // Now we need to gather the instruction bytes.

      // Wastes a copy, but required because the instruction may not be
      // 32-bit aligned, and we want to respect word alignment requirements.
      uint32_t rawbytes_i[4];
      memcpy((char *) rawbytes_i, (char *) INS_Address(ins), INS_Size(ins));

      IARGLIST_AddArguments(arglist,
                            IARG_UINT32, rawbytes_i[0],
                            IARG_UINT32, rawbytes_i[1],
                            IARG_UINT32, rawbytes_i[2],
                            IARG_UINT32, rawbytes_i[3],
                            IARG_END);

      // Now we need to get the values.

      uint32_t valcount = 0;
     
      for(uint32_t i = 0; i < INS_OperandCount(ins); i++) {

         if (INS_OperandIsReg(ins, i) && INS_OperandRead(ins, i)) {

            // TODO: Check for segment register.

            REG r = INS_OperandReg(ins, i);

            uint32_t ty;
            if (REG_is_gr32(r)) ty = VT_REG32;
            else if (REG_is_gr16(r)) ty = VT_REG16;
            else if (REG_is_gr8(r)) ty = VT_REG8;
            else {
               // TODO: Handle more cases, and decide if this catchall is
               // a really bad idea.
               ty = VT_REG32;
            }
            
            IARGLIST_AddArguments(arglist,
                                  IARG_UINT32, ty,
                                  IARG_UINT32, r,
                                  IARG_REG_VALUE, r,
                                  IARG_END);
            valcount++;

         } else if (INS_OperandIsMemory(ins, i)) {
            
            // TODO: Check for segment register.

            REG basereg = INS_OperandMemoryBaseReg(ins, i);
            if (basereg != REG_INVALID()) {

               uint32_t ty;
               if (REG_is_gr32(basereg)) ty = VT_REG32;
               else if (REG_is_gr16(basereg)) ty = VT_REG16;
               else if (REG_is_gr8(basereg)) ty = VT_REG8;
               else {
                  // TODO: Handle more cases, and decide if this catchall is
                  // a really bad idea.
                  ty = VT_REG32;
               }

               IARGLIST_AddArguments(arglist,
                                     IARG_UINT32, ty,
                                     IARG_UINT32, basereg,
                                     IARG_REG_VALUE, basereg,
                                     IARG_END);
               valcount++;
            }

            REG idxreg = INS_OperandMemoryIndexReg(ins, i);
            if (idxreg != REG_INVALID()) {

               uint32_t ty;
               if (REG_is_gr32(idxreg)) ty = VT_REG32;
               else if (REG_is_gr16(idxreg)) ty = VT_REG16;
               else if (REG_is_gr8(idxreg)) ty = VT_REG8;
               else {
                  // TODO: Handle more cases, and decide if this catchall is
                  // a really bad idea.
                  ty = VT_REG32;
               }

               IARGLIST_AddArguments(arglist,
                                     IARG_UINT32, ty,
                                     IARG_UINT32, idxreg,
                                     IARG_REG_VALUE, idxreg,
                                     IARG_END);
               valcount++;
            }

         }

      }

      if (INS_IsMemoryRead(ins)) {

         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, VT_MEM32,
                               IARG_MEMORYREAD_EA,
                               IARG_MEMORYREAD_SIZE,
                               IARG_END);
         valcount++;
      }

      // TODO: Check for second memory read.

      // TODO: Check if valcount has exceed the maximum number of
      // values. Also, figure out what to do if so.

      for (uint32_t i = valcount; i < MAX_VALUES_COUNT; i++) {
         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, VT_NONE,
                               IARG_UINT32, 0,
                               IARG_UINT32, 0,
                               IARG_END);
      }

      IARGLIST_AddArguments(arglist,
                            IARG_UINT32, valcount,
                            IARG_END);
      
      // The argument list has been built, time to insert the call.

      INS_InsertCall(ins, IPOINT_BEFORE,
                     (AFUNPTR) AppendBuffer,
                     IARG_IARGLIST, arglist,
                     IARG_END);
      
      icount++;

   }

   // Add instrumentation call to check if buffer needs to be flushed.
   BBL_InsertIfCall(bbl, IPOINT_BEFORE,
                    (AFUNPTR) CheckBuffer,
                    IARG_UINT32, icount,
                    IARG_END);

   BBL_InsertThenCall(bbl, IPOINT_BEFORE,
                      (AFUNPTR) FlushBuffer,
                      IARG_END);

}

VOID InstrTrace(TRACE trace, VOID *v)
{

   for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {
      InstrBlock(bbl);
   }

}

VOID Fini(INT32 code, VOID *v)
{
   g_tw->finalize();
}

int main(int argc, char *argv[])
{

   if (PIN_Init(argc,argv))
      return 1;

   TRACE_AddInstrumentFunction(InstrTrace, 0);
   PIN_AddFiniFunction(Fini, 0);

   g_tw = new TraceWriter(KnobOut.Value().c_str());

   g_bufidx = 0;

   // Start the program, never returns
   PIN_StartProgram();
   
   return 0;

}
