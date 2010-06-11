#include "pin.H"

#include <iostream>
#include <fstream>
#include <vector>
#include <cstring>
#include <stdint.h>
#include <time.h>

#include "pin_frame.h"
#include "pin_trace.h"
#include "cache.h"

#include "pin_frame.cpp"
#include "pin_trace.cpp"

//
// CONFIGURATION
//

// Note: since we only flush the buffer once per basic block, the number
// of instructions per block should never exceed BUFFER_SIZE.
// TODO: See if there's some way to overcome this limitation, if
// necessary.
#define BUFFER_SIZE 10240

// Add a keyframe every 102,400 instructions.
#define KEYFRAME_FREQ 102400

// Use value caching.
#define USE_CACHING

// Use faster functions to append to the value buffer, where possible.
#define USE_FASTPATH

// Pin's syscall standard.
#define SYSCALL_STD SYSCALL_STANDARD_IA32_LINUX


#ifdef USE_FASTPATH
#define _FASTPATH true
#else
#define _FASTPATH false
#endif


KNOB<string> KnobOut(KNOB_MODE_WRITEONCE, "pintool",
                     "out", "out.bpt",
                     "Trace file to output to.");

KNOB<string> KnobTrigModule(KNOB_MODE_WRITEONCE, "pintool",
                            "trig_mod", "",
                            "Module that trigger point is in.");

//
// IMPORTANT NOTE: Logging only begins on the instruction that next
// executes after the trigger point, i.e. the trigger point instruction
// will not be logged.
// TODO: Fix this limitation.
//
KNOB<int> KnobTrigAddr(KNOB_MODE_WRITEONCE, "pintool",
                       "trig_addr", "",
                       "Address of trigger point.");

KNOB<int> KnobTrigCount(KNOB_MODE_WRITEONCE, "pintool",
                        "trig_count", "0",
                        "Number of times trigger will be executed before activating.");

//
// NOTE: This limit is not a hard limit; the generator only stops logging
// during buffer flushes, so the actual number of instructions logged
// might exceed log_limit, but at the most by BUFFER_SIZE.
// Also note that the limit is in terms of the number of _instructions_,
// not frames; things like keyframes, LoadModuleFrames, etc. are not
// included in the count.
//
KNOB<uint64_t> KnobLogLimit(KNOB_MODE_WRITEONCE, "pintool",
                            "log_limit", "0",
                            "Number of instructions to limit logging to.");


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

// Vector used to collect TOC entries.
vector<uint32_t> g_toc;

FrameBuf g_buffer[BUFFER_SIZE];
uint32_t g_bufidx;

// Counter to keep track of when we should add a keyframe.
uint32_t g_kfcount;

// Caches.
RegCache g_regcache;
MemCache g_memcache;

// Profiling timer.
clock_t g_timer;

// True if we logging is activated.
bool g_active;

// Number of instructions logged so far.
uint64_t g_logcount;

// Number of instructions to limit logging to.
uint64_t g_loglimit;

// True if a trigger was specified.
bool g_usetrigger;

// True if the trigger address was resolved.
bool g_trig_resolved;

uint32_t g_trig_addr;

// We use a signed integer because sometimes the countdown will be
// decremented past zero.
int g_trig_countdown;

// Prototypes.
VOID Cleanup();


ADDRINT CheckTrigger()
{
   return --g_trig_countdown <= 0;
}

VOID Activate()
{

   g_active = true;

   CODECACHE_FlushCache();

}


//
// Returns true if the buffer index with count added to it exceeds the
// maximum size of the buffer.
//
ADDRINT CheckBuffer(UINT32 count)
{
   return (g_bufidx + count) >= BUFFER_SIZE;
}

ADDRINT CheckBufferEx(BOOL cond, UINT32 count, UINT32 count2)
{
   return cond && ((g_bufidx + count + count2) >= BUFFER_SIZE);
}

//
// Writes all instructions stored in the buffer to disk, and resets the
// buffer index to 0. Also checks to see if we need to insert a
// keyframe. If so, inserts the keyframe using the data in the supplied
// context.
//
VOID FlushBuffer(BOOL addKeyframe, CONTEXT *ctx)
{

   //LOG("Begin flushing buffer.\n");

   for(uint32_t i = 0; i < g_bufidx; i++) {

      StdFrame f;
      f.addr = g_buffer[i].addr;
      f.tid = g_buffer[i].tid;
      f.insn_length = g_buffer[i].insn_length;

      //LOG(hexstr(f.addr) + ": writing.\n");

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
               /**
               LOG(hexstr(f.addr) + ": " +
                   "cached: " + hexstr(v.loc) +
                   " has value " + hexstr(g_memcache.elem32(v.loc)) +
                   "\n");
               */
            } else {
               /**
               LOG(hexstr(f.addr) + ": " +
                   "not cached: " + hexstr(v.loc) +
                   " has value " + hexstr(v.value) +
                   ", cache is " + hexstr(g_memcache.elem32(v.loc)) +
                   "\n");
               */

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

   // Update counts.
   g_logcount += g_bufidx;
   g_kfcount += g_bufidx;

   g_bufidx = 0;

   // Check to see if we should insert a keyframe here.
   if (addKeyframe && (g_kfcount >= KEYFRAME_FREQ)) {

      //LOG("Inserting keyframe:\n");
      //LOG("  addr: " + hexstr(PIN_GetContextReg(ctx, REG_EIP)) + "\n");

      KeyFrame kf;
      kf.pos = g_logcount;
      kf.setAll((uint32_t) PIN_GetContextReg(ctx, REG_EAX),
                (uint32_t) PIN_GetContextReg(ctx, REG_EBX),
                (uint32_t) PIN_GetContextReg(ctx, REG_ECX),
                (uint32_t) PIN_GetContextReg(ctx, REG_EDX),
                (uint32_t) PIN_GetContextReg(ctx, REG_ESI),
                (uint32_t) PIN_GetContextReg(ctx, REG_EDI),
                (uint32_t) PIN_GetContextReg(ctx, REG_ESP),
                (uint32_t) PIN_GetContextReg(ctx, REG_EBP),
                (uint32_t) PIN_GetContextReg(ctx, REG_EFLAGS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_CS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_DS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_SS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_ES),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_FS),
                (uint16_t) PIN_GetContextReg(ctx, REG_SEG_GS)
                );


      // Add entry to TOC.
      g_toc.push_back(g_tw->offset());

      // And then add the keyframe.
      g_tw->add(kf);

      // Now repopulate the register cache.
      g_regcache.setAll(kf.eax, kf.ebx, kf.ecx, kf.edx,
                        kf.esi, kf.edi, kf.esp, kf.ebp,
                        kf.eflags,
                        kf.cs, kf.ds, kf.ss,
                        kf.es, kf.fs, kf.gs);
                        
      // Finally clear the data cache.
      g_memcache.clearAll();

      g_kfcount = 0;

   }

   // See if we've gotten sufficient instructions.
   if ((g_loglimit != 0) && (g_logcount >= g_loglimit)) {

      LOG("Logged required number of instructions, quitting.\n");
      Cleanup();
      PIN_Detach();
   }

   //LOG("End flushing buffer.\n");

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
                  // Note: If the valspec is a memory location, _value
                  // will actually contain the size of the memory
                  // location. The value will be retrieved directly using
                  // _loc.
                  UINT32 valspec0_value,

                  UINT32 valspec1_type,
                  UINT32 valspec1_loc,
                  UINT32 valspec1_value,

                  UINT32 valspec2_type,
                  UINT32 valspec2_loc,
                  UINT32 valspec2_value,

                  UINT32 valspec3_type,
                  UINT32 valspec3_loc,
                  UINT32 valspec3_value,

                  UINT32 valspec4_type,
                  UINT32 valspec4_loc,
                  UINT32 valspec4_value,

                  UINT32 valspec5_type,
                  UINT32 valspec5_loc,
                  UINT32 valspec5_value,

                  UINT32 valspec6_type,
                  UINT32 valspec6_loc,
                  UINT32 valspec6_value,

                  UINT32 valspec7_type,
                  UINT32 valspec7_loc,
                  UINT32 valspec7_value,

                  UINT32 valspec8_type,
                  UINT32 valspec8_loc,
                  UINT32 valspec8_value,

                  UINT32 valspec9_type,
                  UINT32 valspec9_loc,
                  UINT32 valspec9_value,

                  UINT32 valspec10_type,
                  UINT32 valspec10_loc,
                  UINT32 valspec10_value,

                  UINT32 valspec11_type,
                  UINT32 valspec11_loc,
                  UINT32 valspec11_value,

                  UINT32 valspec12_type,
                  UINT32 valspec12_loc,
                  UINT32 valspec12_value,

                  UINT32 valspec13_type,
                  UINT32 valspec13_loc,
                  UINT32 valspec13_value,

                  UINT32 valspec14_type,
                  UINT32 valspec14_loc,
                  UINT32 valspec14_value,

                  // This is the last argument because often we don't know
                  // how many values there will be until after we've built
                  // up the preceding arguments in the argument list.
                  UINT32 values_count
                  )
{

#if 0
   // Used to collect instruction argument count and type distribution.
   LOG("FOO: " + decstr(values_count));
   LOG("\t" + decstr(valspec0_type));
   LOG("\t" + decstr(valspec1_type));
   LOG("\t" + decstr(valspec2_type));
   LOG("\t" + decstr(valspec3_type));
   LOG("\t" + decstr(valspec4_type));
   LOG("\t" + decstr(valspec5_type));
   LOG("\t" + decstr(valspec6_type));
   LOG("\t" + decstr(valspec7_type));
   LOG("\t" + decstr(valspec8_type));
   LOG("\t" + decstr(valspec9_type));
   LOG("\t" + decstr(valspec10_type));
   LOG("\t" + decstr(valspec11_type));
   LOG("\t" + decstr(valspec12_type));
   LOG("\t" + decstr(valspec13_type));
   LOG("\t" + decstr(valspec14_type));
   LOG("\n");
#endif

   //LOG("APPEND: " + hexstr(addr) + "\n");

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
      g_buffer[g_bufidx].valspecs[i].value = valspec##i##_value;        \
      break;                                                            \
   case VT_MEM32:                                                       \
      PIN_SafeCopy((VOID* ) &(g_buffer[g_bufidx].valspecs[i].value),    \
                   (const VOID *) valspec##i##_loc, 4);                 \
      break;                                                            \
   case VT_MEM16:                                                       \
      PIN_SafeCopy((VOID* ) &(g_buffer[g_bufidx].valspecs[i].value),    \
                   (const VOID *) valspec##i##_loc, 2);                 \
      break;                                                            \
   case VT_MEM8:                                                        \
      PIN_SafeCopy((VOID* ) &(g_buffer[g_bufidx].valspecs[i].value),    \
                   (const VOID *) valspec##i##_loc, 1);                 \
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

// Fast path append buffer for a single 32-bit register value.
VOID AppendBufferR32(ADDRINT addr,
                     THREADID tid,
                     UINT32 insn_length,

                     UINT32 rawbytes0,
                     UINT32 rawbytes1,
                     UINT32 rawbytes2,
                     UINT32 rawbytes3,
                     
                     UINT32 valspec0_loc,
                     UINT32 valspec0_value)
{

   //LOG("APPENDR32: " + hexstr(addr) + "\n");

   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   g_buffer[g_bufidx].values_count = 1;

   g_buffer[g_bufidx].valspecs[0].type = VT_REG32;
   g_buffer[g_bufidx].valspecs[0].loc = valspec0_loc;
   g_buffer[g_bufidx].valspecs[0].value = valspec0_value;

   g_bufidx++;
   return;

}


// Fast path append buffer for two 32-bit register values.
VOID AppendBufferR32R32(ADDRINT addr,
                        THREADID tid,
                        UINT32 insn_length,
                        
                        UINT32 rawbytes0,
                        UINT32 rawbytes1,
                        UINT32 rawbytes2,
                        UINT32 rawbytes3,

                        UINT32 valspec0_loc,
                        UINT32 valspec0_value,

                        UINT32 valspec1_loc,
                        UINT32 valspec1_value
                        )
{

   //LOG("APPENDR32R32: " + hexstr(addr) + "\n");

   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   g_buffer[g_bufidx].values_count = 2;

   g_buffer[g_bufidx].valspecs[0].type = VT_REG32;
   g_buffer[g_bufidx].valspecs[0].loc = valspec0_loc;
   g_buffer[g_bufidx].valspecs[0].value = valspec0_value;

   g_buffer[g_bufidx].valspecs[1].type = VT_REG32;
   g_buffer[g_bufidx].valspecs[1].loc = valspec1_loc;
   g_buffer[g_bufidx].valspecs[1].value = valspec1_value;

   g_bufidx++;
   return;

}

// Fast path append buffer for 16-bit register, 32-bit register, 32-bit
// memory location values.
VOID AppendBufferR16R32M32(ADDRINT addr,
                           THREADID tid,
                           UINT32 insn_length,
                        
                           UINT32 rawbytes0,
                           UINT32 rawbytes1,
                           UINT32 rawbytes2,
                           UINT32 rawbytes3,

                           UINT32 valspec0_loc,
                           UINT32 valspec0_value,

                           UINT32 valspec1_loc,
                           UINT32 valspec1_value,
                           
                           UINT32 valspec2_loc
                        )
{

   //LOG("APPENDR16R32M32: " + hexstr(addr) + "\n");

   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   g_buffer[g_bufidx].values_count = 3;

   g_buffer[g_bufidx].valspecs[0].type = VT_REG16;
   g_buffer[g_bufidx].valspecs[0].loc = valspec0_loc;
   g_buffer[g_bufidx].valspecs[0].value = valspec0_value;

   g_buffer[g_bufidx].valspecs[1].type = VT_REG32;
   g_buffer[g_bufidx].valspecs[1].loc = valspec1_loc;
   g_buffer[g_bufidx].valspecs[1].value = valspec1_value;

   g_buffer[g_bufidx].valspecs[2].type = VT_MEM32;
   g_buffer[g_bufidx].valspecs[2].loc = valspec2_loc;
   PIN_SafeCopy((VOID *) &(g_buffer[g_bufidx].valspecs[2].value),
                (const VOID *) valspec2_loc,
                4);

   g_bufidx++;
   return;

}



VOID InstrBlock(BBL bbl)
{

   uint32_t icount = BBL_NumIns(bbl);

   // LOG("INS: BBL start.\n");

   if (g_active) {

      if (icount > BUFFER_SIZE) {
         LOG("WARNING: Basic block too many instructions: " + 
             decstr(icount) + "\n");
         LOG("Detaching.\n");
         PIN_Detach();
         // TODO: What else should we do here?
      }

      // Add instrumentation call to check if buffer needs to be flushed.
      BBL_InsertIfCall(bbl, IPOINT_BEFORE,
                       (AFUNPTR) CheckBuffer,
                       IARG_UINT32, icount,
                       IARG_END);

      BBL_InsertThenCall(bbl, IPOINT_BEFORE,
                         (AFUNPTR) FlushBuffer,
                         IARG_BOOL, true,
                         IARG_CONTEXT,
                         IARG_END);

   }

   //LOG("INS: BBL ins start.\n");

   // Count of instructions that have yet to be inserted into the buffer,
   // at the point at which the current instruction will be executed.
   uint32_t insLeft = icount;

   for (INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins)) {

      if (!g_active && g_usetrigger) {
         // Logging has not been activated yet, so all we need to do now
         // is check for the trigger condition.

         if (INS_Address(ins) == g_trig_addr) {
            // Found the trigger address.

            INS_InsertIfCall(ins, IPOINT_BEFORE,
                             (AFUNPTR) CheckTrigger,
                             IARG_END);

            INS_InsertThenCall(ins, IPOINT_BEFORE,
                               (AFUNPTR) Activate,
                               IARG_END);

         }

         // Skip the rest of the analysis and immediately go on to the
         // next instruction.
         continue;

      }

      // Add instrumentation call to insert instruction into buffer.

      if (INS_Category(ins) == XED_CATEGORY_X87_ALU) {

         // TODO: Handle floating point instructions.
         LOG("Not logging FP instruction.\n");
         continue;

      } // TODO: Other special instructions we can't handle.

      // Check if there's a REP prefix.
      if (INS_HasRealRep(ins)) {

         INS_InsertIfCall(ins, IPOINT_BEFORE,
                          (AFUNPTR) CheckBufferEx,
                          IARG_FIRST_REP_ITERATION,
                          IARG_REG_VALUE, INS_RepCountRegister(ins),
                          IARG_UINT32, insLeft,
                          IARG_END);

         INS_InsertThenCall(ins, IPOINT_BEFORE,
                            (AFUNPTR) FlushBuffer,
                            IARG_BOOL, false,
                            IARG_ADDRINT, 0,
                            IARG_END);

      }

      // Pointer to function that will do the appending.
      AFUNPTR appendFunc;

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

      // Used to temporarily store the values we obtain from the operands,
      // to faciliate further analysis for fast paths.
      // [0] == register, [1] == value specifier type.
      uint32_t opndvals[MAX_VALUES_COUNT][2];

      for(uint32_t i = 0; i < INS_OperandCount(ins); i++) {

         if (INS_OperandIsReg(ins, i) && INS_OperandRead(ins, i)) {

            opndvals[valcount][0] = INS_OperandReg(ins, i);
            
            if (REG_is_gr32((REG) opndvals[valcount][0]))
               opndvals[valcount][1] = VT_REG32;
            else if (REG_is_gr16((REG) opndvals[valcount][0])) 
               opndvals[valcount][1] = VT_REG16;
            else if (REG_is_gr8((REG) opndvals[valcount][0])) 
               opndvals[valcount][1] = VT_REG8;
            else {
               // TODO: Handle more cases, and decide if this catchall is
               // a really bad idea.
               opndvals[valcount][1] = VT_REG32;
            }

            valcount++;

            /**
            IARGLIST_AddArguments(arglist,
                                  IARG_UINT32, ty,
                                  IARG_UINT32, r,
                                  IARG_REG_VALUE, r,
                                  IARG_END);
            **/

         } else if (INS_OperandIsMemory(ins, i)) {

            REG segreg = INS_OperandMemorySegmentReg(ins, i);
            if (segreg != REG_INVALID()) {
               opndvals[valcount][0] = segreg;
               opndvals[valcount][1] = VT_REG16;
               valcount++;
            }

            REG basereg = INS_OperandMemoryBaseReg(ins, i);
            if (basereg != REG_INVALID()) {

               opndvals[valcount][0] = basereg;

               if (REG_is_gr32(basereg)) 
                  opndvals[valcount][1] = VT_REG32;
               else if (REG_is_gr16(basereg)) 
                  opndvals[valcount][1] = VT_REG16;
               else if (REG_is_gr8(basereg)) 
                  opndvals[valcount][1] = VT_REG8;
               else {
                  // TODO: Handle more cases, and decide if this catchall is
                  // a really bad idea.
                  opndvals[valcount][1] = VT_REG32;
               }

               valcount++;

            }

            REG idxreg = INS_OperandMemoryIndexReg(ins, i);
            if (idxreg != REG_INVALID()) {

               opndvals[valcount][0] = idxreg;

               if (REG_is_gr32(idxreg))
                  opndvals[valcount][1] = VT_REG32;
               else if (REG_is_gr16(idxreg))
                  opndvals[valcount][1] = VT_REG16;
               else if (REG_is_gr8(idxreg))
                  opndvals[valcount][1] = VT_REG8;
               else {
                  // TODO: Handle more cases, and decide if this catchall is
                  // a really bad idea.
                  opndvals[valcount][1] = VT_REG32;
               }
               
               valcount++;

            }

         }

      }

      bool memRead = INS_IsMemoryRead(ins);
      bool memRead2 = INS_HasMemoryRead2(ins);

      // Value type of memory read.
      uint32_t memReadTy = VT_NONE;
      if (memRead) {
         switch (INS_MemoryReadSize(ins)) {
         case 1: memReadTy = VT_MEM8; break;
         case 2: memReadTy = VT_MEM16; break;
         case 4: memReadTy = VT_MEM32; break;
         case 8:
         default:
            LOG("ERROR: Unsupported memory read size: " +
                decstr(INS_MemoryReadSize(ins)) + "!\n");
            LOG("Instruction: " + INS_Disassemble(ins) + "\n");
            LOG("Instruction category: " +
                CATEGORY_StringShort(INS_Category(ins)) + "\n");
            LOG("Detaching.\n");
            PIN_Detach();
            return;
         }
      }

      // Now we can decide whether or not to use a fast path analysis
      // routine.

      if (_FASTPATH &&
          !memRead && !memRead2 &&
          (valcount == 1) &&
          (opndvals[0][1] == VT_REG32)) {

         // Use Reg32 fast path.

         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, opndvals[0][0],
                               IARG_REG_VALUE, opndvals[0][0],
                               IARG_END);
         
         appendFunc = (AFUNPTR) AppendBufferR32;

      } else if (_FASTPATH &&
                 !memRead && !memRead2 &&
                 (valcount == 2) && 
                 (opndvals[0][1] == VT_REG32) &&
                 (opndvals[1][1] == VT_REG32)) {

         // Use Reg32Reg32 fast path.
         
         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, opndvals[0][0],
                               IARG_REG_VALUE, opndvals[0][0],
                               IARG_UINT32, opndvals[1][0],
                               IARG_REG_VALUE, opndvals[1][0],
                               IARG_END);
         
         appendFunc = (AFUNPTR) AppendBufferR32R32;

      } else if (_FASTPATH &&
                 memRead && (memReadTy == VT_MEM32) &&
                 (valcount == 2) &&
                 (opndvals[0][1] == VT_REG16) &&
                 (opndvals[1][1] == VT_REG32)) {

         // Use Reg16Reg32Mem32 fast path.

         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, opndvals[0][0],
                               IARG_REG_VALUE, opndvals[0][0],
                               IARG_UINT32, opndvals[1][0],
                               IARG_REG_VALUE, opndvals[1][0],
                               IARG_MEMORYREAD_EA,
                               IARG_END);

         appendFunc = (AFUNPTR) AppendBufferR16R32M32;

      } else {

         // We have no choice but to use the generic, slower
         // AppendBuffer.
         
         // Insert opndvals into arglist.
         for (unsigned int i = 0; i < valcount; i++) {
            IARGLIST_AddArguments(arglist,
                                  IARG_UINT32, opndvals[i][1],
                                  IARG_UINT32, opndvals[i][0],
                                  IARG_REG_VALUE, opndvals[i][0],
                                  IARG_END);
         }

         if (memRead) {

            IARGLIST_AddArguments(arglist,
                                  IARG_UINT32, memReadTy,
                                  IARG_MEMORYREAD_EA,
                                  IARG_MEMORYREAD_SIZE,
                                  IARG_END);
            valcount++;
         }

         if (memRead2) {

            IARGLIST_AddArguments(arglist,
                                  IARG_UINT32, memReadTy,
                                  IARG_MEMORYREAD2_EA,
                                  IARG_MEMORYREAD_SIZE,
                                  IARG_END);
            valcount++;
         }

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

         appendFunc = (AFUNPTR) AppendBuffer;

      }

      // The argument list has been built, time to insert the call.

      INS_InsertCall(ins, IPOINT_BEFORE,
                     appendFunc,
                     IARG_IARGLIST, arglist,
                     IARG_END);

      insLeft--;

      // Free the memory.
      IARGLIST_Free(arglist);

   }

   //LOG("INS: bbl ins end.\nINS: BBL end.\n");
   

}

VOID InstrTrace(TRACE trace, VOID *v)
{

   for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {
      InstrBlock(bbl);
   }

}

VOID ModLoad(IMG img, VOID *v)
{

   LoadModuleFrame f;
   f.low_addr = IMG_LowAddress(img);
   f.high_addr = IMG_HighAddress(img);
   f.start_addr = IMG_StartAddress(img);
   f.load_offset = IMG_LoadOffset(img);

   const string &name = IMG_Name(img);

   size_t sz = name.size() < 64 ? name.size() : 63;

   memset(&(f.name), 0, 64);
   memcpy(&(f.name), name.c_str(), sz);

   g_tw->add(f);

   if (g_usetrigger && !g_trig_resolved) {
      // Check if this module can be used to resolve the trigger address.

      // If no trigger module is set, then we just use the first one we
      // find, i.e. the main module.
      if ((KnobTrigModule.Value() == "") ||
          name.find(KnobTrigModule.Value()) != string::npos) {
         // Found the module, resolve address.

         g_trig_addr = KnobTrigAddr.Value() + IMG_LoadOffset(img);
         g_trig_resolved = true;

      }

   }

}

VOID SyscallEntry(THREADID tid, CONTEXT *ctx, SYSCALL_STANDARD std, VOID *v)
{

   // Ignore if not activated.
   if (!g_active) return;

   // First we need to flush the buffer, so we can directly add the
   // syscall frame after the frame for the instruction that led to the
   // syscall.
   FlushBuffer(true, ctx);

   SyscallFrame f;

   // Get the address from instruction pointer (should be EIP).
   f.addr = (uint32_t) PIN_GetContextReg(ctx, REG_INST_PTR);

   f.tid = tid;

   f.callno = (uint32_t) PIN_GetSyscallNumber(ctx, SYSCALL_STD);

   for (int i = 0; i < MAX_SYSCALL_ARGS; i++)
      f.args[i] = 
         (uint32_t) PIN_GetSyscallArgument(ctx, SYSCALL_STD, i);
   
   g_tw->add(f);

}

VOID Fini(INT32 code, VOID *v)
{
   Cleanup();
}

VOID Cleanup()
{
   // Build TOC array.
   LOG("Building TOC...\n");

   uint32_t *toc = new uint32_t[g_toc.size() + 1];

   toc[0] = g_toc.size();
   for(uint32_t i = 0; i < toc[0]; i++)
      toc[i+1] = g_toc[i];
   
   LOG("done.\n");
   LOG("Finalizing trace...\n");

   g_tw->finalize(toc);

   delete toc;

   LOG("done.\n");

   clock_t endtime = clock();

   LOG("Time taken: " + decstr((UINT64) (endtime - g_timer)));

}

int main(int argc, char *argv[])
{

   if (PIN_Init(argc,argv))
      return 1;

   // Check if a trigger was specified.
   if (KnobTrigAddr.Value() != 0) {

      g_active = false;
      g_usetrigger = true;
      g_trig_resolved = false;

      // Set trigger countdown to initial value.
      g_trig_countdown = KnobTrigCount.Value();
      
   } else {
      g_active = true;
      g_usetrigger = false;
   }

   IMG_AddInstrumentFunction(ModLoad, 0);
   TRACE_AddInstrumentFunction(InstrTrace, 0);
   
   PIN_AddSyscallEntryFunction(SyscallEntry, 0);

   PIN_AddFiniFunction(Fini, 0);

   g_tw = new TraceWriter(KnobOut.Value().c_str());

   g_bufidx = 0;
   g_kfcount = 0;

   g_logcount = 0;
   g_loglimit = KnobLogLimit.Value();

   g_timer = clock();

   // Start the program, never returns
   PIN_StartProgram();

   return 0;

}
