#include "pin.H"

#include <iostream>
#include <fstream>
#include <stdint.h>
#include <string.h>
#include <time.h>

#include "frame.h"
#include "trace.h"

#include "frame.cpp"
#include "trace.cpp"

#define BUFFER_SIZE 1024

struct FrameBuf {
   uint32_t addr;
   uint32_t tid;
   uint32_t insn_length;
   uint32_t opnd_count;

   // The raw instruction bytes will be stored as 16 bytes placed over 4
   // integers. The conversion is equivalent to the casting of a char[16]
   // to a uint32_t[4].
   // NOTE: This assumes that MAX_INSN_BYTES == 16!!!
   uint32_t rawbytes0;
   uint32_t rawbytes1;
   uint32_t rawbytes2;
   uint32_t rawbytes3;

   uint32_t opnd0base;
   uint32_t opnd0index;
   uint32_t opnd0value;
   uint32_t opnd1base;
   uint32_t opnd1index;
   uint32_t opnd1value;
   uint32_t opnd2base;
   uint32_t opnd2index;
   uint32_t opnd2value;

};

TraceWriter *g_tw;

FrameBuf g_buffer[BUFFER_SIZE];
uint32_t g_bufidx;

// We need a pointer to a zero value as the dummy value for operands which
// have no "value".
uint32_t g_zero = 0;
uint32_t *g_zeroptr = &g_zero;

//
// Appends "count" to the buffer index, and then returns true if the
// index exceeds the maximum size of the buffer.
//
ADDRINT CheckBuffer(UINT32 count)
{
   g_bufidx += count;
   return g_bufidx >= BUFFER_SIZE;
}

//
// Writes all instructions stored in the buffer to disk, and resets the
// buffer index to 0.
//
VOID FlushBuffer()
{
#if 0
   for(uint32_t i = 0; i < g_bufidx; i++) {

      StdFrame f;
      f.addr = g_buffer[i].addr;
      f.tid = g_buffer[i].tid;
      f.insn_length = g_buffer[i].insn_length;
      f.opnd_count = g_buffer[i].opnd_count;

      memcpy((char *) f.rawbytes, (char *) &(g_buffer[i].rawbytes0), f.insn_length);

      f.opnds[0].base = g_buffer[i].opnd0base;
      f.opnds[0].index = g_buffer[i].opnd0index;
      f.opnds[0].value = g_buffer[i].opnd0value;

      f.opnds[1].base = g_buffer[i].opnd1base;
      f.opnds[1].index = g_buffer[i].opnd1index;
      f.opnds[1].value = g_buffer[i].opnd1value;

      f.opnds[2].base = g_buffer[i].opnd2base;
      f.opnds[2].index = g_buffer[i].opnd2index;
      f.opnds[2].value = g_buffer[i].opnd2value;
     
      g_tw->add(f);

   }
#endif

   g_bufidx = 0;

}

VOID AppendBuffer(ADDRINT addr,
                  THREADID tid,
                  UINT32 insn_length,
                  UINT32 opnd_count,

                  UINT32 rawbytes0,
                  UINT32 rawbytes1,
                  UINT32 rawbytes2,
                  UINT32 rawbytes3,

                  UINT32 opnd0base,
                  UINT32 opnd0index,
                  ADDRINT opnd0addr,

                  UINT32 opnd1base,
                  UINT32 opnd1index,
                  ADDRINT opnd1addr,

                  UINT32 opnd2base,
                  UINT32 opnd2index,
                  ADDRINT opnd2addr)
{
#if 1
   g_buffer[g_bufidx].addr = addr;
   g_buffer[g_bufidx].tid = tid;
   g_buffer[g_bufidx].insn_length = insn_length;
   g_buffer[g_bufidx].opnd_count = opnd_count;

   g_buffer[g_bufidx].rawbytes0 = rawbytes0;
   g_buffer[g_bufidx].rawbytes1 = rawbytes1;
   g_buffer[g_bufidx].rawbytes2 = rawbytes2;
   g_buffer[g_bufidx].rawbytes3 = rawbytes3;

   g_buffer[g_bufidx].opnd0base = opnd0base;
   g_buffer[g_bufidx].opnd0index = opnd0index;
   g_buffer[g_bufidx].opnd0value = *((uint32_t *) opnd0addr);

   /**
   PIN_SafeCopy(&(g_buffer[g_bufidx].opnd0value), 
                (const VOID *) opnd0addr, 
                sizeof(uint32_t));
   **/

   g_buffer[g_bufidx].opnd1base = opnd1base;
   g_buffer[g_bufidx].opnd1index = opnd1index;
   g_buffer[g_bufidx].opnd1value = *((uint32_t *) opnd1addr);
   /**
   PIN_SafeCopy(&(g_buffer[g_bufidx].opnd1value), 
                (const VOID *) opnd1addr, 
                sizeof(uint32_t));
   **/

   g_buffer[g_bufidx].opnd2base = opnd2base;
   g_buffer[g_bufidx].opnd2index = opnd2index;

   LOG("opnd2addr = " + hexstr(opnd2addr) + "\n");

   //g_buffer[g_bufidx].opnd2value = *((uint32_t *) opnd2addr);
   /**
   PIN_SafeCopy(&(g_buffer[g_bufidx].opnd2value), 
                (const VOID *) opnd2addr, 
                sizeof(uint32_t));
   **/
#endif
}

VOID InstrBlock(BBL bbl)
{

   uint32_t icount = 0;
#if 1
   for (INS ins = BBL_InsHead(bbl); INS_Valid(ins); ins = INS_Next(ins)) {
      // Add instrumentation call to insert instruction into buffer.

      // The argument list to be passed into the instruction analysis call.
      IARGLIST arglist = IARGLIST_Alloc();

      // TODO: Need to assert that opnd_count <= MAX_OPND_COUNT.
      uint32_t opnd_count = INS_OperandCount(ins);

      // The first few arguments to AppendBuffer.
      IARGLIST_AddArguments(arglist,
                            IARG_ADDRINT, INS_Address(ins), 
                            IARG_THREAD_ID,
                            IARG_UINT32, INS_Size(ins),
                            IARG_UINT32, opnd_count,
                            IARG_END);

      // Now we need to gather the instruction bytes.

      uint32_t rawbytes_i[4];

      // Wastes a copy, but required because the instruction may not be
      // 32-bit aligned, and we want to respect word alignment requirements.
      memcpy((char *) rawbytes_i, (char *) INS_Address(ins), INS_Size(ins));

      IARGLIST_AddArguments(arglist,
                            IARG_UINT32, rawbytes_i[0],
                            IARG_UINT32, rawbytes_i[1],
                            IARG_UINT32, rawbytes_i[2],
                            IARG_UINT32, rawbytes_i[3],
                            IARG_END);

      // Now we need to get the operand information.
     
      for(uint32_t i = 0; i < opnd_count; i++) {

         if (INS_OperandRead(ins, i)) {

            if (INS_OperandIsReg(ins, i)) {

               IARGLIST_AddArguments(arglist,
                                     IARG_REG_VALUE, INS_OperandReg(ins, i),
                                     IARG_UINT32, 0,
                                     IARG_ADDRINT, g_zeroptr,
                                     IARG_END);

            } else if (INS_OperandIsMemory(ins, i)) {

               REG basereg = INS_OperandMemoryBaseReg(ins, i);
               REG idxreg = INS_OperandMemoryIndexReg(ins, i);

               if (basereg != REG_INVALID()) {
                  IARGLIST_AddArguments(arglist,
                                        IARG_REG_VALUE, basereg,
                                        IARG_END);
               } else {
                  IARGLIST_AddArguments(arglist,
                                        IARG_UINT32, 0,
                                        IARG_END);
               }

               if (idxreg != REG_INVALID()) {
                  IARGLIST_AddArguments(arglist,
                                        IARG_REG_VALUE, idxreg,
                                        IARG_END);
               } else {
                  IARGLIST_AddArguments(arglist,
                                        IARG_UINT32, 0,
                                        IARG_END);
               }

               if (INS_IsMemoryRead(ins)) {
                  IARGLIST_AddArguments(arglist,
                                        IARG_MEMORYREAD_EA,
                                        IARG_END);
               } else {
                  IARGLIST_AddArguments(arglist,
                                        IARG_ADDRINT, g_zeroptr,
                                        IARG_END);
                  LOG("???: " + INS_Disassemble(ins) + "\n");
               }

            } else {
               
               // We can ignore immediates. Any other cases we need to
               // handle?

               IARGLIST_AddArguments(arglist,
                                     IARG_UINT32, 0,
                                     IARG_UINT32, 0,
                                     IARG_ADDRINT, g_zeroptr,
                                     IARG_END);
               
            }

         }

      }

      for (uint32_t i = opnd_count; i < MAX_OPND_COUNT; i++) {
         IARGLIST_AddArguments(arglist,
                               IARG_UINT32, 0,
                               IARG_UINT32, 0,
                               IARG_ADDRINT, g_zeroptr,
                               IARG_END);
      }
      
      // The argument list has been built, time to insert the call.

      INS_InsertCall(ins, IPOINT_BEFORE,
                     (AFUNPTR) AppendBuffer,
                     IARG_IARGLIST, arglist,
                     IARG_END);
      
      icount++;

   }
#endif
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

   g_tw = new TraceWriter("test.bpt");

   g_bufidx = 0;

   // Start the program, never returns
   PIN_StartProgram();
   
   return 0;

}
