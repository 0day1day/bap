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

#define USE_FASTBUF

clock_t g_starttime;

TraceWriter *g_tw;

BUFFER_ID g_bufid;

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

VOID *BufferFull(BUFFER_ID id, THREADID tid, const CONTEXT *ctxt, 
                 VOID *buf, UINT64 count, VOID *v)
{

   FrameBuf *fbs = (FrameBuf *) buf;
  
   for(unsigned int i = 0; i < count; i++) {

      StdFrame f;
      f.addr = fbs[i].addr;
      f.tid = fbs[i].tid;
      f.insn_length = fbs[i].insn_length;
      f.opnd_count = fbs[i].opnd_count;

      memcpy((char *) f.rawbytes, (char *) &(fbs[i].rawbytes0), f.insn_length);

      f.opnds[0].base = fbs[i].opnd0base;
      f.opnds[0].index = fbs[i].opnd0index;
      f.opnds[0].value = fbs[i].opnd0value;

      f.opnds[1].base = fbs[i].opnd1base;
      f.opnds[1].index = fbs[i].opnd1index;
      f.opnds[1].value = fbs[i].opnd1value;

      f.opnds[2].base = fbs[i].opnd2base;
      f.opnds[2].index = fbs[i].opnd2index;
      f.opnds[2].value = fbs[i].opnd2value;
     
      g_tw->add(f);

   }

   return buf;
   
}


VOID Log(ADDRINT addr, THREADID tid)
{

   StdFrame f;
   f.addr = addr;
   f.tid = tid;
   f.opnd_count = 0;

   g_tw->add(f);

}


VOID Ins(INS ins, VOID *v) 
{

#ifdef USE_FASTBUF

   // First we gather the instruction bytes.

   uint32_t rawbytes_i[4];

   // Wastes a copy, but required because the instruction may not be
   // 32-bit aligned, and we want to respect word alignment requirements.
   memcpy((char *) rawbytes_i, (char *) INS_Address(ins), INS_Size(ins));

   // Now we need to get the operand information.

   // 3 because (base, index, value)
   IARGLIST opnd_args[MAX_OPND_COUNT][3];

   for (uint32_t i = 0; i < MAX_OPND_COUNT; i++) {
      opnd_args[i][0] = IARGLIST_Alloc();
      opnd_args[i][1] = IARGLIST_Alloc();
      opnd_args[i][2] = IARGLIST_Alloc();
   }

   uint32_t opnd_count = INS_OperandCount(ins);

   // TODO: Need to assert that opnd_count <= MAX_OPND_COUNT.

   for(uint32_t i = 0; i < opnd_count; i++) {

      if (INS_OperandRead(ins, i)) {

         if (INS_OperandIsReg(ins, i)) {

            IARGLIST_AddArguments(opnd_args[i][0],
                                  IARG_REG_VALUE, INS_OperandReg(ins, i),
                                  IARG_END);

            IARGLIST_AddArguments(opnd_args[i][1],
                                  IARG_UINT32, 0,
                                  IARG_END);

            IARGLIST_AddArguments(opnd_args[i][2],
                                  IARG_UINT32, 0,
                                  IARG_END);

         } else if (INS_OperandIsMemory(ins, i)) {

            IARGLIST_AddArguments(opnd_args[i][0],
                                  IARG_REG_VALUE, INS_OperandMemoryBaseReg(ins, i),
                                  IARG_END);

            IARGLIST_AddArguments(opnd_args[i][1],
                                  IARG_REG_VALUE, INS_OperandMemoryIndexReg(ins, i),
                                  IARG_END);

            IARGLIST_AddArguments(opnd_args[i][2],
                                  IARG_MEMORYREAD_EA,
                                  IARG_END);

         } else {

            // We can ignore immediates. Any other cases we need to
            // handle?

            IARGLIST_AddArguments(opnd_args[i][0], IARG_UINT32, 0, IARG_END);
            IARGLIST_AddArguments(opnd_args[i][1], IARG_UINT32, 0, IARG_END);
            IARGLIST_AddArguments(opnd_args[i][2], IARG_UINT32, 0, IARG_END);

         }

      }

      for (uint32_t i = opnd_count; i < MAX_OPND_COUNT; i++) {
         IARGLIST_AddArguments(opnd_args[i][0], IARG_UINT32, 0, IARG_END);
         IARGLIST_AddArguments(opnd_args[i][1], IARG_UINT32, 0, IARG_END);
         IARGLIST_AddArguments(opnd_args[i][2], IARG_UINT32, 0, IARG_END);
      }

      //
      // TODO: Log down operand values. Also figure out if we need to log
      // down write values as well. Particularly for I/O instructions.
      //

   }


   INS_InsertFillBuffer(ins, IPOINT_BEFORE, g_bufid, 
                        IARG_ADDRINT, INS_Address(ins), offsetof(FrameBuf, addr),
                        IARG_THREAD_ID, offsetof(FrameBuf, tid),
                        IARG_UINT32, INS_Size(ins), offsetof(FrameBuf, insn_length),
                        IARG_UINT32, opnd_count, offsetof(FrameBuf, opnd_count),
                        IARG_UINT32, rawbytes_i[0], offsetof(FrameBuf, rawbytes0),
                        IARG_UINT32, rawbytes_i[1], offsetof(FrameBuf, rawbytes1),
                        IARG_UINT32, rawbytes_i[2], offsetof(FrameBuf, rawbytes2),
                        IARG_UINT32, rawbytes_i[3], offsetof(FrameBuf, rawbytes3),
                        IARG_IARGLIST, opnd_args[0][0], offsetof(FrameBuf, opnd0base),
                        IARG_IARGLIST, opnd_args[0][1], offsetof(FrameBuf, opnd0index),
                        IARG_IARGLIST, opnd_args[0][2], offsetof(FrameBuf, opnd0value),
                        IARG_IARGLIST, opnd_args[1][0], offsetof(FrameBuf, opnd1base),
                        IARG_IARGLIST, opnd_args[1][1], offsetof(FrameBuf, opnd1index),
                        IARG_IARGLIST, opnd_args[1][2], offsetof(FrameBuf, opnd1value),
                        IARG_IARGLIST, opnd_args[2][0], offsetof(FrameBuf, opnd2base),
                        IARG_IARGLIST, opnd_args[2][1], offsetof(FrameBuf, opnd2index),
                        IARG_IARGLIST, opnd_args[2][2], offsetof(FrameBuf, opnd2value),
                        IARG_END);

#else
   INS_InsertCall(ins,
                  IPOINT_BEFORE,
                  (AFUNPTR) Log,
                  IARG_ADDRINT, INS_Address(ins),
                  IARG_THREAD_ID,
                  IARG_END);
#endif

}

VOID Fini(INT32 code, VOID *v)
{
   g_tw->finalize();

   clock_t endtime = clock();

#ifdef USE_FASTBUF
   ofstream fs("/tmp/pintime.fastbuf", ios_base::out | ios_base::app);
#else
   ofstream fs("/tmp/pintime.orig", ios_base::out | ios_base::app);
#endif

   fs << (endtime - g_starttime) << endl;

   fs.close();

}

int main(int argc, char *argv[])
{

   if (PIN_Init(argc,argv))
      return 1;

#ifdef USE_FASTBUF
   cerr << "Using fast buffer method." << endl;
#endif

   g_bufid = PIN_DefineTraceBuffer(sizeof(FrameBuf),
                                   1024,
                                   BufferFull,
                                   NULL);
  
   if (g_bufid == BUFFER_ID_INVALID) {
      cerr << "ERROR: Could not allocate buffer." << endl;
      return 1;
   }

   INS_AddInstrumentFunction(Ins, 0);
   PIN_AddFiniFunction(Fini, 0);

   g_tw = new TraceWriter("test.bpt");

   g_starttime = clock();

   // Start the program, never returns
   PIN_StartProgram();
   
   return 0;

}
