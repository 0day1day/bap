#include "pin.H"

VOID Log(OPCODE op)
{

   printf("0x%x\n", op);

}


VOID Ins(INS ins, VOID *v) 
{

   printf("%s\n", INS_Disassemble(ins).c_str());

   INS_InsertCall(ins,
                  IPOINT_BEFORE,
                  (AFUNPTR) Log,
                  IARG_ADDRINT, INS_Opcode(ins),
                  IARG_END);
  
}

VOID Fini(INT32 code, VOID *v)
{
}

int main(int argc, char *argv[])
{

   if (PIN_Init(argc,argv))
      return 1;
  
   INS_AddInstrumentFunction(Ins, 0);
   // PIN_AddFiniFunction(Fini, 0);

   // Start the program, never returns
   PIN_StartProgram();
   
   return 0;

}
