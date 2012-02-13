#include "reg_mapping_pin.h"

string pin_register_name(uint32_t id)
{

switch (id) {
    //case reg_mapping::REG_INVALID_ :
    //case reg_mapping::REG_NONE :
    //case reg_mapping::REG_FIRST:

    // immediate operand
    //case reg_mapping::REG_IMM8 :
    //case reg_mapping::REG_IMM_BASE:
    //case reg_mapping::REG_IMM:
    //case reg_mapping::REG_IMM32:
    //case reg_mapping::REG_IMM_LAST:

    // memory operand
    //case reg_mapping::REG_MEM:
    //case reg_mapping::REG_MEM_BASE:
    //case reg_mapping::REG_MEM_OFF8:
    //case reg_mapping::REG_MEM_OFF32:
    //case reg_mapping::REG_MEM_LAST:

    // memory-address offset operand
    //case reg_mapping::REG_OFF8:
    //case reg_mapping::REG_OFF_BASE:
    //case reg_mapping::REG_OFF:
    //case reg_mapping::REG_OFF32:
    //case reg_mapping::REG_OFF_LAST:

    //case reg_mapping::REG_MODX:

    // base for all kinds of registers (application: machine: pin)
    //case reg_mapping::REG_RBASE: 

    // Machine registers are individual real registers on the machine
    //case reg_mapping::REG_MACHINE_BASE:

    // Application registers are registers used in the application binary
    // Application registers include all machine registers. In addition:
    // they include some aggregrate registers that can be accessed by
    // the application in a single instruction
    // Essentially: application registers = individual machine registers + aggregrate registers
    
    //case reg_mapping::REG_APPLICATION_BASE: 

    /* !@ todo: should save scratch mmx and fp registers */
    // The machine registers that form a context. These are the registers
    // that need to be saved in a context switch.
    //case reg_mapping::REG_PHYSICAL_CONTEXT_BEGIN:
    
    //case reg_mapping::REG_GR_BASE:
/*#if defined(TARGET_IA32E)
    // Context registers in the Intel(R) 64 architecture
    case reg_mapping::REG_RDI = case reg_mapping::REG_GR_BASE:  ///< rdi
    case reg_mapping::REG_GDI = case reg_mapping::REG_RDI:      ///< edi on a 32 bit machine: rdi on 64
    case reg_mapping::REG_RSI:                ///< rsi
    case reg_mapping::REG_GSI = case reg_mapping::REG_RSI:      ///< esi on a 32 bit machine: rsi on 64
    case reg_mapping::REG_RBP:                ///< rbp
    case reg_mapping::REG_GBP = case reg_mapping::REG_RBP:      ///< ebp on a 32 bit machine: rbp on 64
    case reg_mapping::REG_RSP:                ///< rsp
    case reg_mapping::REG_STACK_PTR = case reg_mapping::REG_RSP:///< esp on a 32 bit machine: rsp on 64
    case reg_mapping::REG_RBX:                ///< rbx
    case reg_mapping::REG_GBX = case reg_mapping::REG_RBX:      ///< ebx on a 32 bit machine: rbx on 64
    case reg_mapping::REG_RDX:                ///< rdx
    case reg_mapping::REG_GDX = case reg_mapping::REG_RDX:      ///< edx on a 32 bit machine: rdx on 64
    case reg_mapping::REG_RCX:                ///< rcx
    case reg_mapping::REG_GCX = case reg_mapping::REG_RCX:      ///< ecx on a 32 bit machine: rcx on 64
    case reg_mapping::REG_RAX:                ///< rax
    case reg_mapping::REG_GAX = case reg_mapping::REG_RAX:      ///< eax on a 32 bit machine: rax on 64
    case reg_mapping::REG_R8:
    case reg_mapping::REG_R9:
    case reg_mapping::REG_R10:
    case reg_mapping::REG_R11:
    case reg_mapping::REG_R12:
    case reg_mapping::REG_R13:
    case reg_mapping::REG_R14:
    case reg_mapping::REG_R15:
    case reg_mapping::REG_GR_LAST = case reg_mapping::REG_R15:

    case reg_mapping::REG_SEG_BASE:
    case reg_mapping::REG_SEG_CS = case reg_mapping::REG_SEG_BASE:
    case reg_mapping::REG_SEG_SS:
    case reg_mapping::REG_SEG_DS:
    case reg_mapping::REG_SEG_ES:
    case reg_mapping::REG_SEG_FS:
    case reg_mapping::REG_SEG_GS:
    case reg_mapping::REG_SEG_LAST = case reg_mapping::REG_SEG_GS:

    case reg_mapping::REG_RFLAGS:
    case reg_mapping::REG_GFLAGS=case reg_mapping::REG_RFLAGS:
    case reg_mapping::REG_RIP:
    case reg_mapping::REG_INST_PTR = case reg_mapping::REG_RIP:
#else*/
    // Context registers in the IA-32 architecture
    case reg_mapping::REG_EDI:  return string("R_EDI");
    //case reg_mapping::REG_GDI:
    //case reg_mapping::REG_EDI:
    case reg_mapping::REG_ESI:  return string("R_ESI");
    //case reg_mapping::REG_GSI:
    //case reg_mapping::REG_ESI:
    case reg_mapping::REG_EBP:  return string("R_EBP");
    //case reg_mapping::REG_GBP:
    //case reg_mapping::REG_EBP:
    case reg_mapping::REG_ESP:  return string("R_ESP");
    //case reg_mapping::REG_STACK_PTR:
    //case reg_mapping::REG_ESP:
    case reg_mapping::REG_EBX:  return string("R_EBX");
    //case reg_mapping::REG_GBX:
    //case reg_mapping::REG_EBX:
    case reg_mapping::REG_EDX:  return string("R_EDX");
    //case reg_mapping::REG_GDX:
    //case reg_mapping::REG_EDX:
    case reg_mapping::REG_ECX:  return string("R_ECX");
    //case reg_mapping::REG_GCX:
    //case reg_mapping::REG_ECX:
    case reg_mapping::REG_EAX: return string("R_EAX");
    //case reg_mapping::REG_GAX: 
    //case reg_mapping::REG_EAX:
    //case reg_mapping::REG_GR_LAST:
    //case reg_mapping::REG_EAX:
    
    //case reg_mapping::REG_SEG_BASE:
    //case reg_mapping::REG_SEG_CS:
    //case reg_mapping::REG_SEG_BASE:
    case reg_mapping::REG_SEG_SS:  return string("SS");
    //case reg_mapping::REG_SEG_DS:
    //case reg_mapping::REG_SEG_ES:
    //case reg_mapping::REG_SEG_FS:
    //case reg_mapping::REG_SEG_GS:
    //case reg_mapping::REG_SEG_LAST:
    //case reg_mapping::REG_SEG_GS:

    case reg_mapping::REG_EFLAGS:  return string("EFLAGS");
    //case reg_mapping::REG_GFLAGS:
    //case reg_mapping::REG_EFLAGS:
    //case reg_mapping::REG_EIP:
    case reg_mapping::REG_INST_PTR:  return string("R_EIP");
    //case reg_mapping::REG_EIP:
//#endif
    
    //case reg_mapping::REG_PHYSICAL_CONTEXT_END:
    //case reg_mapping::REG_INST_PTR:

    // partial registers common to both the IA-32 and Intel(R) 64 architectures.
    case reg_mapping::REG_AL:  return string("R_AL");
    case reg_mapping::REG_AH:  return string("R_AH");
    case reg_mapping::REG_AX:  return string("R_AX");
    
    case reg_mapping::REG_CL:  return string("R_CL");
    case reg_mapping::REG_CH:  return string("R_CH");
    case reg_mapping::REG_CX:  return string("R_CX");
    
    case reg_mapping::REG_DL:  return string("R_DL");
    case reg_mapping::REG_DH:  return string("R_DH");
    case reg_mapping::REG_DX:  return string("R_DX");
    
    case reg_mapping::REG_BL:  return string("R_BL");
    case reg_mapping::REG_BH:  
    case reg_mapping::REG_BX:  return string("R_BX");

    case reg_mapping::REG_BP:  return string("R_BP");
    case reg_mapping::REG_SI:  return string("R_SI");
    case reg_mapping::REG_DI:  return string("R_DI");

    //case reg_mapping::REG_SP:
    //case reg_mapping::REG_FLAGS:
    //case reg_mapping::REG_IP:
    
/*#if defined(TARGET_IA32E)
    // partial registers in the Intel(R) 64 architecture
    case reg_mapping::REG_EDI:
    case reg_mapping::REG_DIL:
    case reg_mapping::REG_ESI:
    case reg_mapping::REG_SIL:
    case reg_mapping::REG_EBP:
    case reg_mapping::REG_BPL:
    case reg_mapping::REG_ESP:
    case reg_mapping::REG_SPL:
    case reg_mapping::REG_EBX:
    case reg_mapping::REG_EDX:
    case reg_mapping::REG_ECX:
    case reg_mapping::REG_EAX:
    case reg_mapping::REG_EFLAGS:
    case reg_mapping::REG_EIP:

    case reg_mapping::REG_R8B:
    case reg_mapping::REG_R8W:
    case reg_mapping::REG_R8D:
    case reg_mapping::REG_R9B:
    case reg_mapping::REG_R9W:
    case reg_mapping::REG_R9D:
    case reg_mapping::REG_R10B:
    case reg_mapping::REG_R10W:
    case reg_mapping::REG_R10D:
    case reg_mapping::REG_R11B:
    case reg_mapping::REG_R11W:
    case reg_mapping::REG_R11D:
    case reg_mapping::REG_R12B:
    case reg_mapping::REG_R12W:
    case reg_mapping::REG_R12D:    
    case reg_mapping::REG_R13B:
    case reg_mapping::REG_R13W:
    case reg_mapping::REG_R13D:
    case reg_mapping::REG_R14B:
    case reg_mapping::REG_R14W:
    case reg_mapping::REG_R14D:
    case reg_mapping::REG_R15B:
    case reg_mapping::REG_R15W:
    case reg_mapping::REG_R15D:    
#endif*/
    
/*
    case reg_mapping::REG_MM_BASE:
    case reg_mapping::REG_MM0 = case reg_mapping::REG_MM_BASE:
    case reg_mapping::REG_MM1:
    case reg_mapping::REG_MM2:
    case reg_mapping::REG_MM3:
    case reg_mapping::REG_MM4:
    case reg_mapping::REG_MM5:
    case reg_mapping::REG_MM6:
    case reg_mapping::REG_MM7:
    case reg_mapping::REG_MM_LAST = case reg_mapping::REG_MM7:
    
    case reg_mapping::REG_EMM_BASE:
    case reg_mapping::REG_EMM0 = case reg_mapping::REG_EMM_BASE:
    case reg_mapping::REG_EMM1:
    case reg_mapping::REG_EMM2:
    case reg_mapping::REG_EMM3:
    case reg_mapping::REG_EMM4:
    case reg_mapping::REG_EMM5:
    case reg_mapping::REG_EMM6:
    case reg_mapping::REG_EMM7:
    case reg_mapping::REG_EMM_LAST = case reg_mapping::REG_EMM7:

    case reg_mapping::REG_MXT:
    
    case reg_mapping::REG_XMM_BASE:
*/
    // case reg_mapping::REG_XMM0 = case reg_mapping::REG_XMM_BASE:
    case reg_mapping::REG_XMM0:
      return string("R_XMM0");
    case reg_mapping::REG_XMM1:
      return string("R_XMM1");
    case reg_mapping::REG_XMM2:
      return string("R_XMM2");
    case reg_mapping::REG_XMM3:
      return string("R_XMM3");
    case reg_mapping::REG_XMM4:
      return string("R_XMM4");
    case reg_mapping::REG_XMM5:
      return string("R_XMM5");
    case reg_mapping::REG_XMM6:
      return string("R_XMM6");
    case reg_mapping::REG_XMM7:
      return string("R_XMM7");
    
      //#if defined(TARGET_IA32E)
    // additional xmm registers in the Intel(R) 64 architecture
    // case reg_mapping::REG_XMM8:
    //   return string("XMM8");
    // case reg_mapping::REG_XMM9:
    //   return string("XMM9");
    // case reg_mapping::REG_XMM10:
    //   return string("XMM10");
    // case reg_mapping::REG_XMM11:
    //   return string("XMM11");
    // case reg_mapping::REG_XMM12:
    //   return string("XMM12");
    // case reg_mapping::REG_XMM13:
    //   return string("XMM13");
    // case reg_mapping::REG_XMM14:
    //   return string("XMM14");
    // case reg_mapping::REG_XMM15:
    //   return string("XMM15");
//     case reg_mapping::REG_XMM_LAST = case reg_mapping::REG_XMM15:
// #else    
//     case reg_mapping::REG_XMM_LAST = case reg_mapping::REG_XMM7:
// #endif
      /*
    case reg_mapping::REG_YMM_BASE:
    case reg_mapping::REG_YMM0 = case reg_mapping::REG_YMM_BASE:
    case reg_mapping::REG_YMM1:
    case reg_mapping::REG_YMM2:
    case reg_mapping::REG_YMM3:
    case reg_mapping::REG_YMM4:
    case reg_mapping::REG_YMM5:
    case reg_mapping::REG_YMM6:
    case reg_mapping::REG_YMM7:
    
#if defined(TARGET_IA32E)
    // additional ymm registers in the Intel(R) 64 architecture
    case reg_mapping::REG_YMM8:
    case reg_mapping::REG_YMM9:
    case reg_mapping::REG_YMM10:
    case reg_mapping::REG_YMM11:
    case reg_mapping::REG_YMM12:
    case reg_mapping::REG_YMM13:
    case reg_mapping::REG_YMM14:
    case reg_mapping::REG_YMM15:
    case reg_mapping::REG_YMM_LAST = case reg_mapping::REG_YMM15:
#else    
    case reg_mapping::REG_YMM_LAST = case reg_mapping::REG_YMM7:
#endif
      */    
 case reg_mapping::REG_MXCSR:
     return string("R_MXCSR");
        /*
    case reg_mapping::REG_DR_BASE:
    case reg_mapping::REG_DR0 = case reg_mapping::REG_DR_BASE:
    case reg_mapping::REG_DR1:
    case reg_mapping::REG_DR2:
    case reg_mapping::REG_DR3:
    case reg_mapping::REG_DR4:
    case reg_mapping::REG_DR5:
    case reg_mapping::REG_DR6:
    case reg_mapping::REG_DR7:
    case reg_mapping::REG_DR_LAST = case reg_mapping::REG_DR7:

    case reg_mapping::REG_CR_BASE:
    case reg_mapping::REG_CR0 = case reg_mapping::REG_CR_BASE:
    case reg_mapping::REG_CR1:
    case reg_mapping::REG_CR2:
    case reg_mapping::REG_CR3:
    case reg_mapping::REG_CR4:
    case reg_mapping::REG_CR_LAST = case reg_mapping::REG_CR4:
    
    case reg_mapping::REG_TSSR:
    
    case reg_mapping::REG_LDTR:
*/
/*
 --- Not clear if following are needed
    case reg_mapping::REG_ESR_BASE:
    case reg_mapping::REG_ESR_LIMIT:
    
    case reg_mapping::REG_CSR_BASE:
    case reg_mapping::REG_CSR_LIMIT:
    
    case reg_mapping::REG_SSR_BASE:
    case reg_mapping::REG_SSR_LIMIT:
    
    case reg_mapping::REG_DSR_BASE:
    case reg_mapping::REG_DSR_LIMIT:
    
    case reg_mapping::REG_FSR_BASE:
    case reg_mapping::REG_FSR_LIMIT:
    
    case reg_mapping::REG_GSR_BASE:
    case reg_mapping::REG_GSR_LIMIT:
    
    case reg_mapping::REG_TSSR_BASE:
    case reg_mapping::REG_TSSR_LIMIT:
    
    case reg_mapping::REG_LDTR_BASE:
    case reg_mapping::REG_LDTR_LIMIT:
    
    case reg_mapping::REG_GDTR_BASE:
    case reg_mapping::REG_GDTR_LIMIT:
    
    case reg_mapping::REG_IDTR_BASE:
    case reg_mapping::REG_IDTR_LIMIT:
*/
/*
    case reg_mapping::REG_TR_BASE:
    case reg_mapping::REG_TR = case reg_mapping::REG_TR_BASE:
    case reg_mapping::REG_TR3:
    case reg_mapping::REG_TR4:
    case reg_mapping::REG_TR5:
    case reg_mapping::REG_TR6:
    case reg_mapping::REG_TR7:
    case reg_mapping::REG_TR_LAST = case reg_mapping::REG_TR7:
    
    case reg_mapping::REG_FPST_BASE:
    case reg_mapping::REG_FP_BASE = case reg_mapping::REG_FPST_BASE:
    case reg_mapping::REG_FPCW = case reg_mapping::REG_FP_BASE:
    case reg_mapping::REG_FPSW:
    case reg_mapping::REG_FPTAG:
    case reg_mapping::REG_FPIP_OFF:
    case reg_mapping::REG_FPIP_SEL:
    case reg_mapping::REG_FPOPCODE:
    case reg_mapping::REG_FPDP_OFF:
    case reg_mapping::REG_FPDP_SEL:
    case reg_mapping::REG_FP_LAST = case reg_mapping::REG_FPDP_SEL:
    
    case reg_mapping::REG_ST_BASE:
    case reg_mapping::REG_ST0 = case reg_mapping::REG_ST_BASE:
    case reg_mapping::REG_ST1:
    case reg_mapping::REG_ST2:
    case reg_mapping::REG_ST3:
    case reg_mapping::REG_ST4:
    case reg_mapping::REG_ST5:
    case reg_mapping::REG_ST6:
    case reg_mapping::REG_ST7:
#if !defined(TARGET_DOXYGEN)
    case reg_mapping::REG_ST_LAST = case reg_mapping::REG_ST7:
    case reg_mapping::REG_FPST_LAST = case reg_mapping::REG_ST_LAST:
    case reg_mapping::REG_MACHINE_LAST = case reg_mapping::REG_FPST_LAST:
    
    case reg_mapping::REG_STATUS_FLAGS:
*/
    case reg_mapping::REG_DF_FLAG: return string("R_DFLAG");
/*    
    case reg_mapping::REG_AGGcase REGATE_BASE:
    case reg_mapping::REG_FPST_ALL = case reg_mapping::REG_AGGcase REGATE_BASE:
    case reg_mapping::REG_AGGcase REGATE_LAST = case reg_mapping::REG_FPST_ALL:

    case reg_mapping::REG_APPLICATION_LAST = case reg_mapping::REG_AGGcase REGATE_LAST: 
    
    case reg_mapping::REG_PIN_BASE:
    case reg_mapping::REG_PIN_GR_BASE = case reg_mapping::REG_PIN_BASE:

    // ia32-specific Pin gr regs
    case reg_mapping::REG_PIN_EDI = case reg_mapping::REG_PIN_GR_BASE:
#if defined(TARGET_IA32)    
    case reg_mapping::REG_PIN_GDI = case reg_mapping::REG_PIN_EDI:                  // PIN_GDI == PIN_EDI on 32 bit: PIN_RDI on 64 bit.
#endif
    case reg_mapping::REG_PIN_ESI:
    case reg_mapping::REG_PIN_EBP:
    case reg_mapping::REG_PIN_ESP:
#if defined (TARGET_IA32)
    case reg_mapping::REG_PIN_STACK_PTR = case reg_mapping::REG_PIN_ESP:
#endif    
    case reg_mapping::REG_PIN_EBX:
    case reg_mapping::REG_PIN_EDX:
#if defined(TARGET_IA32)    
    case reg_mapping::REG_PIN_GDX = case reg_mapping::REG_PIN_EDX:                  
#endif
    case reg_mapping::REG_PIN_ECX:
#if defined(TARGET_IA32)    
    case reg_mapping::REG_PIN_GCX = case reg_mapping::REG_PIN_ECX:                  // PIN_GCX == PIN_ECX on 32 bit: PIN_RCX on 64 bit.
#endif
    case reg_mapping::REG_PIN_EAX:
#if defined(TARGET_IA32)    
    case reg_mapping::REG_PIN_GAX = case reg_mapping::REG_PIN_EAX:                  // PIN_GAX == PIN_EAX on 32 bit: PIN_RAX on 64 bit.
#endif
    case reg_mapping::REG_PIN_AL:
    case reg_mapping::REG_PIN_AH:
    case reg_mapping::REG_PIN_AX:
    case reg_mapping::REG_PIN_CL:
    case reg_mapping::REG_PIN_CH:
    case reg_mapping::REG_PIN_CX:
    case reg_mapping::REG_PIN_DL:
    case reg_mapping::REG_PIN_DH:
    case reg_mapping::REG_PIN_DX:
    case reg_mapping::REG_PIN_BL:
    case reg_mapping::REG_PIN_BH:
    case reg_mapping::REG_PIN_BX:
    case reg_mapping::REG_PIN_BP:
    case reg_mapping::REG_PIN_SI:
    case reg_mapping::REG_PIN_DI:
    case reg_mapping::REG_PIN_SP:

#if defined(TARGET_IA32E)
    // Intel(R) 64 architecture specific pin gr regs
    case reg_mapping::REG_PIN_RDI:
    case reg_mapping::REG_PIN_GDI = case reg_mapping::REG_PIN_RDI:
    case reg_mapping::REG_PIN_RSI:
    case reg_mapping::REG_PIN_RBP:
    case reg_mapping::REG_PIN_RSP:
    
    case reg_mapping::REG_PIN_STACK_PTR = case reg_mapping::REG_PIN_RSP:
    
    case reg_mapping::REG_PIN_RBX:
    case reg_mapping::REG_PIN_RDX:
    case reg_mapping::REG_PIN_GDX = case reg_mapping::REG_PIN_RDX:
    case reg_mapping::REG_PIN_RCX:
    case reg_mapping::REG_PIN_GCX = case reg_mapping::REG_PIN_RCX:
    case reg_mapping::REG_PIN_RAX:
    case reg_mapping::REG_PIN_GAX = case reg_mapping::REG_PIN_RAX:
    case reg_mapping::REG_PIN_R8:
    case reg_mapping::REG_PIN_R9:
    case reg_mapping::REG_PIN_R10:
    case reg_mapping::REG_PIN_R11:
    case reg_mapping::REG_PIN_R12:
    case reg_mapping::REG_PIN_R13:
    case reg_mapping::REG_PIN_R14:
    case reg_mapping::REG_PIN_R15:

    case reg_mapping::REG_PIN_DIL:
    case reg_mapping::REG_PIN_SIL:
    case reg_mapping::REG_PIN_BPL:
    case reg_mapping::REG_PIN_SPL:
    
    case reg_mapping::REG_PIN_R8B:
    case reg_mapping::REG_PIN_R8W:
    case reg_mapping::REG_PIN_R8D:

    case reg_mapping::REG_PIN_R9B:
    case reg_mapping::REG_PIN_R9W:
    case reg_mapping::REG_PIN_R9D:

    case reg_mapping::REG_PIN_R10B:
    case reg_mapping::REG_PIN_R10W:
    case reg_mapping::REG_PIN_R10D:

    case reg_mapping::REG_PIN_R11B:
    case reg_mapping::REG_PIN_R11W:
    case reg_mapping::REG_PIN_R11D:

    case reg_mapping::REG_PIN_R12B:
    case reg_mapping::REG_PIN_R12W:
    case reg_mapping::REG_PIN_R12D:

    case reg_mapping::REG_PIN_R13B:
    case reg_mapping::REG_PIN_R13W:
    case reg_mapping::REG_PIN_R13D:

    case reg_mapping::REG_PIN_R14B:
    case reg_mapping::REG_PIN_R14W:
    case reg_mapping::REG_PIN_R14D:

    case reg_mapping::REG_PIN_R15B:
    case reg_mapping::REG_PIN_R15W:
    case reg_mapping::REG_PIN_R15D:
#endif

    // Every thread is assigned an index so we can implement tls
    case reg_mapping::REG_THREAD_ID:
    
    case reg_mapping::REG_SEG_GS_VAL:  // virtual reg holding actual value of gs
    case reg_mapping::REG_SEG_FS_VAL:  // virtual reg holding actual value of fs

    // ISA-independent gr regs
    case reg_mapping::REG_PIN_INDIRcase REG:  // virtual reg holding indirect jmp target value
    case reg_mapping::REG_PIN_IPRELADDR: // virtual reg holding ip-rel address value
    case reg_mapping::REG_PIN_SYSENTER_RESUMEADDR: // virtual reg holding the resume address from sysenter
    
    // ISA-independent gr regs holding temporary values
    case reg_mapping::REG_PIN_T_BASE:
    case reg_mapping::REG_PIN_T0 = case reg_mapping::REG_PIN_T_BASE:        
    case reg_mapping::REG_PIN_T1:        
    case reg_mapping::REG_PIN_T2:
    case reg_mapping::REG_PIN_T3:
    case reg_mapping::REG_PIN_T0L:    // lower 8 bits of temporary register
    case reg_mapping::REG_PIN_T1L:
    case reg_mapping::REG_PIN_T2L:
    case reg_mapping::REG_PIN_T3L:
    case reg_mapping::REG_PIN_T0W:    // lower 16 bits of temporary register
    case reg_mapping::REG_PIN_T1W:
    case reg_mapping::REG_PIN_T2W:
    case reg_mapping::REG_PIN_T3W:
    case reg_mapping::REG_PIN_T0D:    // lower 32 bits of temporary register
    case reg_mapping::REG_PIN_T1D:
    case reg_mapping::REG_PIN_T2D:
    case reg_mapping::REG_PIN_T3D:
    case reg_mapping::REG_PIN_T_LAST = case reg_mapping::REG_PIN_T3D:

#endif

    // Virtual registers reg holding memory addresses pointed by GS/FS registers
    // These registers are visible for tool writers
    */
    case reg_mapping::REG_SEG_GS_BASE: return string("R_GS_BASE"); ///< Base address for GS segment
    case reg_mapping::REG_SEG_FS_BASE: return string("R_FS_BASE");///< Base address for FS segment
      /*
    // ISA-independent Pin virtual regs needed for instrumentation
    // These are pin registers visible to the pintool writers.
    case reg_mapping::REG_INST_BASE:
    case reg_mapping::REG_INST_SCRATCH_BASE = case reg_mapping::REG_INST_BASE:  ///< First available scratch register
    case reg_mapping::REG_INST_G0 = case reg_mapping::REG_INST_SCRATCH_BASE:    ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G1:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G2:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G3:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G4:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G5:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G6:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G7:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G8:                            ///< Scratch register used in pintools
    case reg_mapping::REG_INST_G9:                            ///< Scratch register used in pintools

    case reg_mapping::REG_INST_TOOL_FIRST = case reg_mapping::REG_INST_G0:     
    case reg_mapping::REG_INST_TOOL_LAST = case reg_mapping::REG_INST_G9:

    case reg_mapping::REG_BUF_BASE0:
    case reg_mapping::REG_BUF_BASE1:
    case reg_mapping::REG_BUF_BASE2:
    case reg_mapping::REG_BUF_BASE3:
    case reg_mapping::REG_BUF_BASE4:
    case reg_mapping::REG_BUF_BASE5:
    case reg_mapping::REG_BUF_BASE6:
    case reg_mapping::REG_BUF_BASE7:
    case reg_mapping::REG_BUF_BASE8:
    case reg_mapping::REG_BUF_BASE9:
    case reg_mapping::REG_BUF_LAST = case reg_mapping::REG_BUF_BASE9:

    case reg_mapping::REG_BUF_END0:
    case reg_mapping::REG_BUF_END1:
    case reg_mapping::REG_BUF_END2:
    case reg_mapping::REG_BUF_END3:
    case reg_mapping::REG_BUF_END4:
    case reg_mapping::REG_BUF_END5:
    case reg_mapping::REG_BUF_END6:
    case reg_mapping::REG_BUF_END7:
    case reg_mapping::REG_BUF_END8:
    case reg_mapping::REG_BUF_END9:
    case reg_mapping::REG_BUF_ENDLAST = case reg_mapping::REG_BUF_END9:

    case reg_mapping::REG_INST_SCRATCH_LAST = case reg_mapping::REG_BUF_ENDLAST:

#if !defined(TARGET_DOXYGEN)
    case reg_mapping::REG_INST_COND:     // for conditional instrumentation.
    case reg_mapping::REG_INST_LAST = case reg_mapping::REG_INST_COND:
    
    // Used for memory rewriting: these are not live outside the region
    // but cannot use general purpose scratch registers: because they're
    // used during instrumentation generation: rather than region generation.
    case reg_mapping::REG_INST_T0:
    case reg_mapping::REG_INST_T0L:  
    case reg_mapping::REG_INST_T0W: 
    case reg_mapping::REG_INST_T0D:
    case reg_mapping::REG_INST_T1:
    case reg_mapping::REG_INST_T2:
    case reg_mapping::REG_INST_T3:

    // Used to preserve the predicate value around repped string ops
    case reg_mapping::REG_INST_PRESERVED_PREDICATE:

    // Used when the AC flag needs to be cleared before analysis routine
    case reg_mapping::REG_FLAGS_BEFORE_AC_CLEARING:
    
    // Virtual regs used by Pin inside instrumentation bridges.
    // Unlike case reg_mapping::REG_INST_BASE to case reg_mapping::REG_INST_LAST: these registers are
    // NOT visible to  Pin clients.
    case reg_mapping::REG_PIN_BRIDGE_ORIG_SP:    // hold the stack ptr value before the bridge
    case reg_mapping::REG_PIN_BRIDGE_APP_IP: // hold the application (not code cache) IP to resume
    case reg_mapping::REG_PIN_BRIDGE_SP_BEFORE_ALIGN: // hold the stack ptr value before the stack alignment
    case reg_mapping::REG_PIN_BRIDGE_MARSHALLING_FRAME: // hold the address of the marshalled reference registers
    case reg_mapping::REG_PIN_BRIDGE_CONTEXT_FRAME: // hold the address of the context frame
    case reg_mapping::REG_PIN_BRIDGE_CONTEXT_ORIG_SP: // hold the sp at which the context was pushed

    case reg_mapping::REG_PIN_SPILLPTR:  // ptr to the pin spill area
    case reg_mapping::REG_PIN_GR_LAST = case reg_mapping::REG_PIN_SPILLPTR:

    // case reg_mapping::REG_PIN_FLAGS is x86-specific: but since it is not a gr: we put it out of
    // case reg_mapping::REG_PIN_GR_BASE and case reg_mapping::REG_PIN_GR_LAST

    case reg_mapping::REG_PIN_STATUS_FLAGS:
    case reg_mapping::REG_PIN_DF_FLAG:

    case reg_mapping::REG_PIN_FLAGS:

    case reg_mapping::REG_PIN_XMM_BASE:
    case reg_mapping::REG_PIN_XMM0 = case reg_mapping::REG_PIN_XMM_BASE:
    case reg_mapping::REG_PIN_XMM1:
    case reg_mapping::REG_PIN_XMM2:
    case reg_mapping::REG_PIN_XMM3:
    case reg_mapping::REG_PIN_XMM4:
    case reg_mapping::REG_PIN_XMM5:
    case reg_mapping::REG_PIN_XMM6:
    case reg_mapping::REG_PIN_XMM7:
    case reg_mapping::REG_PIN_XMM8:
    case reg_mapping::REG_PIN_XMM9:
    case reg_mapping::REG_PIN_XMM10:
    case reg_mapping::REG_PIN_XMM11:
    case reg_mapping::REG_PIN_XMM12:
    case reg_mapping::REG_PIN_XMM13:
    case reg_mapping::REG_PIN_XMM14:
    case reg_mapping::REG_PIN_XMM15:

    case reg_mapping::REG_PIN_YMM_BASE:
    case reg_mapping::REG_PIN_YMM0 = case reg_mapping::REG_PIN_YMM_BASE:
    case reg_mapping::REG_PIN_YMM1:
    case reg_mapping::REG_PIN_YMM2:
    case reg_mapping::REG_PIN_YMM3:
    case reg_mapping::REG_PIN_YMM4:
    case reg_mapping::REG_PIN_YMM5:
    case reg_mapping::REG_PIN_YMM6:
    case reg_mapping::REG_PIN_YMM7:
    case reg_mapping::REG_PIN_YMM8:
    case reg_mapping::REG_PIN_YMM9:
    case reg_mapping::REG_PIN_YMM10:
    case reg_mapping::REG_PIN_YMM11:
    case reg_mapping::REG_PIN_YMM12:
    case reg_mapping::REG_PIN_YMM13:
    case reg_mapping::REG_PIN_YMM14:
    case reg_mapping::REG_PIN_YMM15:
    case reg_mapping::REG_PIN_LAST = case reg_mapping::REG_PIN_YMM15:
#endif
    case reg_mapping::REG_LAST
*/
default: return "Unknown";

} 


}
