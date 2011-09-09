
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-arm/gdefs.h) is                        ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Only to be used within the guest-arm directory. */

#ifndef __LIBVEX_GUEST_ARM_DEFS_H
#define __LIBVEX_GUEST_ARM_DEFS_H

#include "guest-generic/bb_to_IR.h"

/*---------------------------------------------------------*/
/*--- arm to IR conversion                              ---*/
/*---------------------------------------------------------*/

/* Convert one ARM insn to IR.  See the type DisOneInstrFn in
   bb_to_IR.h. */
extern
DisResult disInstr_ARM ( IRSB*        irbb,
			 Bool         put_IP,
                         Bool         (*resteerOkFn) ( void*, Addr64 ),
                         void*        callback_opaque,
                         UChar*       guest_code,
                         Long         delta,
                         Addr64       guest_IP,
                         VexArch      guest_arch,
                         VexArchInfo* archinfo,
                         VexAbiInfo*  abiinfo,
                         Bool         host_bigendian );
/* is this needed?
extern
IRSB* bbToIR_ARM ( UChar*           armCode, 
                   Addr64           eip, 
                   VexGuestExtents* vge,
                   Bool             (*byte_accessible)(Addr64),
                   Bool             (*resteerOkFn)(Addr64),
                   Bool             host_bigendian,
                   VexArchInfo*     archinfo_guest );
*/
/* Used by the optimiser to specialise calls to helpers. */
extern
IRExpr* guest_arm_spechelper ( HChar* function_name,
                               IRExpr** args );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern 
Bool guest_arm_state_requires_precise_mem_exns ( Int, Int );

extern
VexGuestLayout armGuest_layout;


/*---------------------------------------------------------*/
/*--- arm guest helpers                                 ---*/
/*---------------------------------------------------------*/

/* --- CLEAN HELPERS --- */

extern UInt  armg_calculate_flags_all ( 
                UInt cc_op, UInt cc_dep1, UInt cc_dep2 
             );
extern UInt  armg_calculate_flags_c ( 
                UInt cc_op, UInt cc_dep1, UInt cc_dep2 
             );

extern UInt  armg_calculate_condition ( 
                UInt/*ARMCondcode*/ cond, 
                UInt cc_op, 
                UInt cc_dep1, UInt cc_dep2 
             );


/*---------------------------------------------------------*/
/*--- Condition code stuff                              ---*/
/*---------------------------------------------------------*/

/* Flags masks.  Defines positions of flags bits in the CPSR. */
#define ARMG_CC_SHIFT_N  31
#define ARMG_CC_SHIFT_Z  30
#define ARMG_CC_SHIFT_C  29
#define ARMG_CC_SHIFT_V  28

#define ARMG_CC_MASK_N    (1 << ARMG_CC_SHIFT_N)
#define ARMG_CC_MASK_Z    (1 << ARMG_CC_SHIFT_Z)
#define ARMG_CC_MASK_V    (1 << ARMG_CC_SHIFT_V)
#define ARMG_CC_MASK_C    (1 << ARMG_CC_SHIFT_C)

/* Flag thunk descriptors.
   
   Unlike on x86, we don't defer computation of condition codes. The reason
   we don't need to, is that while x86 updates the condition codes on almost
   every instruction, ARM instructions have bit which indicates whether
   CC updates are desired or not. GCC only sets the bit when the CCs would
   be used subsequently, so we mostly only compute them when we would need
   them anyways.

   We use this enum only for the call to setFlags_DEP1_DEP2, so that we can
   generate the flag setting code in one place.
*/

enum {
   ARMG_CC_OP_COPY,    /* DEP1 = current flags, DEP2 = 0 */
                       /* just copy DEP1 to output */

   ARMG_CC_OP_LOGIC,   /* DEP1 = result, DEP2 = shifter_carry_out */
   
   ARMG_CC_OP_SUB,     /* DEP1 = arg1(Rn), DEP2 = arg2 (shifter_op) */
   ARMG_CC_OP_ADD,     /* DEP1 = arg1(Rn), DEP2 = arg2 (shifter_op) */
   
   ARMG_CC_OP_NUMBER
};

/* Defines conditions which we can ask for (ARM sec A3.2) */

typedef
   enum {
      ARMCondEQ     = 0,  /* equal                               : Z=1 */
      ARMCondNE     = 1,  /* not equal                           : Z=0 */

      ARMCondHS     = 2,  /* >=u (higher or same)                : C=1 */
      ARMCondLO     = 3,  /* <u  (lower)                         : C=0 */

      ARMCondMI     = 4,  /* minus (negative)                    : N=1 */
      ARMCondPL     = 5,  /* plus (zero or +ve)                  : N=0 */

      ARMCondVS     = 6,  /* overflow                            : V=1 */
      ARMCondVC     = 7,  /* no overflow                         : V=0 */

      ARMCondHI     = 8,  /* >u   (higher)                       : C=1 && Z=0 */
      ARMCondLS     = 9,  /* <=u  (lower or same)                : C=0 || Z=1 */

      ARMCondGE     = 10, /* >=s (signed greater or equal)       : N=V */
      ARMCondLT     = 11, /* <s  (signed less than)              : N!=V */

      ARMCondGT     = 12, /* >s  (signed greater)                : Z=0 && N=V */
      ARMCondLE     = 13, /* <=s (signed less or equal)          : Z=1 || N!=V */

      ARMCondAL     = 14, /* always (unconditional)              : */
      ARMCondNV     = 15  /* never (basically undefined meaning) : */
                          /* NB: ARM have deprecated the use of the NV condition code
                             - you are now supposed to use MOV R0,R0 as a noop
                               rather than MOVNV R0,R0 as was previously recommended.
                             Future processors may have the NV condition code reused to do other things.  */
   }
   ARMCondcode;

#endif /* ndef __LIBVEX_GUEST_ARM_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   guest-arm/gdefs.h ---*/
/*---------------------------------------------------------------*/
