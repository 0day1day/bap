
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_globals.c) is                            ---*/
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

#include "libvex_basictypes.h"

#include "vex_util.h"
#include "vex_globals.h"


/* Global settings for the VEX library.  These are the
   only library-wide globals. */

/* Are we started yet? */
Bool vex_initdone = False;

/* failure exit function */
__attribute__ ((noreturn))
void (*vex_failure_exit) ( void ) = NULL;

/* logging output function */
void (*vex_log_bytes) ( HChar*, Int nbytes ) = NULL;

/* debug paranoia level */
Int vex_debuglevel = 0;

/* trace flags */
Int vex_traceflags = 0;

/* Are we supporting valgrind checking? */
Bool vex_valgrind_support = False;

/* Max # guest insns per bb */
VexControl vex_control = { 0,0,False,0,0,0 };



/*---------------------------------------------------------------*/
/*--- end                                       vex_globals.c ---*/
/*---------------------------------------------------------------*/

