/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2003 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: bng_alpha.c 9547 2010-01-22 12:48:24Z doligez $ */

/* Code specific to the Alpha architecture. */

#define BngMult(resh,resl,arg1,arg2)                                        \
  asm("mulq %2, %3, %0 \n\t"                                                \
      "umulh %2, %3, %1"                                                    \
      : "=&r" (resl), "=r" (resh)                                           \
      : "r" (arg1), "r" (arg2))
