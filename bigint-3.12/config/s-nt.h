/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: s-nt.h 4933 2002-06-18 13:01:53Z xleroy $ */

/* Operating system dependencies, Intel x86 processors, Windows NT */

#define OCAML_OS_TYPE "Win32"

#undef BSD_SIGNALS
#define HAS_STRERROR
#define HAS_SOCKETS
#define HAS_GETCWD
#define HAS_UTIME
#define HAS_DUP2
#define HAS_GETHOSTNAME
#define HAS_MKTIME
#define HAS_PUTENV
#define HAS_LOCALE
