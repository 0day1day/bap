/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: gethostbyaddr.c 4771 2002-05-06 08:29:52Z xleroy $ */

#ifndef _REENTRANT
/* This helps detection on Digital Unix... */
#define _REENTRANT
#endif

#include <sys/types.h>
#include <netdb.h>

int main(int argc, char ** argv)
{
#if NUM_ARGS == 7
  char * address;
  int length;
  int type;
  struct hostent h;
  char buffer[10];
  int buflen;
  int h_errnop;
  struct hostent * hp;
  hp = gethostbyaddr_r(address, length, type, &h,
                       buffer, buflen, &h_errnop);
#elif NUM_ARGS == 8
  char * address;
  int length;
  int type;
  struct hostent h;
  char buffer[10];
  int buflen;
  int h_errnop;
  struct hostent * hp;
  int rc;
  rc = gethostbyaddr_r(address, length, type, &h,
                       buffer, buflen, &hp, &h_errnop);
#endif
  return 0;
}
