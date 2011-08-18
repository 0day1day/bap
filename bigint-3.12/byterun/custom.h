/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Manuel Serrano and Xavier Leroy, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: custom.h 9547 2010-01-22 12:48:24Z doligez $ */

#ifndef CAML_CUSTOM_H
#define CAML_CUSTOM_H


#ifndef CAML_NAME_SPACE
#include "compatibility.h"
#endif
#include "mlvalues.h"

struct custom_operations {
  char *identifier;
  void (*finalize)(value v);
  int (*compare)(value v1, value v2);
  intnat (*hash)(value v);
  void (*serialize)(value v,
                    /*out*/ uintnat * wsize_32 /*size in bytes*/,
                    /*out*/ uintnat * wsize_64 /*size in bytes*/);
  uintnat (*deserialize)(void * dst);
};

#define custom_finalize_default NULL
#define custom_compare_default NULL
#define custom_hash_default NULL
#define custom_serialize_default NULL
#define custom_deserialize_default NULL

#define Custom_ops_val(v) (*((struct custom_operations **) (v)))

CAMLextern value caml_alloc_custom(struct custom_operations * ops,
                                   uintnat size, /*size in bytes*/
                                   mlsize_t mem, /*resources consumed*/
                                   mlsize_t max  /*max resources*/);

CAMLextern void caml_register_custom_operations(struct custom_operations * ops);

CAMLextern int caml_compare_unordered;
  /* Used by custom comparison to report unordered NaN-like cases. */

/* <private> */
extern struct custom_operations * caml_find_custom_operations(char * ident);
extern struct custom_operations *
          caml_final_custom_operations(void (*fn)(value));

extern void caml_init_custom_operations(void);
/* </private> */

#endif /* CAML_CUSTOM_H */
