/******************************************************************************
 * $Id$
 *
 * Project:  GRASS
 * Purpose:  Include file for GRASS modules that use the PROJ.4
 *           wrapper functions
 *
 ******************************************************************************
 * Copyright (c) 2000, GRASS Development Team
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *****************************************************************************/

#include "config.h"
#ifdef USE_PROJ
  #include "proj_api.h"
#else
  #include "gproj_api.h"
#endif

struct pj_info {
      projPJ     *pj;
      double meters;
      int    zone;
      char   proj[100];
};

#ifndef PROTO

#ifdef __STDC__
# define	PROTO(s) s
#else
# define PROTO(s) ()
#endif

#endif	/* PROTO */

/* do_proj.c */
int pj_do_proj PROTO((double *, double *, struct pj_info *, struct pj_info *));
int pj_do_transform PROTO((int, double *, double *, double *, 
                           struct pj_info *, struct pj_info *));
/* get_proj.c */
int pj_get_kv PROTO((struct pj_info *, struct Key_Value *, struct Key_Value *));
int pj_get_string PROTO((struct pj_info *, char *));
int pj_zero_proj PROTO((struct pj_info *));
const char * set_proj_lib PROTO((const char *));
