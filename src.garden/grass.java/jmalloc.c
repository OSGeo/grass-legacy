/*
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
    USA
*/
/*
 * Copyright (C) 1998 The Information-technology Promotion Agency, Japan(IPA), LGPL
 * $Id$
 */
#include <stdlib.h>
#include "etc.h"

#include <gis.h>

/**
 * The purpose of this function is to throw OutOfMemory error
 * if such a condition occures in GRASS-JNI library.
 */
void *jmalloc( JNIEnv *env, size_t size ) 
{
  void *addr = malloc( size );

  if (addr == NULL) throw(env, "java/lang/OutOfMemoryError",
			       "in GRASS-JNI library");

  return addr;
}
