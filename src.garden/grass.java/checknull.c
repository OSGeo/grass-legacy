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
 * Checks provided arguments and throws IllegalArgument exception in case 
 * of NULL. Argument to be checked has to be followed by its name for 
 * to be used in an error message. Returns 1 on success and 0 on NULL.
 */
int checknull( JNIEnv *env, void *ptr, const char *argname)
{
    char msg[80];

    if (ptr != NULL) return 1;

    if (argname != NULL) sprintf( msg, "%s must not be null", argname);
    else strcpy( msg, "one of the arguments is null" );

    throw( env, "java/lang/IllegalArgumentException", msg );

    return 0;
}
