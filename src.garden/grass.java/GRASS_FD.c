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
#include <stdio.h>
#include <jni.h>
#include <gis.h>

#include "GRASS_FD.h"

/*
 * Class:     GRASS_FD
 * Method:    G_put_map_row_
 * Signature: (IJ)I
 */
JNIEXPORT jint JNICALL Java_GRASS_FD_G_1put_1map_1row_1
  (JNIEnv *env, jobject obj, jint fd, jlong addr)
{
  return G_put_map_row( (int)fd, (CELL *)addr );
}

/*
 * Class:     GRASS_FD
 * Method:    G_put_map_row_random_
 * Signature: (IJIII)I
 */
JNIEXPORT jint JNICALL Java_GRASS_FD_G_1put_1map_1row_1random_1
  (JNIEnv *env, jobject obj, jint fd, jlong addr, 
   jint row, jint col, jint ncells)
{
  return G_put_map_row_random( (int)fd, (CELL *)addr, 
			       (int)row, (int)col, (int)ncells );
}

/**
 * Class:     GRASS_FD
 * Method:    G_get_map_row_
 * Signature: (IJI)I
 */
JNIEXPORT jint JNICALL Java_GRASS_FD_G_1get_1map_1row_1
  (JNIEnv *env, jobject obj, jint fd, jlong cell, jint row)
{
  return (jint)G_get_map_row( (int)fd, (CELL *)cell, (int)row );
}

/**
 * Class:     GRASS_FD
 * Method:    G_get_map_row_nomask_
 * Signature: (IJI)I
 */
JNIEXPORT jint JNICALL Java_GRASS_FD_G_1get_1map_1row_1nomask_1
  (JNIEnv *env, jobject obj, jint fd, jlong cell, jint row)
{
  return (jint)G_get_map_row_nomask( (int)fd, (CELL *)cell, (int)row );
}


/**
 * Class:     GRASS_FD
 * Method:    G_close_cell_
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_FD_G_1close_1cell_1
  (JNIEnv *env, jobject obj, jint jfd)
{
  G_close_cell((int)jfd);
}

/**
 * Class:     GRASS_FD
 * Method:    G_unopen_cell_
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_FD_G_1unopen_1cell_1
  (JNIEnv *env, jobject obj, jint jfd)
{
  G_unopen_cell((int)jfd);
}

