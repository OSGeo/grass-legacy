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
#include <jni.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gis.h>

#include "GRASS_CELL.h"
#include "etc.h"

/**
 * Finds memory address of CELL structure associated with object
 * @return memory address of CELL structure associated with provided object
 */
CELL* CELL_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (CELL *)jaddr;
}

/**
 *
 * Class:     CELL
 * Method:    G_allocate_cell_buf_
 * Signature: ()J
 */
JNIEXPORT void JNICALL Java_GRASS_CELL_G_1allocate_1cell_1buf
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  CELL *buf = G_allocate_cell_buf();
  (*env)->SetLongField(env, obj, fid, (jlong)buf );

  fid = (*env)->GetFieldID(env, cls, "size", "I");
  (*env)->SetIntField(env, obj, fid, (jint)G_window_cols() );

}

/**
 * Class:     CELL
 * Method:    G_zero_cell_buf
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_CELL_G_1zero_1cell_1buf
  (JNIEnv *env, jobject obj)
{
  if ( checkRange( env, obj, G_window_cols() ) )
    G_zero_cell_buf( CELL_ptr (env, obj) );
}

/**
 * Checks if position fells within existing range (0..G_window_cols())
 * @param pos position to be checked
 * @return 1 if position falls in range 0..G_window_cols(), 0 otherwise
 * @exception GRASSException if pos falls outside of the range 0..G_window_cols()
 */
checkRange( JNIEnv *env, jobject obj, int pos )
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "size", "I");
  int     size = (int)((*env)->GetIntField(env, obj, fid));

  if (pos > size ) {
    char msg[80];
    sprintf(msg, "Column number out of range (%i > %i)", pos, size);
    throw(env, "GRASS/GRASSException", msg);
    return 0;
  } else 
    return 1;
}


/**
 * Class:     CELL
 * Method:    cellAt_
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_GRASS_CELL_cellAt_1
  (JNIEnv *env, jobject obj, jlong addr, jint pos)
{
    return checkRange(env, obj, pos) ? (jint)*((CELL*)addr + pos) : 0;
}

/**
 * Class:     CELL
 * Method:    setCellAt_
 * Signature: (II)V
 */
JNIEXPORT void JNICALL Java_GRASS_CELL_setCellAt_1
  (JNIEnv *env, jobject obj, jlong addr, jint pos, jint val)
{
  if (checkRange(env, obj, pos)) *((CELL*)addr + pos) = (int)val;
}

/**
 * Class:     CELL
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_CELL_free
  (JNIEnv *env, jobject obj)
{
  free(CELL_ptr (env, obj));
}

/*
 * Class:     GRASS_CELL
 * Method:    toArray
 * Signature: ([I)[I
 */
JNIEXPORT jintArray JNICALL Java_GRASS_CELL_toArray
  (JNIEnv *env, jobject obj, jintArray jarr) 
{
  jclass   cls;
  jfieldID fid;
  int      size;
  jint    *body;
  jsize    len;

  if (!checknull(env, jarr, "cell array")) return NULL;

  cls  = (*env)->GetObjectClass(env, obj);
  fid  = (*env)->GetFieldID(env, cls, "size", "I");
  size = (int)((*env)->GetIntField(env, obj, fid));

  // check if array is enough to store elements
  len = (*env)->GetArrayLength(env, jarr);
  if (size != len) {
    char msg[80];
    sprintf(msg, "Array size is not equal to the CELL buffer size (%i != %i)",
	    size, len);
    throw(env, "GRASS/GRASSException", msg);
    return jarr;
  }

  // get a copy of the array elements
  body = (*env)->GetIntArrayElements(env, jarr, 0);

  // memcpy contents of CELL into array
  // I am not sure that this code will work correctly on all platforms
  memcpy( body, CELL_ptr( env, obj ), size * sizeof(int) );

  // release array
  (*env)->ReleaseIntArrayElements(env, jarr, body, 0);

  return jarr;
}

/*
 * Class:     GRASS_CELL
 * Method:    fromArray
 * Signature: ([I)V
 */
JNIEXPORT void JNICALL Java_GRASS_CELL_fromArray
  (JNIEnv *env, jobject obj, jintArray jarr)
{
  jclass   cls;
  jfieldID fid;
  int     size;
  jint   *body;
  jsize    len;

  if (!checknull(env, jarr, "cell array")) return;

  cls  = (*env)->GetObjectClass(env, obj);
  fid  = (*env)->GetFieldID(env, cls, "size", "I");
  size = (int)((*env)->GetIntField(env, obj, fid));

  // check if array is enough to store elements
  len = (*env)->GetArrayLength(env, jarr);
  if (size != len) {
    char msg[80];
    sprintf(msg, "Array size is not equal to the CELL buffer size (%i != %i)",
	    size, len);
    throw(env, "GRASS/GRASSException", msg);
    return;
  }

  // get a copy of the array elements
  body = (*env)->GetIntArrayElements(env, jarr, 0);

  // memcpy contents of CELL into array
  // I am not sure that this code will work correctly on all platforms
  memcpy( CELL_ptr( env, obj ), body, size * sizeof(int) );

  // release array
  (*env)->ReleaseIntArrayElements(env, jarr, body, 0);

  return;
}

/*
 * Class:     GRASS_CELL
 * Method:    toColorArray
 * Signature: ([I)[I
 */
JNIEXPORT jintArray JNICALL Java_GRASS_CELL_toColorArray
  (JNIEnv *env, jobject obj, jintArray jarr, jobject colors) 
{
  jclass   cls;
  jfieldID fid;
  int  size, i;
  jint *body;
  unsigned char *r, *g, *b, *s;
  jsize len;

  if (!checknull( env, jarr, "cell array" ) ||
      !checknull( env, colors, "colors")) return jarr;

  cls = (*env)->GetObjectClass(env, obj);
  fid = (*env)->GetFieldID(env, cls, "size", "I");
  size = (int)((*env)->GetIntField(env, obj, fid));

  // check if array is enough to store elements
  len = (*env)->GetArrayLength(env, jarr);
  if (size != len) {
    char msg[80];
    sprintf(msg, "Array size is not equal to the CELL buffer size (%i != %i)",
	    size, len);
    throw(env, "GRASS/GRASSException", msg);
    return 0;
  }

  // allocate new 4 arrays
  r = jmalloc( env, 4 * size );
  if ( r == NULL) return jarr;
  g = r + size;
  b = g + size;
  s = b + size;

  // G_lookup_colors
  G_lookup_colors( CELL_ptr( env, obj ), r, g, b, s, size, 
		   Colors_ptr( env, colors) );

  // get a copy of the array elements
  body = (*env)->GetIntArrayElements(env, jarr, 0);

  // copy rgb array into body (this can have problems with bit order)
  for (i=0; i<size; i++) 
    body[i] = (255 << 24) | (r[i] << 16) | (g[i] << 8) | b[i];

  // release array
  (*env)->ReleaseIntArrayElements(env, jarr, body, 0);
  free( r );

  return jarr;
}








