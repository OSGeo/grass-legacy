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
#include <Vect.h>

#include "GRASS_linepnts.h"

/**
 * Extracts Colors structure address from instance object 
 *
 *
 * @return pointer to struct Color from GRASS library associated with provided object
 */
struct line_pnts* line_pnts_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct line_pnts *)jaddr;
}

/*
 * Class:     GRASS_linepnts
 * Method:    n_points
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_linepnts_n_1points
  (JNIEnv *env, jobject obj)
{
  return (jint)(line_pnts_ptr( env, obj )->n_points);
}

/*
 * Class:     GRASS_linepnts
 * Method:    Vect_new_line_struct
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_GRASS_linepnts_Vect_1new_1line_1struct
  (JNIEnv *env, jobject obj)
{
  return (jlong)Vect_new_line_struct();
}

/*
 * Class:     GRASS_linepnts
 * Method:    Vect_copy_xy_to_pnts
 * Signature: ([D[D)V
 */
JNIEXPORT void JNICALL Java_GRASS_linepnts_Vect_1copy_1pnts_1to_1xy
  (JNIEnv *env, jobject obj, jdoubleArray jxarr, jdoubleArray jyarr)
{
  struct line_pnts *Points;
  int np;
  jsize xlen, ylen;
  jdouble *x, *y;
  
  if (!checknull( env, jxarr, "x array") ||
      !checknull( env, jyarr, "y array")) return;

  Points = line_pnts_ptr( env, obj ); 
  np = (jint)(Points->n_points);
  xlen = (*env)->GetArrayLength(env, jxarr);
  ylen = (*env)->GetArrayLength(env, jyarr);
  
  if ( (np > xlen) || (np > ylen) ) {
    char msg[80];
    sprintf(msg, 
	    "Array sizes are smaller than the number of points (xlen=%i, ylen=%i, n_points=%i)",
	    xlen, ylen, np);
    throw(env, "GRASS/GRASSException", msg);
    return;
  }
    
  x = (*env)->GetDoubleArrayElements(env, jxarr, 0);
  y = (*env)->GetDoubleArrayElements(env, jyarr, 0);
  Vect_copy_pnts_to_xy( Points, x, y, &np);
  (*env)->ReleaseDoubleArrayElements(env, jxarr, x, 0);
  (*env)->ReleaseDoubleArrayElements(env, jyarr, y, 0);

}

/*
 * Class:     GRASS_linepnts
 * Method:    Vect_copy_pnts_to_xy
 * Signature: ([D[D)V
 */
JNIEXPORT void JNICALL Java_GRASS_linepnts_Vect_1copy_1xy_1to_1pnts
  (JNIEnv *env, jobject obj, jdoubleArray jxarr, jdoubleArray jyarr)
{
  struct line_pnts *Points;
  jsize xlen, ylen;
  jdouble *x, *y;
  
  if (!checknull( env, jxarr, "x array") ||
      !checknull( env, jyarr, "y array")) return;

  Points = line_pnts_ptr( env, obj ); 
  xlen = (*env)->GetArrayLength(env, jxarr);
  ylen = (*env)->GetArrayLength(env, jyarr);
  
  if ( xlen != ylen ) {
    char msg[80];
    throw(env, "GRASS/GRASSException", msg);
    return;
  }
    
  x = (*env)->GetDoubleArrayElements(env, jxarr, 0);
  y = (*env)->GetDoubleArrayElements(env, jyarr, 0);
  Vect_copy_xy_to_pnts( Points, x, y, xlen);
  (*env)->ReleaseDoubleArrayElements(env, jxarr, x, 0);
  (*env)->ReleaseDoubleArrayElements(env, jyarr, y, 0);

}

/*
 * Class:     GRASS_linepnts
 * Method:    Vect_destroy_line_struct
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_linepnts_Vect_1destroy_1line_1struct
  (JNIEnv *env, jobject obj)
{
  Vect_destroy_line_struct( line_pnts_ptr( env, obj ) );
}

