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
#include <gis.h>
#include "etc.h"

#include "GRASS_Cellhead.h"

/* converts address from instance object */
/**
 * Extracts Cell_head address address from instance object 
 *
 *
 * @return pointer to struct Cell_head from GRASS library associated with provided object
 */
struct Cell_head* Cell_head_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Cell_head *)jaddr;
}

/**
 * Class:     Cellhead
 * Method:    allocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_allocate
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  void *head = jmalloc( env, sizeof(struct Cell_head) );
  if (head == NULL) return;
  memset( head, '\0', sizeof(struct Cell_head) ); // fill with zeroes
  (*env)->SetLongField(env, obj, fid, (jlong)head );
}

/**
 * Class:     Cellhead
 * Method:    format
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Cellhead_format
  (JNIEnv *env, jobject obj)
{
   return (jint)(Cell_head_ptr( env, obj )->format);
}

/**
 * Class:     Cellhead
 * Method:    setFormat
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setFormat
  (JNIEnv *env, jobject obj, jint format)
{
   Cell_head_ptr( env, obj )->format = (int)format;
}

/**
 * Class:     Cellhead
 * Method:    compressed
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_GRASS_Cellhead_compressed
  (JNIEnv *env, jobject obj)
{
   return (jboolean)(Cell_head_ptr( env, obj )->compressed);
}

/**
 * Class:     Cellhead
 * Method:    setCompressed
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setCompressed
  (JNIEnv *env, jobject obj, jboolean compressed)
{
   Cell_head_ptr( env, obj )->compressed = (int)compressed;
}

/**
 * Class:     Cellhead
 * Method:    rows
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Cellhead_rows
  (JNIEnv *env, jobject obj)
{
   return (jint)(Cell_head_ptr( env, obj )->rows);
}

/**
 * Class:     Cellhead
 * Method:    setRows
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setRows
  (JNIEnv *env, jobject obj, jint rows)
{
   Cell_head_ptr( env, obj )->rows = (int)rows;
}

/**
 * Class:     Cellhead
 * Method:    cols
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Cellhead_cols
  (JNIEnv *env, jobject obj)
{
   return (jint)(Cell_head_ptr( env, obj )->cols);
}

/**
 * Class:     Cellhead
 * Method:    setCols
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setCols
  (JNIEnv *env, jobject obj, jint cols)
{
   Cell_head_ptr( env, obj )->cols = (int)cols;
}

/**
 * Class:     Cellhead
 * Method:    proj
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Cellhead_proj
  (JNIEnv *env, jobject obj)
{
   return (jint)(Cell_head_ptr( env, obj )->proj);
}

/**
 * Class:     Cellhead
 * Method:    setProj
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setProj
  (JNIEnv *env, jobject obj, jint proj)
{
   Cell_head_ptr( env, obj )->proj = (int)proj;
}

/**
 * Class:     Cellhead
 * Method:    zone
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Cellhead_zone
  (JNIEnv *env, jobject obj)
{
   return (jint)(Cell_head_ptr( env, obj )->zone);
}

/**
 * Class:     Cellhead
 * Method:    setZone
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setZone
  (JNIEnv *env, jobject obj, jint zone)
{
   Cell_head_ptr( env, obj )->zone = (int)zone;
}

/**
 * Class:     Cellhead
 * Method:    ew_res
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_ew_1res
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->ew_res);
}

/**
 * Class:     Cellhead
 * Method:    setEw_res
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setEw_1res
  (JNIEnv *env, jobject obj, jdouble ew_res)
{
   Cell_head_ptr( env, obj )->ew_res = (double)ew_res;
}

/**
 * Class:     Cellhead
 * Method:    ns_res
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_ns_1res
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->ns_res);
}

/**
 * Class:     Cellhead
 * Method:    setNs_res
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setNs_1res
  (JNIEnv *env, jobject obj, jdouble ns_res)
{
   Cell_head_ptr( env, obj )->ns_res = (double)ns_res;
}

/**
 * Class:     Cellhead
 * Method:    north
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_north
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->north);
}

/**
 * Class:     Cellhead
 * Method:    setNorth
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setNorth
  (JNIEnv *env, jobject obj, jdouble north)
{
   Cell_head_ptr( env, obj )->north = (double)north;
}

/**
 * Class:     Cellhead
 * Method:    south
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_south
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->south);
}

/**
 * Class:     Cellhead
 * Method:    setSouth
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setSouth
  (JNIEnv *env, jobject obj, jdouble south)
{
   Cell_head_ptr( env, obj )->south = (double)south;
}

/**
 * Class:     Cellhead
 * Method:    east
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_east
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->east);
}

/**
 * Class:     Cellhead
 * Method:    setEast
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setEast
  (JNIEnv *env, jobject obj, jdouble east)
{
   Cell_head_ptr( env, obj )->east = (double)east;
}

/**
 * Class:     Cellhead
 * Method:    west
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_west
  (JNIEnv *env, jobject obj)
{
   return (jdouble)(Cell_head_ptr( env, obj )->west);
}

/**
 * Class:     Cellhead
 * Method:    setWest
 * Signature: (D)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_setWest
  (JNIEnv *env, jobject obj, jdouble west)
{
   Cell_head_ptr( env, obj )->west = (double)west;
}

/** 
 * Class:     GRASSlib
 * Method:    G_adjust_Cell_head
 * Signature: (LCellhead;ZZ)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_G_1adjust_1Cell_1head
  (JNIEnv *env, jobject obj, jboolean rflag, jboolean cflag)
{
  // the following line produces a warning 'initialization makes pointer 
  // from integer without a cast' because G_adjust_Cell_head is not
  // declared anywhere in GRASS headers
  char *msg = G_adjust_Cell_head( Cell_head_ptr( env, obj ), 
				  (int)rflag, (int)cflag);
  // check for result and throws exception if needed
  if (msg != NULL) {
    throw(env, "GRASS/GRASSException", msg);
  }
}

/*
 * Class:     GRASS_Cellhead
 * Method:    G_col_to_easting
 * Signature: (D)D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_G_1col_1to_1easting
  (JNIEnv *env, jobject obj, jdouble jcol)
{
  return (jdouble)G_col_to_easting( (double)jcol, Cell_head_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Cellhead
 * Method:    G_row_to_northing
 * Signature: (D)D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_G_1row_1to_1northing
  (JNIEnv *env, jobject obj, jdouble jrow)
{
  return (jdouble)G_row_to_northing( (double)jrow, Cell_head_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Cellhead
 * Method:    G_easting_to_col
 * Signature: (D)D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_G_1easting_1to_1col
  (JNIEnv *env, jobject obj, jdouble jeast)
{
  return (jdouble)G_easting_to_col( (double)jeast, Cell_head_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Cellhead
 * Method:    G_northing_to_row
 * Signature: (D)D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_Cellhead_G_1northing_1to_1row
  (JNIEnv *env, jobject obj, jdouble jnorth)
{
  return (jdouble)G_northing_to_row( (double)jnorth, Cell_head_ptr( env, obj ) );
}

/**
 * Class:     Cellhead
 * Method:    reallocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_reallocate
  (JNIEnv *env, jobject obj)
{
  // get address filed ID
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");

  // get old address
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  struct Cell_head* ch_old = (struct Cell_head *)jaddr;
  
  // allocate new memory
  struct Cell_head* ch_new = jmalloc( env, sizeof(struct Cell_head) );
  if (ch_new == NULL) return;

  // copy content from the old address
  memcpy( ch_new, ch_old, sizeof(struct Cell_head) ); // fill with zeroes
  (*env)->SetLongField(env, obj, fid, (jlong)ch_new );
}

/**
 * Class:     Cellhead
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellhead_free
  (JNIEnv *env, jobject obj)
{
  free( Cell_head_ptr( env, obj ) );
}


