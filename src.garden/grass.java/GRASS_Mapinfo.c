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

#include "GRASS_Mapinfo.h"

/**
 * Extracts Cell_stats structure address from instance object 
 *
 *
 * @return pointer to struct Cell_stats from GRASS library associated with provided object
 */
struct Map_info* Map_info_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Map_info *)jaddr;
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_level
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Mapinfo_Vect_1level
  (JNIEnv *env, jobject obj)
{
  return (jint)Vect_level( Map_info_ptr(env, obj) );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_read_next_line
 * Signature: ()LGRASS/linepnts;
 */
JNIEXPORT jobject JNICALL Java_GRASS_Mapinfo_Vect_1read_1next_1line
  (JNIEnv *env, jobject obj)
{
  jobject jpoints = NULL;
  struct line_pnts *Points = Vect_new_line_struct();
  int r = Vect_read_next_line( Map_info_ptr( env, obj ), Points );
  // r combines result code and feature type

  if ( r > 0 ) { // Create new linepnts
    
    jclass jpoints_cl = (*env)->FindClass(env, "GRASS/linepnts");
    jmethodID mid = (*env)->GetMethodID(env, jpoints_cl, "<init>", "(JI)V");
    jpoints = (*env)->NewObject( env, jpoints_cl, mid, 
				 (jlong)Points, (jint)r );

  } else if (r == -1) { // Error

    // it could be a memory leak here since I do not know
    // if there is a need to free line_pnts struct after
    // reading has failed
    throw(env, "GRASS/GRASSException", 
	  "Error while reading linepnts (Out of memory?)");
  }

  return jpoints;
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    V2_num_lines
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Mapinfo_V2_1num_1lines
  (JNIEnv *env, jobject obj)
{
  return (jint)V2_num_lines( Map_info_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    V2_num_areas
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Mapinfo_V2_1num_1areas
  (JNIEnv *env, jobject obj)
{
  return (jint)V2_num_areas( Map_info_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_get_area_points
 * Signature: (I)LGRASS/linepnts;
 */
JNIEXPORT jobject JNICALL Java_GRASS_Mapinfo_Vect_1get_1area_1points
  (JNIEnv *env, jobject obj, jint jn)
{
  jobject jpoints = NULL;
  struct line_pnts *Points = Vect_new_line_struct();
  int r = Vect_get_area_points( Map_info_ptr( env, obj ), (long)jn, Points );
  // r combines result code and feature type

  if ( r >= 0 ) { // Create new linepnts
    
    jclass jpoints_cl = (*env)->FindClass(env, "GRASS/linepnts");
    jmethodID mid = (*env)->GetMethodID(env, jpoints_cl, "<init>", "(JI)V");
    jpoints = (*env)->NewObject( env, jpoints_cl, mid, 
				 (jlong)Points, (jint)AREA );

  } else if (r == -1) { // Error

    // it could be a memory leak here since I do not know
    // if there is a need to free line_pnts struct after
    // reading has failed
    throw(env, "GRASS/GRASSException", 
	  "Error while reading linepnts (Out of memory?)");
  }

  return jpoints;
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    V2_read_line
 * Signature: (I)LGRASS/linepnts;
 */
JNIEXPORT jobject JNICALL Java_GRASS_Mapinfo_V2_1read_1line
  (JNIEnv *env, jobject obj, jint jn)
{
  jobject jpoints = NULL;
  struct line_pnts *Points = Vect_new_line_struct();
  int r = V2_read_line( Map_info_ptr( env, obj ), Points, (long)jn );
  // r combines result code and feature type

  if ( r > 0 ) { // Create new linepnts
    
    jclass jpoints_cl = (*env)->FindClass(env, "GRASS/linepnts");
    jmethodID mid = (*env)->GetMethodID(env, jpoints_cl, "<init>", "(JI)V");
    jpoints = (*env)->NewObject( env, jpoints_cl, mid, 
				 (jlong)Points, (jint)r );

  } else if (r == -1) { // Error

    // it could be a memory leak here since I do not know
    // if there is a need to free line_pnts struct after
    // reading has failed
    throw(env, "GRASS/GRASSException", 
	  "Error while reading linepnts (Out of memory?)");
  }

  return jpoints;
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    V2_line_att
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_GRASS_Mapinfo_V2_1line_1att
  (JNIEnv *env, jobject obj, jint jn)
{
  return (jint)V2_line_att( Map_info_ptr( env, obj ), (int)jn );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    V2_area_att
 * Signature: (I)I
 */

JNIEXPORT jint JNICALL Java_GRASS_Mapinfo_V2_1area_1att
  (JNIEnv *env, jobject obj, jint jn)
{
  return (jint)V2_area_att( Map_info_ptr( env, obj ), (int)jn );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_rewind
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Mapinfo_Vect_1rewind
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "offset", "J");
  (*env)->SetLongField(env, obj, fid, (jlong)0 );

  Vect_rewind( Map_info_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_set_constraint_region
 * Signature: (DDDD)V
 */
JNIEXPORT void JNICALL Java_GRASS_Mapinfo_Vect_1set_1constraint_1region
  (JNIEnv *env, jobject obj, jdouble n, jdouble s, jdouble e, jdouble w)
{
  Vect_set_constraint_region( Map_info_ptr( env, obj ),
			      (double)n, (double)s, (double)e, (double)w
			      );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_set_constraint_type
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Mapinfo_Vect_1set_1constraint_1type
  (JNIEnv *env, jobject obj, jint type)
{
  Vect_set_constraint_type( Map_info_ptr( env, obj ), (int)type );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_remove_constraints
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Mapinfo_Vect_1remove_1constraints
  (JNIEnv *env, jobject obj)
{
  Vect_remove_constraints( Map_info_ptr( env, obj ) );
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_write_line
 * Signature: (ILGRASS/linepnts;)J
 */
JNIEXPORT jlong JNICALL Java_GRASS_Mapinfo_Vect_1write_1line_1
  (JNIEnv *env, jobject obj, jint type, jlong addr)
{
  long r = Vect_write_line( Map_info_ptr( env, obj ), (int)type, 
			    (struct line_pnts *)addr );

  if ( r > 0 ) { // OK, set offset field
    jclass   cls = (*env)->GetObjectClass(env, obj);
    jfieldID fid = (*env)->GetFieldID(env, cls, "offset", "J");
    (*env)->SetLongField(env, obj, fid, (jlong)r );
  }

  return (jlong)r;
}

/*
 * Class:     GRASS_Mapinfo
 * Method:    Vect_close
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Mapinfo_Vect_1close
  (JNIEnv *env, jobject obj)
{
  Vect_close( Map_info_ptr( env, obj ) );
}

