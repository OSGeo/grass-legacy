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

#include "GRASS_Range.h"

/**
 * Extracts structure address from object instance 
 *
 *
 * @return pointer to struct Range from GRASS library associated with provided object
 */
struct Range* Range_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Range *)jaddr;
}

/**
 *
 *
 * Class:     Range
 * Method:    allocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_allocate
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  void *Range = jmalloc( env, sizeof(struct Range) );
  (*env)->SetLongField(env, obj, fid, (jlong)Range );
}

/*
 * Class:     GRASS_Range
 * Method:    G_read_range
 * Signature: (Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_G_1read_1range
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset) {

  const char *name;
  const char *mapset; 

  if (!checknull( env, jname,   "name") ||
      !checknull( env, jmapset, "mapset")) return;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);
  mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

  if ( G_read_range( name, mapset, Range_ptr (env, obj ) ) != 0 ) {
    char msg[80];
    sprintf(msg, "Failed to read Range from %s in mapset %s", name, mapset );
    throw(env, "GRASS/GRASSException", msg);
  }    

  (*env)->ReleaseStringUTFChars(env, jname,   name);
  (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

}

/*
 * Class:     GRASS_Range
 * Method:    G_write_range
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_G_1write_1range
  (JNIEnv *env, jobject obj, jstring jname) {

  const char *name;

  if (!checknull( env, jname, "name")) return;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);

  if ( G_write_range( name, Range_ptr (env, obj ) ) != 0 ) {
    char msg[80];
    sprintf(msg, "Failed to write Range for %s", name);
    throw(env, "GRASS/GRASSException", msg);
  }    

  (*env)->ReleaseStringUTFChars(env, jname, name);
}

/*
 * Class:     GRASS_Range
 * Method:    G_init_range
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_G_1init_1range
  (JNIEnv *env, jobject obj) {
  
  G_init_range( Range_ptr ( env, obj ) );
}

/*
 * Class:     GRASS_Range
 * Method:    G_update_range
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_G_1update_1range
  (JNIEnv *env, jobject obj, jint n) {
    
  G_update_range( (int)n, Range_ptr ( env, obj ) );
}

/*
 * Class:     GRASS_Range
 * Method:    G_row_update_range
 * Signature: (LGRASS/CELL;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_G_1row_1update_1range_1
  (JNIEnv *env, jobject obj, jlong addr, jint n) {

  G_row_update_range( (CELL *)addr, (int)n, Range_ptr ( env, obj ) );
}

/*
 * Class:     GRASS_Range
 * Method:    min
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Range_min
  (JNIEnv *env, jobject obj) {

  int min, max;
  G_get_range_min_max ( Range_ptr ( env, obj ), &min, &max );

  return (jint)min;
}

/*
 * Class:     GRASS_Range
 * Method:    max
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Range_max
  (JNIEnv *env, jobject obj) {

  int min, max;
  G_get_range_min_max ( Range_ptr ( env, obj ), &min, &max );

  return (jint)max;
}

/*
 * Class:     GRASS_Range
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Range_free
  (JNIEnv *env, jobject obj) {
    free( Range_ptr( env, obj ) );
}




