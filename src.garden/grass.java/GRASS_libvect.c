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
#include "etc.h"

#include "GRASS_libvect.h"

/*
 * Class:     GRASS_libvect
 * Method:    Vect_open_old
 * Signature: (Ljava/lang/String;Ljava/lang/String;)LGRASS/Mapinfo;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libvect_Vect_1open_1old
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
  const char *name, *mapset;
  struct Map_info *mapinfo;
  jobject jmapinfo = NULL;
  int level;

  if (!checknull( env, jname,   "name") ||
      !checknull( env, jmapset, "mapset")) return NULL;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);
  mapset = (*env)->GetStringUTFChars(env, jmapset, 0);
  mapinfo = jmalloc( env, sizeof(struct Map_info) );

  if (mapinfo == NULL) return NULL;
  level = Vect_open_old( mapinfo, name, mapset );

  if ( level > 0 ) {

    jclass mapinfo_cl = (*env)->FindClass(env, "GRASS/Mapinfo");
    jmethodID mid = (*env)->GetMethodID(env, mapinfo_cl, "<init>", "(J)V");
    jmapinfo = (*env)->NewObject( env, mapinfo_cl, mid,	(jlong)mapinfo );

  } else {
    char msg[80];
    sprintf(msg, "Failed to open vector file %s in mapset %s", name, mapset );
    throw(env, "GRASS/GRASSException", msg);
  }

  (*env)->ReleaseStringUTFChars(env,   jname,   name);
  (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

  return jmapinfo;
}

/*
 * Class:     GRASS_libvect
 * Method:    Vect_open_new
 * Signature: (Ljava/lang/String;)LGRASS/Mapinfo;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libvect_Vect_1open_1new
  (JNIEnv *env, jobject obj, jstring jname)
{
  const char *name;
  struct Map_info *mapinfo;
  jobject jmapinfo = NULL;
  int level;

  if (!checknull( env, jname, "name")) return NULL;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);
  mapinfo = jmalloc( env, sizeof(struct Map_info) );

  if (mapinfo == NULL) return NULL;
  level = Vect_open_new( mapinfo, name );

  if ( level > 0 ) {

    jclass mapinfo_cl = (*env)->FindClass(env, "GRASS/Mapinfo");
    jmethodID mid = (*env)->GetMethodID(env, mapinfo_cl, "<init>", "(JI)V");
    jmapinfo = (*env)->NewObject( env, mapinfo_cl, mid, 
				 (jlong)mapinfo, (jint)level );

  } else {
    char msg[80];
    sprintf(msg, "Failed to open vector file %s", name );
    throw(env, "GRASS/GRASSException", msg);
  }

  (*env)->ReleaseStringUTFChars(env,   jname,   name);

  return jmapinfo;
}

/*
 * Class:     GRASS_libvect
 * Method:    Vect_set_open_level
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_GRASS_libvect_Vect_1set_1open_1level
  (JNIEnv *env, jobject obj, jint level)
{
  Vect_set_open_level( (int)level );
}

