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

#include "GRASS_Colors.h"

/**
 * Extracts Colors structure address from instance object 
 *
 *
 * @return pointer to struct Color from GRASS library associated with provided object
 */
struct Colors* Colors_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Colors *)jaddr;
}

/**
 *
 *
 * Class:     Colors
 * Method:    allocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Colors_allocate
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  void *colors = jmalloc( env, sizeof(struct Colors) );
  if (colors == NULL) return;
  (*env)->SetLongField(env, obj, fid, (jlong)colors );
}

/*
 * Class:     GRASS_Colors
 * Method:    G_init_colors
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Colors_G_1init_1colors
  (JNIEnv *env, jobject obj)
{
  G_init_colors( Colors_ptr(env, obj) );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_read_colors
 * Signature: (Ljava/lang/String;Ljava/lang/String;)LColors;
 */
JNIEXPORT void JNICALL Java_GRASS_Colors_G_1read_1colors
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset) 
{
  // prepare name and mapset
  const char *name;
  const char *mapset;
  int res; // result code

  if (!checknull( env, jname,   "name") ||
      !checknull( env, jmapset, "mapset")) return;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);
  mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

  // execute GRASS function 
  // printf("Addr=%i\n", colors);
  res = G_read_colors( name, mapset, Colors_ptr( env, obj )  );
  // printf("Addr=%i\n", colors);
  
  // check result
  // Heres is a problem: I have to supply newly created Colors object
  // with this result code indicating its origin (1 - existing, 0 - generated)
  if ( res == 0 || res == 1) {

    // code here to set the field in Colors object to r

  } else {
    char msg[80];
    sprintf(msg, "Failed reading of the color table %s in mapset %s", 
	    name, mapset );
    throw(env, "GRASS/GRASSException", msg);
  }
  
  // free strings
  (*env)->ReleaseStringUTFChars(env,   jname,   name);
  (*env)->ReleaseStringUTFChars(env, jmapset, mapset);
  
}

/**
 *
 * Class:     Colors
 * Method:    G_get_color_
 * Signature: (JI)Ljava/awt/Color;
 */
JNIEXPORT jobject JNICALL Java_GRASS_Colors_G_1get_1color
  (JNIEnv *env, jobject obj, jint cat)
{
  // convert cat to rgb
  int r, g, b;
  
  // create color object
  jclass color_cl = (*env)->FindClass(env, "java/awt/Color");
  jmethodID mid = (*env)->GetMethodID(env, color_cl, "<init>", "(III)V");
  //if ( color_cl==NULL || mid==NULL ) printf("^=============^\n");

  G_get_color( cat, &r, &g, &b, Colors_ptr( env, obj ) );
  return (*env)->NewObject( env, color_cl, mid, (jint)r, (jint)g, (jint)b );

}

/*
 * Class:     GRASS_Colors
 * Method:    intColor
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_GRASS_Colors_intColor
  (JNIEnv *env, jobject obj, jint cat) 
{
  // convert cat to rgb
  int r, g, b;
  G_get_color( cat, &r, &g, &b, Colors_ptr( env, obj ) );

  // does not seem to be platform idependent
  return (jint)( (255 << 24) | (r << 16) | (g << 16) | b );
}

/**
 *
 *
 * Class:     Colors
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Colors_G_1free_1colors
  (JNIEnv *env, jobject obj)
{
  G_free_colors( Colors_ptr( env, obj ) );
}

