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

#include "GRASS_Categories.h"

/**
 * Extracts structure address from object instance 
 *
 *
 * @return pointer to struct Categories from GRASS library associated with provided object
 */
struct Categories* Categories_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Categories *)jaddr;
}

/*
 * Class:     GRASS_Categories
 * Method:    allocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_allocate
  (JNIEnv *env, jobject obj) 
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  void *categories = jmalloc( env, sizeof(struct Categories) );
  if (categories == NULL) return;
  (*env)->SetLongField(env, obj, fid, (jlong)categories );
}


/*
 * Class:     GRASS_Categories
 * Method:    G_init_cats
 * Signature: (ILjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1init_1cats
  (JNIEnv *env, jobject obj, jint n, jstring jtitle) 
{

  const char *title;
  jclass   icls;
  jfieldID ifid;

  if (!checknull( env, jtitle, "title")) return;

  title = (*env)->GetStringUTFChars(env, jtitle, 0);

  icls = (*env)->GetObjectClass(env, obj);
  ifid = (*env)->GetFieldID(env, icls, "initialized", "Z");
  (*env)->SetBooleanField(env, obj, ifid, JNI_TRUE);

  G_init_cats( (int)n, title, Categories_ptr( env, obj ) );

  (*env)->ReleaseStringUTFChars(env, jtitle, title);

}


/*
 * Class:     GRASS_Categories
 * Method:    G_write_cats
 * Signature: (Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1write_1cats
  (JNIEnv *env, jobject obj, jstring jname) 
{
  const char *name;

  if (!checknull( env, jname, "name")) return;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);

  if ( G_write_cats( name, Categories_ptr (env, obj ) ) != 0 ) {
    char msg[80];
    sprintf(msg, "Failed to write Categories into %s",	name );
    throw(env, "GRASS/GRASSException", msg);
  }    

  (*env)->ReleaseStringUTFChars(env, jname,   name);
}


/*
 * Class:     GRASS_Categories
 * Method:    G_write_vector_cats
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1write_1vector_1cats
  (JNIEnv *env, jobject obj, jstring jname)
{
  const char *name;

  if (!checknull( env, jname, "name")) return;

  name   = (*env)->GetStringUTFChars(env,   jname, 0);

  if ( G_write_vector_cats( name, Categories_ptr (env, obj ) ) != 0 ) {
    char msg[80];
    sprintf(msg, "Failed to write Categories into %s",	name );
    throw(env, "GRASS/GRASSException", msg);
  }    

  (*env)->ReleaseStringUTFChars(env, jname,   name);
}

/*
 * Class:     GRASS_Categories
 * Method:    num
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_Categories_num
  (JNIEnv *env, jobject obj)
{
  return (int)(Categories_ptr (env, obj )->num);
}

/*
 * Class:     GRASS_Categories
 * Method:    G_get_cat
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_Categories_G_1get_1cat
  (JNIEnv *env, jobject obj, jint cat) 
{
  return (*env)->NewStringUTF( env, 
			       G_get_cat( cat, Categories_ptr(env, obj) ) 
			     );
}


/*
 * Class:     GRASS_Categories
 * Method:    G_get_cats_title
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_Categories_G_1get_1cats_1title
  (JNIEnv *env, jobject obj) 
{
  return (*env)->NewStringUTF( env, 
			       G_get_cats_title( Categories_ptr(env, obj) ) 
			     );
}


/*
 * Class:     GRASS_Categories
 * Method:    G_set_cats_title
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1set_1cats_1title
  (JNIEnv *env, jobject obj, jstring jtitle) 
{
  const char *title;

  if (!checknull( env, jtitle, "title")) return;

  title  = (*env)->GetStringUTFChars(env, jtitle, 0);
  G_set_cats_title( title, 
		    Categories_ptr(env, obj) 
		    );
  (*env)->ReleaseStringUTFChars(env, jtitle, title);
}


/*
 * Class:     GRASS_Categories
 * Method:    G_set_cat
 * Signature: (ILjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1set_1cat
  (JNIEnv *env, jobject obj, jint cat, jstring jlabel) 
{
  const char *label;

  if (!checknull( env, label, "label")) return;

  label = (*env)->GetStringUTFChars(env, jlabel, 0);
  G_set_cat( cat, label,
	     Categories_ptr(env, obj) 
	     );
  (*env)->ReleaseStringUTFChars(env, jlabel, label);
}


/*
 * Class:     GRASS_Categories
 * Method:    G_free_cats
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Categories_G_1free_1cats
  (JNIEnv *env, jobject obj) 
{
  jclass   icls = (*env)->GetObjectClass(env, obj);
  jfieldID ifid = (*env)->GetFieldID(env, icls, "initialized", "Z");
  if ( (*env)->GetBooleanField(env, obj, ifid) == JNI_TRUE )
    G_free_cats( Categories_ptr( env, obj)  );
  else
    free ( Categories_ptr( env, obj)  );
}


