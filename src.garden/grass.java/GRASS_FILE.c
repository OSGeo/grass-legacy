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

#include "GRASS_FILE.h"

/**
 * Extracts address address from instance object 
 *
 *
 * @return pointer to FILE associated with provided object
 */
FILE* FILE_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (FILE *)jaddr;
}

/*
 * Class:     GRASS_FILE
 * Method:    G_get_site
 * Signature: ()LGRASS/Site;
 */
JNIEXPORT jobject JNICALL Java_GRASS_FILE_G_1get_1site
  (JNIEnv *env, jobject obj)
{
  jobject site = NULL;
  double east, north;
  char *desc;

  int r = G_get_site( FILE_ptr( env, obj ), &east, &north, &desc );
  //printf("Read site: %f, %f, %s\n", (jfloat)east, (jfloat)north, desc);

  if ( r>0 ) {
   
    jclass site_cl = (*env)->FindClass(env, "GRASS/Site");

    jmethodID mid = (*env)->GetMethodID(env, site_cl, "<init>", 
					"(FFLjava/lang/String;)V");

    //if ( site_cl==NULL || mid==NULL ) printf("^=============^\n");

    site = (*env)->NewObject( env, site_cl, mid, 
			      (jfloat)east, (jfloat)north, 
			      (*env)->NewStringUTF(env, desc)
			      );
  }
  
  return site;

}

/*
 * Class:     GRASS_FILE
 * Method:    G_put_site
 * Signature: (FFLjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_GRASS_FILE_G_1put_1site
  (JNIEnv *env, jobject obj, jfloat east, jfloat north, jstring jdesc)
{
  const char *desc;

  if (!checknull( env, jdesc, "description")) return;

  desc = (*env)->GetStringUTFChars(env, jdesc, 0);
  G_put_site( FILE_ptr( env, obj ), (float)east, (float)north, desc );
  (*env)->ReleaseStringUTFChars(env, jdesc, desc);
}

/*
 * Class:     GRASS_FILE
 * Method:    close
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_FILE_close
  (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "open", "Z");
  (*env)->SetBooleanField(env, obj, fid, JNI_FALSE );
  
  fclose( FILE_ptr( env, obj ) );
}

