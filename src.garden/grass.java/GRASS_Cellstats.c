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

#include "GRASS_Cellstats.h"

/**
 * Extracts Cell_stats structure address from instance object 
 *
 *
 * @return pointer to struct Cell_stats from GRASS library associated with provided object
 */
struct Cell_stats* Cell_stats_ptr (JNIEnv *env, jobject obj)
{
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  jlong  jaddr = (*env)->GetLongField(env, obj, fid);
  return (struct Cell_stats *)jaddr;
}

/*
 * Class:     GRASS_Cellstats
 * Method:    allocate
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellstats_allocate
  (JNIEnv *env, jobject obj) {
  jclass   cls = (*env)->GetObjectClass(env, obj);
  jfieldID fid = (*env)->GetFieldID(env, cls, "addr", "J");
  void *cell_stats = jmalloc( env, sizeof(struct Cell_stats) );
  if (cell_stats == NULL) return;
  (*env)->SetLongField(env, obj, fid, (jlong)cell_stats );
}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_init_cell_stats
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellstats_G_1init_1cell_1stats
  (JNIEnv *env, jobject obj) {

  G_init_cell_stats( Cell_stats_ptr( env, obj ) );

}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_update_cell_stats_
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellstats_G_1update_1cell_1stats_1
  (JNIEnv *env, jobject obj, jlong addr, jint n) {

  G_update_cell_stats( (CELL *)addr, n, Cell_stats_ptr ( env, obj ) );

}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_find_cell_stats
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_GRASS_Cellstats_G_1find_1cell_1stat
  (JNIEnv *env, jobject obj, jint cat) {

  long count = -1;
  int r = G_find_cell_stat( cat, &count, Cell_stats_ptr ( env, obj ) );

  if (r == 0) {
    char msg[50];
    sprintf(msg, "Category %i was not found", cat);
    throw(env, "GRASS/GRASSException", msg);
  }

  return (jlong)count;
}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_free_cell_stats
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellstats_G_1free_1cell_1stats
  (JNIEnv *env, jobject obj) {

  G_free_cell_stats( Cell_stats_ptr ( env, obj ) );
}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_rewind_cell_stats_
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_GRASS_Cellstats_G_1rewind_1cell_1stats
  (JNIEnv *env, jobject obj) {

  G_rewind_cell_stats( Cell_stats_ptr ( env, obj ) );

}


/*
 * Class:     GRASS_Cellstats
 * Method:    G_next_cell_stat
 * Signature: ()LGRASS/Cellstats$Pair;
 */
JNIEXPORT jobject JNICALL Java_GRASS_Cellstats_G_1next_1cell_1stat
  (JNIEnv *env, jobject obj) {

  CELL cat;
  long count;
  jobject Pair = NULL;
  int r = G_next_cell_stat( &cat, &count, Cell_stats_ptr ( env, obj ) );


  if ( r > 0 ) {
    jclass Pair_cl = (*env)->FindClass(env, "GRASS/Cellstats$Pair");
    jmethodID mid = (*env)->GetMethodID(env, Pair_cl, "<init>", 
					"(LGRASS/Cellstats;IJ)V");
    //    if ( Pair_cl==NULL || mid==NULL ) printf("^=============^\n");

    Pair = (*env)->NewObject( env, Pair_cl, mid, 
			      obj, (jint)cat, (jlong)count );
  }
  
  return Pair;
}


