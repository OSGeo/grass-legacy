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
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include <jni.h>
#include <gis.h>
#include "etc.h"

#include "GRASS_libgis.h"

/**
 * Class:     GRASS_libgis
 * Method:    putenv
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_putenv
  (JNIEnv *env, jobject obj, jstring env_var)
{
    const char *str; 
    char *str2; 
    int res;

    if (!checknull( env, env_var, "env_var")) return -1;

    str = (*env)->GetStringUTFChars(env, env_var, 0);
    str2 = jmalloc( env, strlen(str) + 1 );

    if (str2 == NULL) return -1;
    strcpy( str2, str );
    res = putenv( str2 );
    //printf("Setting %s\n", str2);
    (*env)->ReleaseStringUTFChars(env, env_var, str);
    return (jint)res;
}

/**
 * Class:     GRASS_libgis
 * Method:    getpid
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_getpid
  (JNIEnv *env, jobject obj) {
    return (jint)getpid();
}

/**
 * Class:     GRASS_libgis
 * Method:    G_gisinit
 * Signature: (Ljava/lang/String;)V
 *
 */
JNIEXPORT void JNICALL 
Java_GRASS_libgis_G_1gisinit(JNIEnv *env, jobject obj, jstring appname) 
{

    const char *str;

    if ( !checknull( env, appname, "appname" ) ) return;

    str = (*env)->GetStringUTFChars(env, appname, 0);
    //printf("gisinit %s\n", str);
    G_gisinit(str);
    (*env)->ReleaseStringUTFChars(env, appname, str);
    return;
}

/**
 * Class:     GRASS_libgis
 * Method:    G_sleep_on_error
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_GRASS_libgis_G_1sleep_1on_1error
  (JNIEnv *env, jobject obj, jboolean flag) {
    G_sleep_on_error( !flag );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_supress_warnings
 * Signature: (Z)V
 */
JNIEXPORT void JNICALL Java_GRASS_libgis_G_1suppress_1warnings
  (JNIEnv *env, jobject obj, jboolean flag) {
    G_suppress_warnings( !flag );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_location
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1location
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF( env, G_location() );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_mapset
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1mapset
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF( env, G_mapset() );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_myname
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1myname
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, G_myname() );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_gisbase
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1gisbase
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, G_gisbase() );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_gisdbase
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1gisdbase
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, G_gisdbase() );
}

/**
 * Class:     GRASS_libgis
 * Method:    G_location_path
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1location_1path
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, G_location_path() );
}

/*
 * Class:     GRASS_libgis
 * Method:    G_get_window
 * Signature: ()LGRASS/Cellhead;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1get_1window
  (JNIEnv *env, jobject obj)
{
    // create a new Cellhead object
    jclass ch_cl = (*env)->FindClass(env, "GRASS/Cellhead");
    jmethodID mid = (*env)->GetMethodID(env, ch_cl, 
				      "<init>", "()V");
    jobject jch = (*env)->NewObject( env, ch_cl, mid);

    // read region into it
    G_get_window( Cell_head_ptr( env, jch ) );

    return jch;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_get_default_window
 * Signature: ()LGRASS/Cellhead;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1get_1default_1window
  (JNIEnv *env, jobject obj)
{
    // create a new Cellhead object
    jclass ch_cl = (*env)->FindClass(env, "GRASS/Cellhead");
    jmethodID mid = (*env)->GetMethodID(env, ch_cl, 
				      "<init>", "()V");
    jobject jch = (*env)->NewObject( env, ch_cl, mid);

    // read region into it
    G_get_default_window( Cell_head_ptr( env, jch ) );

    return jch;
}

/**
 * Class:     GRASS_libgis
 * Method:    G_window_rows
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_G_1window_1rows
  (JNIEnv *env, jobject obj)
{
    return (jint)G_window_rows();
}

/**
 * Class:     GRASS_libgis
 * Method:    G_window_cols
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_G_1window_1cols
  (JNIEnv *env, jobject obj)
{
    return (jint)G_window_cols();
}

/**
 * Class:     GRASS_libgis
 * Method:    G_set_window
 * Signature: (J)I
 */
JNIEXPORT void JNICALL Java_GRASS_libgis_G_1set_1window
  (JNIEnv *env, jobject obj, jobject jch)
{
    if (!checknull( env, jch, "cellhead")) return;

    if ( G_set_window( Cell_head_ptr( env, jch ) ) == -1 ) 
	throw( env, "GRASS/GRASSException", "Attempt to set invalid region");
}

/**
 * Class:     GRASS_libgis
 * Method:    G_set_window
 * Signature: (J)I
 */
JNIEXPORT void JNICALL Java_GRASS_libgis_G_1put_1window
  (JNIEnv *env, jobject obj, jobject jch)
{
    if (!checknull( env, jch, "cellhead")) return;

    if ( G_put_window( Cell_head_ptr( env, jch ) ) == -1 ) 
	throw( env, "GRASS/GRASSException", "Failed to save database region");
}

/**
 * Class:     GRASS_libgis
 * Method:    G_get_set_window_
 * Signature: (J)V
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1get_1set_1window
  (JNIEnv *env, jobject obj)
{
    // create a new Cellhead object
    jclass ch_cl = (*env)->FindClass(env, "GRASS/Cellhead");
    jmethodID mid = (*env)->GetMethodID(env, ch_cl, 
				      "<init>", "()V");
    jobject jch = (*env)->NewObject( env, ch_cl, mid);

    // read region into it
    G_get_set_window( Cell_head_ptr( env, jch ) );

    return jch;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_begin_cell_area_calculations
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_G_1begin_1cell_1area_1calculations
  (JNIEnv *env, jobject obj)
{
    return (jint)G_begin_cell_area_calculations();
}

/*
 * Class:     GRASS_libgis
 * Method:    G_area_of_cell_at_row
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_libgis_G_1area_1of_1cell_1at_1row
  (JNIEnv *env, jobject obj, jint jrow)
{
    return (jdouble)G_area_of_cell_at_row( (int)jrow );
}


/**
 * Class:     GRASS_libgis
 * Method:    G_find_cell_
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1find_1cell
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name; 
    const char *mapset; 
    const char *file; 
    jstring jfile = NULL;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

    file   = G_find_cell( name, mapset );

    if (file != NULL) {

	jfile = (*env)->NewStringUTF(env, file);

    } else { // it means that raster was not found
	char msg[80];
	sprintf(msg, "Failed to find %s in mapset %s", name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jfile;
}

/**
 * Class:     GRASS_libgis
 * Method:    G_open_cell_old_
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1open_1cell_1old
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name;
    const char *mapset; 
    jobject jfd = NULL;
    int fd;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);
  
    fd = G_open_cell_old( name, mapset );

    if ( fd >= 0 ) {
    
	jclass fd_cl = (*env)->FindClass(env, "GRASS/FD");
	jmethodID mid = (*env)->GetMethodID(env, fd_cl, 
					"<init>", "(LGRASS/libgis;I)V");
	jfd = (*env)->NewObject( env, fd_cl, mid, obj, (jint)fd );

    } else {
	char msg[80];
	sprintf(msg, "Failed to open cell file %s in mapset %s", 
		name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }
  
    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jfd;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_open_cell_new
 * Signature: (Ljava/lang/String;)LGRASS/FD;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1open_1cell_1new
  (JNIEnv *env, jobject obj, jstring jname)
{
    const char *name;
    jobject jfd = NULL;
    int fd;

    if (!checknull( env, jname, "name")) return NULL;

    name = (*env)->GetStringUTFChars(env,   jname, 0);
    fd = G_open_cell_new( name );

    if ( fd >= 0 ) {
    
	jclass fd_cl = (*env)->FindClass(env, "GRASS/FD");
	jmethodID mid = (*env)->GetMethodID(env, fd_cl, 
					    "<init>", "(LGRASS/libgis;I)V");
	jfd = (*env)->NewObject( env, fd_cl, mid, obj, (jint)fd );

    } else {
	char msg[80];
	sprintf(msg, "Failed to open new cell file %s ", name );
	throw(env, "GRASS/GRASSException", msg);
    }
  
    (*env)->ReleaseStringUTFChars(env, jname,   name);

    return jfd;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_open_cell_new_random
 * Signature: (Ljava/lang/String;)LGRASS/FD;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1open_1cell_1new_1random
  (JNIEnv *env, jobject obj, jstring jname)
{
    const char *name;
    jobject jfd = NULL;
    int fd;

    if (!checknull( env, jname, "name")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    fd = G_open_cell_new_random( name );

    if ( fd >= 0 ) {
    
	jclass fd_cl = (*env)->FindClass(env, "GRASS/FD");
	jmethodID mid = (*env)->GetMethodID(env, fd_cl, 
					"<init>", "(LGRASS/libgis;I)V");
	jfd = (*env)->NewObject( env, fd_cl, mid, obj, (jint)fd );

    } else {
	char msg[80];
	sprintf(msg, "Failed to open new cell file %s ", name );
	throw(env, "GRASS/GRASSException", msg);
    }
  
    (*env)->ReleaseStringUTFChars(env, jname,   name);

    return jfd;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_read_cats
 * Signature: (Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1read_1cats
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset) 
{
    const char *name, *mapset;
    jclass cats_cl;
    jmethodID mid;
    jobject jcats;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

    // find and instantiate object Categories 
    cats_cl = (*env)->FindClass(env, "GRASS/Categories");
    mid = (*env)->GetMethodID(env, cats_cl, 
					"<init>", "()V");
    jcats = (*env)->NewObject( env, cats_cl, mid);

    if ( G_read_cats( name, mapset, Categories_ptr( env, jcats ) ) == 0 ) {
	//printf("Categories read\n");

	// set flag showing that G_init_cats or similar was called
	jfieldID ifid = (*env)->GetFieldID(env, cats_cl, "initialized", "Z");
	(*env)->SetBooleanField(env, jcats, ifid, JNI_TRUE);

    } else {
	char msg[80];
	sprintf(msg, "Failed to read Categories from %s in mapset %s", 
		name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }    

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jcats;
}


/*
 * Class:     GRASS_libgis
 * Method:    G_read_vector_cats
 * Signature: (Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1read_1vector_1cats
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name, *mapset;
    jclass cats_cl;
    jmethodID mid;
    jobject jcats;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

    // find and instantiate object Categories 
    cats_cl = (*env)->FindClass(env, "GRASS/Categories");
    mid = (*env)->GetMethodID(env, cats_cl, 
					"<init>", "()V");
    jcats = (*env)->NewObject( env, cats_cl, mid);

    if ( G_read_vector_cats( name, mapset, Categories_ptr( env, jcats ) ) == 0 ) {
	//printf("Categories read\n");

	// set flag showing that G_init_cats or similar was called
	jfieldID ifid = (*env)->GetFieldID(env, cats_cl, "initialized", "Z");
	(*env)->SetBooleanField(env, jcats, ifid, JNI_TRUE);

    } else {
	char msg[80];
	sprintf(msg, "Failed to read Categories from %s in mapset %s", 
		name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }    

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jcats;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_find_sites
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1find_1sites
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name, *mapset, *file;
    jstring jfile = NULL;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);
    file   = G_find_sites( name, mapset );

    if (file != NULL) {

	jfile = (*env)->NewStringUTF(env, file);

    } else { // it means that raster was not found
	char msg[80];
	sprintf(msg, "Failed to find %s in mapset %s", name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);
    
    return jfile;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_fopen_sites_new
 * Signature: (Ljava/lang/String;)LGRASS/FD;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1fopen_1sites_1new
  (JNIEnv *env, jobject obj, jstring jname)
{
    const char *name;
    FILE* f;
    jobject jfile = NULL;

    if (!checknull( env, jname,   "name")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    f = G_fopen_sites_new( name );

    if ( f != NULL ) {
    
	jclass jfile_cl = (*env)->FindClass(env, "GRASS/FILE");
	jmethodID mid = (*env)->GetMethodID(env, jfile_cl, "<init>", "(J)V");
	jfile = (*env)->NewObject( env, jfile_cl, mid, (jlong)f );

    } else {
	char msg[80];
	sprintf(msg, "Failed to open site list %s", name );
	throw(env, "GRASS/GRASSException", msg);
    }
  
    (*env)->ReleaseStringUTFChars(env, jname,   name);

    return jfile;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_fopen_sites_old
 * Signature: (Ljava/lang/String;Ljava/lang/String;)LGRASS/FD;
 */
JNIEXPORT jobject JNICALL Java_GRASS_libgis_G_1fopen_1sites_1old
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name, *mapset;
    FILE* f;
    jobject jfile = NULL;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);
  
    f = G_fopen_sites_old( name, mapset);

    if ( f != NULL ) {
    
	jclass jfile_cl = (*env)->FindClass(env, "GRASS/FILE");
	jmethodID mid = (*env)->GetMethodID(env, jfile_cl, "<init>", "(J)V");
	return (*env)->NewObject( env, jfile_cl, mid, (jlong)f );

    } else {
	char msg[80];
	sprintf(msg, "Failed to open site list %s in mapset %s", 
		name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jfile;
} 

/*
 * Class:     GRASS_libgis
 * Method:    G_find_vector
 * Signature: (Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1find_1vector
  (JNIEnv *env, jobject obj, jstring jname, jstring jmapset)
{
    const char *name, *mapset, *file;
    jstring jfile = NULL;

    if (!checknull( env, jname,   "name") ||
	!checknull( env, jmapset, "mapset")) return NULL;

    name   = (*env)->GetStringUTFChars(env,   jname, 0);
    mapset = (*env)->GetStringUTFChars(env, jmapset, 0);

    file   = G_find_vector( name, mapset );

    if (file != NULL) {

	jfile = (*env)->NewStringUTF(env, file);

    } else { // it means that raster was not found
	char msg[80];
	sprintf(msg, "Failed to find %s in mapset %s", name, mapset );
	throw(env, "GRASS/GRASSException", msg);
    }

    (*env)->ReleaseStringUTFChars(env, jname,   name);
    (*env)->ReleaseStringUTFChars(env, jmapset, mapset);

    return jfile;
}

/*
 * Class:     GRASS_libgis
 * Method:    G_database_projection_name
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1database_1projection_1name
  (JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF( env, G_database_projection_name( ) );
}

/*
 * Class:     GRASS_libgis
 * Method:    G_projection
 * Signature: ()I;
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_G_1projection
  (JNIEnv *env, jobject obj)
{
    return (jint)G_projection();
}

/*
 * Class:     GRASS_libgis
 * Method:    G_database_unit_name
 * Signature: (Z)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_GRASS_libgis_G_1database_1unit_1name
  (JNIEnv *env, jobject obj, jboolean jplural)
{
    return (*env)->NewStringUTF( env, G_database_unit_name( (int)jplural ) );
}

/*
 * Class:     GRASS_libgis
 * Method:    G_database_units_to_meters_factor
 * Signature: ()D
 */
JNIEXPORT jdouble JNICALL Java_GRASS_libgis_G_1database_1units_1to_1meters_1factor
  (JNIEnv *env, jobject obj)
{
  return (jdouble)G_database_units_to_meters_factor();
}

/*
 * Class:     GRASS_libgis
 * Method:    G_zone
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_GRASS_libgis_G_1zone
  (JNIEnv *env, jobject obj)
{
    return (jint)G_zone();
}


