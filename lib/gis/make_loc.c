/******************************************************************************
 *
 * Project:  libgrass
 * Purpose:  Function to create a new location automatically given a 
 *           "Cell_head", PROJ_INFO and PROJ_UNITS information.
 * Author:   Frank Warmerdam, warmerda@pobox.com
 *
 ******************************************************************************
 * Copyright (c) 2000, Frank Warmerdam
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 ******************************************************************************
 *
 */

#include "gis.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#ifdef __MINGW32__
# define mkdir(name, mode) ((mkdir) (name))
#endif

/*
 * Returns 0 on success.
 * Returns -1 to indicate a system error (check errno).
 */
 

int G__make_location( 
    char *location_name,
    struct Cell_head *wind, 
    struct Key_Value *proj_info, 
    struct Key_Value *proj_units,
    FILE *report_file )

{
    char	path[2048];
    int         out_stat;

    /* Try to create the location directory, under the gisdbase. */
    sprintf( path, "%s/%s", G_gisdbase(), location_name );
    if( mkdir( path, 0775 ) != 0 )
        return -1;

    /* Make the PERMANENT mapset. */
    sprintf( path, "%s/%s/%s", G_gisdbase(), location_name, "PERMANENT" );
    if( mkdir( path, 0775 ) != 0 )
        return -1;

    /* make these the new current location and mapset */
    G__setenv( "LOCATION_NAME", location_name );
    G__setenv( "MAPSET", "PERMANENT" );

    /* Create the default, and current window files */
    G__put_window( wind, "", "DEFAULT_WIND" );
    G__put_window( wind, "", "WIND" );

    /* Write out the PROJ_INFO, and PROJ_UNITS if available. */
    if( proj_info != NULL )
    {
        G__file_name( path, "", "PROJ_INFO", "PERMANENT" );
        G_write_key_value_file( path, proj_info, &out_stat );
        if( out_stat != 0 )
            return -2;
    }

    if( proj_units != NULL )
    {
        G__file_name( path, "", "PROJ_UNITS", "PERMANENT" );
        G_write_key_value_file( path, proj_units, &out_stat );
        if( out_stat != 0 )
            return -2;
    }

    return 0;
}

int G_make_location( 
    char *location_name,
    struct Cell_head *wind, 
    struct Key_Value *proj_info, 
    struct Key_Value *proj_units,
    FILE *report_file )

{
    int	err;

    err = G__make_location( location_name, wind, proj_info, proj_units, 
                            report_file );

    if( err == 0 )
        return 0;

    if( err == -1 )
    {
        perror( "G_make_location" );
    }

    G_fatal_error( "G_make_location failed." );
    
    return 1;
}


/************************************************************************/
/*                       G_compare_projections()                        */
/************************************************************************/

int 
G_compare_projections( struct Key_Value *proj_info1, 
                       struct Key_Value *proj_units1, 
                       struct Key_Value *proj_info2, 
                       struct Key_Value *proj_units2 )

{
    char  buf1[512], buf2[512];
    
    if( proj_info1 == NULL && proj_info2 == NULL )
        return TRUE;
    
/* -------------------------------------------------------------------- */
/*      Are they both in the same projection?                           */
/* -------------------------------------------------------------------- */
    if( G_find_key_value( "proj", proj_info1 ) != NULL
        && G_find_key_value( "meters", proj_units1 ) != NULL
        && atof(G_find_key_value( "meters", proj_units1 ))
           != atof(G_find_key_value( "meters", proj_units2 )) )
        return -1;

/* -------------------------------------------------------------------- */
/*      Verify that the linear unit translation to meters is OK.        */
/* -------------------------------------------------------------------- */
    if( proj_units1 != NULL && proj_units2 != NULL
        && G_find_key_value( "meters", proj_units1 ) != NULL
        && G_find_key_value( "meters", proj_units2 ) != NULL
        && atof(G_find_key_value( "meters", proj_units1 ))
           != atof(G_find_key_value( "meters", proj_units2 )) )
        return -2;

/* -------------------------------------------------------------------- */
/*      Do they both have the same ellipsoid?                           */
/*      Lets just check the semi-major axis for now to keep it simple   */
/* -------------------------------------------------------------------- */
    
    {
        double a1=0, a2=0;
        if(G_find_key_value( "a", proj_info1) != NULL)
           a1 = atof(G_find_key_value( "a", proj_info1 ));
        if(G_find_key_value( "a", proj_info2) != NULL)
           a2 = atof(G_find_key_value( "a", proj_info2 ));

        if ( a1 && a2 && ( abs(a2-a1) > 0.000001 ) )
            return -4;
    }

/* -------------------------------------------------------------------- */
/*      Zone check specially for UTM                                    */
/* -------------------------------------------------------------------- */
    {   
        if(   G_find_key_value( "proj", proj_info1 ) == "utm"
	   && G_find_key_value( "proj", proj_info2 ) == "utm"
	   &&    atof(G_find_key_value( "zone", proj_info1 ))
	      != atof(G_find_key_value( "zone", proj_info2 )) )
	   return -5;
    }

/* -------------------------------------------------------------------- */
/*      Add more details in later.                                      */
/* -------------------------------------------------------------------- */

    return TRUE;
}
