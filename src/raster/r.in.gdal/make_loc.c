/******************************************************************************
 * $Id$
 *
 * Project:  libgrass
 * Purpose:  Function to create a new location automatically given a 
 *           "Cell_head", PROJ_INFO and PROJ_UNITS information.
 * Author:   Frank Warmerdam, warmerda@home.com
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
 * $Log$
 * Revision 1.1  2000-09-26 13:04:33  frankw
 * New
 *
 */

#include "gis.h"

#include <stdlib.h>
#include <unistd.h>

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

