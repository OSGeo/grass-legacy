/****************************************************************
 *
 * MODULE:       g.mapset
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Change current mapset
 *               
 * COPYRIGHT:    (C) 2004 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "raster.h"
#include "display.h"
#include "D.h"

int 
main (int argc, char *argv[])
{
    int    ret;
    struct GModule *module;
    struct Option *gisdbase_opt, *location_opt, *mapset_opt;
    char   *gisdbase_old, *location_old, *mapset_old;
    char   *gisdbase_new, *location_new, *mapset_new;
    char   *gis_lock; 
    char   *mapset_old_path, *mapset_new_path;
    char   *lock_prog, *buf;
    char   *shell, *monitor;
    struct MON_CAP *cap;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Change current mapset";

    mapset_opt = G_define_option() ;
    mapset_opt->key         = "mapset" ;
    mapset_opt->type        = TYPE_STRING ;
    mapset_opt->required    = YES ;
    mapset_opt->multiple    = NO ;
    mapset_opt->description = "New MAPSET name" ;

    location_opt = G_define_option() ;
    location_opt->key         = "location" ;
    location_opt->type        = TYPE_STRING ;
    location_opt->required    = NO ;
    location_opt->multiple    = NO ;
    location_opt->description = "New LOCATION name (not location path)" ;

    gisdbase_opt = G_define_option() ;
    gisdbase_opt->key         = "gisdbase" ;
    gisdbase_opt->type        = TYPE_STRING ;
    gisdbase_opt->required    = NO ;
    gisdbase_opt->multiple    = NO ;
    gisdbase_opt->description = "New GISDBASE (full path to the directory where the new location is)" ;

    if (G_parser(argc, argv))
    	exit(1);

    /* Store original values */
    gisdbase_old = G__getenv ("GISDBASE");
    location_old = G__getenv ("LOCATION_NAME");
    mapset_old = G__getenv ("MAPSET");
    G_asprintf ( &mapset_old_path, "%s/%s/%s", gisdbase_old, location_old,
		mapset_old );
    monitor = G__getenv ("MONITOR");

    /* New values */
    if ( gisdbase_opt->answer )
        gisdbase_new = gisdbase_opt->answer;
    else
	gisdbase_new = gisdbase_old;

    if ( location_opt->answer )
	location_new = location_opt->answer;
    else
	location_new = location_old;

    mapset_new = mapset_opt->answer;
    G_asprintf ( &mapset_new_path, "%s/%s/%s", gisdbase_new, location_new, 
		mapset_new );

    /* TODO: this should be checked better (repeated '/' etc.) */
    if ( strcmp(mapset_old_path, mapset_new_path) == 0 )
	G_fatal_error ( "%s is already the current mapset", mapset_new );
    
    /* Check if the mapset exists and user is owner */
    G_debug ( 2, "check : %s", mapset_new_path );
    
    ret = G__mapset_permissions2 ( gisdbase_new, location_new, mapset_new );
    switch ( ret ) {
	case 0:
	    G_fatal_error ( "You don't have permission to use this mapset." );
	    break;
	case -1:
	    G_fatal_error ( "The mapset does not exist." );
	    break;
	default:
	    break;
    }

    /* Check if the mapset is in use */
    gis_lock = getenv ( "GIS_LOCK" );
    if ( !gis_lock )
	G_fatal_error ( "Cannot read GIS_LOCK enviroment variable." );

    G_asprintf ( &lock_prog, "%s/etc/lock", G_gisbase() );
    
    G_asprintf ( &buf, "%s %s/.gislock %s", lock_prog, mapset_new_path, gis_lock );
    G_debug ( 2, buf );

    ret = system ( buf ) ;
    G_debug ( 2, "lock result = %d", ret );
    G_free( buf );
    G_free( lock_prog );

    /* Warning: the value returned by system() is not that returned by exit() in executed program
     *          e.g. exit(1) -> 256 (multiplied by 256) */
    if ( ret != 0 )
	G_fatal_error ( "%s is currently running GRASS in selected mapset or lock file cannot be checked.",
	        	G_whoami());

    /* Erase monitors */
    fprintf (stderr, "Erasing monitors ...\n" );
    while ((cap = R_parse_monitorcap(MON_NEXT,"")) != NULL) {
	G__setenv("MONITOR",cap->name);
	R__open_quiet();
	if ( R_open_driver() == 0 ) {
            Derase("white");
	    D_add_to_list("d.erase");
	    R_close_driver();
	    R_release_driver();
	}
    }
    if ( monitor )
	G_setenv ( "MONITOR", monitor );

    /* Clean temporary directory */
    G_asprintf ( &buf, "/bin/sh -c \"%s/etc/clean_temp > /dev/null\"", 
		G_gisbase() );
    fprintf (stderr, "Cleaning up temporary files ...\n" );
    system( buf );
    G_free( buf );
    
    /* Reset variables */
    G_setenv ( "GISDBASE", gisdbase_new );
    G_setenv ( "LOCATION_NAME", location_new );
    G_setenv ( "MAPSET", mapset_new );

    /* Remove old lock */
    G_asprintf ( &buf, "%s/.gislock", mapset_old_path );

    unlink ( buf );
    G_free( buf );
    G_free( mapset_old_path );

    G_warning ( "Your shell continues to use the history for the old mapset." );
    
    if ( (shell=getenv("SHELL")) ) {
	if ( strstr(shell,"bash") ) {
            fprintf (stderr, "You can switch the history by commands:\n"
	         "history -w; history -r %s/.bash_history; HISTFILE=%s/.bash_history\n",
		 mapset_new_path, mapset_new_path );
	} else if ( strstr(shell,"tcsh") ) {
            fprintf (stderr, "You can switch the history by commands:\n"
	         "history -S; history -L %s/.history; setenv histfile=%s/.history\n",
		 mapset_new_path, mapset_new_path );
	}
    }
    G_free( mapset_new_path );

    return (0);
}

