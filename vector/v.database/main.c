/***************************************************************
 *
 * MODULE:       v.database
 * 
 * AUTHOR(S):    Radim Blazek
 *               
 * PURPOSE:      Set default settings for vector map attribute storage
 *               
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"

int
main(int argc, char *argv[])
{
    struct Flag *print;
    struct Option *driver, *database;
    struct GModule *module;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    
    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current values";
    
    driver = G_define_option() ;
    driver->key        = "driver" ;
    driver->type       = TYPE_STRING ;
    driver->required   = NO  ;
    driver->multiple   = NO ;
    driver->description= "driver name:" ;
    driver->options    = db_list_drivers();

    database = G_define_option() ;
    database->key        = "database" ;
    database->type       = TYPE_STRING ;
    database->required   = NO  ;
    database->multiple   = NO ;
    database->description= "Database name:" ;

    /* Set description */
    module              = G_define_module();
    module->description = "Set default driver / database for new vector attributes.";

    if(G_parser(argc, argv)) exit(1);

    /* set */
    if ( driver->answer )
	G_setenv2 ( "GV_DRIVER", driver->answer, G_VAR_MAPSET );
		
    if ( database->answer )
	G_setenv2 ( "GV_DATABASE", database->answer, G_VAR_MAPSET );

    /* get and print */
    if( print->answer)  {
        fprintf(stdout, "driver:%s\n", G__getenv2( "GV_DRIVER", G_VAR_MAPSET) );
        fprintf(stdout, "database:%s\n", G__getenv2( "GV_DATABASE", G_VAR_MAPSET) );    
    }

    exit(0);
}
