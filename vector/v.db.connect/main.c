/***************************************************************
 *
 * MODULE:       v.db.connect
 * 
 * AUTHOR(S):    
 *               
 * PURPOSE:      prints DB connection for a given vector map
 *               
 * COPYRIGHT:    (C) 2002 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 * TODO: add DB file management here
 *
 **************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include  "gis.h"
#include "Vect.h"
#include "dbmi.h" 

int main (int argc, char **argv)
{
    char *input, *mapset;
    struct GModule *module;
    struct Option *inopt, *field_opt;
/*    struct Flag *print; */
    dbDriver *driver;
    struct field_info *Fi;
    int field;

    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"prints DB connection for a vector map";

    inopt = G_define_standard_option(G_OPT_V_MAP);
    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

/*    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current connection parameters and exit";
*/

    G_gisinit (argv[0]);
    /* heeeerrrrrre's the   PARSER */
    if (G_parser (argc, argv))
        exit (-1);

    /* set input vector file name and mapset */
    input = inopt->answer;
    mapset = G_find_vector2 (input, "") ;
    if(field_opt->answer)
       field = atoi (field_opt->answer);
    else
       field = 1;

    if (!mapset)
	G_fatal_error("Vector file [%s] not available in search list", input);
      
    G_debug ( 3, "Mapset = %s", mapset);

    Fi = Vect_get_field_info( input, mapset, field);
    if (Fi == NULL)
    {
       fprintf(stderr, "Database connection for this driver is not defined in DB file\n");
       exit(0);
    }
    driver = db_start_driver(Fi->driver);
    if (driver == NULL)
            G_fatal_error("Cannot open driver %s", Fi->driver) ;

    fprintf(stderr,"Vector map <%s> is connected to table <%s> in database <%s> through driver <%s>\n",  input, Fi->table, Fi->database, driver);

    /* here code should be added to optionally modify DB file */
    
    exit(0);
}
