/***************************************************************
 *
 * MODULE:       v.db.connect
 * 
 * AUTHOR(S):    Markus Neteler
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
    struct Option *inopt, *field_opt, *dbkey, *dbdriver, *dbdatabase, *dbtable;
    struct Flag *print;
    dbDriver *driver;
    struct field_info *fi;
    int field;
    struct Map_info Map;

    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"prints/sets DB connection for a vector map";

    inopt = G_define_standard_option(G_OPT_V_MAP);
    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

    dbtable = G_define_option();
    dbtable->key         = "table";
    dbtable->type        = TYPE_STRING;
    dbtable->required    = NO;         
    dbtable->description = "table name";

    dbkey = G_define_option() ;
    dbkey->key        = "key" ;
    dbkey->type       = TYPE_STRING ;
    dbkey->required   = NO  ;
    dbkey->multiple   = NO ;
    dbkey->answer    = "cat";
    dbkey->description= "key name:" ;

    dbdriver = G_define_option() ;
    dbdriver->key        = "driver" ;
    dbdriver->type       = TYPE_STRING ;
    dbdriver->options    = db_list_drivers();
    dbdriver->required   = NO  ;
    dbdriver->multiple   = NO ;
    dbdriver->description= "driver name:" ;

    dbdatabase = G_define_option() ;
    dbdatabase->key        = "database" ;
    dbdatabase->type       = TYPE_STRING ;
    dbdatabase->required   = NO  ;
    dbdatabase->multiple   = NO ;
    dbdatabase->description= "database name:" ;

    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current connection parameters and exit";

    G_gisinit (argv[0]);

    /* heeeerrrrrre's the PARSER */
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

    Vect_set_open_level (1);
    Vect_open_old ( &Map, inopt->answer, mapset);

    if (print->answer)
    {
      fi = Vect_get_field( &Map, field);
      if (fi == NULL)
      {
         fprintf(stderr, "Database connection for map <%s> is not defined in DB file\n", input);
         exit(0);
      }
      driver = db_start_driver(fi->driver);
      if (driver == NULL)
            G_fatal_error("Cannot open driver %s", fi->driver) ;

      fprintf(stderr,"Vector map <%s> is connected to table <%s> in database <%s> through driver <%s>\n",  input, fi->table, fi->database, fi->driver);
    }
    else /* define new dbln settings */
    {
       if (field_opt->answer && dbtable->answer && dbkey->answer
           && dbdatabase->answer && dbdriver->answer)
       {
         fi = (struct field_info *) G_malloc( sizeof(struct field_info) );
         fi->name     = NULL;
         fi->table    = dbtable->answer;
         fi->key      = dbkey->answer;
         fi->database = dbdatabase->answer;
         fi->driver   = dbdriver->answer;
       
         /* it is automatically checked if field already defined */
         Vect_map_add_dblink ( &Map, atoi(field_opt->answer), fi->name, fi->table, fi->key, fi->database, fi->driver);
         Vect_write_dblinks ( Map.name, Map.mapset, Map.dblnk );
       }
       else
          G_fatal_error("You have to specify all connection parameters.");
    }

    Vect_close ( &Map);
    
    exit(0);
}
