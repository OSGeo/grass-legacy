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
    struct Option *inopt, *dbdriver, *dbdatabase, *dbtable, *field_opt, *dbkey;
    struct Flag *overwrite, *print;
    dbDriver *driver;
    struct field_info *fi;
    int field, ret, num_dblinks, i;
    struct Map_info Map;

    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"prints/sets DB connection for a vector map";

    inopt = G_define_standard_option(G_OPT_V_MAP);

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

    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current connection parameters and exit";

    overwrite = G_define_flag();
    overwrite->key               = 'o';
    overwrite->description       = "overwrite connection parameter for certain field";

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

    Vect_open_update ( &Map, inopt->answer, G_mapset());
    Vect_hist_command ( &Map );

    if (print->answer)
    {
      num_dblinks = Vect_get_num_dblinks(&Map);
      if (num_dblinks <= 0)
      {
         fprintf(stderr, "Database connection for map <%s> is not defined in DB file\n", input);
         exit(0);
      }
      else /* num_dblinks > 0 */
      {
        fprintf(stderr,"Vector map <%s> is connected by:\n", input);
        for (i = 1; i <= num_dblinks; i++) {
          fi = Vect_get_field( &Map, i);
          driver = db_start_driver(fi->driver);
          if (driver == NULL)
              G_warning("Cannot open driver %s", fi->driver) ; /* error ? */
          fprintf(stderr,"field <%d> table <%s> in database <%s> through driver <%s>\n", i, fi->table, fi->database, fi->driver);
        }
      } /* else */

    } /* print */
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
       
         ret = Vect_map_check_dblink ( &Map, atoi(field_opt->answer), fi->name, fi->table, fi->key, fi->database, fi->driver);
         G_debug(3, "Vect_map_check_dblink: %d", ret);
         if ( ret == 0) {
           /* field already defined */
           if( !overwrite->answer )
               G_fatal_error("Use -o to overwrite existing link for field <%d>",atoi(field_opt->answer));
           else
           {
               if( db_table_exists ( dbdriver->answer, dbdatabase->answer, dbtable->answer) < 1 )
                   G_fatal_error("Table <%s> does not exist in database <%s>",dbtable->answer, dbdatabase->answer);
               if( Vect_map_replace_dblink( &Map, atoi(field_opt->answer), fi->name, fi->table, fi->key, fi->database, fi->driver) == 0)
                   G_warning ( "The table <%s> is now part of vector map <%s> and may be deleted or overwritten by GRASS modules.", dbtable->answer, input);
           }
         }
         else
         { /* field not yet defined, add new field */
            if( db_table_exists ( dbdriver->answer, dbdatabase->answer, dbtable->answer) < 1 )
               G_fatal_error("Table <%s> does not exist in database <%s>",dbtable->answer, dbdatabase->answer);

            if( Vect_map_add_dblink ( &Map, atoi(field_opt->answer), fi->name, fi->table, fi->key, fi->database, fi->driver) == 0)
               G_warning ( "The table <%s> is now part of vector map <%s> and may be deleted or overwritten by GRASS modules.", dbtable->answer, input);
         }
       }
       else
          G_fatal_error("For defining a new connection you have to specify these parameters: driver, database, table [, key [, field]]");
    } /* end define new dbln settings */

    Vect_close ( &Map);
    
    exit(0);
}
