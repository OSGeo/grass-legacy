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
 * TODO: - fix -o flag (needs fix in Vect lib)
 *       - add check that key is INTEGER (needs test for column type)
 *       - add check if column exists
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
    struct Flag *overwrite, *print, *columns, *delete;
    dbDriver *driver;
    dbString table_name;
    dbTable *table;
    dbHandle handle;
    struct field_info *fi;
    int field, ret, num_dblinks, i, ncols, col;
    struct Map_info Map;
    char *drv, *db;
    char *fakestart;
    
    /* set up the options and flags for the command line parser */

    module = G_define_module();
    module->description =
	"prints/sets DB connection for a vector map";

    inopt = G_define_standard_option(G_OPT_V_MAP);

    /* fake session for HTML generation with parser */
    fakestart = getenv( "GRASS_FAKE_START" );

    dbdriver = G_define_option() ;
    dbdriver->key        = "driver" ;
    dbdriver->type       = TYPE_STRING ;
    dbdriver->options    = db_list_drivers();
    dbdriver->required   = NO  ;
    dbdriver->multiple   = NO ;
    dbdriver->description= "driver name:" ;
    if ( fakestart == NULL && (drv=G__getenv2("GV_DRIVER",G_VAR_MAPSET)) )
	dbdriver->answer = G_store ( drv );

    dbdatabase = G_define_option() ;
    dbdatabase->key        = "database" ;
    dbdatabase->type       = TYPE_STRING ;
    dbdatabase->required   = NO  ;
    dbdatabase->multiple   = NO ;
    dbdatabase->description= "database name:" ;
    if ( fakestart == NULL && (db=G__getenv2("GV_DATABASE",G_VAR_MAPSET)) )
	dbdatabase->answer = G_store ( db );

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
    dbkey->description= "key name (integer column):" ;

    field_opt = G_define_standard_option(G_OPT_V_FIELD) ;

    print = G_define_flag();
    print->key               = 'p';
    print->description       = "print current connection parameters and exit";

    columns = G_define_flag();
    columns->key               = 'c';
    columns->description       = "print types/names of table columns for specified field and exit";

    overwrite = G_define_flag();
    overwrite->key               = 'o';
    overwrite->description       = "overwrite connection parameter for certain field";
    
    delete = G_define_flag();
    delete->key               = 'd';
    delete->description       = "Delete connection for certain field (not the table)";

    G_gisinit (argv[0]);

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

    if (print->answer || columns->answer)
      Vect_open_old ( &Map, inopt->answer, mapset);
    else
    {
      Vect_open_update_head ( &Map, inopt->answer, G_mapset());
      Vect_hist_command ( &Map );
    }

    if (print->answer || columns->answer)
    {
      num_dblinks = Vect_get_num_dblinks(&Map);
      if (num_dblinks <= 0)
      {
         fprintf(stderr, "Database connection for map <%s> is not defined in DB file\n", input);
         exit(0);
      }
      else /* num_dblinks > 0 */
      {
        if (print->answer)
        {
          fprintf(stderr,"Vector map <%s> is connected by:\n", input);
          for (i = 0; i < num_dblinks; i++) {
            if ( (fi = Vect_get_dblink( &Map, i)) == NULL)
               G_fatal_error("Database connection not defined");
            driver = db_start_driver(fi->driver);
            if (driver == NULL)
                G_warning("Cannot open driver %s", fi->driver) ; /* G_fatal_error ? */
            fprintf(stderr,"field <%d> table <%s> in database <%s> through driver <%s> with key <%s>\n", fi->number, fi->table, fi->database, fi->driver, fi->key);
          }
        } /* end print */
        else /* columns */
        {
          if ( (fi = Vect_get_dblink( &Map, field-1)) == NULL)
               G_fatal_error("Database connection not defined for field <%d>", field);
          driver = db_start_driver(fi->driver);
          if (driver == NULL)
                G_warning("Cannot open driver %s", fi->driver) ; /* G_fatal_error ? */
          
          db_init_handle (&handle);
          db_set_handle (&handle, fi->database, NULL);
          if (db_open_database(driver, &handle) != DB_OK)
             G_fatal_error("Cannot open database <%s>", fi->database);
          db_init_string(&table_name);
          db_set_string(&table_name, fi->table);
          if(db_describe_table (driver, &table_name, &table) != DB_OK)
             G_fatal_error("Cannot open table <%s>", fi->table);

          db_close_database(driver);
          db_shutdown_driver(driver);

          ncols = db_get_table_number_of_columns(table);
          for (col = 0; col < ncols; col++)
	      fprintf (stdout,"%s|%s\n", db_sqltype_name(db_get_column_sqltype(db_get_table_column(table, col))), db_get_column_name(db_get_table_column(table, col)));
        }
      } /* end else num_dblinks */
    } /* end print/columns */
    else /* define new dbln settings or delete */
    {
	if ( delete->answer ) {
            Vect_map_del_dblink (  &Map, atoi(field_opt->answer) );
	} else { 
	   if (field_opt->answer && dbtable->answer && dbkey->answer
	       && dbdatabase->answer && dbdriver->answer)
	   {
	     fi = (struct field_info *) G_malloc( sizeof(struct field_info) );
	     fi->name     = NULL;
	     fi->table    = dbtable->answer;
	     fi->key      = dbkey->answer;
	     fi->database = dbdatabase->answer;
	     fi->driver   = dbdriver->answer;
	   
	     ret = Vect_map_check_dblink ( &Map, atoi(field_opt->answer) );
	     G_debug(3, "Vect_map_check_dblink = %d", ret);
	     if ( ret == 1) {
	       /* field already defined */
	       if( !overwrite->answer )
		   G_fatal_error("Use -o to overwrite existing link for field <%d>",atoi(field_opt->answer));
	       else
	       {
		   if( db_table_exists ( dbdriver->answer, dbdatabase->answer, dbtable->answer) < 1 )
		       G_fatal_error("Table <%s> does not exist in database <%s>",dbtable->answer, dbdatabase->answer);
		   ret = Vect_map_del_dblink (  &Map, atoi(field_opt->answer) );
		   if( Vect_map_add_dblink ( &Map, atoi(field_opt->answer), 
					     fi->name, fi->table, fi->key, fi->database, fi->driver) == 0) 
		   {
		       G_warning ( "The table <%s> is now part of vector map <%s> and may be deleted "
				   "or overwritten by GRASS modules.", dbtable->answer, input);
		   }
	       }
	     }
	     else
	     { /* field not yet defined, add new field */
		if( db_table_exists ( dbdriver->answer, dbdatabase->answer, dbtable->answer) < 1 )
		   G_fatal_error("Table <%s> does not exist in database <%s>",dbtable->answer, dbdatabase->answer);

		if( Vect_map_add_dblink ( &Map, atoi(field_opt->answer), 
					  fi->name, fi->table, fi->key, fi->database, fi->driver) == 0) 
		{
		   G_warning ( "The table <%s> is now part of vector map <%s> and may be deleted "
			       "or overwritten by GRASS modules.", dbtable->answer, input);
		}
	     }
	   }
	   else /* incomplete parameters given */
	      G_fatal_error("For defining a new connection you have to specify these parameters: driver, database, table [, key [, field]]");
	}
    } /* end define new dbln settings */

    Vect_close ( &Map);
    
    exit(0);
}
