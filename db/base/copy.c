/**************************************************************
 * db.copy from_driver=name from_database=name from_table=name
 *         to_driver=name to_database=name to_table=name
 *
 *
 *   copy a table
 ****************************************************************/

#include "gis.h"
#include "dbmi.h"
#include "codes.h"
#include <stdlib.h>
#include "glocale.h"

int
main(int argc, char *argv[])
{
    int    ret;
    struct Option *from_driver, *from_database, *from_table;
    struct Option *to_driver, *to_database, *to_table;
    struct Option *where, *select;
    struct GModule *module;
    char   *drv, *db;

    /* Set description */
    module              = G_define_module();
    module->description = _("Copy a table. Either 'from_table' (optionaly with 'where') can be used "
			"or 'select' option, but not 'from_table' and 'select' at the same time.");

    from_driver		     = G_define_option();
    from_driver->key 	     = "from_driver";
    from_driver->type 	     = TYPE_STRING;
    from_driver->options     = db_list_drivers();
    from_driver->required    = YES;
    from_driver->description = _("Input driver name");
    if ( (drv=db_get_default_driver_name()) )
         from_driver->answer = drv;

    from_database 	       = G_define_option();
    from_database->key 	       = "from_database";
    from_database->type        = TYPE_STRING;
    from_database->required    = YES;
    from_database->description = _("Input database name");
    if ( (db=db_get_default_database_name()) )
	from_database->answer = db;

    from_table 		    = G_define_option();
    from_table->key 	    = "from_table";
    from_table->type 	    = TYPE_STRING;
    from_table->required    = NO;
    from_table->description = _("Input table name (only if select is not specified)");

    to_driver		   = G_define_option();
    to_driver->key 	   = "to_driver";
    to_driver->type 	   = TYPE_STRING;
    to_driver->options     = db_list_drivers();
    to_driver->required    = YES;
    to_driver->description = _("Output driver name");
    if ( (drv=db_get_default_driver_name()) )
         to_driver->answer = drv;

    to_database 	     = G_define_option();
    to_database->key 	     = "to_database";
    to_database->type        = TYPE_STRING;
    to_database->required    = YES;
    to_database->description = _("Output database name");
    if ( (db=db_get_default_database_name()) )
	to_database->answer = db;

    to_table 		  = G_define_option();
    to_table->key 	  = "to_table";
    to_table->type 	  = TYPE_STRING;
    to_table->required    = YES;
    to_table->description = _("Output table name");

    where 		= G_define_option();
    where->key 	    	= "where";
    where->type 	= TYPE_STRING;
    where->required	= NO;
    where->description	= _("Optional where condition (without WHERE key word), e.g.:\n"
			  "\t\tobec = 'Liptakov'");

    select 		= G_define_option();
    select->key 	= "select";
    select->type 	= TYPE_STRING;
    select->required	= NO;
    select->description	= _("Full select statement (only if from_table and where is not used), e.g.:\n"
			  "\t\tSELECT dedek FROM starobince WHERE obec = 'Frimburg'");

    if(G_parser(argc, argv)) exit(ERROR);

    /* Check options and copy tables */
    if ( from_table->answer ) {
	if ( select->answer )
	    G_fatal_error ( _("Cannot combine 'from_table' and 'select' options") );

	if ( where->answer ) {
	    ret = db_copy_table_where ( from_driver->answer, from_database->answer, from_table->answer,
				        to_driver->answer, to_database->answer, to_table->answer,
		   			where->answer );
	} else { 
	    ret = db_copy_table ( from_driver->answer, from_database->answer, from_table->answer,
				  to_driver->answer, to_database->answer, to_table->answer );
	}
    } else {
       	if ( !select->answer )	
	    G_fatal_error ( _("Either 'from_table' or 'select' option must be given.") );

       	if ( where->answer )
	    G_fatal_error ( _("Cannot combine 'select' and 'where' options") );

	ret = db_copy_table_select ( from_driver->answer, from_database->answer, from_table->answer,
			             to_driver->answer, to_database->answer, to_table->answer,
	       			     select->answer );
    }

    if ( ret == DB_FAILED ) {
	G_warning ( _("Copy table failed") );
	exit(1);
    }

    exit(0);
}

