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

int
main(int argc, char *argv[])
{
    int    ret;
    struct Option *from_driver, *from_database, *from_table;
    struct Option *to_driver, *to_database, *to_table;
    struct GModule *module;

    /* Set description */
    module              = G_define_module();
    module->description = "Copy a table.";

    from_driver		     = G_define_option();
    from_driver->key 	     = "from_driver";
    from_driver->type 	     = TYPE_STRING;
    from_driver->options     = db_list_drivers();
    from_driver->required    = YES;
    from_driver->description = "Input driver name";

    from_database 	       = G_define_option();
    from_database->key 	       = "from_database";
    from_database->type        = TYPE_STRING;
    from_database->required    = YES;
    from_database->description = "Input database name";

    from_table 		    = G_define_option();
    from_table->key 	    = "from_table";
    from_table->type 	    = TYPE_STRING;
    from_table->required    = YES;
    from_table->description = "Input table name";

    to_driver		   = G_define_option();
    to_driver->key 	   = "to_driver";
    to_driver->type 	   = TYPE_STRING;
    to_driver->options     = db_list_drivers();
    to_driver->required    = YES;
    to_driver->description = "Input driver name";

    to_database 	     = G_define_option();
    to_database->key 	     = "to_database";
    to_database->type        = TYPE_STRING;
    to_database->required    = YES;
    to_database->description = "Input database name";

    to_table 		  = G_define_option();
    to_table->key 	  = "to_table";
    to_table->type 	  = TYPE_STRING;
    to_table->required    = YES;
    to_table->description = "Input table name";

    if(G_parser(argc, argv)) exit(ERROR);

    ret = db_copy_table ( from_driver->answer, from_database->answer, from_table->answer,
	                  to_driver->answer, to_database->answer, to_table->answer );

    if ( ret == DB_FAILED ) {
	G_warning ( "Copy table failed" );
	exit(1);
    }

    exit(0);
}
