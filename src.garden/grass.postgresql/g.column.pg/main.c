/*
 *   g.column.inf
 *
 *#----------------------- A.Sh. -12.99
 *   Generate list of database columns for a specified table
 *   in the currently selected SQL database identified
 *   by the environment variable DATABASE.
 *
 *   jaf 12/26/91
 */

/*  modifcation 11/98
 *      to connect to Postgress v6.4
 *      to use libpq
 *  Carl Anderson 11/13/98
*/

#include "gis.h"
#include <stdio.h>
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{ 
    char  *dbname ;
    struct Flag *flag ;
    struct Option  *tbl ;
    int   stat;   
    static char SQL[1024] ;
    
	   
	tbl = G_define_option() ;
	tbl->key        = "table" ;
	tbl->type       = TYPE_STRING ;
	tbl->required   = YES  ;
	tbl->multiple   = NO ;
	tbl->description= "The table name in the selected database:" ;

	flag = G_define_flag();
	flag->key		= 'v';
	flag->description	= "Use flag -v for full output.";

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                   "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
          { exit(-1); }
          
/*      Old style lookup
 *	if (flag->answer) {
 *		sprintf(SQL,"SELECT (pg_attribute.attname,pg_type.typname)
 *		  where  pg_class.relname = \"%s\"
 *		  and    pg_attribute.atttypid = pg_type.oid 
 *		  and pg_attribute.attrelid=pg_class.oid 
 *		  and   pg_attribute.attnum   > 0 
 *		  sort   by attname", tbl->answer );
 *	}
 *	else {
 *		sprintf(SQL,"SELECT (pg_attribute.attname)
 *		  where  pg_class.relname = \"%s\"
 *		  and pg_attribute.attrelid=pg_class.oid 
 *		  and   pg_attribute.attnum   > 0 
 *		  sort   by attname",  tbl->answer );		
 *	}
*/

/* Postgres 6.x style */

	if (flag->answer) {
		sprintf(SQL, "SELECT a.attname as ColumnName, t.typname as Type,
		  a.attlen as Length,
		  a.atttypmod as \"Mod_Length\"
		 FROM pg_class c, pg_attribute a, pg_type t
		 WHERE  c.relname = '%s' 
		  and a.attnum > 0
		  and a.attrelid = c.oid
		  and a.atttypid = t.oid
		  order by attnum", tbl->answer );	
/*		  and a.oid = d.oid     OUTER JOINS Not Supported yet   */
	}
	else {
	/* no column descriptions */
		sprintf(SQL, "SELECT a.attname as ColumnName
		 FROM pg_class c, pg_attribute a
		 WHERE  c.relname = '%s' 
		  and a.attnum > 0
		  and a.attrelid = c.oid
		  order by attnum", tbl->answer );		
	}

/*************** INFX driver code begins ***************/
        
	stat = infxColumn(SQL);

	return(stat);

}

