/*
 *   v.reclass.db
 *
 *
 *   Reclass vector(s) associated the results of a
 *   database query.
 *
 *   jaf 2/9/92
 *
 *   INGRES modifications - Name of subroutine has been changed:
 *			    sqlQry <=> infxQry
 *
 *   Katarina Johnsson 930416
 */

#include <stdio.h>
#include "gis.h"
#include "dbvect.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *colname;  
    char *mapset ;
    char buf[1024];

    int i;

    struct Join *joinargs;
    struct Option *sql, *key, *input, *output;
    struct Flag *flag;

/* JOIN needs to work as per d.what.db (sites) where 
	JOIN="tablename, tableKey, primeKey"
   using G_parser multiple option stuff in d.vect.cats
*/

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "Name of file containing SQL query statements." ;


	key = G_define_option() ;
	key->key        = "key" ;
	key->type       = TYPE_STRING ;
	key->required   = YES  ;
	key->multiple   = NO ;
	key->description= "Column corresponding to cats in vector map [map]" ;


	input = G_define_option() ;
	input->key        = "input" ;
	input->gisprompt  = "old,dig,vector" ;
	input->type       = TYPE_STRING ;
	input->required   = YES  ;
	input->multiple   = NO ;
	input->description= "Name of existing vector file.";

	output = G_define_option() ;
	output->key        = "output" ;
	output->gisprompt  = "new,dig,vector" ;
	output->type       = TYPE_STRING ;
	output->required   = YES  ;
	output->multiple   = NO ;
	output->description= "Name for new vector file.";


	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.db to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);


	if ((mapset=G_find_file2("dig",input->answer,""))==NULL)  {
	     fprintf(stderr,"Vector file %s not found.\n",input->answer);
             exit(-1);
	}


/*************** INGRES driver code begins ***************/
        
	sqlQry(sql->answer, key->answer, input->answer, output->answer, mapset);

	exit(0);
}
