/*
 *   r.rescale.db
 *
 *
 *   Generate a reclass image using the results of 
 *   multiple database queries. The output map
 *   has one category representing the hits for
 *   each of the queries specified in the selectfile.
 *
 *   jaf 2/5/92
 */

#include <stdio.h>
#include "gis.h"
#include "infx.h"
#include "dbrescale.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *mapset ;
    char buf[1024];
    int stat;

    struct Query *query1;
    struct Option *tab, *key, *col, *cats, *input, *output, *join;
    struct Flag *flag;


	stat = 0;

	tab = G_define_option() ;
	tab->key        = "tab" ;
	tab->type       = TYPE_STRING ;
	tab->required   = YES  ;
	tab->multiple   = NO ;
	tab->description= "DB table containing linked to GIS." ;

	key = G_define_option() ;
	key->key        = "key" ;
	key->type       = TYPE_STRING ;
	key->required   = YES  ;
	key->multiple   = NO ;
	key->description= "Key column in db." ;


	col = G_define_option() ;
	col->key        = "col" ;
	col->type       = TYPE_STRING ;
	col->required   = YES  ;
	col->multiple   = NO ;
	col->description= "Column to rescale." ;

	cats = G_define_option() ;
	cats->key        = "cats" ;
	cats->type       = TYPE_INTEGER ;
	cats->required   = YES  ;
	cats->multiple   = NO ;
	cats->description= "Number of categories in output map." ;

	input = G_define_option() ;
	input->key        = "input" ;
	input->type       = TYPE_STRING ;
	input->required   = YES  ;
	input->multiple   = NO ;
	input->description= "Name of existing raster file to be rescaled." ;


	output = G_define_option() ;
	output->key        = "output" ;
	output->type       = TYPE_STRING ;
	output->required   = YES  ;
	output->multiple   = NO ;
	output->description= "Name of new raster (rescaled), file.";

	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
        join->key_desc   = "tab,tabkey,pkey" ;
	join->description= "JOIN rules (eg. table,key,primekey). ";



	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check DATABASE env variable */
        if ((dbname=G__getenv("DATABASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.inf to identify a current database.\n");
	    exit(-1);
           }

	/* Invoke parser */
	if (G_parser(argc, argv))
	    exit(-1);

        
	if (! (mapset = G_find_cell(input->answer,"")))  {
	     fprintf(stderr,"Raster file %s not found.\n",input->answer);
             exit(-1);
	}


/*************** INFX driver code begins ***************/
        
	stat = infxQry(tab->answer, key->answer, col->answer, cats->answer ,
		input->answer, output->answer,join->answers, mapset);

	exit(stat);

}
