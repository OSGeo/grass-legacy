/*
 *   r.reclass.inf
 *
 *
 *   Generate a reclass image using the results of 
 *   multiple database queries. The output map
 *   has one category representing the hits for
 *   each of the queries specified in the selectfile.
 *
 *   jaf 2/7/92
 */

#include "gis.h"
#define MAIN

main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *selectFile, *colname;  
    char buf[1024];
    int stat = 0 ;

    struct Query *query1;
    struct Option *sql, *key, *input, *output, *label;
    struct Flag *flag;


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
	key->description= "Key column in currently selected database." ;

	input = G_define_option() ;
	input->key        = "input" ;
	input->type       = TYPE_STRING ;
	input->gisprompt  = "old,cell,raster" ;
	input->required   = YES  ;
	input->multiple   = NO ;
	input->description= "Name of existing raster file to be reclassed using query output." ;


	output = G_define_option() ;
	output->key        = "output" ;
	output->type       = TYPE_STRING ;
	output->gisprompt  = "new,cell,raster" ;
	output->required   = NO  ;
	output->multiple   = NO ;
	output->description= "Name of new raster (reclass), file.";

	label = G_define_option() ;
	label->key        = "label" ;
	label->type       = TYPE_STRING ;
	label->required   = NO  ;
	label->multiple   = NO ;
	label->description= "Label for new categories. ";


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

        
	if (! (G_find_cell(input->answer,"")))  {
	     fprintf(stderr,"Raster file %s not found.\n",input->answer);
             exit(-1);
	}

	selectFile = sql->answer;

	/* Open file with SQL commands */
	if((fp = fopen(selectFile,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",selectFile);
	    exit(-1);
           }

	/* Initialze SQL query structure */
	/*
	query1 = (struct Query *)G_malloc(sizeof(struct Query)) ;
	G_zero (query1, sizeof(struct Query));
	*/




/*************** INFX driver code begins ***************/
        
	stat = infxQry(selectFile, key->answer, input->answer,
		output->answer,label->answer);

	exit(stat) ;
}
