/*
 *   v.reclass.inf
 *
 *
 *   Generate a reclass vector using the results of
 *   one or more database queries.
 *
 *   jaf 4/5/93
 */


#include <stdio.h>
#include "gis.h"
#define MAIN


main(argc, argv)
int argc ;
char **argv ;
{
    FILE *fp;
    char *dbname;
    char *colname;  
    char buf[1024];

    char *mapset;
    int  i, j, stat;
    struct Option *sql, *input,*output, *vtype, *key  ;
    struct Flag *disolve;
    char *col ;



	stat = 0 ;

        disolve = G_define_flag();
        disolve->key     = 'd';
        disolve->description     = "Dissolve common boundaries (default is no)." ;


        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc   = "file";
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL statements specifying selection criteria. ";

        key = G_define_option() ;
        key->key        = "key" ;
        key->type       = TYPE_STRING ;
        key->required   = YES  ;
        key->multiple   = NO ;
        key->description= "Key column in currently selected database." ;

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

        vtype = G_define_option() ;
        vtype->key        = "type" ;
        vtype->type       = TYPE_STRING ;
        vtype->required   = YES  ;
        vtype->multiple   = NO ;
	vtype->options    =  "area,line,site";
        vtype->description= "Select area, line or site.";



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

        if ((mapset=G_find_file2("dig",input->answer,""))==NULL)  {
             fprintf(stderr,"Vector file %s not found.\n",input->answer);
             exit(-1);
        }

/*************** INFX driver code begins ***************/

        stat = runSQL( sql->answer, input->answer, output->answer, key->answer, col, 0,
			vtype->answer, disolve->answer);
	
	exit(stat) ;
}
