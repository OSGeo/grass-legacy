/* getSelectOpts.c - passes select range of program options to G_parser.
                     The [-s] option indicates that an input
                     file with SQL commands is being provided. This
                     ability to include a well formed SQL command
                     file gives the user more control over output
                     columns and complex database joins etc.
                     If the sql file requires input from GRASS
                     (eg category val or coord X,Y use a [?]
                     as a placeholder as per PREPARE). The SQL
                     input file will be parsed and the [?] will 
                     replaced prior to executing the query.


                  jaf 2/19/92
*/

#include "gis.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;


{

        char *mapset;
        int  i, j, stat;

        struct Option *sql, *input,*output  ;
        struct Flag *select;
	char *key, *col ;



	stat = 0 ;

        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [s] flag to select db records using an input file." ;

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc   = "file";
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL statements specifying selection criteria. ";

        input = G_define_option() ;
        input->key        = "input" ;
	input->gisprompt  = "old,cell,raster" ;
        input->type       = TYPE_STRING ;
        input->required   = YES  ;
        input->multiple   = NO ;
        input->description= "Name of existing raster file.";

        output = G_define_option() ;
        output->key        = "output" ;
	output->gisprompt  = "new,cell,raster" ;
        output->type       = TYPE_STRING ;
        output->required   = NO  ;
        output->multiple   = NO ;
        output->description= "Name of for new reclass file.";


        /* Check for help flag */
        for (i=0; i<argc; i++)
                if(strcmp(argv[i],"help")==0)
                        argv[1] = "help";

        if((argc == 2)&&(strcmp(argv[1],"-s")==0 )) {        /* Run interactive parser */
                argv[1] == NULL ;
                argc = 1;
           }




        /* Invoke parser */
        if (G_parser(argc, argv)) 
            exit(-1);

        if (! (G_find_cell(input->answer,"")))  {
             fprintf(stderr,"Raster file %s not found.\n",input->answer);
             exit(-1);
	}

/*************** INFX driver code begins ***************/

        stat = runSQL( sql->answer, input->answer, output->answer, key, col, 0);
	
	return(stat) ;
}
