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
/*
------------libpq mods by A.Sh. dec'99
*/

#include "gis.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;


{

        char *mapset;
        int  i, j, stat,k;

        struct Option *sql, *input,*output  ;
        struct Flag *select;

	char buf[1024] = "";
	char SQL_stmt[1024] = "";
	char tmpstr[8] = "";
	char ch;
	
	FILE *fp;



	stat = 0 ;

        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [-s] flag for query input from file.";

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc   = "file";
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL command file: ";

        input = G_define_option() ;
        input->key        = "input" ;
	input->gisprompt  = "old,cell,raster" ;
        input->type       = TYPE_STRING ;
        input->required   = YES  ;
        input->multiple   = NO ;
        input->description= "Raster map (must exist):";

        output = G_define_option() ;
        output->key        = "output" ;
	output->gisprompt  = "new,cell,raster" ;
        output->type       = TYPE_STRING ;
        output->required   = NO  ;
        output->multiple   = NO ;
        output->description= "Reclass map (new):";


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
             fprintf(stderr,"Raster map %s not found.\n",input->answer);
             exit(-1);
	}


	if((fp = fopen(sql->answer,"r")) == NULL) {
            fprintf(stderr, "File read error on select file (%s)\n",sql->answer);
            exit(-1);
           }

			k=0;

			while (!feof(fp) || k >= 1023) {
				ch = getc(fp);
				k++;
				
					sprintf(tmpstr,"%c",ch);
					strncat(buf,tmpstr,1);
			} 
			fclose (fp);

/*			no, i ain't heard of fread() yet
*/
	strncpy(SQL_stmt,buf,strlen(buf)-1);
	stat = runInfxFile( SQL_stmt, input->answer, output->answer);
  	return(stat) ; 	
}
