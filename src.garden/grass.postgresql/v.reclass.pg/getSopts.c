/*		alex shevlakov, jan'00, libpq update
*/

#include "gis.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;


{

        char *mapset;
        int  i, j, stat,k;

        struct Option *sql, *input,*output, *vtype;
        struct Flag *select, *disolve;
	char *key, *col;
	char buf[1024] = "";
	char SQL_stmt[1024] = "";
	char tmpstr[8] = "";
	char ch;
	
	FILE *fp;



	stat = 0 ;

        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [-s] for file SQL input.";
	
	disolve = G_define_flag();
        disolve->key     = 'd';
        disolve->description = "Dissolve common boundaries (default is no)." ;

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc   = "file";
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "File with SQL command.";

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
        output->required   = NO  ;
        output->multiple   = NO ;
        output->description= "Name of new reclass file.";
	
	vtype = G_define_option() ;
        vtype->key        = "type" ;
        vtype->type       = TYPE_STRING ;
        vtype->required   = YES  ;
        vtype->multiple   = NO ;
	vtype->options    =  "area,line,site";
        vtype->description= "Select area, line or site.";

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

        if (! (G_find_vector2(input->answer,"")))  {
             fprintf(stderr,"Vector map %s not found.\n",input->answer);
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
	strncpy(SQL_stmt,buf,strlen(buf)-1);
	stat = runInfxFile( SQL_stmt, input->answer, output->answer,
	vtype->answer, disolve->answer);
  	return(stat) ; 	
}
