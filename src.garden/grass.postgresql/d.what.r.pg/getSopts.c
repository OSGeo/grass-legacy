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
//-------------- A.Sh. 06 Jan. 00
#define GLOBAL
#include "what.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;

{
        struct Option *sql, *map, *hv;
        struct Flag *select;
        int button, i;
	int stat = 0 ;
	FILE *fp;   
    	char ch;
    	char buf[1024] = "";
    	char tmpstr[1024] = "";
    	char SQL_stmt[1024] = "";
    	char *print_out="";
	int end_l,k;




        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [s] for input from SQL-file." ;

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL command file:";

        map = G_define_option() ;
        map->key        = "map" ;
	map->gisprompt   ="old,cell,raster" ;
        map->type       = TYPE_STRING ;
        map->required   = YES  ;
        map->multiple   = NO ;
        map->description= "Raster map to query:";
	
	hv = G_define_option() ;
	hv->key        = "hv" ;
	hv->type       = TYPE_STRING ;
	hv->answer     = "v" ;
	hv->description= "Database output format - [v(ert)/h(oriz)]:";

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

	print_out = hv->answer;

         /* Initialize screen graphics and get mouse input */

        R_open_driver();
        D_setup(0);
        if (( fd = opencell (map->answer, mapset) ) >= 0)
        do
        {
                button=getCat(map->answer);
                if ( (button != 3) && (dbCat > 0) ) {
		
			/* read all lines of sql stmt into a var  */
        
			if((fp = fopen(sql->answer,"r")) == NULL) {
            			fprintf(stderr, "File read error on %s\n",sql->answer);
            			exit(-1);
           		}

			end_l=0;
			k=0;
			strcpy(buf,"");

			while (!feof(fp) || k >= 1023) {
				ch = getc(fp);
				k++;
				if(ch == '?') {
					sprintf(tmpstr,"%d",dbCat);
					strncat(buf,tmpstr,strlen(tmpstr));
				} else {
					sprintf(tmpstr,"%c",ch);
					strncat(buf,tmpstr,1);
				}

				
			} 
			fclose (fp);
			
			
			strncpy(SQL_stmt,buf,strlen(buf)-1);
			
                       stat = runInfxQry(SQL_stmt,print_out);
		}
        } while (button != 3);

        R_close_driver();

	return stat ;

}
