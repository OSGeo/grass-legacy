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

//-----------------A.Sh. Print_out switch added Jan.00

#include "gis.h"
#include "infx.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;

{

        char *mapset;
        int button, i, j;
        static char SQL_stmt[1024];
        /* arbitrary limit of query size */
        /* cfa 11/098  */
        FILE	*fp;
        struct Flag *select, *qtype;
        struct Option *sql, *distance, *hv;
	struct Sql *pts;
	int stat = 0 ;
	char *print_out="";



        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [s] for query with a command file." ;


        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = NO  ;
        sql->multiple   = NO ;
        sql->description= "SQL query. ";


        distance = G_define_option() ;
        distance->key        = "distance" ;
        distance->type       = TYPE_STRING ;
        distance->required   = YES  ;
        distance->multiple   = NO ;
        distance->description= "Cursor radius." ;
/* add interactive distance g.select.dist
   and mouse choosing  into this routine
   cfa 11/98  */
   	
	hv = G_define_option() ;
	hv->key        = "hv" ;
	hv->type       = TYPE_STRING ;
	hv->answer     = "v" ;
	hv->description= "DB output type - [v(ert)/h(oriz)]:";


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

        if((fp = fopen(sql->answer,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",sql->answer);
            exit(-1);
           }
 
        fread(SQL_stmt,1024,1,fp); 
     /* read all lines of sql stmt into a var  */
        fclose (fp);
        

	stat = runInfxFile(SQL_stmt,distance->answer, print_out);

	return stat ;
}

