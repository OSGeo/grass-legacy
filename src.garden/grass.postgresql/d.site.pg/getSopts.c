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
        static char   SQL_stmt [1024];
	int colr, i;

        struct Option *sql, *map, *plot ;
        struct Flag *select;
        FILE*	fp;



        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [-s] flag to select db records using an in put file." ;


        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "postQUEL statements specifying selection criteria. ";

        map = G_define_option() ;
        map->key        = "map" ;
        map->type       = TYPE_STRING ;
        map->required   = NO  ;
        map->multiple   = NO ;
        map->description= "Name of sites list to output.";


        plot = G_define_option() ;
        plot->key        = "plot" ;
        plot->type       = TYPE_STRING ;
        plot->required   = NO  ;
        plot->multiple   = NO ;
        plot->key_desc    ="Color,icon,size" ;
        plot->answer      ="gray,x,3" ;
        plot->description= "Colors:red,orange,yellow,green,blue,indigo,violet,magenta,brown,gray,white,black; Icon: diamond, box, plus, x; Size: 1-9. ";



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

           
        if((fp = fopen(sql->answer,"r")) == NULL) {
            fprintf(stderr, "File read error on %s\n",sql->answer);
            exit(-1);
           }
 
        fread(SQL_stmt,1024,1,fp); 
     /* read all lines of sql stmt into a var  */
        fclose (fp);
        
        i = runInfxFile( SQL_stmt, map->answer, plot->answers );
	return(i) ;

}
