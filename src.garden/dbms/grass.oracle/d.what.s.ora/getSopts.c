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
#include "infx.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;

{

        char *mapset;
        int button, i, j;

        struct Flag *select, *qtype;
        struct Option *sql, *distance ;
	struct Sql *pts;
	int stat = 0 ;



        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [s] flag to select db records using an input file." ;


        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = NO  ;
        sql->multiple   = NO ;
        sql->description= "SQL statements specifying selection criteria. ";


        distance = G_define_option() ;
        distance->key        = "distance" ;
        distance->type       = TYPE_STRING ;
        distance->required   = NO  ;
        distance->multiple   = NO ;
        distance->description= "Distance from mouse location to search." ;


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


        /* Initialze SQL query structure 	*/
        pts = (struct Sql *)G_malloc(sizeof(struct Sql)) ;
        G_zero (pts, sizeof(struct Sql)) ;                         


        if (distance->answer)  /* otherwise get perimeter with mouse  */
                pts->distance = atoi(distance->answer);


        /* Initialize screen graphics and get mouse input */

        R_open_driver();
        D_setup(0);
        do
        {
                button=getArea(pts);
                if (button != 3)
                       stat = runInfxFile(sql->answer, pts );

        } while (button != 3);

        R_close_driver();

	return stat ;
}

