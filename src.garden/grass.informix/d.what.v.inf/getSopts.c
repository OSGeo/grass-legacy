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

#include "what.h"
#include "digit.h"

getSelectOpts(argc, argv)
        int argc;
        char **argv;

{

    char *openvect();
    int button;
    int level;
    int stat = 0 ;
    struct Map_info Map;
    struct Categories Cats;

      struct Option *sql, *map;
        struct Flag *select;
        int i;




        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [-s] flag to select db records using an input file." ;

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL statements specifying selection criteria. ";

        map = G_define_option() ;
        map->key        = "map" ;
	map->gisprompt  = "old,dig,vector" ;
        map->type       = TYPE_STRING ;
        map->required   = YES  ;
        map->multiple   = NO ;
        map->description= "Name of existing vector file.";

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


        /* Initialize screen graphics and get mouse input */

	name = map->answer;

        if ( (mapset = openvect(name) ) == NULL) {
                fprintf(stderr, "Unable to open %s\n", map->answer);
                exit(1);
        }

        R_open_driver();
        D_setup(0);

        level = Vect_open_old( &Map, name, mapset) ;
        if (level < 0)
                G_fatal_error ("Can't open vector file");
        if (level < 2)
                G_fatal_error ("You must first run v.support on vector file");

        if (G_read_vector_cats(name, mapset, &Cats) < 0)
                Cats.num = -1;
        do
        {
                button=getCat(&Map, &Cats);
                if ( (button != 3) && (dbCat > 0) )
                        stat = runInfxFile(sql->answer,dbCat);
        } while (button != 3);

        R_close_driver();
        Vect_close(&map);

	exit(stat) ;
}
