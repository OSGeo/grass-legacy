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

   INGRES modifications - Name of subroutine has been changed:
			  runSQL <=> runInfxFile

   Improvements - The option 'key' has been added to restrict the
		  SQL selection to current category (i.e. the
  		  category indicated by cursor position).

   Katarina Johnsson 930413
*/

#define GLOBAL
#include "what.h"


getSelectOpts (argc, argv)
    int argc;
    char **argv;

{
        struct Option *sql, *map;
	struct Option *key;
        struct Flag *select;
        int button, i;
	int stat = 0 ;




        select = G_define_flag();
        select->key     = 's';
        select->description     = "Use [s] flag to select db records using an in put file." ;

        sql = G_define_option() ;
        sql->key        = "sql" ;
	sql->key_desc	= "file" ;
        sql->type       = TYPE_STRING ;
        sql->required   = YES  ;
        sql->multiple   = NO ;
        sql->description= "SQL statements specifying selection criteria. ";

        map = G_define_option() ;
        map->key        = "map" ;
	map->gisprompt   ="old,cell,raster" ;
        map->type       = TYPE_STRING ;
        map->required   = YES  ;
        map->multiple   = NO ;
        map->description= "Name of existing raster file.";


        /* Option key added to improve selection. KJ 930413 */
	key = G_define_option();
	key->key	= "key";
	key->type	= TYPE_STRING;
	key->required	= YES;
	key->multiple	= NO;
	key->description= "Column corresponding to cats in raster map [input]";
       
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

        R_open_driver();
        D_setup(0);
        if (( fd = opencell ( map->answer, name, mapset) ) >= 0)
        do
        {
                button=getCat(map->answer);
                if ( (button != 3) && (dbCat > 0) ) 
                       stat = runSQL(sql->answer,key->answer,dbCat);
        } while (button != 3);

        R_close_driver();

	return stat ;

}
