/* GetAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicoling that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/

#define GLOBAL
#include "what.h"


getAllOpts(argc, argv)
        int argc;
        char **argv;

{
    struct Option *map, *keytable, *col, *join;
    int button ;
    int stat = 0 ;

	map = G_define_option() ;
	map->key        = "map" ;
	map->gisprompt  = "old,cell,raster" ;
	map->type       = TYPE_STRING ;
	map->required   = YES  ;
	map->multiple   = NO ;
	map->description= "Name of raster map associated with database." ;

	keytable = G_define_option() ;
	keytable->key        = "tab" ;
	keytable->type       = TYPE_STRING ;
	keytable->required   = YES  ;
	keytable->multiple   = NO ;
	keytable->description= "Table containing column associated with raster category values." ;

	col = G_define_option() ;
	col->key        = "col" ;
	col->type       = TYPE_STRING ;
	col->required   = YES  ;
	col->multiple   = NO ;
	col->description= "Column associated with raster category values." ;


	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,key,pkey";
	join->description= "JOIN rules (eg. table,key,primekey). ";



        /* Invoke parser */
        if (G_parser(argc, argv)) {
		system("d.what.r.inf -s help") ;
            	exit(-1);
	}

	

        /* Initialize screen graphics and get mouse input */

        R_open_driver();
        D_setup(0);
        if (( fd = opencell ( map->answer, name, mapset) ) >= 0)
        do
        {
                button=getCat(map->answer);
                if ( (button != 3) && (dbCat > 0) )
                       stat = buildInfxQry(keytable,col, join->answers,dbCat);
        } while (button != 3);

        R_close_driver();
	exit(stat);
}
