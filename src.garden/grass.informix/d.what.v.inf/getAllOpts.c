/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicoling that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/

#include "what.h"
#include "digit.h"

getAllOpts(argc, argv)
        int argc;
        char **argv;

{

    char *openvect();
    int button;
    int level;
    int stat = 0 ;
    struct Map_info Map;
    struct Categories Cats;

    struct Option *map, *keytable, *col, *join;


	map = G_define_option() ;
	map->key        = "map" ;
	map->gisprompt  = "old,dig,vector" ;
	map->type       = TYPE_STRING ;
	map->required   = YES  ;
	map->multiple   = NO ;
	map->description= "Name of vector map associated with database." ;

	keytable = G_define_option() ;
	keytable->key        = "tab" ;
	keytable->type       = TYPE_STRING ;
	keytable->required   = YES  ;
	keytable->multiple   = NO ;
	keytable->description= "Name of table containing column with category values." ;

	col = G_define_option() ;
	col->key        = "col" ;
	col->type       = TYPE_STRING ;
	col->required   = YES  ;
	col->multiple   = NO ;
	col->description= "Name of column with category values." ;


	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,key,pkey";
	join->description= "JOIN rules (eg. table,key,primekey). ";


	/* Invoke parser */
	if (G_parser(argc, argv)) {
		system("d.what.v.inf -s help") ;
	    	exit(-1);
 	}       

	name = map->answer;

	/* Initialize screen graphics and get mouse input */

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
		stat =	buildInfxQry(keytable, col, join->answers,dbCat);
	} while (button != 3);

	R_close_driver();
	Vect_close(&map);
	exit(stat) ;

}
