/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicoling that he
                  is providing am input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/

#include "what.h"
#include "gis.h"
#include "display.h"
#include "Vect.h"


int getAllOpts(argc, argv)
        int argc;
        char **argv;

{

    char *openvect();
    int button;
    int level;
    int stat = 0 ;
    struct Map_info P_map;
    struct Categories Cats;
    char  *SQL_stmt;
    char  *print_out="";
    
    int colr; 
    int fillcolr=0;
    struct Option *map, *keytable, *col, *color, *fillcolor, *hv;
    struct Flag *flag1;
    int fill=0;

	
	
	map = G_define_option() ;
	map->key        = "map" ;
	map->gisprompt  = "old,dig,vector" ;
	map->type       = TYPE_STRING ;
	map->required   = YES  ;
	map->multiple   = NO ;
	map->description= "Vector map to run query on:" ;

	keytable = G_define_option() ;
	keytable->key        = "tab" ;
	keytable->type       = TYPE_STRING ;
	keytable->required   = YES  ;
	keytable->multiple   = NO ;
	keytable->description= "Postgres table with categories:" ;

	col = G_define_option() ;
	col->key        = "col" ;
	col->type       = TYPE_STRING ;
	col->required   = YES  ;
	col->multiple   = NO ;
	col->description= "Column with categories from this table:" ;

	color = G_define_option() ;
        color->key        = "color" ;
        color->type       = TYPE_STRING ;
        color->required   = NO  ;
        color->multiple   = NO ;
	color->description= "Selected lines color:";
	color->answer="yellow";
	
	flag1 = G_define_flag() ;
	flag1->key         = 'f' ;
	flag1->description = "Fill polygons?" ;
	
	fillcolor = G_define_option() ;
	fillcolor->key        = "fillcolor" ;
	fillcolor->type       = TYPE_STRING ;
	fillcolor->answer     = "gray" ;
	fillcolor->options    = D_color_list();
	fillcolor->description= "Selected areas color (for fill):";
	
	hv = G_define_option() ;
	hv->key        = "hv" ;
	hv->type       = TYPE_STRING ;
	hv->answer     = "v" ;
	hv->description= "Type of database output - [h/v]:";
	

	/* Invoke parser */
	if (G_parser(argc, argv)) {
		system("d.what.v.pg -s help") ;
	    	exit(-1);
 	}       

	name = map->answer;
	print_out = hv->answer;
	fill = flag1->answer;

	/* Initialize screen graphics and get mouse input */
	
	R_open_driver();
	colr = D_translate_color(color->answer);
	fillcolr = D_translate_color(fillcolor->answer);
	D_setup(0);
	
	if ( (mapset = openvect(name) ) == NULL) {
		fprintf(stderr, "Unable to open %s\n", map->answer);
		exit(1);
	}
	

	level = Vect_open_old( &P_map, name, mapset) ;
	if (level < 0)
                G_fatal_error ("Can't open vector file");
        if (level < 2)
                G_fatal_error ("You must first run v.support on vector file");

	if (G_read_vector_cats(name, mapset, &Cats) < 0)
		Cats.num = -1;
	
	h_num=1;
	
	do
	{
        	button=getCat(&P_map, &Cats,colr,fillcolr,fill);
		if ( (button != 3) && (dbCat > 0) ){ 
		SQL_stmt = (char *) buildInfxQry(keytable, col,dbCat);
		stat = runInfxQry(SQL_stmt, print_out);
		}
	} while (button != 3);

	
	R_close_driver();
	Vect_close(&P_map);
	exit(stat) ;

}
