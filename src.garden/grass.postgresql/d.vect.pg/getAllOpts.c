/* getAllOpts.c - passes full range of program options to G_parser. If
		  the user uses the [-s] option indicating that he
		  is providing am input file with SQL commands the
		  function getSelectOpts is called in place of this
		  routine.

		  jaf 2/19/92
*/
/*		 alex shevlakov -Jan 00 Coloring functions added
*/

#include "gis.h"


getAllOpts(argc, argv)
	int argc;
	char **argv;
	
{

	struct Option *key, *where, *tab, *map,*color;
	struct Flag *flag1;
	char *mapset;
	int colr, fillcolr, retval;


	key = G_define_option() ;
        key->key        = "key" ;
        key->type       = TYPE_STRING ;
        key->required   = YES  ;
        key->multiple   = NO ;
        key->description= "Column with category IDs from the table:" ;


        tab = G_define_option() ;
        tab->key        = "tab" ;
        tab->type       = TYPE_STRING ;
        tab->required   = YES  ;
        tab->multiple   = NO ;
        tab->description= "Table containing this column:" ;

        where = G_define_option() ;
        where->key        = "where" ;
        where->type       = TYPE_STRING ;
        where->required   = NO  ;
        where->multiple   = NO ;
        where->description= "Query clause (e.g. obj='paved'):" ;

        map = G_define_option() ;
        map->key        = "map" ;
	map->gisprompt	= "old,dig,vector";
        map->type       = TYPE_STRING ;
        map->required   = YES  ;
        map->multiple   = NO ;
        map->description= "Vector map:";


        color = G_define_option() ;
        color->key        = "color" ;
        color->type       = TYPE_STRING ;
        color->required   = NO  ;
        color->multiple   = NO ;
	
	flag1 = G_define_flag() ;
	flag1->key         = 'f' ;
	flag1->description = "Fill polygons" ;

	/* Invoke parser */
	if (G_parser(argc, argv)) {
		system("d.vect.pg -s help");
	    	exit(-1);
	}

	if (color->answer == NULL) 
		colr = D_translate_color("white");
	  else
		colr = D_translate_color(color->answer);
	
	
	fillcolr = flag1->answer;
	
	
	if ((mapset=G_find_file2("dig",map->answer,""))==NULL)  {
	     fprintf(stderr,"Vector map %s not found.\n",map->answer);
             exit(-1);
	}

	retval = buildInfxQry(key->answer,where->answer,tab->answer,
		map->answer, mapset, colr, fillcolr);
		
	exit(retval) ;


}

