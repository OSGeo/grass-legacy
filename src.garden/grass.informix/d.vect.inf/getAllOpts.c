/* getAllOpts.c - passes full range of program options to G_parser. If
		  the user uses the [-s] option indicating that he
		  is providing am input file with SQL commands the
		  function getSelectOpts is called in place of this
		  routine.

		  jaf 2/19/92
*/

#include "gis.h"
getAllOpts(argc, argv)
	int argc;
	char **argv;

{

	struct Option *key, *where, *tab, *map,*color, *sql, *join;

	char *mapset;
	int colr, retval;


	key = G_define_option() ;
        key->key        = "key" ;
        key->type       = TYPE_STRING ;
        key->required   = YES  ;
        key->multiple   = NO ;
        key->description= "Column corresponding to cats in vector map [map]" ;


        tab = G_define_option() ;
        tab->key        = "tab" ;
        tab->type       = TYPE_STRING ;
        tab->required   = YES  ;
        tab->multiple   = NO ;
        tab->description= "Table containing column to base query on." ;

        where = G_define_option() ;
        where->key        = "where" ;
        where->type       = TYPE_STRING ;
        where->required   = YES  ;
        where->multiple   = NO ;
        where->description= "Where clause for query (ie. where col='paved'). " ;

        map = G_define_option() ;
        map->key        = "map" ;
	map->gisprompt	= "old,dig,vector";
        map->type       = TYPE_STRING ;
        map->required   = YES  ;
        map->multiple   = NO ;
        map->description= "Name of existing vector file.";


        color = G_define_option() ;
        color->key        = "color" ;
        color->type       = TYPE_STRING ;
        color->required   = NO  ;
        color->multiple   = NO ;


        join = G_define_option() ;
        join->key        = "join" ;
        join->type       = TYPE_STRING ;
        join->required   = NO  ;
        join->multiple   = NO ;
        join->key_desc   = "tab,tabkey,pkey" ;
        join->description= "JOIN rules (eg. table,key,primekey). ";

	/* Invoke parser */
	if (G_parser(argc, argv)) {
		system("d.vect.inf -s help");
	    	exit(-1);
	}

	if (color->answer == NULL) 
		colr = D_translate_color("white");
	  else
		colr = D_translate_color(color->answer);

	if ((mapset=G_find_file2("dig",map->answer,""))==NULL)  {
	     fprintf(stderr,"Vector file %s not found.\n",map->answer);
             exit(-1);
	}


	retval = buildInfxQry(key->answer,where->answer,tab->answer,
		map->answer, join->answers, mapset, colr);
		

	exit(retval) ;


}

