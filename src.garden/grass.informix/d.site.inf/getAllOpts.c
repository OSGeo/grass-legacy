/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicating that he
                  is providing an input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/19/92
*/

#include "gis.h"
getAllOpts(argc, argv)
        int argc;
        char **argv;

{

    int retval ;
    struct Join *joinargs;
    struct Option  *tab, *coords, *where, *plot, *join, *map ;

	retval = 0; 

	tab = G_define_option() ;
	tab->key        = "tab" ;
	tab->type       = TYPE_STRING ;
	tab->required   = YES  ;
	tab->multiple   = NO ;
	tab->description= "Table containing X,Y coordinate values." ;


	coords = G_define_option() ;
	coords->key        = "coords" ;
	coords->type       = TYPE_STRING ;
	coords->required   = YES  ;
	coords->multiple   = NO ;
	coords->key_desc   = "X-column,Y-column" ;
	coords->description= "Column containing coordinate values." ;

	where = G_define_option() ;
	where->key        = "where" ;
	where->type       = TYPE_STRING ;
	where->required   = YES  ;
	where->multiple   = NO ;
	where->description= "Where clause for query (eg. where col='paved', col=56). " ;

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
	plot->key_desc	  ="Color,icon,size" ;
	plot->answer	  ="gray,x,3" ;
	plot->description= "Colors:red,orange,yellow,green,blue,indigo,violet,magenta,brown,gray,white,black; Icon: diamond, box, plus, x; Size: 1-9. ";


	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,tabkey,pkey" ;
	join->description= "JOIN rules (eg. table,key,primekey). ";


        /* Invoke parser */
        if (G_parser(argc, argv)) {
	    system("d.site.inf -s help");
            exit(-1);
	}

/*************** INFX driver code begins ***************/
        retval = buildInfxQry(tab->answer,coords->answers,where->answer,
                map->answer, join->answers, plot->answers);


	return(retval) ;
}
