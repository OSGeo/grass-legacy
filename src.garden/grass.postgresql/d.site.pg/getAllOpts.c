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
    struct Option  *tab, *coordx, *coordy, *cats,*where, *plot, *join, *map ;

	retval = 0; 

	tab = G_define_option() ;
	tab->key        = "tab" ;
	tab->type       = TYPE_STRING ;
	tab->required   = YES  ;
	tab->multiple   = NO ;
	tab->description= "Table with X,Y coors:" ;


	coordx = G_define_option() ;
	coordx->key        = "coordx" ;
	coordx->type       = TYPE_STRING ;
	coordx->required   = YES  ;
	coordx->multiple   = NO ;
	coordx->key_desc   = "column of X coordinates" ;
	coordx->answer	   = "X";
	coordx->description= "Column with X-coor:" ;

	coordy = G_define_option() ;
	coordy->key        = "coordy" ;
	coordy->type       = TYPE_STRING ;
	coordy->required   = YES  ;
	coordy->multiple   = NO ;
	coordy->key_desc   = "column of Y coordinates" ;
	coordy->answer	   = "Y";
	coordy->description= "Column with Y-coor:" ;

	cats = G_define_option() ;
	cats->key        = "cats" ;
	cats->type       = TYPE_STRING ;
	cats->required   = NO  ;
	cats->multiple   = NO ;
	cats->key_desc   = "category-column" ;
//-- A.Sh	cats->answer	  = "cat";
	cats->description= "Column with categories:" ;

	where = G_define_option() ;
	where->key        = "where" ;
	where->type       = TYPE_STRING ;
	where->required   = NO  ;
	where->multiple   = NO ;
	where->description= "Clause for SQL query (e.g. obj='huts', num > 56, etc.):" ;

	map = G_define_option() ;
	map->key        = "map" ;
	map->type       = TYPE_STRING ;
	map->required   = NO  ;
	map->multiple   = NO ;
	map->description= "New site map name:";


	plot = G_define_option() ;
	plot->key        = "plot" ;
	plot->type       = TYPE_STRING ;
	plot->required   = NO  ;
	plot->multiple   = NO ;
	plot->key_desc	  ="Color,icon,size" ;
	plot->answer	  ="gray,x,3" ;

plot->description="Colors:red,orange,yellow,green,blue,indigo,violet,magenta,brown,gray,white,black;Icon:diamond, box, plus, x; Size: 1-9. ";


	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,tabkey,pkey" ;
	join->description= "**obsolete.** JOIN rules (напр. table,key,primekey).\n\tNot used in this interface.";


        /* Invoke parser */
        if (G_parser(argc, argv)) {
	    system("d.site.pg -s help");
            exit(-1);
	}

/*************** INFX driver code begins ***************/
        retval = buildInfxQry(tab->answer,coordx->answer,coordy->answer,
                cats->answer,where->answer,
                map->answer, join->answers, plot->answers);


	return(retval) ;
}
