/* getAllOpts.c - passes full range of program options to G_parser. If
                  the user uses the [-s] option indicating that he
                  is providing an input file with SQL commands the
                  function getSelectOpts is called in place of this
                  routine.

                  jaf 2/21/92
*/

#include <stdio.h>
#include "gis.h"
#include "infx.h"



getAllOpts(argc, argv)
        int argc;
        char **argv;

{

    struct Option *keytable, *xcol, *ycol, *distance, *join;
    struct Flag *qtype;
    struct Sql *pts;
    int button ;
    int stat = 0;



	keytable = G_define_option() ;
	keytable->key        = "tab" ;
	keytable->type       = TYPE_STRING ;
	keytable->required   = YES  ;
	keytable->multiple   = NO ;
	keytable->description= "Name of table containing X,Y coordinates." ;

	xcol = G_define_option() ;
	xcol->key        = "xcol" ;
	xcol->type       = TYPE_STRING ;
	xcol->required   = YES  ;
	xcol->multiple   = NO ;
	xcol->description= "X coordinate (E/W) column in keytable." ;

	ycol = G_define_option() ;
	ycol->key        = "ycol" ;
	ycol->type       = TYPE_STRING ;
	ycol->required   = YES  ;
	ycol->multiple   = NO ;
	ycol->description= "Y coordinate (N/S) column in keytable." ;


	distance = G_define_option() ;
	distance->key        = "distance" ;
	distance->type       = TYPE_STRING ;
	distance->required   = YES  ;
	distance->multiple   = NO ;
	distance->description= "Distance from mouse location to search." ;

	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,key,pkey";
	join->description= "JOIN rules (eg. table,key,primekey). ";



        /* Invoke parser */
        if (G_parser(argc, argv)) {
		system("d.what.s.inf help -s");
            	exit(-1);
	}

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
                       stat = buildInfxQry(keytable,ycol,xcol,distance,pts,
                                join->answers);

        } while (button != 3);

        R_close_driver();

	return stat ;

}

