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

    struct Option *keytable, *xcol, *ycol, *distance, *join, *xpos, *ypos, *hv;
    struct Flag *qtype;
    struct Sql *pts;
    int button ;
    int stat = 0;
    char  *SQL_stmt;
    double  atof ();
    char *print_out="";


	keytable = G_define_option() ;
	keytable->key        = "tab" ;
	keytable->type       = TYPE_STRING ;
	keytable->required   = YES  ;
	keytable->multiple   = NO ;
	keytable->description= "Name of the table with X,Y coord.:" ;

	xcol = G_define_option() ;
	xcol->key        = "xcol" ;
	xcol->type       = TYPE_STRING ;
	xcol->required   = YES  ;
	xcol->multiple   = NO ;
	xcol->description= "X coord. (E/W) column:" ;

	ycol = G_define_option() ;
	ycol->key        = "ycol" ;
	ycol->type       = TYPE_STRING ;
	ycol->required   = YES  ;
	ycol->multiple   = NO ;
	ycol->description= "Y coord. (N/S) column:" ;

	xpos = G_define_option() ;
	xpos->key        = "xpos" ;
	xpos->type       = TYPE_STRING ;
	xpos->required   = NO  ;
	xpos->multiple   = NO ;
	xpos->description= "X coord. (E/W) of search:" ;

	ypos = G_define_option() ;
	ypos->key        = "ypos" ;
	ypos->type       = TYPE_STRING ;
	ypos->required   = NO  ;
	ypos->multiple   = NO ;
	ypos->description= "Y coord. (N/S) of search:" ;


	distance = G_define_option() ;
	distance->key        = "distance" ;
	distance->type       = TYPE_STRING ;
	distance->required   = YES  ;
	distance->multiple   = NO ;
	distance->description= "Radius of cursor:" ;
/*
	join = G_define_option() ;
	join->key        = "join" ;
	join->type       = TYPE_STRING ;
	join->required   = NO  ;
	join->multiple   = NO ;
	join->key_desc	 = "tab,key,pkey";
	join->description= "**not used.** JOIN rules (eg. table,key,primekey). ";
*/
	hv = G_define_option() ;
	hv->key        = "hv" ;
	hv->type       = TYPE_STRING ;
	hv->answer     = "v" ;
	hv->description= "DB output type - [v(ert)/h(oriz)]:";



        /* Invoke parser */
        if (G_parser(argc, argv)) {
		system("d.what.s.pg help -s");
            	exit(-1);
	}

	print_out = hv->answer;
	
        /* Initialize screen graphics and get mouse input */


        SQL_stmt = (char*) buildInfxQry(keytable,ycol,xcol,distance,pts,join->answers);

        if ( (xpos->answer == NULL) || (ypos->answer == NULL)) 
           { stat = runInfxFile(SQL_stmt, distance->answer, print_out); }
        else
           {
     /* I'm lazy this pts stuff should be handled in getArea *.      
           
           /* Initialze SQL query structure 	*/
             pts = (struct Sql *)G_malloc(sizeof(struct Sql)) ;
             G_zero (pts, sizeof(struct Sql)) ;                         

 	     pts->centX = atof (xpos->answer);
 	     pts->centY = atof (ypos->answer);
	     if (distance->answer)  /* otherwise get perimeter with mouse  */
                pts->distance = atof(distance->answer);
             /* make interactive later */   

             pts->minX = pts->centX - pts->distance;
             pts->minY = pts->centY - pts->distance;
             pts->maxX = pts->centX + pts->distance;             
             pts->maxY = pts->centY + pts->distance;             

             stat = runqry (SQL_stmt,pts, print_out);
            }
       
        return stat ;

}

