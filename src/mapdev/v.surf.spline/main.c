/**************************************************************************/
/***									***/
/***				v.surf.spline				***/
/*** GRASS module to interpolate vector contour data using constrained	***/
/*** spline fitting. Can be used to calculate RMS error for each of the	***/
/*** interpolated cells (Yeoli, 1986).					***/
/***									***/
/*** Jo Wood, Department of Geography, V1.0 5th December, 1991		***/
/*** V2.0 Modified to conform to GRASS module structure, 23rd July 1995	***/
/***									***/
/**************************************************************************/

#define MAIN 

#include "spline.h"

main(argc,argv) 
    int argc;
    char *argv[];
{
    /*------------------------------------------------------------------*/
    /*				INITIALISE				*/
    /*------------------------------------------------------------------*/

    struct Map_info	vmap;		/* Vector map information.	*/
    interface(argc,argv);		/* Parse command line input.	*/

    /*------------------------------------------------------------------*/
    /*				OPEN FILES				*/
    /*------------------------------------------------------------------*/

    open_vect(vmap);

    /*------------------------------------------------------------------*/
    /*			INTERPOLATE VECTOR CONTOURS			*/
    /*------------------------------------------------------------------*/

    process();
}
