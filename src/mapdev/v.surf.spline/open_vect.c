/**************************************************************************/
/***				open_vect()				***/
/*** 		Opens vector file and checks that it has topology. 	***/
/***									***/
/*** Jo Wood, Department of Geography, V1.0 5th December, 1991		***/
/*** V2.0 Modified to conform to GRASS module structure, 23rd July 1995	***/
/***									***/
/**************************************************************************/

#include "spline.h"

open_vect(vmap) 
    struct Map_info vmap;	/* Stores vector information.		*/
{
    if (Vect_open_old(&vmap,vect_in_name,mapset_in)<2)
    {
	char err[256];
	sprintf(err,"Vector file %s needs topology building.\nRun v.support.",
		vect_in_name);
	G_fatal_error(err);
    }
    Vect_close(&vmap);
}
