/* @(#)do_dots.c	2.1   6/26/87 */
#include "ply_to_cll.h"

do_dots(xarray, yarray, num_verticies, category )
	double xarray[], yarray[] ;
	int num_verticies ;
	int category ;
{
	int node ;

	for (node=0; node<num_verticies; node++)
	{
		write_record(
			(int)(yarray[node]+.5),
			(float)xarray[node],
			(float)xarray[node],
			category ) ;
	}
}
