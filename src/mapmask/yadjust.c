/* %W% %G% */
#include "mapmask.h"

/* FIRST  change all Y grid coordinates to array coordinates.
 *        (add .5 to all Y grid coordinates)
 *        X array coordinates are simply (int)(GRID_X)
 * THEN   add .0001 to all integer y values 
 *        This takes care of all sorts of evils and time later on  
 */
yadjust(num_verticies)
    int num_verticies ;
{
    register int row, incr ;
    register double *pointer ;

    for(pointer = Ay, incr=0; incr<=num_verticies; incr++, pointer++)
    {
	*pointer += .5 ;
	row = *pointer ;
	if (*pointer == (double)row )
	    *pointer += .0001 ;
    }
}
