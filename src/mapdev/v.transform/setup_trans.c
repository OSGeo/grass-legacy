
/*  
*  Functions in this file:
*     setup_transform ( n_points)
*         returns:  ALL_OK, POINTS_NOT_SPREAD, NEED_MORE_POINTS
*     init_transform_arrays()
*     print_transform_error(stat)
*
*  Written by the GRASS Team, 02/16/90, -mh .
*/


#include	<stdio.h>
#include	"trans.h"

setup_transform ( n_points)
	int  n_points ;
{

	int  status ;


	/*  compute_transformation_coef() returns:
	*   -2,  Not enough points
	*   1,   everything is okay
	*   -1,  points weren't spread out enough
	*/

	/*  if there are enough points registered compute residuals  */
	if(n_points >= MIN_COOR)
		status = compute_transformation_coef (ax, ay, bx, by, use, MAX_COOR) ;
	else
		return(NEED_MORE_POINTS) ;

	if(status != ALL_OK)
		return(POINTS_NOT_SPREAD) ;

	residuals_a_predicts_b (ax, ay, bx, by, use, MAX_COOR, residuals, &rms) ;

	return (ALL_OK) ;


}			/*  setup_transform()   */

init_transform_arrays()
{
	int i ;

/*  initiliaze use[],  no points valid  */
for (i=0 ;  i<MAX_COOR ;  ++i)
 {
	*(use+i) = 0 ;  	*(bx+i) = 0 ;  	*(by+i) = 0 ;
	*(residuals+i) = 0 ;
 }

reg_cnt = 0 ;
}


print_transform_error(stat)
    int stat ;
{
    char buff[128];

	switch(stat)
	{
		case POINTS_NOT_SPREAD:
			printf("  The points weren't spread out enough.\n\n");
			break ;
		case NEED_MORE_POINTS:
			printf("  You need to enter more points.") ;
			printf("  You need at least %d points\n", MIN_COOR);
			break ;
		default:
			printf("\n\n  Program ERROR:  Your calling print_transform_error() with no error.\n");
			break ;
	}


printf("\n\n\n\n                    <Hit return to continue>");
if (NULL == gets(buff))
	exit(-1) ;

}


