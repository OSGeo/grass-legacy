

#include <stdio.h>
#include	"trans.h"


print_transform_resids ( n_points)
	int  n_points ;
{
	int	i ;

	printf("\n\n\n");
	printf("                          CHECK MAP RESIDUALS\n\n");
	printf("                Current Map                  New Map\n");
	printf(" POINT      X coord    Y coord  |        X coord   Y coord    |      residuals\n");
	printf("\n");

	for (  i = 0 ;  i < n_points;  i++ )
 	{

		if ( use[i])
			printf (" %2d.  %12.2lf %12.2lf | %12.2lf   %12.2lf | %12.2lf\n",
			 i+1, ax[i], ay[i], bx[i], by[i], residuals[i]) ;
	
 	}
		
	printf("\n\n  Number of points: %d\n", n_points) ;
	printf("  Residual mean average   : %lf", rms) ;

	return (0) ;

}		/*  print_transform_resid()  */


