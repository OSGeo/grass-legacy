

#include <stdio.h>
#include	"trans.h"


int 
print_transform_resids (int n_points)
{
	int	i ;

	fprintf (stdout,"\n\n\n");
	fprintf (stdout,"                          CHECK MAP RESIDUALS\n\n");
	fprintf (stdout,"                Current Map                  New Map\n");
	fprintf (stdout," POINT      X coord    Y coord  |        X coord   Y coord    |      residuals\n");
	fprintf (stdout,"\n");

	for (  i = 0 ;  i < n_points;  i++ )
 	{

		if ( use[i])
			fprintf (stdout," %2d.  %12.2f %12.2f | %12.2f   %12.2f | %12.2f\n",
			 i+1, ax[i], ay[i], bx[i], by[i], residuals[i]) ;
	
 	}
		
	fprintf (stdout,"\n\n  Number of points: %d\n", n_points) ;
	fprintf (stdout,"  Residual mean average   : %f", rms) ;

	return (0) ;

}		/*  print_transform_resid()  */


