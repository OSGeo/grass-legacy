
#include "map.h"

double	ax[MAX_COOR] ;		/*  table (digitizer)  */
double	ay[MAX_COOR] ;

double	bx[MAX_COOR] ;		/*  map  */
double	by[MAX_COOR] ;

int	reg_cnt ;		/*  count of registered points */
int	use[MAX_COOR] ;		/*  where the coordinate came from */
double	residuals[MAX_COOR], 	rms ;

