/*****************************************************************************/
/***                                                                       ***/
/***                            write_site()                               ***/
/***           Writes out surface info in GRASS `sites' format.		   ***/
/***           Jo Wood, Project ASSIST, V1.0 7th February 1993             ***/
/***                                                                       ***/
/*****************************************************************************/

#include "feature.h"

write_site(easting,northing,elev,feature)
double 	easting,
	northing;
CELL	elev,
	feature;
{
	printf("%.0lf|%.0lf|%d| ",easting,northing,elev);

	if (feature == PIT)
	    printf("#Pit\n");
  	else
	    if (feature == PEAK)
		printf("#Peak\n");
	    else
		if (feature == PASS)
		    printf("#Pass\n");
		    	else
			    printf("#\n");
	
}
