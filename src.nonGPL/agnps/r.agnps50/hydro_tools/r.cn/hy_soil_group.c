/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	int hy_soil_group(value)

	To get the corresponding column from the CN table based
	on the hydrological soil map layer and returns the column
*/

#include "CN.h"


int hy_soil_group(value)

int	value;
{
	char 	*label;

	label = G_get_cat(value, &hy_soil_group_cats);

	if (strcmp(label,"A") == 0) return(0);
	else if (strcmp(label,"B") == 0) return(1);
	else if (strcmp(label,"C") == 0) return(2);
	else if (strcmp(label,"D") == 0) return(3);
	else 
	{
	     fprintf (stderr,"unknown catagories other than A,B,C,D in %s map layer",hy_soil_group_name);
	     exit(-1);
	}
}
