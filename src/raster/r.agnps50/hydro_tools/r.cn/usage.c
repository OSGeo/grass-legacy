/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	void usage(me)

	To print the usage of the program me for any error at the
	command line
*/

#include <stdio.h>

usage(me)
{
    char buf[300];

    sprintf (buf, "usage: %s sg=hydrological_soil_group_map lu=land_use_map\n pr=treatment_or_practice_map hc=hydrologic_condition_map\n cn=curve_number_map amc=AMC_condition_number(1,2 or 3)", me);
    G_fatal_error (buf);
    exit(1);
}
