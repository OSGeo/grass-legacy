/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	void usage(me)

	To print an error message if there is any at the command line
*/

#include <stdio.h>

usage(me)
{
    char buf[300];

	sprintf (buf, "usage: %s input=curve_number_map output=weighted_cn_map", me);
	G_fatal_error (buf);
	exit(1);
}
