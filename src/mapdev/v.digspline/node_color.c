/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

/* return translated color for node given number of lines attached to node */
dig_node_color (num)
    int num;
{
    register int color;

    if (num < 0)
	color = YELLOW;
    else
	switch (num) {
	    case 0: 
		color = CLR_0_NODE;
		break;
	    case 1: 
		color = CLR_1_NODE;
		break;
	    default:
		color = CLR_2_NODE;
		break;
	}
    return (color);
}
