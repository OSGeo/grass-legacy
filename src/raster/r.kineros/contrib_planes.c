#include <stdio.h>
#include "define.h"
#include "global.h"

contrib_planes()
{
    int ee;
/*
 *  Initialize.  In GRASS planes do not have any lateral contributions
 *  from other planes or channels.
 */
    for(ee=1;ee<=num_ele;ee++) {
	element[ee].plane[0] = 0;
	element[ee].plane[1] = 0;
	element[ee].plane[2] = 0;
    }
/*
 *  Assign plane elements to channels.
 */
    for(ee=3;ee<=num_ele;ee=ee+3) {
	if(element[ee].index > 0) {
	    element[ee].plane[1] = ee-1;
	    element[ee].plane[2] = ee-2;
/*
 *  Assign order to planes:
 */
            element[ee-1].order = element[ee].order - 1;
            element[ee-2].order = element[ee].order - 2;
	}
    }
}
