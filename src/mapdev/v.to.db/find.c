#include <stdlib.h>
#include <string.h>
#include  "global.h"

int
find_cat (int cat)
{
    int i;

    switch (options.list) {
        case LIST_CI:
	    for (i=0;i<vstat.rcat;i++)    
		if (list_ci[i].cat == cat) 
		    return i;
    	    break;
        case LIST_CD:
	    for (i=0;i<vstat.rcat;i++)    
		if (list_cd[i].cat == cat) 
		    return i;
    	    break;
	case LIST_CI2D:
	    for (i=0;i<vstat.rcat;i++)    
		if (list_ci2d[i].cat == cat) 
		    return i;    	    
	    break;
        case LIST_CC:
	    for (i=0;i<vstat.rcat;i++)    
		if (list_cc[i].cat == cat) 
		    return i;
    	    break;
    } 
    return (-1);
}

