#include "table.h"

get_params()
{
    int i,n;

    n = 0;
    for(i = 0; i < 7; i++)
	if(param[i][0])
	    n++;

    return(n);
}
