#include <stdlib.h>
#include <string.h>
#include  "global.h"

/* returns index to array of values, inserts new if necessary */
int
find_cat (int cat)
{
    int i;

    for (i=0;i<vstat.rcat;i++)    
	if (Values[i].cat == cat) 
	    return i;

    /* Not found -> add new */
    Values[vstat.rcat].cat = cat;
    vstat.rcat++;
    
    return (vstat.rcat-1);
}

