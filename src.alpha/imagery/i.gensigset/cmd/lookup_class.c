#include "gis.h"
/* build index of cell values from list */
/* -1 means not found in list */
lookup_class(cats, ncats, list, nlist, class)
    CELL *cats,   /* input: category numbers to lookup */
	 *list,   /* input: list of known categories (sorted) */
         *class;  /* output: resultant class - where each cat is found in list */
    int  ncats,   /* input: number of categories to translate */
	 nlist;   /* input: "size" of the list - number of cats in list. */
{
    int left, right, cur;
    CELL c;

    while (ncats-- > 0)
    {
	c = *cats++;	/* extract the category */
	if (c == 0)
	{
	    *class++ = -1;
	    continue;
	}
	left = 0;
	right = nlist - 1;
	for(;;)
	{
	    cur = (left+right)/2;
	    if (c < list[cur])
		right = cur-1;
	    else
		left = cur+1;
	    if (c == list[cur])
	    {
		*class++ = cur;
		break;
	    }
	    else if (left > right)
	    {
		*class++ = -1;	/* this should never happen */
		break;
	    }
	}
    }
}
