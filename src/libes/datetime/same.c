#include "datetime.h"

static 
int isequal (char *src, char *dst, int n)
{
    while (n-- > 0)
	if(*dst++ != *src++)
	    return(0);
    return(1);
}

int 
datetime_is_same (DateTime *src, DateTime *dst)
{
    return( isequal ((char *)src, (char *)dst, sizeof(DateTime)) );
}
