#include "globals.h"

#define QUOTE '\''

void
dup_single_quotes (src, dst)
    char *src;
    char *dst;
{
    while (*src)
    {
	if(*src == QUOTE)
	    *dst++ = QUOTE;
	*dst++ = *src++;
    }
    *dst = 0;
}
