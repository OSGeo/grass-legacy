#include "list.h"

get_description_len (n)
{
    int len;
    int l;
    int i;

    len = 1;
    for (i = 0; i < list[n].nelem; i++)
    {
	l = strlen (list[n].desc[i]);
	if (l > len) len = l;
    }
    return len;
}
