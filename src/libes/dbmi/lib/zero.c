#include "dbmi.h"

void
db_zero(s, n)
    char *s;
{
    while (n-- > 0)
	*s++ = 0;
}
