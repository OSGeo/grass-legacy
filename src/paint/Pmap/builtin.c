#include "gis.h"

#include "misc.h"
#include "parms.h"

builtin_patterns()
{
    char name[20];
    int n,cat;

    parms.pattern[0] = NULL;
    for (n=1,cat = parms.pcolr.min; cat <= parms.pcolr.max; cat++,n++)
    {
	sprintf (name, "#%d", n);
	set_pattern (cat, name);
    }
}
