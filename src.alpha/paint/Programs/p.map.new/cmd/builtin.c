#include "gis.h"

#include "misc.h"
#include "parms.h"

builtin_patterns()
{
    char name[20];
    int n,cat;

    set_pattern (0, NULL);
    for (n=1,cat = parms.min_color; cat <= parms.max_color; cat++,n++)
    {
	sprintf (name, "#%d", n);
	set_pattern (cat, name);
    }
}
