
#include "gis.h"

quick_range (name, mapset, min, max)
    char *name, *mapset;
    long *min, *max;
{
    struct Range range;

    if (G_read_range (name, mapset, &range) <= 0)
	return 0;
    if (range.pmax)
	*max = range.pmax;
    else
	*max = range.nmax;
    if (range.nmin)
	*min = range.nmin;
    else
	*min = range.pmin;
    return 1;
}

slow_range (name, mapset, min, max)
    char *name, *mapset;
    long *min, *max;
{
    FILE *fd, *popen();
    int first;
    long n;
    int ok;
    char buf[512];

    *min = *max = 0;

    printf ("one moment ...\n");
    sprintf (buf, "Gdescribe -r -1 '%s in %s'",name,mapset);
    fd = popen (buf,"r");
    if (fd == NULL)
	return 0;
    ok = 1;
    first = 1;
    while (ok && fgets(buf,sizeof(buf),fd))
    {
	ok = (sscanf (buf, "%ld", &n)==1);
	if (!ok) break;
	if (n==0) continue;
	*max = n;
	if (first) *min = n;
	first = 0;
    }
    pclose (fd);
    if (!ok)
	*min = *max = 0;
    return ok;
}
