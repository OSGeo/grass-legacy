#include "gis.h"

int 
quick_range (char *name, char *mapset, long *min, long *max)
{
    struct Range range;

    if (G_read_range (name, mapset, &range) <= 0)
	return 0;
    if (range.max)
	*max = range.max;
    else
	*max = range.max;
    if (range.min)
	*min = range.min;
    else
	*min = range.min;
    return 1;
}

int 
slow_range (char *name, char *mapset, long *min, long *max)
{
    FILE *fd;
    int first;
    long n;
    int ok;
    char buf[512];

    *min = *max = 0;

    fprintf (stdout,"one moment ...\n");
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
