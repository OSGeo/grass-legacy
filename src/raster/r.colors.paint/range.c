
#include "gis.h"

int 
quick_range (char *name, char *mapset, long *min, long *max)
{
    struct Range range;
    CELL cmin, cmax;

    if (G_read_range (name, mapset, &range) <= 0)
	return 0;
    G_get_range_min_max(&range, &cmin, &cmax);
    *min = (long) cmin; *max = (long) cmax;
    return 1;
}

int 
slow_range (char *name, char *mapset, long *min, long *max)
{
    FILE *fd, *popen();
    int first;
    long n;
    int ok;
    char buf[512];

    *min = *max = 0;

    fprintf (stdout,"one moment ...\n");
    sprintf (buf, "r.stats -r -1 '%s in %s'",name,mapset);
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
