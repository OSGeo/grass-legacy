#include "gis.h"
int 
parse_line (char *key, char **s, double *e1, double *n1, double *e2, double *n2, int projection)
{
    int err;
    err = 0;
    if(!G_scan_easting (s[0], e1, projection))
	err |= 1;
    if(!G_scan_northing (s[1], n1, projection))
	err |= 2;
    if(!G_scan_easting (s[2], e2, projection))
	err |= 4;
    if(!G_scan_northing (s[3], n2, projection))
	err |= 8;

    if(err)
    {
	fprintf (stderr, "%s=", key);
	fprintf (stderr, "%s%s%s,",
	    err&1?"<":"", s[0], err&1?">":"");
	fprintf (stderr, "%s%s%s,",
	    err&2?"<":"", s[1], err&2?">":"");
	fprintf (stderr, "%s%s%s,",
	    err&4?"<":"", s[2], err&4?">":"");
	fprintf (stderr, "%s%s%s",
	    err&8?"<":"", s[3], err&8?">":"");
	fprintf (stderr, " ** invalid coordinate(s) **\n");
    }
    return err;
}
