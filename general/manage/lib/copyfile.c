#include "gis.h"
#include <unistd.h>
int 
copyfile (char *element, char *old, char *mapset, char *new)
{
    char buf[1024];
    int in, out;
    int ix, ox;

    in = out = -1;

    in = G_open_old (element, old, mapset);
    if (in < 0)
	return -1;
    out = G_open_new (element, new);
    if (out < 0)
    {
	close (in);
	return -2;
    }
    while ((ix = read (in, buf, sizeof buf)) > 0)
	if ((ox = write (out, buf, ix)) != ix)
	    break;

    close (in);
    close (out);

    if (ix < 0)
	return -3;
    if (ix != 0 && ix != ox)
	return -4;
    return 0;
}
