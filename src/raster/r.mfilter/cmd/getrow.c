#include "gis.h"
#include "glob.h"

getmaprow (fd, buf, row, len)
    char *buf;
{
    if (G_get_map_row (fd, buf, row) < 0)
	exit(1);
    return 1;
}

getrow (fd, buf, row, len)
    char *buf;
{
    if (direction > 0)
	lseek (fd, (long) row*len, 0);
    else
	lseek (fd, (long) (nrows-row-1)*len, 0);
    if (read (fd, buf, len) != len)
	G_fatal_error ("error reading temporary file");
    return 1;
}
