#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include "gis.h"
#include "glob.h"

int getmaprow (int fd,CELL *buf,int row,int len)
{
    if (G_get_map_row (fd, buf, row) < 0)
	exit(1);
    return 1;
}

int getrow (int fd, CELL *buf, int row, int len)
{
    if (direction > 0)
	lseek (fd, (long) row*len, 0);
    else
	lseek (fd, (long) (nrows-row-1)*len, 0);
    if (read (fd, buf, len) != len)
	G_fatal_error ("error reading temporary file");
    return 1;
}
