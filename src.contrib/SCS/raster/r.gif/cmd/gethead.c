/* %W% %G% */

#include "gis.h"
gethead (fd, cellhd)
    FILE *fd;
    struct Cell_head *cellhd;
{
    int n,s,e,w,r,c;
    long offset;
    char buf[300];
    char dummy[2];

    n=s=e=w=r=c=0;

    cellhd->zone = G_zone();
    cellhd->proj = G_projection();
    while (1)
    {
	offset = ftell (fd);
	if (!fgets(buf,sizeof buf, fd)) break;
	if (sscanf (buf, "north: %lf", &cellhd->north) == 1)
	    n++;
	else if (sscanf (buf, "south: %lf", &cellhd->south) == 1)
	    s++;
	else if (sscanf (buf, "east: %lf", &cellhd->east) == 1)
	    e++;
	else if (sscanf (buf, "west: %lf", &cellhd->west) == 1)
	    w++;
	else if (sscanf (buf, "rows: %d", &cellhd->rows) == 1)
	    r++;
	else if (sscanf (buf, "cols: %d", &cellhd->cols) == 1)
	    c++;
	else if (sscanf (buf, "%1s", dummy) == 1)	/* skip blank lines */
	    break;
    }
    if (n != 1 || s != 1 || e != 1 || w != 1 || r != 1 || c != 1)
	return 0;
    if (cellhd->north <= cellhd->south)
	return 0;
    if (cellhd->east <= cellhd->west)
	return 0;
    if (cellhd->rows <= 0 || cellhd->cols <= 0)
	return 0;
    
    cellhd->ns_res = (cellhd->north - cellhd->south)/cellhd->rows;
    cellhd->ew_res = (cellhd->east - cellhd->west)/cellhd->cols;

    fseek (fd, offset, 0);
    return 1;
}
