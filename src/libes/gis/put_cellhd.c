/**********************************************************************
 *
 *  G_put_cellhd (name, cellhd)
 *      char *name                   name of map
 *      struct Cell_head *cellhd    structure holding cell header info
 *
 *  Writes the cell file header information associated with map layer "map"
 *  into current mapset from the structure "cellhd".
 *
 *  returns:     0  if successful
 *              -1  on fail
 *
 ***********************************************************************/

#include "gis.h"
int G_put_cellhd (char *name, struct Cell_head *cellhd)
{
    FILE *fd ;

    if (!(fd = G_fopen_new ("cellhd", name)))
    {
	char buf[1024];

	sprintf (buf, "Unable to create header file for [%s]", name);
	G_warning (buf);
	return -1;
    }

    G__write_Cell_head (fd, cellhd, 1);
    fclose (fd);

    return 0;
}
