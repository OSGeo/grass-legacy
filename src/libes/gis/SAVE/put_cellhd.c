/* %W% %G% */
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
G_put_cellhd (name, cellhd)
    char *name ;
    struct Cell_head *cellhd ;
{
    FILE *fd ;

    if (!(fd = G_fopen_new ("cellhd", name)))
	return -1;


    fprintf(fd,"proj:       %d\n",  cellhd->proj);
    fprintf(fd,"zone:       %d\n",  cellhd->zone);
    fprintf(fd,"north:      %lf\n", cellhd->north);
    fprintf(fd,"south:      %lf\n", cellhd->south);
    fprintf(fd,"west:       %lf\n", cellhd->west);
    fprintf(fd,"east:       %lf\n", cellhd->east);
    fprintf(fd,"n-s resol:  %lf\n", cellhd->ns_res);
    fprintf(fd,"e-w resol:  %lf\n", cellhd->ew_res);
    fprintf(fd,"format:     %d\n",  cellhd->format);
    fprintf(fd,"compressed: %d\n",  cellhd->compressed);
    fclose (fd);

    return(1) ;
}
