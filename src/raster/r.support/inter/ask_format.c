
#include <string.h>
#include "vask.h"
#include "gis.h"

int 
ask_format (char *name, struct Cell_head *cellhd, long filesize)
{
    char title[80];
    char temp[80];
    char no_zeros[80];
    RASTER_MAP_TYPE data_type;

    data_type = G_raster_map_type(name,G_mapset());

    *no_zeros = 0;
    sprintf (title, "Please enter the following information for [%s]", name);

    V_clear ();
    V_line (0, title);
    V_line (2, "        Number of rows");
    V_line (3, "        Number of cols");

    V_line (4, (data_type == CELL_TYPE ?
			    "        Number of bytes per cell" :
			    "        Floating point map"));
    
    if (cellhd->compressed)
	V_const (&cellhd->rows, 'i', 2, 1, 5);
    else
	V_ques (&cellhd->rows, 'i', 2, 1, 5);
    V_ques (&cellhd->cols, 'i', 3, 1, 5);

    if(data_type == CELL_TYPE)
        V_ques (&cellhd->format, 'i', 4, 1, 5);

    if (data_type == CELL_TYPE && cellhd->compressed == 0 && 
	cellhd->rows * cellhd->cols * cellhd->format != filesize)
    {
	sprintf (temp, "rows * cols * bytes per cell must be same as file size (%ld)", filesize);
	V_line (6, temp);
	V_line (7, "If you need help figuring them out, just hit ESC");
    }
    V_line (10, no_zeros);
    while(1)
    {
	V_intrpt_ok ();
	if (!V_call())
	    exit(1);
	*no_zeros = 0;
	if (data_type == CELL_TYPE &&
	    cellhd->rows > 0 && cellhd->cols > 0 && cellhd->format > 0)
	    break;

	if (data_type != CELL_TYPE &&
	    cellhd->rows > 0 && cellhd->cols > 0)
	    break;

	if (!cellhd->compressed)
	{
	    if (cellhd->rows >= 0 && cellhd->cols >= 0 && cellhd->format >= 0)
		break;
	    strcpy (no_zeros, "** Negative values not allowed!");
	}
	else
	    strcpy (no_zeros, "** Positive values only please!");
    }
    return 0;
}
