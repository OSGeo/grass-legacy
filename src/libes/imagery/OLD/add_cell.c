/*********************************************************
* I_add_cell_to_image (image, cell, table, map, n)
*
*   unsigned int *image     image to be modified
*   CELL *cell              cell data to be added to image
*   int *table              returned by I_color_conversion_table()
*   int *map                computed by I_histo_eq()
*   int n                   number of columns of data
*
* adds color to the image based on the color conversion
* table and map using CELL data
**********************************************************
* I_add_cell_to_cell (result, cell, table, map, n)
*
*   CELL *result            cell data to be modified
*   CELL *cell              cell data to be added to result
*   int *table              returned by I_color_conversion_table()
*   int *map                computed by I_histo_eq()
*   int n                   number of columns of data
*
* adds color to the result CELL buffer based on the color conversion
* table and map using CELL data
**********************************************************/

#include "gis.h"

I_add_cell_to_image (image, cell, table, map, n)
    register unsigned int *image;
    register CELL *cell;
    int *table;
    int *map;
    register int n;
{
    while (n-- > 0)
	*image++ += table[map[*cell++]];
}

I_add_cell_to_cell (result, cell, table, map, n)
    register CELL *result;
    register CELL *cell;
    int *table;
    int *map;
    register int n;
{
    while (n-- > 0)
	*result++ += table[map[*cell++]];
}
