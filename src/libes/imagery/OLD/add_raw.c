/*********************************************************
* I_add_raw_to_image (image, raw, table, map, n)
*
*   unsigned int *image     image to be modified
*   unsigned char *raw      raw data to be added to image
*   int *table              returned by I_color_conversion_table()
*   int *map                computed by I_histo_eq()
*   int n                   number of columns of data
*
* adds color to the image based on the color conversion
* table and map using raw data
**********************************************************/

I_add_raw_to_image (image, raw, table, map, n)
    register unsigned int *image;
    register unsigned char *raw;
    int *table;
    int *map;
    register int n;
{
    while (n-- > 0)
	*image++ += table[map[*raw++]];
}
