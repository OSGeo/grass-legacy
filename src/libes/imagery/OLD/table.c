/*************************************************************
* I_color_conversion_table (nlevels, r, g, b)
*
*    int *table;
*
*    table = I_color_conversion_table (nlevels, r, g, b)
*    
*  prepare color conversion table. This routine is a bit obtuse
*  but the idea is to compute a table lookup for each color
*  which can be used when building a full color image
*  instead of having to perform calls to I_color_n()
*  for each pixel in the image.
*
*  returns a pointer to an allocated table (which can be
*  freed using the free() routine).
*
*  level is the number of levels of the particular color
*  r,g,b are used to indicate which color. One should be set
*  to 1, the other 2 to 0.
*
*  should be used as follows
*
*     int *rtable, *gtable, *btable
*     int rlevel, glevel, blevel
*
*     I_get_color_levels (&rlevel, &glevel, &blevel)
*
*     rtable = I_color_conversion_table (rlevel, 1, 0, 0)
*     gtable = I_color_conversion_table (glevel, 0, 1, 0)
*     btable = I_color_conversion_table (blevel, 0, 0, 1)
*
*  The tables are then passed to the routine I_add_color_to_image()
*  to add the particular color to the image.
*
*  Of course, if a color component is to be left out, the
*  corresponding call can be skipped.
*******************************************************************/

int *
I_color_conversion_table (level, r, g, b)
{
    int i;
    int *table;
    char *malloc();

    table = (int *) malloc (level * sizeof(int));
    for (i = 0; i < level; i++)
	table[i] = I_color_n (i*r, i*g, i*b);

    return table;
}
