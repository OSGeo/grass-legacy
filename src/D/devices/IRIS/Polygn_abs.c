/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */


extern int SCREEN_BOTTOM;


Polygon_abs (xarray, yarray, number)
    int *xarray, *yarray ;
    int number ;
{
    /*unsigned short array[257][2];*/
    register int i;
    int vert[2];

    if (number > 256)
	number = 256;

    bgnpolygon ();
	for (i = 0 ; i < number ; i++)
	{
	    vert[0] = xarray[i];
	    vert[1] = SCREEN_BOTTOM - yarray[i];
	    v2i (vert);
	}
    endpolygon ();
}

/*
    for ( i = 0 ; i < number ; i++)
    {
    
	array[i][0] = xarray[i];
	array[i][1] =  SCREEN_BOTTOM -  yarray[i];
    }
    polf2s (number, array);
*/
