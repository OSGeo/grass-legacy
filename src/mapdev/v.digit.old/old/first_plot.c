/*  @(#)first_plot.c    2.1  6/26/87  */

#include "digit.h"

/* this is defunct.  It should be removed -dpg */

first_plot(digit)
    FILE *digit;

{
    char type;
    int n_points;
    double *xarray;
    double *yarray;
    double x1, x2, y1, y2;
    int alloc_points;
    long addr1, addr2;
    long ftell();
    int new_nodes;
    int line;

    /* until I get this re-re-written */
    exit (-1);

    alloc_points     = 1024;
    xarray = (double *) dig_falloc(alloc_points, sizeof(double));
    yarray = (double *) dig_falloc(alloc_points, sizeof(double));

    while (1)
    {
/* THIS ALL HAS TO CHANGE!!! */
	addr1 = ftell(digit);
	if (0 >= fread(&type, sizeof(int), 1, digit) )
	    goto done;
	if (0 >= fread(&n_points, sizeof(int), 1, digit) )
	    goto done;
	if (n_points >= alloc_points)
	{
	    alloc_points = n_points + 128;
	    xarray = (double *)dig_frealloc((char *)xarray, alloc_points, sizeof(double));
	    yarray = (double *)dig_frealloc((char *)yarray, alloc_points, sizeof(double));
	}
	if (0 >= fread(xarray, sizeof(double), n_points, digit) )
	    goto done;
	if (0 >= fread(yarray, sizeof(double), n_points, digit) )
	    goto done;

	if (type != AREA && type != LINE)
	    continue;

	x1 = *xarray;
	x2 = xarray[n_points-1];
	y1 = *yarray;
	y2 = yarray[n_points-1];

	/*
	new_nodes = oldcheck_nodes(type, n_points, xarray, yarray, 0);
	*/

	if (x1 != *xarray ||
	    x2 != xarray[n_points-1] ||
	    y1 != *yarray ||
	    y2 != yarray[n_points-1] )
	{
	    addr2 = ftell(digit);
	    fseek(digit, addr1, 0);
	    fwrite(&type, sizeof(int), 1, digit);
	    fwrite(&n_points, sizeof(int), 1, digit);
	    fwrite(xarray, sizeof(double), n_points, digit);
	    fwrite(yarray, sizeof(double), n_points, digit);
	    fflush(digit);
	    fseek(digit, addr2, 0);
	}

	if (do_graphics())
	{
	    if (type == AREA)
		plot_points(type, n_points, xarray, yarray, "white", "red");
	    else
		plot_points(type, n_points, xarray, yarray, "blue", "red");
	}
    }

done:
    free((char*)xarray);
    free((char*)yarray);
}
