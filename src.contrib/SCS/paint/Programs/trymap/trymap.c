
/* Program: trymap
**
** This is a test program to produce a PostScript file for printing a
** GRASS raster map layer.
**
** USAGE: trymap <raster map layer> <PostScript output file>
**
** Author: P.W. Carlson		Feb. 1992
*/

#include <stdio.h>
#include "G.h"

#define WINDOW	G__.window

main(argc, argv)
int argc; 
char *argv[];
{
    struct Colors colors;
    struct Cell_head wind;
    char *name, *mapset, buf[128];
    int r, g, b, n, w, h, cell_fd, row, col, nrows, ncols;
    double pix_per_cell;
    CELL *cellbuf;
    FILE *fp;

    if (argc != 3)
    {   
	printf("USAGE: trymap <raster map layer> <PostScript output file>\n");
   	exit(1);
    }

    /* initialize */
    G_gisinit(argv[0]);

    /* get the mapset */
    name = argv[1];
    mapset = G_find_cell2(name, "");
 
    /* initialize the colors structure */
    G_init_colors(&colors);

    /* read the colors */
    if (G_read_colors(name, mapset, &colors) == -1)
    {   
	sprintf(buf, "Color file for %s not found", name);
	G_fatal_error(buf);
    }

    /* set current window */
    G_get_set_window(&wind);
    if (G_set_window(&wind) == -1)
	G_fatal_error("Current window not settable");

    /* open cell file (this reads header info) */
    if ((cell_fd = G_open_cell_old(name, mapset)) == -1)
    {
	sprintf(buf, "Can't open cell file \"%s\"", name);
	G_fatal_error(buf);
    }
    cellbuf = G_allocate_cell_buf();
    nrows = WINDOW.rows;
    ncols = WINDOW.cols;

    if ((fp = fopen(argv[2], "w")) == NULL)
    {	
	printf("Can't open %s for output.\n", argv[2]);
	exit(1);
    }

    /* write header */
    fprintf(fp, "%%!PS-Adobe-2.0 EPSF-1.2\n");
    fprintf(fp, "%%%%EndComments\n");

    /* define a string to hold "ncols" RGB values */
    fprintf(fp, "/mapstrg %d string def\n", 3 * ncols);

    /* lower left corner is at x = 1.5, y = 2.0 inches */
    fprintf(fp, "108 144 translate\n");

    /* make image fit into a 6 inch (hor) by 8 inch (vert) box */
    if (ncols * 6  > nrows * 8) pix_per_cell = 6.0 * 72.0 / (double)ncols;
    else pix_per_cell = 8.0 * 72.0 / (double)nrows;
    w = (int)(pix_per_cell * (double)ncols + 0.5);
    h = (int)(pix_per_cell * (double)nrows + 0.5);

    /* mapping of image to w x h unit rectangle */
    fprintf(fp, "%d %d scale\n", w, h);

    /* dimensions of source image */
    fprintf(fp, "%d %d 8\n", ncols, nrows);

    /* mapping of unit rectangle to unit source */
    fprintf(fp, "[%d 0 0 -%d 0 %d]\n", ncols, nrows, nrows);

    /* image will be read from program file */
    fprintf(fp, "{currentfile mapstrg readhexstring pop}\n");

    /* single data source, 3 colors */
    fprintf(fp, "false 3\n");

    fprintf(fp, "colorimage\n");

    /* build the image RGB string */
    n = 0;
    for (row = 0; row < nrows; row++)
    {
	G_get_map_row(cell_fd, cellbuf, row);
	for (col = 0; col < ncols; col++) 
	{   
	    G_get_color(cellbuf[col], &r, &g, &b, &colors);
	    fprintf(fp, "%02X%02X%02X", r, g, b);
	    if (++n == 13)
	    {	
		n = 0;
		fprintf(fp, "\n");
	    }
	}
    }
    fprintf(fp, "\n");
    G_free_colors(&colors);
    G_close_cell(cell_fd);
    free(cellbuf);

    fprintf(fp, "showpage\n");
    fclose(fp);
}



/* Gmakefile to create the program "trymap" in the current directory using
   "gmake4.0".
   The file "G.h" must exist in the $(SRC)/libes directory (it may need to
   be copied from $(SRC)/libes/gis).

./trymap: trymap.o $(GISLIB)
	$(CC) $(LDFLAGS) trymap.o $(GISLIB) -o $@

*/
