#include <unistd.h>
#include <string.h>
#include "gis.h"

int input(char *, int, char *);

int main (int argc, char *argv[])
{
    int fd;
    CELL *cell;
    struct Cell_head region;
    int nrows, ncols;
    int row, col;
    int projection;
    char buf[1024], buf1[1024], buf2[1024];
    char *ebuf, *nbuf;
    double east, north;
    char fmt[50];
    char *mapset;

	struct GModule *module;
    struct 
    {
	struct Option *mask, *input, *fs;
    } parm;
    struct
    {
	struct Flag *reverse;
    } flag;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Examines and filters lists of points constituting lines "
		"to determine if they fall within current region and mask "
		"and optionally an additional raster map.";

    parm.mask = G_define_option();
    parm.mask->key = "mask";
    parm.mask->description="Raster map used to mask points";
    parm.mask->gisprompt="old,cell,raster";
    parm.mask->type = TYPE_STRING;
    parm.mask->multiple = NO;
    parm.mask->required = NO;

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->description="Unix input containing point list";
    parm.input->type = TYPE_STRING;
    parm.input->multiple = NO;
    parm.input->required = NO;

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->description="Input field separater character";
    parm.fs->type = TYPE_STRING;
    parm.fs->multiple = NO;
    parm.fs->required = NO;

    flag.reverse = G_define_flag();
    flag.reverse->key = 'r';
    flag.reverse->description = "Coordinates are reversed: north east";

    if (!isatty(0)) G_disable_interactive();
    if (G_parser(argc, argv))
	    exit(1);

    G_get_window (&region);
    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();
    projection = G_projection();

    for (col = 0; col < ncols; col++)
	cell[col] = 1;			/* in case there is no mask */

    if (parm.mask->answer)
    {
	mapset = G_find_cell (parm.mask->answer, "");
	if (mapset == NULL)
	{
	    char error[256];
	    sprintf (error, "<%s> raster map not found\n",
		parm.mask->answer);
	    G_fatal_error (error);
	}
	fd = G_open_cell_old (parm.mask->answer, mapset);
	if (fd < 0) exit(1);
    }
    else
	fd = G_maskfd();
    
    if (parm.input->answer)
    {
	if (freopen (parm.input->answer, "r", stdin) == NULL)
	{
	    perror (parm.input->answer);
	    exit(1);
	}
    }
    if (parm.fs->answer)
    {
	char fs;
	fs = parm.fs->answer[0];
	sprintf (fmt, "%%[^%c]%c%%[^%c]",fs, fs, fs);
    }
    else
	strcpy (fmt, "%s %s");

    while (input(buf, flag.reverse->answer, parm.fs->answer))
    {
	if (sscanf (buf, fmt, ebuf=buf1, nbuf=buf2)  != 2) 
	    continue; /* not a coordinate pair line */
	if (flag.reverse->answer)
	{
	    ebuf = buf2;
	    nbuf = buf1;
	}
	if (!G_scan_northing(nbuf, &north, projection)) 
	    continue; /* not a valid northing */
	if (!G_scan_easting(ebuf, &east, projection)) 
	    continue; /* not a valid easting */

    /* convert north,east to row,col and check if they fall in the region */
	row = (int) G_northing_to_row (north, &region);
	if (row < 0 || row >= nrows) continue;
	col = (int) G_easting_to_col (east, &region);
	if (col < 0 || col >= ncols) continue;

	if (fd >= 0)
	{
	    if(G_get_map_row (fd, cell, row) < 0)
		exit(1);
	}
	if (cell[col])
	    fprintf (stdout,"%s\n", buf);
    }
    if (fd >= 0)
	G_close_cell(fd);
    G_free (cell);
    exit(0);
}

int input (char *buf, int reverse, char *fs)
{
    if (fs == NULL) fs = " ";
    if (isatty(fileno(stdin)))
	fprintf (stderr, "%s%s%s%s[text] >",
		reverse?"north":"east", fs,
		reverse?"east":"north", fs);
    return fgets(buf,1024,stdin) != NULL;
}
