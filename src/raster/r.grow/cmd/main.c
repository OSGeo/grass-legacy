#include "gis.h"

int main(int argc, char *argv[])
{
    struct Colors colr;
    struct Categories cats;
    static int verbose = 1;
    static int binmap = 0;
    static int colrfile = 0;
    int if_fd;
    int of_fd;
    CELL *if_cell[3], *temp;
    CELL *of_cell;

    char *if_name;
    char *of_name;
    char buf[300];
    char *mapset;
    int nrows, row;
    int ncols, col;
    CELL *c1, *c2, *c3, *c4;
	struct GModule *module;
    struct Option *opt1, *opt2;
    struct Flag *flagq, *flagb ;


	module = G_define_module();
	module->description =
		"Generates an output raster map layer "
		"with contiguous areas grown by one cell (pixel).";

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->gisprompt  = "old,cell,raster" ;
    opt1->description= "Name of existing input raster file" ;

    opt2 = G_define_option() ;
    opt2->key        = "output" ;
    opt2->type       = TYPE_STRING ;
    opt2->required   = YES ;
    opt2->gisprompt  = "new,cell,raster" ;
    opt2->description= "Name of output raster file" ;

    flagb = G_define_flag() ;
    flagb->key         = 'b' ;
    flagb->description = "Binary map output" ;

    flagq = G_define_flag() ;
    flagq->key         = 'q' ;
    flagq->description = "Quiet" ;

    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
        exit(1);

    if_name = opt1->answer;
    of_name = opt2->answer;

    verbose = (!flagq->answer);
    binmap  = flagb->answer;


    if (G_legal_filename (of_name) < 0)
    {
        sprintf(buf,"<%s> -  invalid output filename", of_name);
        G_fatal_error (buf);
        exit(1);
    }

    /* check input file existence */
    mapset = G_find_cell2 ( if_name, "");
    if (!mapset)
    {
        sprintf (buf, "input file [%s] not found\n", if_name);
        G_fatal_error (buf);
        exit(1);
    }


    nrows = G_window_rows();
    ncols = G_window_cols();

    /* open the input file for reading */
    if_fd = G_open_cell_old (if_name, mapset);
    if (if_fd < 0) exit(1);

    /* open the output file for writing */
    of_fd = G_open_cell_new (of_name);
    if (of_fd < 0) exit(1);


    if (!binmap)
    {
        if (G_read_cats(if_name,mapset,&cats) == -1)
        {
            fprintf(stderr," error in reading cats file for %s\n",
                if_name);
            exit(4);
        }
	if (G_read_colors(if_name,mapset,&colr) == -1)
	{
	    fprintf(stderr,"WARNING: error in reading colr file for %s\n",
		if_name);
	    colrfile = 0;
	}
	else
	    colrfile = 1;
    }

    if_cell[0] = G_allocate_cell_buf();
    if_cell[1] = G_allocate_cell_buf();
    if_cell[2] = G_allocate_cell_buf();

    of_cell = G_allocate_cell_buf();

    G_get_map_row (if_fd, if_cell[1], 0);
    G_get_map_row (if_fd, if_cell[2], 1);

    c2 = if_cell[1];
    c3 = if_cell[2];
    c4 = of_cell;

    for (col=0; col<ncols; col++)
    {
        if (*c2 > 0)
            if (binmap) *c4++ = 1;
            else *c4++ = *c2;
        else *c4++ = 0;
        c2++;
    }

    c2 = if_cell[1];
    c4 = of_cell;

    if (*c2 == 0)
    {
        if ( *(c2+1) > 0 || *c3 > 0)
            *c4 = 1;
    }
    c2++;
    c3++;
    c4++;

    for (col = 1; col < ncols-1; col++)
    {
        if (*c2 == 0)
        {
            if ( *(c2-1) > 0 || *(c2+1) > 0 || *c3 > 0)
                *c4 = 1;
        }
        c2++;
        c3++;
        c4++;
    }

    if (*c2 == 0)
    {
        if ( *(c2-1) > 0 || *c3 > 0)
            *c4 = 1;
    }

    G_put_map_row(of_fd, of_cell);

    if (verbose) fprintf (stderr, "percent complete: ");
    for (row = 2; row < nrows; row++)
    {
        if (verbose) G_percent (row, nrows, 10);

        temp = if_cell[0];
        if_cell[0] = if_cell[1];
        if_cell[1] = if_cell[2];
        G_get_map_row (if_fd, if_cell[2]=temp, row);

        c1 = if_cell[0];
        c2 = if_cell[1];
        c3 = if_cell[2];
        c4 = of_cell;

        for (col=0; col<ncols; col++)
        {
            if (*c2 > 0)
                if (binmap) *c4++ = 1;
                else *c4++ = *c2;
            else *c4++ = 0;
            c2++;
        }

        c2 = if_cell[1];
        c4 = of_cell;

        if (*c2 == 0)
        {
            if ( *c1 > 0 || *(c2+1) > 0 || *c3 > 0)
                *c4 = 1;
        }
        c1++;
        c2++;
        c3++;
        c4++;

        for (col = 1; col < ncols-1; col++)
        {
            if (*c2 == 0)
            {
                if ( *c1 > 0 || *(c2-1) > 0 || *(c2+1) > 0 || *c3 > 0)
                    *c4 = 1;
            }
            c1++;
            c2++;
            c3++;
            c4++;
        }

        if (*c2 == 0)
        {
            if ( *c1 > 0 || *(c2-1) > 0 || *c3 > 0)
                *c4 = 1;
        }

        G_put_map_row(of_fd, of_cell);

    }
    c2 = if_cell[1];
    c3 = if_cell[2];
    c4 = of_cell;

    for (col=0; col<ncols; col++)
    {
        if (*c3 > 0)
            if (binmap) *c4++ = 1;
            else *c4++ = *c3;
        else *c4++ = 0;
        c3++;
    }

    c3 = if_cell[2];
    c4 = of_cell;

    if (*c3 == 0)
    {
        if ( *(c3+1) > 0 || *c2 > 0)
            *c4 = 1;
    }
    c2++;
    c3++;
    c4++;

    for (col = 1; col < ncols-1; col++)
    {
        if (*c3 == 0)
        {
            if ( *(c3-1) > 0 || *(c3+1) > 0 || *c2 > 0)
                *c4 = 1;
        }
        c2++;
        c3++;
        c4++;
    }

    if (*c3 == 0)
    {
        if ( *(c3-1) > 0 || *c2 > 0)
            *c4 = 1;
    }

    if (verbose) G_percent (row, nrows, 10);

    G_put_map_row(of_fd, of_cell);

    G_close_cell (if_fd);
    G_close_cell (of_fd);

    if (!binmap)
    {
        if (G_write_cats(of_name,&cats) == -1)
        {
            fprintf(stderr,"error in writing cats file for %s\n",
                of_name);
        }
        if (colrfile)
        {
            if (G_write_colors(of_name,mapset,&colr) == -1)
            {
                fprintf(stderr,"error in writing colr file for %s\n",
                    of_name);
            }
        }
    }

    exit(0);

}
