/* %W% %G% */

#include "gis.h"

/**************************************************************
 * input is from command line.
 * arguments are input file, output file names
 * optional no verbose flag (-v)
 * optional binary flag (-b)
 * usage: Ggrow [-v] [-b] inputmap outputmap
 **************************************************************/

main (argc, argv) char *argv[];
{
    struct Colors colr;
    struct Categories cats;
    static int verbose = 1;
    static int binmap = 0;
    static int colrfile = 1;
    static int file_flag = 0;
    int if_fd;
    int of_fd;
    CELL *if_cell[3], *temp;
    CELL *of_cell;
    int i;

    char *if_name;
    char *of_name;
    char buf[300];
    char *mapset;
    int nrows, row;
    int ncols, col;
    CELL *c1, *c2, *c3, *c4;


    G_gisinit ("GGROW");

/* parse command line */
    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	{
	    char *opt;

	    opt = argv[i]+1;
	    if (*opt == 0)
	    {
		fprintf (stderr, "- illegal option\n");
		usage(argv[0]);
	    }
	    while (*opt)
	    {
		switch (*opt)
		{
		case 'b': binmap = 1; break;
		case 'v': verbose = 0; break;
		default: fprintf (stderr, "-%c illegal option\n", *opt);
			 usage(argv[0]);
		}
		opt++;
	    }
            continue;
        }
        if (!file_flag)
        {
            if_name = argv[i];
            file_flag++;
            continue;
        }
        else
        {
            of_name = argv[i];
            file_flag++;
            continue;
        }
    }

    if ((file_flag < 2) || (file_flag > 2)) usage(argv[0]);

    if (G_legal_filename (of_name) < 0)
    {
        sprintf(buf,"<%s> -  invalid filename", of_name);
	G_fatal_error (buf);
	exit(1);
    }

/* check input file existence */
    mapset = G_find_file ("cell", if_name, "");
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
    of_fd = G_open_cell_new (of_name, G_mapset());
    if (of_fd < 0) exit(1);


    if (!binmap)
    {
        if (G_read_cats(if_name,mapset,&cats) == -1)
        {
            fprintf(stderr," error in reading cats file for %s\n",
                if_name);
            exit(4);
        }
        if (!G_find_file("colr",if_name,""))
            colrfile = 0;
        if (colrfile)
        {
            if (G_read_colors(if_name,mapset,&colr) == -1)
            {
                fprintf(stderr," error in reading colr file for %s\n",
                    if_name);
                exit(4);
            }
        }
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
    c2++;c3++;c4++;

    for (col = 1; col < ncols-1; col++)
    {
        if (*c2 == 0)
        {
            if ( *(c2-1) > 0 || *(c2+1) > 0 || *c3 > 0)
                *c4 = 1;
        }
        c2++;c3++;c4++;
    }

    if (*c2 == 0)
    {
        if ( *(c2-1) > 0 || *c3 > 0)
            *c4 = 1;
    }

    G_put_map_row(of_fd, of_cell, 0);

    if (verbose) fprintf (stderr, "percent complete: ");
    for (row = 2; row < nrows; row++)
    {
        if (verbose) percent (row, nrows, 10);

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
        c1++;c2++;c3++;c4++;

        for (col = 1; col < ncols-1; col++)
        {
            if (*c2 == 0)
            {
                if ( *c1 > 0 || *(c2-1) > 0 || *(c2+1) > 0 || *c3 > 0)
                    *c4 = 1;
            }
            c1++;c2++;c3++;c4++;
        }

        if (*c2 == 0)
        {
            if ( *c1 > 0 || *(c2-1) > 0 || *c3 > 0)
                *c4 = 1;
        }

        G_put_map_row(of_fd, of_cell, row-1);

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
    c2++;c3++;c4++;

    for (col = 1; col < ncols-1; col++)
    {
        if (*c3 == 0)
        {
            if ( *(c3-1) > 0 || *(c3+1) > 0 || *c2 > 0)
                *c4 = 1;
        }
        c2++;c3++;c4++;
    }

    if (*c3 == 0)
    {
        if ( *(c3-1) > 0 || *c2 > 0)
            *c4 = 1;
    }

    if (verbose) percent (row, nrows, 10);

    G_put_map_row(of_fd, of_cell, row);

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
