/* %W% %G% */
/* program to filter data */
#include "gis.h"

#define NOMASK 1

static struct Cell_head window;
static filter_matrix[3][3] =
{
    {1, 2, 1},
    {2, 4, 2},
    {1, 2, 1}
};
static filter_divisor = 16;

main(argc, argv) char *argv[];
{
    int nrows;
    int ncols;
    int pass;
    int npasses;
    char *elev_mapset ;
    char elev_name[40];
    char *newelev_mapset ;
    char newelev_name[40];
    FILE *in_fd, *out_fd;
    int incell_fd, outcell_fd;
    int row;
    char *temp1_name, *temp2_name;
    CELL *buf;
    int verbose=0;
    int elev_flag=0;
    int newelev_flag=0;
    int passes_flag=0;
    int i;


    G_gisinit (argv[0]);

/* parse command line */

    for (i=1; i < argc; i++)
    {
        if (strcmp (argv[i], "-v") == 0)
        {
            verbose = 1;
            continue;
        }
        if (sscanf (argv[i], "in=%[^\n]", elev_name) == 1)
        {
            if (elev_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "out=%[^\n]", newelev_name) == 1)
        {
            if (newelev_flag++) usage(argv[0]);
            continue;
        }
        if (sscanf (argv[i], "passes=%d", &npasses) == 1)
        {
            if (passes_flag++) usage(argv[0]);
            continue;
        }
        usage(argv[0]);
    }

    if ((!elev_flag) || (!newelev_flag) || (!passes_flag))
        usage(argv[0]);

    if (verbose)
       printf("\n Running...\n");

    elev_mapset = G_find_file2("cell",elev_name,"");
    if (!elev_mapset)
    {
        sprintf(buf, "input file [%s] not found\n",
           elev_name);
        G_fatal_error (buf);
        exit(1);
    }

    set_window(elev_name,elev_mapset);
    G_get_set_window (&window);
    nrows = G_window_rows();
    ncols = G_window_cols();

/*  perform the filter  */
    if (npasses < 0)
    {
        fprintf(stderr,"Error - must enter positive number of iterations\n");
        exit(1);
    }
    else if (npasses == 0)
    {
        fprintf(stderr,"Zero iterations is a copy - use GRASS copy feature\n");
        exit(1);
    }
    else if (npasses >= 1)
    {
        incell_fd = G_open_cell_old(elev_name, elev_mapset);
        if (incell_fd < 0)
        {
            fprintf(stderr,"Error in opening cell file\n");
            exit(2);
        }

        buf = G_allocate_cell_buf();

        temp1_name = G_tempfile();
        temp2_name = G_tempfile();

        out_fd = fopen(temp1_name, "w");

        for (row=0; row<nrows; row++)
        {
            if (!G_get_map_row(incell_fd,buf,row))
            {
                fprintf(stderr,"Error reading original file\n");
                exit(3);
            }

            if (fwrite(buf,sizeof(CELL),ncols,out_fd) != ncols)
            {
                fprintf(stderr,"Error writing temp file\n");
                exit(3);
            }
        }

        G_close_cell(incell_fd);
        fclose(out_fd);

        for (pass = 1; pass <= npasses; pass++)
        {
            if (verbose)
	    {
		fprintf (stderr, "FILTER STEP (pass %d) ... ", pass);
                fflush (stderr);
	    }

            if ((pass/2)*2 != pass)
            {
                in_fd = fopen(temp1_name, "r");
                out_fd = fopen(temp2_name, "w");
            }
            else
            {
                in_fd = fopen(temp2_name, "r");
                out_fd = fopen(temp1_name, "w");
            }

            filter(in_fd,out_fd,nrows,ncols,filter_matrix,filter_divisor,verbose);

            fclose(in_fd);
            fclose(out_fd);
        }

        if ((npasses/2)*2 != npasses)
            in_fd = fopen(temp2_name, "r");
        else
            in_fd = fopen(temp1_name, "r");

        outcell_fd = G_open_cell_new(newelev_name, newelev_mapset);

        if (outcell_fd < 0)
        {
            fprintf(stderr,"Error in opening new cell file\n");
            exit(4);
        }

        for (row=0; row<nrows; row++)
        {
            if (fread(buf,sizeof(CELL),ncols,in_fd) != ncols)
            {
                fprintf(stderr,"Error reading temp file\n");
                exit(4);
            }

            if (!G_put_map_row(outcell_fd,buf,row))
            {
                fprintf(stderr,"Error writing new cell file\n");
                exit(4);
            }

        }

        fclose(in_fd);
        G_close_cell(outcell_fd);
	exit(0);

    }

}
