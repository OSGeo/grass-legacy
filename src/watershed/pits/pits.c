/* %W% %G% */
#include "gis.h"

#define NOMASK 1

static CELL *map;       /* elevation map data */
static int nrows;
static int ncols;
static struct Cell_head window;
typedef struct
{
    int row;
    int col;
    CELL elev;
} PITT;
static PITT *pitts;
static int verbose=0;
    

main(argc, argv) char *argv[];
{
    char elev_name[30];
    char *elev_mapset;
    CELL *read_map();
    int pass;
    int npasses;
    int npits;
    int i;
    char pitfile[30];
    FILE *fd;
    int cmp();
    char *calloc();
    int elev_flag=0;
    char buf[100];

    G_gisinit (argv[0]);

/* parse command line */

    for (i=1; i < argc; i++)
    {
        if (strcmp (argv[i], "-v") == 0)
        {
            verbose=1;
            continue;
        }
        if (sscanf (argv[i], "elev=%[^\n]", elev_name) == 1)
        {
            if (elev_flag++) usage(argv[0]);
            continue;
        }
        usage(argv[0]);
    }

    if (!elev_flag)
        usage(argv[0]);

    if (verbose)
	printf("\n Running...\n");

    elev_mapset = G_find_file2("cell",elev_name,"");
    if (!elev_mapset)
    {
        sprintf(buf, "elevation file [%s] not found\n",
           elev_name);
        G_fatal_error (buf);
        exit(1);
    }

/* want to read the elevation file directly - no window */

    set_window (elev_name, elev_mapset);
    G_get_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose)
	printf ("%d rows, %d cols\n", nrows, ncols);
/* read the elevation layer */
    map = read_map (elev_name, elev_mapset, NOMASK, verbose);

/* open the pit list */
    fd = G_fopen_new("watershed/pits",elev_name);
    if (!fd)
    {
    perror ("pitfile");
    exit(1);
    }

    if (verbose)
	fprintf (stderr, "FINDING PITS ... ");
    fflush (stderr);
    npits = pits (fd);
    fclose (fd);
    free (map);
    pitts = (PITT *)calloc (npits, sizeof(PITT));
    fd = G_fopen_old ( "watershed/pits", elev_name, G_mapset() );
    if (verbose)
    {
	fprintf (stderr, "%d pits found\n", npits);
	fprintf (stderr, "SORTING PITS ... ");
        fflush (stderr);
    }
    for ( i=0; i<npits; i++)
    {
    fread (&pitts[i].row, sizeof(pitts[i].row), 1, fd);
    fread (&pitts[i].col, sizeof(pitts[i].col), 1, fd);
    fread (&pitts[i].elev, sizeof(pitts[i].elev), 1, fd);
    }
    qsort ( pitts, npits, sizeof(PITT), cmp);
    fclose ( fd );
    if (verbose)
    {
        fprintf( stderr, "Sort complete\n");
        fprintf( stderr, "Writing file...\n");
    }
    fd = G_fopen_new("watershed/pits", elev_name);

    fwrite (&npits, sizeof(npits), 1, fd);
    for ( i=0; i<npits; i++)
    output (fd, pitts[i].row, pitts[i].col);
    fclose (fd);
    if (verbose)
	fprintf (stderr, "RESULTS in %s/watershed/pits/%s\n",
         G_mapset(), elev_name );
    exit(0);
}

static
pits (fd)
    FILE *fd;
{
    register CELL *m;
    int row;
    register int col;
    int npits;
    int x;

    m = map + ncols + 1;    /* second row, second col */
    npits = 0;

    for (row = 2; row < nrows; row++)
    {
    if (verbose)
	percent (row, nrows, 25);
    for (col = 2; col < ncols; col++)
    {
        if (m[-ncols-1] >= m[0] &&
        m[-ncols]   >= m[0] &&
        m[-ncols+1] >= m[0] &&
        m[-1]       >= m[0] &&
        m[1]        >= m[0] &&
        m[ncols-1]  >= m[0] &&
        m[ncols]    >= m[0] &&
        m[ncols+1]  >= m[0])
        {
        x = row-1;
        fwrite (&x, sizeof x, 1, fd);
        x = col-1;
        fwrite (&x, sizeof x, 1, fd);
        fwrite (&m[0], sizeof m[0], 1, fd);
        npits++;
        }
        m++;
    }

    m += 2;
    }
    if (verbose)
	percent (row, nrows, 25);
    return npits;
}

static
output (fd, row, col)
    FILE *fd;
{
    double easting,northing;

    easting = window.west + (col + .5) * window.ew_res;
    fwrite (&easting, sizeof(easting), 1, fd);
    northing = window.north - (row + .5) * window.ns_res;
    fwrite (&northing, sizeof(northing), 1, fd);
}

static
cmp (a,b)

    PITT *a, *b;

{
    if (a->elev > b->elev) return -1;
    if (a->elev < b->elev) return 1;
    return 0;
}
