/* %W% %G% */

#include "usgs.h"

#define max(a,b)   ((a) > (b) ? (a) : (b))

extern double file_north, file_south, file_east, file_west;

getargs(argc, argv)
char *argv[];
{
    int i;
    char temp[100], *strcpy();

    if (argc < 2)
        return 0;
    *tapename = *outname = 0;
    tapefile = stdin;
    ignoreWin = 0;

    for (i=1; i < argc; i++) {
        if (sscanf(argv[i], "if=%s", temp) == 1) {
            (void)strcpy(tapename, temp);
            continue;
        }
        if (sscanf(argv[i], "of=%s", temp) == 1) {
            (void)strcpy(outname, temp);
            continue;
        }
        if (strncmp(argv[i], "-nowin", 6) == 0) {
            ignoreWin = 1;
            continue;
        }
        return 0;
    }
    if (*outname == 0)
        return 0;
    return 1;
}


usgs_init()
{
    int col;
    FILE *fopen();
    double WindowMax(), WindowMin();

    buf_start = buffer = G_malloc((unsigned)RECSIZE + 1);
    buf_end = buffer;
    /*  open file */
    if (*tapename == 0)
        tapefile = stdin;
    else {
        if ((tapefile = fopen(tapename, "r")) == NULL) {
            fprintf(stderr, "can't open input file %s\n", tapename);
            return 0;
        }
    }
    if (!get_hdr())
        return 0;
    hdr_list(stdout);

    /* get full window information  */
    G_get_set_window(&cellhd);

    if (ignoreWin) {
        /* get window settings from dem */
        cellhd.ns_res = y_res;
        cellhd.ew_res = x_res;
		north = WindowMax(file_north, y_res);
		south = WindowMin(file_south, y_res);
		east = WindowMax(file_east, x_res);
		west = WindowMin(file_west, x_res);
        cellhd.north = north + cellhd.ns_res / 2.0;
        cellhd.south = south - cellhd.ns_res / 2.0;
        cellhd.east = east + cellhd.ew_res / 2.0;
        cellhd.west = west - cellhd.ew_res / 2.0;
        cellhd.rows = (cellhd.north - cellhd.south) / y_res;
        cellhd.cols = (cellhd.east - cellhd.west) / x_res;
        cellhd.proj = PROJECTION_UTM;
        cellhd.zone = ref_zone;
        G_set_window(&cellhd);
    } else {
    	north = cellhd.north - cellhd.ns_res / 2.0;
    	south = cellhd.south + cellhd.ns_res / 2.0;
    	east = cellhd.east - cellhd.ew_res / 2.0;
    	west = cellhd.west + cellhd.ew_res / 2.0;
	}
    window_list(stderr);

    /*  allocate column profile buffer */
    profile_size = max(cellhd.cols, cellhd.rows) * sizeof(CELL);
    profile_buf = (CELL *)G_malloc(profile_size);
    of = G_tempfile();
    inf = G_tempfile();
    if ((fd = creat(inf, 0660)) < 0) {
        G_warning("usgs_init: can't creat tempfile");
        return 0;
    }
    bzero((char*)profile_buf, profile_size);
    for (col=0; col < cellhd.cols; col++)
        (void)write(fd, (char*)profile_buf, cellhd.rows * sizeof(CELL));
    return 1;
}


window_list(file)
FILE *file;
{
    fprintf(file, "Current Window Settings-------------------------\n");
    fprintf(file, "rows:    %d\n",  cellhd.rows);
    fprintf(file, "cols:    %d\n",  cellhd.cols);
    fprintf(file, "north:   %lf\n", cellhd.north);
    fprintf(file, "south:   %lf\n", cellhd.south);
    fprintf(file, "east:    %lf\n", cellhd.east);
    fprintf(file, "west:    %lf\n", cellhd.west);
    fprintf(file, "ns_res:  %lf\n", cellhd.ns_res);
    fprintf(file, "ew_res:  %lf\n", cellhd.ew_res);
    fprintf(file, "zone: %d   proj: %d\n",  cellhd.zone,  cellhd.proj);
    fprintf(file, "\n");
}

double WindowMax(value, res)
double value, res;
{
    int bound, tmp;

    bound = (int)(value);
    tmp = (int)res;
    bound = ((bound + (tmp-1)) / tmp) * tmp;
    return (double)bound;
}


double WindowMin(value, res)
double value, res;
{
    int bound, tmp;

    bound = (int)(value + 0.5);
    tmp = (int)res;
    bound = (bound / tmp) * tmp;
    return (double)bound;
}
