#include "gis.h"
#include "btree.h"
main(argc,argv) char *argv[];
{
    CELL *cell;
    double *xcell;
    struct Categories cats;
    BTREE btree;
    char *mapset, name[40];
    int cmp();

    int fd, ncols, nrows, row, col;

    G_gisinit (argv[0]);
    mapset = G_ask_cell_old ("",name);
    if (mapset == NULL) exit(0);

    if (G_read_cats (name, mapset, &cats) < 0)
	exit(1);
    fd = G_open_cell_old (name, mapset);
    if (fd < 0) exit(1);

    btree_create (&btree, cmp, 1);

    nrows = G_window_rows();
    ncols = G_window_cols();
    cell = G_allocate_cell_buf();
    xcell = (double *) G_calloc (ncols, sizeof(double));
    for (row = 0; row < nrows; row++)
    {
	if(G_get_map_row (fd, cell, row) < 0)
	    exit(1);
	translate_from_cats (cell, xcell, ncols, &cats, &btree);
	for (col = 0; col < ncols; col++)
	    printf ("%d:%lf ", cell[col], xcell[col]);
	printf ("\n");
    }
}
/* for btree key comparisons */
static cmp (a,b)
    int *a, *b;
{
    return (*a - *b);
}
