#include "glob.h"
#include "btree.h"
#include "rowio.h"
static struct list
{
    char name[50];
    char mapset[50];
    int fd;
    struct Categories cats;
    struct Colors colors;
    int have_cats;
    int have_colors;
    BTREE btree;
    ROWIO rowio;
    int min_row, max_row, use_rowio;
} *list = NULL;

static int nmaps = 0;
static int count = 0;

static unsigned char *red = NULL;
static unsigned char *grn = NULL;
static unsigned char *blu = NULL;
static unsigned char *set = NULL;


openmap (name, mapset, code, row)
    char *name, *mapset;
{
    int i;
    int use_cats;
    int use_colors;

    use_cats = code == '@';
    use_colors = (code == '#' || code == 'r' || code == 'g' || code == 'b');

    for (i=0; i<nmaps; i++)
	if (strcmp (list[i].name,name)==0 &&
	    strcmp (list[i].mapset,mapset)==0)
	    {
		if (row < list[i].min_row) list[i].min_row = row;
		if (row > list[i].max_row) list[i].max_row = row;
		if (use_cats && !list[i].have_cats && !initcats(i))
		    return -1;
		if (use_colors && !list[i].have_colors && !initcolors(i))
		    return -1;
		return i;
	    }

    if (nmaps >= count)
	list = (struct list *) G_realloc (list, (count = nmaps+2) * sizeof(*list));

    strcpy (list[nmaps].name, name);
    strcpy (list[nmaps].mapset, mapset);
    list[nmaps].have_cats = 0;
    list[nmaps].have_colors = 0;
    list[nmaps].use_rowio = 0;
    list[nmaps].min_row = list[nmaps].max_row = row;
    if (use_cats && !initcats(nmaps))
	return -1;
    if (use_colors && !initcolors(nmaps))
	return -1;


    if((list[nmaps++].fd = G_open_cell_old (name, mapset)) < 0)
	return -1;
    else
	return nmaps-1;
}

configmaps(ncols)
{
    int nrows;
    int readrow();
    int i;

    for (i=0; i<nmaps; i++)
    {
	nrows = list[i].max_row - list[i].min_row + 1;
	if (nrows > 1 && nrows <= max_rows_in_memory)
	{
	    if(rowio_setup (&list[i].rowio, list[i].fd, nrows,
		ncols * sizeof(CELL), readrow, NULL) < 0)
		    exit(1); /* out of memory - diagnostic printed by rowio */
	    list[i].use_rowio = 1;
	}
	else
	    list[i].use_rowio = 0;
    }
}

closemaps()
{
    int i;

    for (i=0; i < nmaps; i++)
    {
	if (list[i].fd >= 0)
	{
	    G_close_cell (list[i].fd);
	    if (list[i].have_cats)
	    {
		btree_free (&list[i].btree);
		G_free_cats (&list[i].cats);
		list[i].have_cats = 0;
	    }
	    if (list[i].have_colors)
	    {
		G_free_colors(&list[i].colors);
		list[i].have_colors = 0;
	    }
	    if (list[i].use_rowio)
	    {
		rowio_release (&list[i].rowio);
		list[i].use_rowio = 0;
	    }
	}
    }
    nmaps = 0;
}

readmap(i, cell, row, col, nrows, ncols)
    CELL *cell;
{
    CELL *c;
    int n;

    if (row < 0 || row >= nrows)
    {
	while (ncols-- > 0)
	    *cell++ = 0;
	return 1;
    }
    if (list[i].use_rowio)
    {
	/* rowio will call readrow to read this row */
	c = (CELL *)rowio_get (&list[i].rowio, row);
	if (c == NULL) return 0;
	n = ncols;
	while(n-- > 0)
	    *cell++ = *c++;
	cell -= ncols;
    }
    else if (!readrow (list[i].fd, cell, row, 0))
	return 0;
    if (col)
	column_shift (cell, col, ncols);
    return 1;
}

initcats(i)
{
    int compare_ints();

    if (G_read_cats (list[i].name, list[i].mapset, &list[i].cats) < 0)
    {
	fprintf (stderr, "Error reading category file for [%s in %s]\n",
		list[i].name, list[i].mapset);
	return 0;
    }
    if (!btree_create (&list[i].btree, compare_ints, 1))
	G_fatal_error ("Out of Memory");
    list[i].have_cats = 1; /* so we don't do this twice */
    return 1;
}

initcolors(i)
{
    if(red == NULL)
    {
	int ncols;
	ncols = G_window_cols();
	red = (unsigned char *) G_malloc (ncols);
	grn = (unsigned char *) G_malloc (ncols);
	blu = (unsigned char *) G_malloc (ncols);
	set = (unsigned char *) G_malloc (ncols);
    }
    if (G_read_colors (list[i].name, list[i].mapset, &list[i].colors) < 0)
    {
	fprintf (stderr, "Error reading color file for [%s in %s]\n",
		list[i].name, list[i].mapset);
	return 0;
    }
    list[i].have_colors = 1; /* so we don't do this twice */
    return 1;
}

translate_from_colors (cell, ncols, k, code)
    CELL *cell;
{
/* convert cell to color
 * Grey scale is based on the C.I.E X,Y,Z system where Y is luminance.
 * Jain, Anil K., Fundamentals of Digital Image Processing, 1989
 * Prentice Hall, NJ., p. 67.
 */
    G_lookup_colors (cell, red, grn, blu, set, ncols, &list[k].colors);
    switch(code)
    {
    case '#': /* grey */
	while (--ncols >= 0)
	    cell[ncols] = (18*red[ncols] + 81*grn[ncols] + blu[ncols] + 50)/100;
	    /*
	    cell[ncols] = .177*red[ncols] + .813*grn[ncols] + .011*blu[ncols];
	    */
	break;
    case 'r': /* red */
	while (--ncols >= 0)
	    cell[ncols] = red[ncols];
	break;
    case 'g': /* green */
	while (--ncols >= 0)
	    cell[ncols] = grn[ncols];
	break;
    case 'b': /* blue */
	while (--ncols >= 0)
	    cell[ncols] = blu[ncols];
	break;
    }
}

/* convert cell values to double based on the values in the
 * category file.
 *
 * This requires performing sscanf() of the category label
 * and only do it it for new categories. Must maintain
 * some kind of list of already scaned values.
 *
 * This list is a hybrid tree, where the data in each node
 * of the tree is an array of, for example, 64 values, and
 * the key of the tree is the category represented by the 
 * first index of the data
 *
 * To speed things up a little, use shifts instead of divide or multiply
 * to compute the key and the index
 *
 * This uses the BTREE library to manage the tree itself
 * btree structure must already be intialized
 * pcats structure must already contain category labels
 */

#define SHIFT 6
static int NCATS = 1<<SHIFT ;

translate_from_cats (cell, xcell, ncols, k)
    CELL *cell;
    double *xcell;
{
    struct Categories *pcats;
    BTREE *btree;
    int i, idx;
    CELL cat, key;
    double vbuf[1<<SHIFT];
    double *values;
    char *label;

    btree = &list[k].btree;
    pcats = &list[k].cats;

    while (ncols--)
    {
	cat = *cell++;

/* compute key as cat/NCATS * NCATS, adjusting down for negatives
 * and idx so that key+idx == cat
 */
	if (cat < 0)
	    key = - (((-cat-1) >> SHIFT) << SHIFT) - NCATS;
	else
	    key = (cat >> SHIFT) << SHIFT;
	idx = cat - key;

/* If key not already in the tree, sscanf() all cats for this key
 * and put them into the tree
 */
	if (!btree_find (btree, &key, &values))
	{
	    values = vbuf;
	    for (i = 0; i < NCATS; i++)
	    {
		if ((label = G_get_cat ((CELL)(i+key), pcats)) == NULL
		|| sscanf (label, "%lf", values) != 1)
			*values = 0.0;
		values++;
	    }
	    btree_update (btree, &key, sizeof(key), values = vbuf, sizeof(vbuf));
	}

/* and finally lookup the translated value */
	*xcell++ = values[idx];
    }
}

static
compare_ints (a, b)
    int *a, *b;
{
    return (*a - *b);
}

static
column_shift (cell, col, ncols)
    register int col, ncols;
    register CELL *cell;
{
    register int i;

/* if column offset, copy cell to itself shifting by col */
    if (col>0)
    {
	for (i = 0; i <ncols-col; i++)
	    cell[i] = cell[i+col];
	while (i < ncols)
	    cell[i++] = 0;
    }
    else if (col < 0)
    {
	col = -col;
	for (i = ncols-1; i >= col; i--)
	    cell[i] = cell[i-col];
	while (i >= 0)
	    cell[i--] = 0;
    }
}

static
readrow(fd, cell, row, dummy)
    CELL *cell;
{
    return G_get_map_row (fd, cell, row) >= 0;
}
