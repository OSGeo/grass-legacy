#include "glob.h"

static struct list
{
    char name[50];
    char mapset[50];
    void *fd;
    struct Categories cats;
    struct Colors colors;
    int have_cats;
    int have_colors;
} *list = NULL;

static int nmaps = 0;
static int count = 0;

static unsigned char *red = NULL;
static unsigned char *grn = NULL;
static unsigned char *blu = NULL;
static unsigned char *set = NULL;


openmap (name, mapset, code, row)
    char *name, *mapset;
    int *code;
{
    int i;
    int use_cats;
    int use_colors;

    use_cats = *code == '@';
    use_colors = (*code == '#' || *code == 'r' || *code == 'g' || *code == 'b');

    for (i=0; i<nmaps; i++)
	if (strcmp (list[i].name,name)==0 &&
	    strcmp (list[i].mapset,mapset)==0)
	    {
        	if (use_cats && !list[i].have_cats && !initcats(i))
		    return -1;
        	if (use_colors && !list[i].have_colors && !initcolors(i))
		    return -1;
		return i;
            }

    if (nmaps >= count)
	list = (struct list *) G3d_realloc (list, (count = nmaps+2) * sizeof(*list));

    strcpy (list[nmaps].name, name);
    strcpy (list[nmaps].mapset, mapset);
    list[nmaps].have_cats = 0;
    list[nmaps].have_colors = 0;
    if (use_cats && !initcats(nmaps))
	return -1;
    if (use_colors && !initcolors(nmaps))
	return -1;

    if((list[nmaps++].fd = G3d_openCellOld (name, mapset,&current_region,G3D_TILE_SAME_AS_FILE,G3D_USE_CACHE_DEFAULT)) < 0)
	return -1;
    else
    {
	return nmaps-1;
    }
}

closemaps()
{
    int i;

    for (i=0; i < nmaps; i++)
    {
	if (list[i].fd >= 0)
	{
	    G3d_closeCell (list[i].fd);
	    if (list[i].have_cats)
	    {
		list[i].have_cats = 0;
	    }
	    if (list[i].have_colors)
	    {
		list[i].have_colors = 0;
	    }
	}
    }
    nmaps = 0;
}

readmap(i, xcell, row, col, depth,  nrows, ncols, ndepths)
    double *xcell;
{
    double *bp;

    bp = xcell;
    if (!readrow (list[i].fd, bp, row, col, depth, nrows, ncols, ndepths))
        return 0;

    return 1;
}

initcats(i)
{
    int compare_ints();

    if (G3d_readCats (list[i].name, list[i].mapset, &list[i].cats) < 0)
    {
	fprintf (stderr, "Error reading category file for [%s in %s]\n",
		list[i].name, list[i].mapset);
	return 0;
    }
    list[i].have_cats = 1; /* so we don't do this twice */
    return 1;
}

initcolors(i)
{
    if(red == NULL)
    {
	int ncols;
        ncols = current_region.cols;
	red = (unsigned char *) G3d_malloc (ncols);
	grn = (unsigned char *) G3d_malloc (ncols);
	blu = (unsigned char *) G3d_malloc (ncols);
	set = (unsigned char *) G3d_malloc (ncols);
    }
    if (G3d_readColors (list[i].name, list[i].mapset, &list[i].colors) < 0)
    {
	fprintf (stderr, "Error reading color file for [%s in %s]\n",
		list[i].name, list[i].mapset);
	return 0;
    }
    list[i].have_colors = 1; /* so we don't do this twice */
    return 1;
}

translate_from_colors (rast, xcell, ncols, k, code)
    double *rast;
    double *xcell;
{
/* convert cell to color
 * Grey scale is based on the C.I.E X,Y,Z system where Y is luminance.
 * Jain, Anil K., Fundamentals of Digital Image Processing, 1989
 * Prentice Hall, NJ., p. 67.
 */
    G_lookup_d_raster_colors (rast, red, grn, blu, set, ncols, &list[k].colors);
    switch(code)
    {
    case '#': /* grey */
	while (--ncols >= 0)
	    xcell[ncols] = (double) ((18*red[ncols] + 81*grn[ncols] + blu[ncols] + 50)/100);
	    /*
	    xcell[ncols] = .177*red[ncols] + .813*grn[ncols] + .011*blu[ncols];
	    */
	break;
    case 'r': /* red */
	while (--ncols >= 0)
	    xcell[ncols] = (double) red[ncols];
	break;
    case 'g': /* green */
	while (--ncols >= 0)
	    xcell[ncols] = (double) grn[ncols];
	break;
    case 'b': /* blue */
	while (--ncols >= 0)
	    xcell[ncols] = (double) blu[ncols];
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

translate_from_cats (dcell, xcell, ncols, k)
    double *dcell;
    double *xcell;
{
    struct Categories *pcats;
    int i;
    double cat, key, idx;
    double vbuf[1<<SHIFT];
    double value;
    char *label;

    pcats = &list[k].cats;

    for (; ncols-- > 0; dcell++, xcell++)
    {
	cat = *dcell;

        if(ISNULL_D(dcell))
	{
	    SETNULL_D(xcell);
	    continue;
	}
        
        if ((label = G_get_cat ((int) cat, pcats)) == NULL
		|| sscanf (label, "%lf", &value) != 1)
            SETNULL_D(xcell);
        else
            *xcell=value;
    }
}

static
compare_ints (a, b)
    int *a, *b;
{
    return (*a - *b);
}

map_is_fp (map)
    void *map;
{
    return (G3d_fileTypeMap (map) != G3D_DOUBLE);
}
