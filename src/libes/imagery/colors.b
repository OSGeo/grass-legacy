/* %W%  %G% */
/**********************************************************
* I_read_group_colors (group, &ref);
*    determines, from the ref structure, the cellfiles which
*    are to be used for the red,grn,blu colors.
*    
* returns
*   1 ok
*   0 no cellfiles have color associations
*
* First in looks within the group, under the "colors" directory
*       for a color table for each cell file
* If not found, it then uses the histogram for the cell file to
*       construct a table
*
**********************************************************/
#include "imagery.h"

static int err;
unsigned char *read_color();

I_read_group_colors (group, ref)
    char *group;
    struct Ref *ref;
{
    if (ref->red.n < 0 && ref->blu.n < 0 && ref->blu.n < 0)
	return 0;

    I_read_group_red_colors (group, ref);
    I_read_group_grn_colors (group, ref);
    I_read_group_blu_colors (group, ref);

    return 1;
}

I_read_group_red_colors (group, ref)
    char *group;
    struct Ref *ref;
{
    int n;

    if (ref->red.table)
	free (ref->red.table);
    ref->red.table = NULL;
    if ((n = ref->red.n) >= 0)
	ref->red.table = read_color (group, "RED",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->red.min, &ref->red.max);
}

I_read_group_grn_colors (group, ref)
    char *group;
    struct Ref *ref;
{
    int n;

    if (ref->grn.table)
	free (ref->grn.table);
    ref->grn.table = NULL;
    if ((n = ref->grn.n) >= 0)
	ref->grn.table = read_color (group, "GRN",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->grn.min, &ref->grn.max);
}

I_read_group_blu_colors (group, ref)
    char *group;
    struct Ref *ref;
{
    int n;

    if (ref->blu.table)
	free (ref->blu.table);
    ref->blu.table = NULL;
    if ((n = ref->blu.n) >= 0)
	ref->blu.table = read_color (group, "BLU",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->blu.min, &ref->blu.max);
}

I_free_group_colors (ref)
    struct Ref *ref;
{
    if (ref->red.table != NULL)
	free (ref->red.table);
    ref->red.table = NULL;

    if (ref->grn.table != NULL)
	free (ref->grn.table);
    ref->grn.table = NULL;

    if (ref->blu.table != NULL)
	free (ref->blu.table);
    ref->blu.table = NULL;
}

static
unsigned char *
read_color (group, type, name, mapset, min, max)
    char *group;
    char *type;
    char *name, *mapset;
    CELL *min, *max;
{
    unsigned char *table;
    struct Histogram histo;
    unsigned char *get_colors();

#ifdef DEBUG
printf("read_color(%s: %s in %s)\n", type, name, mapset);
#endif

    if(table = get_colors (group, name, mapset, min, max))
	return table;
    I_get_histogram (name, mapset, &histo);
    I_histo_eq (&histo, &table, min, max);
    return table;
}

static
unsigned char *
get_colors (group, name, mapset, min, max)
    char *group, *name, *mapset;
    CELL *min, *max;
{
    long xmin, xmax;
    int color, n;
    char element[200];
    FILE *fd;
    unsigned char *table, *t;

    sprintf (element, "group/%s/colors/%s", group, mapset);

    fd = G_fopen_old (element, name, G_mapset());
    if (fd == NULL)
	return (table = NULL);

    if (fscanf (fd, "%ld %ld", &xmin, &xmax) != 2)
    {
	fclose (fd);
	return (table = NULL);
    }
    if (xmin > xmax)
    {
	long temp;
	temp = xmin;
	xmin = xmax;
	xmax = temp;
    }
    *min = xmin;
    *max = xmax;
    t = table = (unsigned char *) G_malloc (xmax - xmin + 1);
    for (n = xmin; n <= xmax; n++)
    {
	if (fscanf (fd, "%d", &color) != 1)
	    break;
	*t++ = color;
    }
    while (n++ <= xmax)
	*t++ = 0;
    fclose (fd);

    return table;
}

I_write_group_colors (group, ref)
    struct Ref *ref;
{
    int n;

    err = 0;

    if((n = ref->red.n) >= 0)
	write_colors (group, ref->file[n].name, ref->file[n].mapset,
		ref->red.table, ref->red.min, ref->red.max);
    if((n = ref->grn.n) >= 0)
	write_colors (group, ref->file[n].name, ref->file[n].mapset,
		ref->grn.table, ref->grn.min, ref->grn.max);
    if((n = ref->blu.n) >= 0)
	write_colors (group, ref->file[n].name, ref->file[n].mapset,
		ref->blu.table, ref->blu.min, ref->blu.max);

    return err ? -1 : 1;
}

static
write_colors (group, name, mapset, table, min, max)
    char *group, *name, *mapset;
    unsigned char *table;
    CELL min, max;
{
    FILE *fd;
    char element[200];

    sprintf (element, "group/%s/colors/%s", group, mapset);

    fd = G_fopen_new (element, name);
    if (fd == NULL)
    {
	char msg[300];
	sprintf (msg, "group [%s] - can't write colors for [%s] in [%s]",
		group, name, mapset);
	G_warning (msg);
	err = 1;
	return;
    }
    fprintf (fd, "%ld %ld\n", (long) min, (long) max);
    while (min++ <= max)
	fprintf (fd, "%d\n", (int) *table++);
    fclose (fd);
}
