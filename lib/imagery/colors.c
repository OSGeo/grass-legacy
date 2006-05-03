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
#include <stdlib.h>
#include <grass/imagery.h>


static unsigned char *read_color(char *,char *,char *,char *,CELL *,CELL *);
static unsigned char *get_colors(char *,char *,char *,char *,CELL *,CELL *);
static unsigned char *build_index(CELL,CELL);
static int write_colors(char *,char *,char *,char *,unsigned char *,CELL,CELL);


int I_read_group_colors ( char *group, struct Ref *ref)
{
    if (ref->red.n < 0 && ref->blu.n < 0 && ref->blu.n < 0)
	return 0;

    I_read_group_red_colors (group, ref);
    I_read_group_grn_colors (group, ref);
    I_read_group_blu_colors (group, ref);

    return 1;
}

int I_read_group_red_colors ( char *group, struct Ref *ref)
{
    int n;

    if (ref->red.table != NULL)
	free (ref->red.table);
    ref->red.table = NULL;

    if (ref->red.index != NULL)
	free (ref->red.index);
    ref->red.index = NULL;

    if ((n = ref->red.n) >= 0)
    {
	ref->red.table = read_color (group, "RED",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->red.min, &ref->red.max);

	if (ref->red.min < 0 || ref->red.max > 255)
	    ref->red.index = build_index (ref->red.min, ref->red.max);
    }

    return 0;
}

int I_read_group_grn_colors ( char *group, struct Ref *ref)
{
    int n;

    if (ref->grn.table != NULL)
	free (ref->grn.table);
    ref->grn.table = NULL;

    if (ref->grn.index != NULL)
	free (ref->grn.index);
    ref->grn.index = NULL;

    if ((n = ref->grn.n) >= 0)
    {
	ref->grn.table = read_color (group, "GRN",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->grn.min, &ref->grn.max);

	if (ref->grn.min < 0 || ref->grn.max > 255)
	    ref->grn.index = build_index (ref->grn.min, ref->grn.max);
    }

    return 0;
}

int I_read_group_blu_colors (char *group, struct Ref *ref)
{
    int n;

    if (ref->blu.table != NULL)
	free (ref->blu.table);
    ref->blu.table = NULL;

    if (ref->blu.index != NULL)
	free (ref->blu.index);
    ref->blu.index = NULL;

    if ((n = ref->blu.n) >= 0)
    {
	ref->blu.table = read_color (group, "BLU",
	    ref->file[n].name, ref->file[n].mapset,
	    &ref->blu.min, &ref->blu.max);

	if (ref->blu.min < 0 || ref->blu.max > 255)
	    ref->blu.index = build_index (ref->blu.min, ref->blu.max);
    }

    return 0;
}

int I_free_group_colors  (struct Ref *ref)

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

    return 0;
}

static unsigned char *read_color (
    char *group,
    char *file,
    char *name,char *mapset,
    CELL *min,CELL *max)
{
    unsigned char *table;
    struct Histogram histo;

    G_debug(2, "read_color(%s: %s in %s)", file, name, mapset);

    if(table = get_colors (group, file, name, mapset, min, max))
	return table;

    I_get_histogram (name, mapset, &histo);
    I_histo_eq (&histo, &table, min, max);

    return table;
}

static unsigned char *get_colors (
    char *group, char *file, char *name,
    char *mapset,
    CELL *min, CELL *max)
{
    long xmin, xmax;
    int value, n;
    char element[200];
    FILE *fd;
    unsigned char *table, *t;

    sprintf (element, "group/%s/colors/%s/%s", group, mapset, name);

    fd = G_fopen_old (element, file, G_mapset());
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
	if (fscanf (fd, "%d", &value) != 1)
	    break;
	*t++ = value;
    }
    while (n++ <= xmax)
	*t++ = 0;
    fclose (fd);

    return table;
}

/*
 * this routine builds a translation index for both data and colors
 * if the color table (and thus the data) is outside the range 0-255
 */
static unsigned char *build_index(CELL min,CELL max)
{
    int n;
    CELL i;
    double width;
    unsigned char *index;

    width = n = max - min + 1;
    index = (unsigned char *) G_malloc (n);

    for (i = min; i <= max; i++)
    {
	n = (i - min) * 256 / width;
	if (n >= 256) n = 255;
	index[i-min] = n;
    }
    return index;
}

int I_write_group_colors (char *group, struct Ref *ref)
{
    int stat;

    stat = 1;
    if(!I_write_group_red_colors (group, ref))
	stat = 0;
    if(!I_write_group_grn_colors (group, ref))
	stat = 0;
    if(!I_write_group_blu_colors (group, ref))
	stat = 0;
    return stat;
}

int I_write_group_red_colors (char *group, struct Ref *ref)
{
    int n;
    int stat;

    stat = 1;

    if((n = ref->red.n) >= 0)
	stat = write_colors (group, "RED",
		ref->file[n].name, ref->file[n].mapset,
		ref->red.table, ref->red.min, ref->red.max);

    return stat;
}

int I_write_group_grn_colors (char *group, struct Ref *ref)
{
    int n;
    int stat;

    stat = 1;
    if((n = ref->grn.n) >= 0)
	stat = write_colors (group, "GRN",
		ref->file[n].name, ref->file[n].mapset,
		ref->grn.table, ref->grn.min, ref->grn.max);

    return stat;
}

int I_write_group_blu_colors (char *group, struct Ref *ref)
{
    int n;
    int stat;

    stat = 1;
    if((n = ref->blu.n) >= 0)
	stat = write_colors (group, "BLU",
		ref->file[n].name, ref->file[n].mapset,
		ref->blu.table, ref->blu.min, ref->blu.max);

    return stat;
}

static int write_colors (
    char *group, char *file, char *name,
    char *mapset,
    unsigned char *table,
    CELL min,CELL max)
{
    FILE *fd;
    char element[200];

    sprintf (element, "group/%s/colors/%s/%s", group, mapset, name);

    fd = G_fopen_new (element, file);
    if (fd == NULL)
    {
	G_warning("group [%s] - can't write %s colors for [%s] in [%s]",
		group, file, name, mapset);

	return 0;
    }
    fprintf (fd, "%ld %ld\n", (long) min, (long) max);

    while (min++ <= max)
	fprintf (fd, "%d\n", (int) *table++);

    fclose (fd);

    return 1;
}
