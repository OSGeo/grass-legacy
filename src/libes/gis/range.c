/**********************************************************************
 *
 *  G_read_range (name, mapset, range)
 *      char *name                   name of map
 *      char *mapset                 mapset that map belongs to
 *      struct Range *range          struct to hold range info
 *
 *  Reads the data range information associated with map layer "map"
 *  in mapset "mapset" 
 *
 *   returns:    1  if successful
 *               0  missing, range faked using largest category
 *              -1  on fail
 *
 *  note:   a warning message is printed if the file is missing or incorrect
 *
 **********************************************************************
 *
 *  G_write_range (name, range)
 *      char *name                  name of map
 *      struct Range *range         struct holding range info
 *
 *  Writes the range information associated with map layer "map"
 *
 *   returns:    0  if successful
 *              -1  on fail
 *
 **********************************************************************
 *
 * G_init_range (range)
 *      struct Range *range         struct for range info
 *
 * initializes range structure for call to G_update_range()
 *
 **********************************************************************
 *
 * G_update_range (cat, range)
 *    CELL cat                    cat to be factored into range
 *    struct Range *range         struct for range info
 **********************************************************************
 *
 * G_get_range_min_max (range, min, max)
 *    struct Range *range;
 *    CELL *min, *max;
 **********************************************************************/

#include "gis.h"

G_read_range (name, mapset, range)
    char *name, *mapset;
    struct Range *range;
{
    FILE *fd;
    long x[4];
    char buf[200];
    int i;
    CELL ncats;

    range->nmin = 0 ;
    range->nmax = 0 ;
    range->pmin = 0 ;
    range->pmax = 0 ;
    fd = NULL;

    sprintf (buf,"cell_misc/%s", name);
    if (G_find_file2 (buf, "range", mapset))
    {
	fd = G_fopen_old (buf, "range", mapset);
	if (!fd)
	    goto error;

	if (!fgets (buf, sizeof buf, fd))
	    goto error;
	x[0]=x[1]=x[2]=x[3]=0;
	i = sscanf (buf, "%ld%ld%ld%ld", &x[0], &x[1], &x[2], &x[3]);
	if (i <= 0)
	    goto error;

	while (i-- > 0)
	{
	    if (x[i] < 0)
	    {
		if (range->nmin == 0 || x[i] < range->nmin)
		    range->nmin = x[i];
		if (range->nmax == 0 || x[i] > range->nmax)
		    range->nmax = x[i];
	    }
	    else if (x[i] > 0)
	    {
		if (range->pmin == 0 || x[i] < range->pmin)
		    range->pmin = x[i];
		if (range->pmax == 0 || x[i] > range->pmax)
		    range->pmax = x[i];
	    }
	}
	fclose(fd) ;
	return 1;
    }
    else
    {
	ncats = G_number_of_cats (name, mapset);
	if (ncats < 0)
	    return -1;
	range->pmax = ncats;
	if(range->pmax > 0) range->pmin = 1;
	return 0;
    }

error:
    if (fd)
	fclose(fd) ;
    sprintf (buf, "can't read range file for [%s in %s]", name, mapset);
    G_warning (buf);
    return -1;
}

G_write_range (name, range)
    char *name;
    struct Range *range;
{
    FILE *fd;
    char buf[200];

    sprintf (buf,"cell_misc/%s", name);
    fd = G_fopen_new (buf, "range");
    if (!fd)
	goto error;

    fprintf (fd, "%ld %ld %ld %ld\n",
	(long)range->nmin, (long)range->nmax,
	(long)range->pmin, (long)range->pmax);
    fclose (fd);
    return 0;

error:
    sprintf (buf, "can't write range file for [%s in %s]",
	name, G_mapset());
    G_warning (buf);
    return -1;
}

G_update_range (cat, range)
    CELL cat;
    struct Range *range;
{
    if (cat > 0)
    {
	if (range->pmin == 0 || cat < range->pmin)
	    range->pmin = cat;
	if (range->pmax == 0 || cat > range->pmax)
	    range->pmax = cat;
    }
    else if (cat < 0)
    {
	if (range->nmin == 0 || cat < range->nmin)
	    range->nmin = cat;
	if (range->nmax == 0 || cat > range->nmax)
	    range->nmax = cat;
    }
}

G_row_update_range (cell, n, range)
    CELL *cell;
    struct Range *range;
{
    CELL cat;

    while (n-- > 0)
    {
	cat = *cell++;
	if (cat > 0)
	{
	    if (range->pmin == 0 || cat < range->pmin)
		range->pmin = cat;
	    if (range->pmax == 0 || cat > range->pmax)
		range->pmax = cat;
	}
	else if (cat < 0)
	{
	    if (range->nmin == 0 || cat < range->nmin)
		range->nmin = cat;
	    if (range->nmax == 0 || cat > range->nmax)
		range->nmax = cat;
	}
    }
}

G_init_range (range)
    struct Range *range;
{
    range->nmin = 0 ;
    range->nmax = 0 ;
    range->pmin = 0 ;
    range->pmax = 0 ;
}

G_get_range_min_max(range, min, max)
    struct Range *range;
    CELL *min, *max;
{
    *min = range->nmin ? range->nmin : range->pmin;
    *max = range->pmax ? range->pmax : range->nmax;
}
