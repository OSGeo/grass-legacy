
#include "gis.h"

/* 
 * FILE: range_rw.c
 *
 * range_read()
 * range_write()
 *
 * These are alternate versions of the standard G_read_range()
 * and G_write_range() library functions that will read ranges 
 * from user-defined database directories
 *
 */ 

/******************/
/*** read_range ***/
/******************/

read_range (element, name, mapset, range)
    char *element, *name, *mapset;
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

    sprintf (buf,"%s/%s", element, name);
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
	return 0;
    }

error:
    if (fd)
	fclose(fd) ;
    sprintf (buf, "can't read range file for [%s in %s]", name, mapset);
    G_warning (buf);
    return -1;
}

/*******************/
/*** write_range ***/
/*******************/

write_range (element, name, range)
    char *element, *name;
    struct Range *range;
{
    FILE *fd;
    char buf[200];

    sprintf (buf,"%s/%s", element, name);
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

