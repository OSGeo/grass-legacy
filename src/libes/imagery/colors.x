/* %W%  %G% */
/**********************************************************
* I_read_ref_colors (&ref);
*    determines, from the ref structure, the cellfiles which
*    are to be used for the red,grn,blu colors, reads the
*    GRASS color table for each file into the ref structure
*    
* returns 1 ok, 0 no cellfiles have color associations
*        -1 error reading GRASS color tables
*
* NOTE: these routines make some assumptions about the
*       gis.h Colors structure and how it is read by
*       G_read_colors().
**********************************************************/
#include "imagery.h"

static int err;

I_read_ref_colors (ref)
    struct Ref *ref;
{
    unsigned char *read_color();

    err = 0;

    ref->red.table = read_color (ref,
	ref->red.n,
	&ref->red.min,
	&ref->red.max,
	&ref->red.c0);
    ref->grn.table = read_color (ref,
	ref->grn.n,
	&ref->grn.min,
	&ref->grn.max,
	&ref->grn.c0);
    ref->blu.table = read_color (ref,
	ref->blu.n,
	&ref->blu.min,
	&ref->blu.max,
	&ref->blu.c0);

    if (ref->red.n < 0 && ref->blu.n < 0 && ref->blu.n < 0)
	return 0;
    return err ? -1 : 1;
}

/* reads the color table for the cell file and returns the red component
   Using only the red essentially assumes that the color table is
   a grey scale. If not, at least we will get consistent images
   when the cell is once as red, another time as blu or grn.
*/

static
unsigned char *
read_color (ref, n, min, max, c0)
    struct Ref *ref;
    CELL *min, *max;
    unsigned char *c0;
{
    struct Colors color;
    unsigned char *table;

    if (n < 0)
	return (table = NULL);

#ifdef DEBUG
printf("read_color(%s in %s)\n", ref->file[n].name, ref->file[n].mapset);
#endif

    if (G_read_colors (ref->file[n].name, ref->file[n].mapset, &color) < 0)
    {
	err = 1;
	return (table = NULL);
    }

    *min = color.min;
    *max = color.max;
    *c0  = color.r0;

    table = color.red;
    free (color.grn);
    free (color.blu);

/* make sure 0 is handled properly if range goes from neg to positive */
    if (*min <= 0 && *max >= 0)
	table[-(*min)] = *c0;

    return table;
}

I_write_ref_colors (ref)
    struct Ref *ref;
{
    int n;

    err = 0;

    if((n = ref->red.n) >= 0)
	write_colors (ref->file[n].name, ref->file[n].mapset,
		ref->red.table,
		ref->red.min,
		ref->red.max,
		ref->red.c0);
    if((n = ref->grn.n) >= 0)
	write_colors (ref->file[n].name, ref->file[n].mapset,
		ref->grn.table,
		ref->grn.min,
		ref->grn.max,
		ref->grn.c0);
    if((n = ref->blu.n) >= 0)
	write_colors (ref->file[n].name, ref->file[n].mapset,
		ref->blu.table,
		ref->blu.min,
		ref->blu.max,
		ref->blu.c0);

    return err ? -1 : 1;
}

static
write_colors (name, mapset, table, min, max, c0)
    char *name, *mapset;
    unsigned char *table, c0;
    CELL min, max;
{
    struct Colors colr;

    colr.red = colr.grn = colr.blu = table;
    colr.r0  = colr.g0  = colr.b0  = c0;
    colr.min = min;
    colr.max = max;

    if (G_write_colors (name, mapset, &colr) < 0)
	err = 1;
}
