#include "digit.h"
#include "gis.h"

static
struct list
{
    double size;
    int index;
    CELL cat;
} *list;
static int nareas ;

do_areas (Map, Points)
    struct Map_info *Map;
    struct line_pnts *Points;
{
    int i,index;
    CELL cat;

    if (nareas <= 0) return 0;

    for (i = 0; i < nareas; i++)
    {
	index = list[i].index;
	cat   = list[i].cat;
	set_cat (cat);
	if(Vect_get_area_points (Map, index, Points) <= 0)
	{
	    fprintf (stderr, "*** Get area [%d] failed ***\n", index);
	    return -1;
	}
	G_plot_polygon (Points->x, Points->y, Points->n_points);
    }
    return nareas;
}

sort_areas (Map, Points)
    struct Map_info *Map;
    struct line_pnts *Points;
{
    int i,index;
    CELL cat;
    int compare();
    double G_area_of_polygon();

    G_begin_polygon_area_calculations();

/* first count valid areas */
    for (nareas = 0, index = 1; index <= Map->n_areas; index++)
    {
	if (area_ok(Map, index, &cat))
	    nareas++;
    }
    if (nareas == 0) return 0;

/* allocate list to hold valid area info */
    list = (struct list *) G_calloc (nareas, sizeof (struct list));

/* store area size,cat,index in list */
    for (i = 0, index = 1; index <= Map->n_areas; index++)
    {
	if (area_ok(Map, index, &cat))
	{
	    list[i].index = index;
	    list[i].cat = cat;
	    if(Vect_get_area_points (Map, index, Points) <= 0)
	    {
		fprintf (stderr, "*** Get area [%d] failed ***\n", index);
		return -1;
	    }
	    list[i].size = G_area_of_polygon (Points->x, Points->y, Points->n_points);
	    i++;
	}
    }

/* sort the list by size */
    qsort (list, nareas, sizeof(struct list), compare);
    return nareas;
}

static
compare (a, b)
    struct list *a, *b;
{
    if (a->size < b->size)
	return 1;
    if (a->size > b->size)
	return -1;
    return 0;
}

static
area_ok(Map, index, cat)
    struct Map_info *Map;
    CELL *cat;
{
    int att;

    if (!AREA_ALIVE(&Map->Area[index]))
	return 0;
    att = Map->Area[index].att;
    if (att == 0)
	return 0;
    *cat = Map->Att[att].cat;

    return (*cat != 0);
}
