/* plot() - vector reading */

#include "gis.h"
#include "Vect.h"
#include "digit.h"

static
struct list
{
    double size;
    int index;
    CELL cat;
} *list;
static int nareas ;

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();
extern double D_u_to_d_col();
extern double D_u_to_d_row();
 
extern int D_move_abs();
extern int D_cont_abs();
extern struct Cell_head window;
extern int linecolor;


plot (name, mapset, Points, colors)
    char *name, *mapset;
    struct line_pnts *Points;
    struct Colors *colors;
{
    int i, j, ncolor, natt;
    struct Map_info Map;
    double *x, *y, px, py;
    int line;
    double N, S, E, W;


    N = S = E = W = 0;
    /*fd = open_vect (name, mapset);*/
    i = Vect_open_old (&Map, name, mapset);

    if (i < 2)
	G_fatal_error ("Failed opening vector file");

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);

    printf ("Sorting areas by size ... "); fflush (stdout);
    nareas = sort_areas(&Map, Points);
    printf ("Plotting ... "); fflush (stdout);
/*
    nlines = V2_num_areas(&Map);
*/

    for (i = 0;i < nareas; i++)
    {
	line = list[i].index;
	switch (Vect_get_area_points(&Map, line, Points))
	{
	case -1:
	    Vect_close (&Map);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	    break;
	case -2:
	    printf ("Done\n");
	    Vect_close (&Map);
	    return  0;
	    break;
	}
	V2_get_area_bbox(&Map , line, &N, &S, &E, &W);
	if ( S > window.north || N < window.south || W > window.east || E < window.west)
		continue;
	natt = list[i].cat;
	D_color((CELL) natt, colors);
	G_plot_polygon(Points->x,Points->y,Points->n_points);
	if (linecolor > 0)
		R_standard_color(linecolor);
	
	px = *(Points->x);
	py = *(Points->y);
	for(j=1; j < Points->n_points; j++)
	{
		G_plot_line(px, py, *(Points->x+j),*(Points->y+j));
		px = *(Points->x+j);
		py = *(Points->y+j);
	}
    }
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
	*cat = 0; /* unlabeled */
    else
	*cat = Map->Att[att].cat;

    return 1;
}
