#include "Vect.h"
#include "gis.h"
#include "null.h"

static
struct list
{
    double size;
    int index;
    int equal;
    CELL cat;
} *list;
static int nareas ;

do_areas (Map, Points)
    struct Map_info *Map;
    struct line_pnts *Points;
{
    int i,index, k;
    CELL cat, last_cat;
    static struct line_pnts *Last_Points = NULL, *tmp_Points = NULL;
    int comp_double();

    if (nareas <= 0) return 0;
    if(!Last_Points)
       Last_Points = Vect_new_line_struct();
    if(!tmp_Points)
       tmp_Points = Vect_new_line_struct();

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
	if((list[i].equal<0) /* if we never checked area against previous area */
	    && (list[i].size == list[i-1].size) 
	    && (Points->n_points == Last_Points->n_points) )
        {
	/* check if the area is the same as the last area */
	   list[i].equal = 1;
	   /* copying Points to tmp_Points */
	   dig_alloc_points(tmp_Points, Points->n_points);
	   tmp_Points->n_points = Points->n_points;
	   for(k=0;k<Points->n_points; k++)
	   {
	      tmp_Points->y[k] = Points->y[k];
	      tmp_Points->x[k] = Points->x[k];
           }
	   /* sorting because points might be in different order */
	   Last_Points->n_points = Points->n_points;
	   /* since areas might start and end with different point,
	      and since first and last points are the same, we skip the
	      first point in both areas */
	   qsort(Last_Points->x + 1, Last_Points->n_points - 1, sizeof (double), 
	      comp_double);
	   qsort(tmp_Points->x + 1, tmp_Points->n_points - 1, sizeof (double), 
	      comp_double);
	   qsort(Last_Points->y + 1, Last_Points->n_points - 1, sizeof (double), 
	      comp_double);
	   qsort(tmp_Points->y + 1, tmp_Points->n_points - 1, sizeof (double), 
	      comp_double);
	   for(k=0;k<Points->n_points; k++)
	   {
	      if(k>0)
	      {
	         if(Last_Points->y[k] != tmp_Points->y[k]) list[i].equal = 0;
	         if(Last_Points->x[k] != tmp_Points->x[k]) list[i].equal = 0;
	      }
	      Last_Points->y[k] = Points->y[k];
	      Last_Points->x[k] = Points->x[k];
           }
         } /* checking if area is the same as previous area */
	 else
	 {
	   list[i].equal = 0;
	   /* copying Points to tmp_Points */
	   dig_alloc_points(Last_Points, Points->n_points);
	   Last_Points->n_points = Points->n_points;
	   for(k=0;k<Points->n_points; k++)
	   {
	      Last_Points->y[k] = Points->y[k];
	      Last_Points->x[k] = Points->x[k];
           }
         }

	 if(list[i].equal == 1)
	 {
	    fprintf(stderr, "WARNING! There are 2 identical areas in your map\n");
	    if(!is_labeled(cat) && !is_labeled(last_cat))
	    {
	       fprintf(stderr, "Both of them unlabeled\n", cat);
	       continue;
            }
	    if(cat == last_cat)
	    {
	       fprintf(stderr, "Both of them are labeled %ld\n", (long)cat);
	       continue;
            }
	    if(is_labeled(cat) && is_labeled(last_cat))
	    {
	       fprintf(stderr, "one of them is labeled %ld another one is labeled %ld\n", (long)last_cat, (long)cat);
	       fprintf(stderr, "Ignoring the area labeled %d\n", cat);
	       continue;
            }
            if(is_labeled(last_cat))
	    {
	       fprintf(stderr, "one of them is labeled %ld another one is unlabeled\n", (long) last_cat);
	       fprintf(stderr, "Ignoring the unlabeled area\n");
	       continue;
            }
 	    fprintf(stderr, "One of them is unlabeled another one is labeled %d\n", cat);
	    fprintf(stderr, "Ignoring the unlabeled area\n");
	    /* don't skip here, go to drawing area */
        } /* processing equal areas */

        last_cat = cat;
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
	if (area_ok(Map, index))
	    nareas++;
    }
    if (nareas == 0) return 0;

/* allocate list to hold valid area info */
    list = (struct list *) G_calloc (nareas, sizeof (struct list));

/* store area size,cat,index in list */
    for (i = 0, index = 1; index <= Map->n_areas; index++)
    {
	if (area_ok(Map, index))
	{
	    get_area_label(Map, index, &cat);
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

    /* initialize equals */
    for (i = 0; i < nareas; i++)
    {
	    if(i==0) list[i].equal = 0;
	    else list[i].equal = -1;
    }
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

static int
comp_double(i, j)
   double *i, *j;
{
   if(*i < *j)
       return -1;

   if(*i > *j)
       return 1;

   return 0;
}

static
area_ok(Map, index)
    struct Map_info *Map;
{
    return AREA_ALIVE(&Map->Area[index]);
}
