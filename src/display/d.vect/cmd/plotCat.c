/* plotCat - uses level 2 access to vector data for reads */
#include "gis.h"
#include "display.h"
#include "Vect.h"
extern int quiet;

static struct xy_arrays {
    int nused;
    int nparts;
    int *npnts;
    double **xs;
    double **ys;
} ring_data = {0,0, NULL, NULL, NULL};

static void init_xy_arrays (int parts)
{
    ring_data.nused  = 0;
    ring_data.npnts  = G_calloc (parts, sizeof(int));
    ring_data.xs     = (double **)G_malloc (sizeof(double *) * parts);
    ring_data.ys     = (double **)G_malloc (sizeof(double *) * parts);
    ring_data.nparts = parts;
}

static void free_xy_arrays (void)
{
    register int i;
    
    if (ring_data.nparts) {
        free (ring_data.npnts);
        for (i = 0; i < ring_data.nused; i++) {
            free (ring_data.xs[i]);
            free (ring_data.ys[i]);
        }
    }
    ring_data.nparts = 0;
    ring_data.nused  = 0;
    ring_data.npnts  = NULL;
    ring_data.xs     = NULL;
    ring_data.ys     = NULL;
}
    

static void
add_boundary_to_xy_arrays (struct line_pnts *pnts)
{
    register int i, count;

    if (ring_data.nused >= ring_data.nparts)
        G_fatal_error (" [%s :: %d] Bad call to add_boundary_to_xy_arrays",
                __FILE__, __LINE__);
    
    count = pnts->n_points;
    
    ring_data.xs[ring_data.nused] = G_malloc (sizeof (double) * count);
    ring_data.ys[ring_data.nused] = G_malloc (sizeof (double) * count);
    
    for (i = 0; i < count; i++) {
        ring_data.xs[ring_data.nused][i] = pnts->x[i];
        ring_data.ys[ring_data.nused][i] = pnts->y[i];
    }
    ring_data.npnts[ring_data.nused++] = count;
}

static int *catlist = NULL;
static int catcnt = 0;

static int
build_catlist (char **cats)
{
    int i, cat;
    char *cur;
    
    catlist = NULL;
    catcnt  = 0;
    
    for (i = 0, cur = cats[i] ; cur; cur = cats[++i])
        ;
    if (i) 
    {
        catlist = G_calloc (sizeof (int), i);
        for (i = 0, cur = cats[i]; cur; cur = cats[++i])
        {
            cat = atoi(cur);
            if (cat)
                catlist[catcnt++] = cat;
        }
        if (!catcnt)
        {
            free (catlist);
            catlist = NULL;
        }
        i = catcnt;
    }

    return i;
}

static int
lookup_cat (int cat)
{
    int i;

    for (i = 0; i < catcnt; i++)
    {
        if (catlist[i] == cat)
            return cat;
    }
    /* not found */
    return 0;
}
    
int plotCat (
    char *name,char *mapset,
    struct line_pnts *Points,
    char **vect_cats,int fill)
{
    double *x, *y;
    double N,S,E,W;
    char buf[128];
    int i, j, np;
    int line, nlines;
    int nareas, nisles, area_cnt, a_index;
    int catscountL=0, catscountA=0; /* count number of displayed lines/areas */
    struct Cell_head window;
    struct Map_info P_map;
    P_AREA *pa;
    
    fflush (stdout);

    Vect_set_open_level(2);
    if ( 0 > Vect_open_old (&P_map, name, mapset))
    {
	G_warning("Cannot open vector %s@%s on level 2. Run v.support on this vector.", name, mapset); 
	return -1;
    }

    if (!build_catlist (vect_cats)) {
        G_warning("Bad category list");	
        return -2;
    }

    Vect__get_window (&P_map, &N, &S, &E, &W);
    if(!quiet)
      Vect_print_header(&P_map);

    G_get_set_window (&window);
    if (!quiet) {
		fprintf (stdout,"\n");
		G_format_northing (N, buf, window.proj);
		fprintf (stdout," North: %s\n", buf);

		G_format_northing (S, buf, window.proj);
		fprintf (stdout," South: %s\n", buf);

		G_format_easting (E, buf, window.proj);
		fprintf (stdout," East:  %s\n", buf);

		G_format_easting (W, buf, window.proj);
		fprintf (stdout," West:  %s\n", buf);
		fprintf (stdout,"\n");
	}

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    nlines = V2_num_lines (&P_map);
    nareas = V2_num_areas (&P_map);

/*********************** Note ***********************

   Removed original ifdef OLD that dpg had in d.vect
   code. jaf

****************************************************/


    /* let library do window checking for us */

    Vect_set_constraint_region (&P_map, window.north, window.south, window.east, window.west);

   if (nareas > 0)
   {
       for(area_cnt=1; area_cnt<=nareas; area_cnt++)
       {
           if(lookup_cat(V2_area_att(&P_map,area_cnt)))
           {
               a_index = P_map.Att[P_map.Area[area_cnt].att].index;
               /* Points = Vect_new_line_struct(); */
               V2_get_area (&P_map, area_cnt, &pa);
               nisles = pa->n_isles;
               Vect_get_area_points(&P_map,a_index,Points);
               np = Points->n_points;
               x  = Points->x;
               y =  Points->y;
               if (nisles && fill) {
                   init_xy_arrays (1 + nisles);
                   add_boundary_to_xy_arrays (Points);
                   for (j = 0; j < nisles; j++) {
                       Vect_get_isle_points (&P_map, pa->isles[j], Points);
                       add_boundary_to_xy_arrays (Points);
                   }
                   G_plot_area (ring_data.xs,
                           ring_data.ys,
                           ring_data.npnts,
                           ring_data.nused);
                   free_xy_arrays();
               }
               else if (fill) 
               {
                   G_plot_polygon(x,y,np);
               }
               else
               {
                   for (i=1; i < np; i++)
                   {
                       G_plot_line (x[0], y[0], x[1], y[1]);
                       x++;
                       y++;
                   }
               }
               catscountA++;
           }
       } /* end of for() */
   } /* end if nareas > 0 */
 
   if(nlines > 0)
   {

       for (line = 1; line <= nlines; line++)
       {
           int ret;

           if(lookup_cat(V2_line_att(&P_map,line)))
           {
               if (0 > (ret = V2_read_line (&P_map, Points, line)))
               {
                   if (ret == -2)
                       G_warning ("Read error\n");
                   break;
               }
               np = Points->n_points;
               x  = Points->x;
               y =  Points->y;
               for (i=1; i < np; i++)
               {
                   G_plot_line (x[0], y[0], x[1], y[1]);
                   x++;
                   y++;
               }
               catscountL++;
           }
       }    /* end for lines */
   } /* end if nlines > 0 */

   
    if ((catscountL + catscountA) == 0)
       fprintf(stderr, "No vectors found for selected categories\n");

    return 0;
}
