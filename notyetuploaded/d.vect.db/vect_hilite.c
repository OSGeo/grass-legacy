/* plot2() - Level Two vector reading */

#include "gis.h"
#include "digit.h"
#include "Vect.h"

extern double D_get_d_north();
extern double D_get_d_south();
extern double D_get_d_west();
extern double D_get_d_east();

extern int D_move_abs();
extern int D_cont_abs();

hilite (P_map, cat, color, hilite_color)
    struct Map_info *P_map;
    int color, hilite_color;
{
    double *x, *y;
    int i, np, l;
    int line, nlines, area;
    double N,S,E,W;
    struct Cell_head window;
    /*char *dig__P_init(), *err;*/
    struct line_pnts *Points;
    P_AREA *Area;
    P_LINE *Line;
    static int last_cat, last_cat_set=0;

    Points = Vect_new_line_struct ();
    G_get_set_window (&window);

    G_setup_plot (
	D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east(),
	D_move_abs, D_cont_abs);


    nlines = V2_num_lines (P_map);

    for (line = 1; line <= nlines; line++)
    {
	Line = &(P_map->Line[line]);

	if (!G_window_overlap (&window, Line->N, Line->S, Line->E, Line->W))
	    continue;

        if(!cat && !last_cat_set)
	    R_standard_color(color);
        else if(Line->att && last_cat_set && P_map->Att[Line->att].cat == last_cat)
	    R_standard_color(color);
	else if(Line->att && P_map->Att[Line->att].cat == cat)
	    R_standard_color(hilite_color) ;
	else continue;

        if (V2_read_line (P_map, Points, line) < 0)
	{
	    fprintf (stderr, "\nWARNING: Error reading line number %d\n", line);
	    return -1;
	}
	np = Points->n_points;
	x  = Points->x;
	y =  Points->y;

        /* TEMP do SITES */

	for(i=1; i < np; i++)
	{
	    G_plot_line (x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }

    for (area = 1; area <= P_map->n_areas; area++)
    {
	Area = &(P_map->Area[area]);
	if (!G_window_overlap (&window, Area->N, Area->S, Area->E, Area->W))
	    continue;

        if(!Area->att) continue;
	/* do only labeled objects */

	if(P_map->Att[Area->att].cat == cat)
	    R_standard_color(hilite_color) ;
        else if(last_cat_set && P_map->Att[Area->att].cat == last_cat)
	    R_standard_color(color);
	else continue;

        for(l = 0; l< Area->n_lines; l++)
	{
	    if(Area->lines[l] > 0) line = Area->lines[l];
	    else line = (-1) * Area->lines[l];

            if (V2_read_line (P_map, Points, line) < 0)
	    {
	        fprintf (stderr, "\nWARNING: Error reading line number %d\n", line);
	        return -1;
	    }
	    np = Points->n_points;
	    x  = Points->x;
	    y =  Points->y;

	    for(i=1; i < np; i++)
	    {
	        G_plot_line (x[0], y[0], x[1], y[1]);
	        x++;
	        y++;
	    }
        } /* drawing area boundary */
    }
    if(cat)
    {
       last_cat = cat;
       if(!last_cat_set) last_cat_set = 1;
    }
    return 0;
}
