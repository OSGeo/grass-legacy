/* plot1() - Level One vector reading */
/* --------------------------------------------------------------
 * 2000-02-25: Eric G. Miller <egm2@jps.net>
 * Added ScreenPoly code to handle islands.  We don't want outer
 * boundary filling islands that are really holes.  Basically, the
 * code connects the outer ring with the inner ring(s) via two
 * colinear line segments.
 */
/* 12-30-1999 Bill Hughes
     Changed to dynamic allocation of x_screen, y_screen to remove the
     4096 vector line limit. */

#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"
#include "screenpoly.h"

extern int fillcolor;
extern int linecolor;
extern struct Cell_head window;


int plot1 (char *name, char *mapset, struct line_pnts *Points)
{
    int i, j, dofill;
    struct Map_info Map;
    double N, S, E, W;
    int t,b,l,r;
    int line, nisles;
    int nlines;
    int *x_screen, *y_screen;
    struct line_pnts *Isle;
    P_AREA *pa;
    SCREENPOLY *spArea, *spLine, *spIsle, *spClip, *spTmp;

    i = Vect_open_old (&Map, name, mapset);

    if (2 > i)
	G_fatal_error ("Failed opening vector file");

    t = D_get_d_north();
    b = D_get_d_south();
    l = D_get_d_west();
    r = D_get_d_east();

    G_setup_plot (t, b, l, r, D_move_abs, D_cont_abs);

    /* TODO: Not working yet */
    spClip = ScreenPolyNew (4);
    ScreenPolyAddPoint (spClip, l, t);
    ScreenPolyAddPoint (spClip, r, t);
    ScreenPolyAddPoint (spClip, r, b);
    ScreenPolyAddPoint (spClip, l, b);

    fprintf (stderr,"Plotting ... "); fflush (stderr);
    nlines = V2_num_areas(&Map);
    for (line = 1;line <= nlines; line++)
    {
        /* Percentages */
        G_percent (line, nlines, 5);

        dofill = V2_area_att (&Map, line); /* Returns 0 if area is unlabelled */
        /* Skip areas that we wont fill or draw outlines for */
        if (!dofill && linecolor <= 0)
            continue;
        
	switch (Vect_get_area_points(&Map, line, Points))
	{
	case -1:
	    Vect_close (&Map);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	    break;
	case -2:
	    fprintf (stdout,"Done\n");
	    Vect_close (&Map);
	    return  0;
	    break;
	}

	V2_get_area_bbox(&Map, line, &N, &S, &E, &W);
	if ( S > window.north || N < window.south || 
	     W > window.east || E < window.west)
		continue;

        /* Get a SCREENPOLY type for the polygon line */
        spLine = ScreenPolyNew(Points->n_points);
	for(i=0; i < Points->n_points; i++)
	{
            /* Populate the SCREENPOLY type with screen coordinates */
            ScreenPolyAddPoint (
                spLine,
                (int) (D_u_to_d_col (Points->x[i])),
                (int) (D_u_to_d_row (Points->y[i]))
            );
	}
        
        /* Clip lines to screen window 
         * TODO: Not working yet.
         *********
        spTmp = ScreenPolyClip (spLine, spClip);
        ScreenPolyDestroy (spLine);
        if (spTmp == NULL)
            continue;
        spLine = spTmp;
        *******/

        /* Are we filling this poly */
        if (dofill)
        {
            /* Make copy of polygon */
            spArea = ScreenPolyNew (spLine->count);
            ScreenPolyCopy (spLine, spArea);
       
            /* Need to do extra work to handle islands */
            V2_get_area (&Map, line, &pa);
            if ((nisles = pa->n_isles) > 0)
            {
                /* Then we need to "merge" in island polys */
                for (i = 0; i < nisles ; i++)
                {
                    Isle = Vect_new_line_struct();
                    Vect_get_isle_points (&Map, pa->isles[i], Isle);
                    spIsle = ScreenPolyNew(Isle->n_points);
                    for (j = 0; j < Isle->n_points; j++)
                        ScreenPolyAddPoint (
                                spIsle, 
                                (int) (D_u_to_d_col (Isle->x[j])),
                                (int) (D_u_to_d_row (Isle->y[j]))
                        );
                    Vect_destroy_line_struct (Isle);
                    /* TODO: Not working yet.
                     *********
                    spTmp = ScreenPolyClip (spIsle, spClip);
                    ScreenPolyDestroy (spIsle);
                    if (spTmp == NULL)
                        continue;
                    spIsle = spTmp;
                    **********/
                    spTmp = ScreenPolyMerge (spArea, spIsle);
                    ScreenPolyDestroy (spIsle);
                    ScreenPolyDestroy (spArea);
                    if (spTmp == NULL)
                        G_fatal_error ("Failed to merge islands with outer poly");
                    spArea = spTmp;
                }
                    
            }
           
            i = ScreenPolyToArrays (spArea, &x_screen, &y_screen);
            ScreenPolyDestroy (spArea);

            if (i == 0)
            {
                /* G_warning ("No points in point struct ??"); */
                continue;
            }
            else if (i < 0)
                G_fatal_error ("Converting ScreenPoly to Arrays");
            else
            {
                R_standard_color(fillcolor);
                R_polygon_abs(x_screen, y_screen, i);
            }
            G_free (x_screen);
            G_free (y_screen);
            
        }

        /* Are we drawing poly outlines ? */
	if (linecolor > 0)
	{
            i = ScreenPolyToArrays (spLine, &x_screen, &y_screen);   
            if (i > 0)
            {
                R_standard_color(linecolor);
                R_polyline_abs(x_screen,y_screen, i);
                G_free(x_screen);
                G_free(y_screen);
            }
            else if (i < 0)
                G_fatal_error ("Converting ScreenPoly to Arrays");
	}
        ScreenPolyDestroy (spLine);

    }
    /* do newline */
    fprintf (stderr, "\n");

    /* TODO: Clipping not yet functional */
    /* ScreenPolyDestroy (spClip); */
    return 0;
}


/* vim: set softtabstop=4 shiftwidth=4 expandtab: */
