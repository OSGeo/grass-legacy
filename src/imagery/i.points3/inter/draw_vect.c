#include "globals.h"
#include "display.h"
#include "Vect.h"
#include <signal.h>

extern int interrupt;
static int set_signals();
static int reset_signals();
static void sigint(int);

int plot1 ( char *name,char *mapset, int  side)
{
    int i;
    struct Map_info Map;
    struct line_pnts *Points;
    double *x, *y;

    /* mbaba */
    double d_top, d_bottom, d_left, d_right;

    /*fd = open_vect (name, mapset);*/
    if (1 > Vect_open_old (&Map, name, mapset))
	G_fatal_error ("Failed opening vector file");
    Points = Vect_new_line_struct ();


    if (side == 0) {  	/* VIEW_MAP2 */
        d_top    = (VIEW_MAP2->vect.top); 
        d_bottom = (VIEW_MAP2->vect.bottom); 
        d_left   = (VIEW_MAP2->vect.left); 
        d_right  = (VIEW_MAP2->vect.right); 

        G_set_window (&VIEW_MAP2->vect.head); 

        D_set_clip_window((int) d_top, (int) d_bottom, 
			  (int) d_left, (int) d_right);

        D_do_conversions(&VIEW_MAP2->vect.head, 
			(int) d_top, (int) d_bottom, 
			(int) d_left, (int) d_right);

        G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		        D_move_abs, D_cont_abs);

    }
    else if (side == 1) {  	/* VIEW_MAP2_ZOOM */

        d_top    = (VIEW_MAP2_ZOOM->vect.top); 
        d_bottom = (VIEW_MAP2_ZOOM->vect.bottom); 
        d_left   = (VIEW_MAP2_ZOOM->vect.left); 
        d_right  = (VIEW_MAP2_ZOOM->vect.right); 

        G_set_window (&VIEW_MAP2_ZOOM->vect.head); 

        D_set_clip_window((int) d_top, (int) d_bottom, 
			  (int) d_left, (int) d_right);

        D_do_conversions(&VIEW_MAP2_ZOOM->vect.head, 
			(int) d_top, (int) d_bottom, 
			(int) d_left, (int) d_right);

        G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		        D_move_abs, D_cont_abs);
    }


    /* fprintf (stderr, "Plotting Vectors... "); fflush (stdout); */
    while (1)
    {
        switch (Vect_read_next_line (&Map, Points))
	{
	case -1:
	    Vect_close (&Map);
            Vect_destroy_line_struct (Points);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	case -2: /* EOF */
	    /* fprintf (stderr, "Done\n"); */
	    Vect_close (&Map);
            Vect_destroy_line_struct (Points);
	    return  0;
	}

	x = Points->x;
	y = Points->y;

	for(i=1; i < Points->n_points; i++)
	{
	    G_plot_line(x[0], y[0], x[1], y[1]);
	    x++;
	    y++;
	}
    }

}


/******************************************************************
 **  plot1_warp  (name, mapset, side, E, N, order)
 **     char *name, *mapset;
 **     int  side;
 **     double E[];
 **     double N[];
 **     int  order;
 ******************************************************************/
int plot1_warp (char *name,char *mapset,int  side)
{
    int i;
    struct Map_info Map;
    struct line_pnts *Points;
    double *x, *y;
    double z[2];

    /* mbaba */
    double d_top, d_bottom, d_left, d_right;
    int elev0, elev1;


    /*fd = open_vect (name, mapset);*/
    if (1 > Vect_open_old (&Map, name, mapset))
	G_fatal_error ("Failed opening vector file");
    Points = Vect_new_line_struct ();


    if (side == 0) {  	/* VIEW_MAP1 */
        d_top    = (VIEW_MAP1->vect.top); 
        d_bottom = (VIEW_MAP1->vect.bottom); 
        d_left   = (VIEW_MAP1->vect.left); 
        d_right  = (VIEW_MAP1->vect.right); 

        G_set_window (&VIEW_MAP1->vect.head); 

        D_set_clip_window((int) d_top, (int) d_bottom, 
			  (int) d_left, (int) d_right);

        D_do_conversions(&VIEW_MAP1->vect.head, 
			(int) d_top, (int) d_bottom, 
			(int) d_left, (int) d_right);

        G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		        D_move_abs, D_cont_abs);

    }
    else if (side == 1) {  	/* VIEW_MAP1_ZOOM */

        d_top    = (VIEW_MAP1_ZOOM->vect.top); 
        d_bottom = (VIEW_MAP1_ZOOM->vect.bottom); 
        d_left   = (VIEW_MAP1_ZOOM->vect.left); 
        d_right  = (VIEW_MAP1_ZOOM->vect.right); 

        G_set_window (&VIEW_MAP1_ZOOM->vect.head); 

        D_set_clip_window((int) d_top, (int) d_bottom, 
			  (int) d_left, (int) d_right);

        D_do_conversions(&VIEW_MAP1_ZOOM->vect.head, 
			(int) d_top, (int) d_bottom, 
			(int) d_left, (int) d_right);

        G_setup_plot (  d_top, d_bottom, d_left,  d_right,
		        D_move_abs, D_cont_abs);
    }


    /* try to catch the interupt */
    set_signals() ;
    interrupt = 0 ;
    fprintf(stderr, "\nPlotting warped vector file <%s>\n", name) ;
    fprintf(stderr, "      Hit %s to abort plotting\n", G_unctrl(G_intr_char())) ;


    /* fprintf (stderr, "Plotting Vectors... "); fflush (stdout); */
    while (1)
    {

        if (interrupt) { 
	   Vect_close (&Map);
           Vect_destroy_line_struct (Points);
	   reset_signals();
	   return 0;
	}

        switch (Vect_read_next_line (&Map, Points))
	{
	case -1:
	    Vect_close (&Map);
            Vect_destroy_line_struct (Points);
	    fprintf (stderr, "\nERROR: vector file [%s] - can't read\n", name);
	    return -1;
	case -2: /* EOF */
	    fprintf (stderr, "Done plotting %s\n", name); 
	    Vect_close (&Map);
            Vect_destroy_line_struct (Points);
	    return  0;
	}

	x = Points->x;
	y = Points->y;


	/* read the elevation */
	read_elev (&elev0, x[0], y[0]);
	z[0] = (double) elev0;

	group.inverse_trans (&group, &x[0], &y[0], &z[0], x[0], y[0], z[0]);

	for(i=1; i < Points->n_points; i++)
	{

	  read_elev (&elev1, x[1], y[1]);
	  z[1] = (double) elev1;
	  group.inverse_trans (&group, &x[1], &y[1], &z[1], x[1], y[1], z[1]);

	  G_plot_line(x[0], y[0], x[1], y[1]);
	  x++;
	  y++;
	}

    }

}

static int set_signals()
{
/* set the ctrlz ignore */
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif
 
/* set other signal catches */
 
    interrupt = 0;
 
    signal (SIGINT, sigint);

    return 0;
}

static int reset_signals()
{
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_DFL);
#endif
    signal (SIGINT, SIG_DFL);

    return 0;
}

static void sigint(int n)
{
    signal (n,sigint);
    interrupt = n;
}

