#include "what.h"
#include "sites.h"

void G_get_set_window(struct Cell_head *window);
void R_get_location_with_pointer(int *screen_x, int *screen_y, int *button);

static int nlines = 100;

static void show_buttons (once)
{
    if (once)
    {
        fprintf (stderr, "\nClick mouse button on desired location\n\n");
        nlines = 3;
    }
    else if (nlines >= 18)      /* display prompt every screen full */
    {
        fprintf (stderr, "\n");
        fprintf (stderr, "Buttons\n");
        fprintf (stderr, " Left:  what's here\n");
        fprintf (stderr, " Right: quit\n\n");
        nlines = 4;
    }
}

void what(int once, int terse)
{
    int row, col;
    int nrows, ncols;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north;
    int button ;
    Site *site;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    do
    {
	show_buttons(once);
        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	if (!once)
	{
	    if (button == 2) continue;
	    if (button == 3) break;
	}
        east  = D_d_to_u_col((double)screen_x) ;
        north = D_d_to_u_row((double)screen_y) ;
        row = (window.north - north) / window.ns_res ;
        col = (east - window.west) / window.ew_res ;
        if (row < 0 || row >= nrows) continue;
        if (col < 0 || col >= ncols) continue;

/*	fprintf(stdout, "%s\n", site_format(closest_site(east, north)));*/
	site=closest_site(east, north);
	fprintf(stdout, "%g|%g|%s\n", site->x, site->y, site->desc);
        fflush(stdout);
    } while (! once) ;
}
