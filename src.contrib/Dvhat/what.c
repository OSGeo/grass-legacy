/*  %W%  %G%  */

#include "gis.h"
#include "digit.h"
#include "what.h"

what ()
{
    int width;
    int i;
    int row, col;
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y ;
    double east, north ;
    int button ;
    double D_get_d_north(), D_get_d_south() ;
    double D_get_d_east(), D_get_d_west() ;
    double D_d_to_u_row(), D_d_to_u_col() ;
    P_LINE *Line;
    plus_t line;

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();

    screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
    screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

    width = 0;
    for (i=0; i < nlayers; i++)
    {
	int n;
	n = strlen (name);
	if (n > width) width = n;
    }

    while (1)
    {
	/*
	show_mouse();
	*/
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	if (button == 2) continue;
	if (button == 3) break;
	east  = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	row = (window.north - north) / window.ns_res ;
	col = (east - window.west) / window.ew_res ;
	if (row < 0 || row >= nrows) continue;
	if (col < 0 || col >= ncols) continue;
	/*
	show_utm (north, east);
	*/
	/*
	for (i = 0; i < nlayers; i++)
	*/
	{
	    line = dig_point_to_line (&Map, east, north, -1);
	    if (line == 0)
	    {
		printf ("%d - no line found.\n",0);
		continue;
	    }
		/*
		show_none ();
		*/
	    Line = &(Map.Line[line]);
	    if (!LINE_ALIVE (Line) || !(Line->att))
	    {
		printf ("%d - line is not labelled\n",0);
		break;
		/*
		show_cat (width, name, 0, "No Label");
		*/
	    }
	    else
		printf ("%d\n", Map.Att[Line->att].cat);
		/*
		show_cat (width, name, fd[i].Att[Line->att], G_get_cat (buf[col], &cats[i]));
		*/
	}
	    /*
	    if (G_get_map_row (fd[i], buf, row) < 0)
		show_cat (width, name[i], 0, "error reading cell file");
	    else
		show_cat (width, name[i], buf[col], G_get_cat (buf[col], &cats[i]));
	    */
    }
}
