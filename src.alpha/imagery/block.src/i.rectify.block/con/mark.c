#include "globals.h"
mark(x,y,button)
{
    if (button != 1)
	return where (x,y);

    if (VIEW_MAP1->cell.configured && In_view (VIEW_MAP1, x, y))
	mark_point (VIEW_MAP1, x, y);
    else if (VIEW_MAP1_ZOOM->cell.configured && In_view (VIEW_MAP1_ZOOM, x, y))
	mark_point (VIEW_MAP1_ZOOM, x, y);
    return 0 ; /* return but don't quit */
}

mark_point (view, x, y)
    View *view;
{
    double e0, n0;
    double e1,n1,z1;
    double e2,n2,z2;
    double ee1, nn1;
    int row,col;

    char buf[100];

/* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e0 = col_to_easting (&view->cell.head, col, 0.5); 
    row = view_to_row (view, y);
    n0 = row_to_northing (&view->cell.head, row, 0.5);
 
/*  These are image coordinates not photo coordinates */
    ee1 = e0;
    nn1 = n0;

/*  e1, n1 now become photo coordinates */
    I_georef (e0, n0, &e1, &n1, group.E12, group.N12); 
    z1 = - group.cam_info.CFL; 

    Curses_clear_window (MENU_WINDOW);
    sprintf (buf, "Point %d marked at IMAGE COORDINATES", group.con_points.count+1);
    Curses_write_window (MENU_WINDOW, 1, 1, buf);
    sprintf (buf, "East:   %10.2lf", ee1);
    Curses_write_window (MENU_WINDOW, 3, 3, buf);
    sprintf (buf, "North:  %10.2lf", nn1);
    Curses_write_window (MENU_WINDOW, 4, 3, buf);
    Curses_clear_window (INFO_WINDOW);

    R_standard_color (ORANGE);
    save_under_dot (x,y);
    dot(x,y);

    if (!get_point2(&e2, &n2, &z2))
    {
	Curses_clear_window (MENU_WINDOW);
	Curses_clear_window (INFO_WINDOW);
	restore_under_dot();
    }
    else
    {
	Curses_write_window (MENU_WINDOW, 7, 1, "Control Point location:");
	sprintf (buf, "East:      %10.2lf", e2);
	Curses_write_window (MENU_WINDOW, 8, 3, buf);
	sprintf (buf, "North:     %10.2lf", n2);
	Curses_write_window (MENU_WINDOW, 9, 3, buf);
	sprintf (buf, "Elevation: %10.2lf", z2);
	Curses_write_window (MENU_WINDOW, 10, 3, buf);

	I_new_con_point (&group.con_points,  ee1, nn1, z1, e2, n2, z2, 1);
	I_new_con_point (&group.photo_points, e1, n1, z1, e2, n2, z2, 1);

	I_put_con_points (group.name, &group.con_points);

        sprintf (buf,"Computing equations ...");
	Curses_write_window (MENU_WINDOW,13,1,buf);
	Compute_ortho_equation();
	display_conz_points(1);
	Curses_clear_window (MENU_WINDOW);
	Curses_clear_window (INFO_WINDOW);
    }
    release_under_dot();
}

static double N,E,Z;

static
get_point2 (east, north, elev)
    double *east, *north, *elev;
{
    int digitizer();
    int keyboard();
    int stat;
    int screen();
    int cancel();
    static int use = 1;
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
	INFO ("Mark control point on target image", &use),
	OTHER (screen, &use),
	{0}
    };

    if (from_digitizer > 0)
	stat = Input_other (digitizer, "Digitizer") > 0;
    else if (from_screen > 0)
	stat = Input_pointer(objects) > 0;
    else
	stat = Input_other (keyboard, "Keyboard");

    if(stat)
    {
	*east = E;
	*north = N;
        *elev = Z;
    }

    return stat ;
}

static
keyboard()
{
    int ok;
    ok = _keyboard ();
    return ok;
}

static
_keyboard()
{
    char buf[100];

    while(1)
    {
        Curses_clear_window (INFO_WINDOW);
	Curses_prompt_gets ("Enter CONTROL COORDINATES as east north elevation: ", buf);
	G_strip (buf);
	if (*buf == 0)
	{
	    return 0;
	}
	if (sscanf (buf, "%lf %lf %lf", &E, &N, &Z) != 3)
	{
	    Beep();
	    continue;
	}
	Curses_clear_window (INFO_WINDOW);
	sprintf (buf, "East:      %lf\n", E);
	Curses_write_window (INFO_WINDOW, 2, 2, buf);
	sprintf (buf, "North:     %lf\n", N);
	Curses_write_window (INFO_WINDOW, 3, 2, buf);
	sprintf (buf, "Elevation: %lf\n", Z);
	Curses_write_window (INFO_WINDOW, 4, 2, buf);
	Curses_write_window (INFO_WINDOW, 6, 1, "Look ok? (y/n) ");

	while(1)
	{
	    int c;
	    c = Curses_getch(0);
	    if (c == 'y' || c == 'Y')
		return 1;
	    if (c == 'n' || c == 'N')
		break;
	    Beep();
	}
    }
/*    return 0;    dont get here */
}

static 
digitizer()
{ 
int ok,c;
char buf[100];
    
        ok = digitizer_point (&E, &N);
        if (ok)
	  {
	    Z = get_z_from_cell (N,E);
	    Curses_clear_window (INFO_WINDOW);
	    sprintf (buf, "East:      %lf\n", E);
	    Curses_write_window (INFO_WINDOW, 3, 2, buf);
	    sprintf (buf, "North:     %lf\n", N);
	    Curses_write_window (INFO_WINDOW, 4, 2, buf);
	    sprintf (buf, "Elevation: %lf\n", Z);
	    Curses_write_window (INFO_WINDOW, 5, 2, buf);
	    Curses_write_window (INFO_WINDOW, 7, 1, "Look ok? (y/n) ");
	  
	    while(1)
	      {
		c = Curses_getch(0);
		if (c == 'y' || c == 'Y')
		  {ok = 1; break;}
		if (c == 'n' || c == 'N')
		  {ok = -1; break;}
		Beep();
	      }
	    Curses_clear_window (INFO_WINDOW);
	    return ok;
	  }
return 0;
}


static
screen (x,y,button)
{
    int row,col,zint,ok;
    char buf[50];
    View *view;

    if (In_view (VIEW_MAP2, x, y) && VIEW_MAP2->cell.configured)
	view = VIEW_MAP2;
    else if (In_view (VIEW_MAP2_ZOOM, x, y) && VIEW_MAP2_ZOOM->cell.configured)
	view = VIEW_MAP2_ZOOM;
    else
	return 0; /* ignore mouse event */

    col = view_to_col (view, x);
    E = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    N = row_to_northing (&view->cell.head, row, 0.5);
    
    if (!(Z = get_z_from_cell (N,E))) return 0;

    Curses_clear_window (INFO_WINDOW);
    sprintf (buf, "East:      %10.2lf\n", E);
    Curses_write_window (INFO_WINDOW, 3, 2, buf);
    sprintf (buf, "North:     %10.2lf\n", N);
    Curses_write_window (INFO_WINDOW, 4, 2, buf);
    sprintf (buf, "Elevation: %10.2lf\n", Z);
    Curses_write_window (INFO_WINDOW, 5, 2, buf);
    Curses_write_window (INFO_WINDOW, 7, 1, "Look ok? (y/n) ");

	while(1)
	{
	    int c;
	    c = Curses_getch(0);
	    if (c == 'y' || c == 'Y')
	      { ok = 1; break;}
	    if (c == 'n' || c == 'N')
              { ok = -1; break;}
	    Beep();
	}
    Curses_clear_window (INFO_WINDOW);
    if (button == 1)
	return ok;

    return 0;
}

static
get_z_from_cell (north,east)
double north, east;
{ 
char buf[100];
int row, col;
struct Cell_head *elevhd;

/* allocate the elev buffer */
    select_target_env();
    G_get_cellhd (elev_layer, mapset_elev, &elevhd);
    G_set_window(&elevhd);
    elev = G_open_cell_old (elev_layer, mapset_elev);
    elevbuf = G_allocate_cell_buf(); 

/* find row, col in elevation cell file */
    row = (int) northing_to_row (&elevhd, north);
    col = (int) easting_to_col  (&elevhd, east);

    if (!G_get_map_row ( elev, elevbuf, row))
    {
        sprintf (buf, "ERROR reading elevation layer file %s", elev_layer);
        Curses_write_window (INFO_WINDOW, 5, 1, buf);
        Beep();
        sleep (1);
        Curses_clear_window (INFO_WINDOW);

        Curses_prompt_gets ("Enter elevation coordinate: ", buf);
        G_strip (buf);
        if (*buf == 0)
        {
          G_close_cell (elev);
          select_current_env();
          return -1;
        }
        if (sscanf (buf, "%lf ", &Z) != 1)
        {
           Beep();
           /* continue; */
        }      
        G_close_cell (elev);
        select_current_env();
        return (Z);
    }
    else
    {
       Z = elevbuf[col];
       G_close_cell (elev);
       select_current_env();
       return (Z);
    }
}

static
cancel()
{
    return -1;
}


