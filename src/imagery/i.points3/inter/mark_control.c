/*======================================================================
				i.points
  mark_control.c --
    void mark_control(void)

       Routine to allow user to mark control points (lat/lon format)
       by using mouse to identify the point on the source imagery.
       Target locations can be enterd on keyboar, through digitizer,
       or via mouse on raster or vect displayed on monitor.

       The menu contains the following options:
       
           CANCEL    -  cancel()       contained internally
           ZOOM      -  zoom()         driver.c
 
	   -- When user marks a point in source imagery:
	             -  _mark()        contained internally

	   -- INPUT DEVICE OPTIONS: menu just sets a flag for choosen
	   DIGITIZER -  
	   KEYBOAD   -  
	   SCREEN    -  
	   


       The routine Input_pointer() loops untill the select option
       returns a non-zero status.  That is it quits only when the 
       selected routine returns a non-zero.

======================================================================*/

#include "globals.h"
#include "raster.h"
#include "vask.h"

static double N,E;
static int use = 1;

static int _mark(int,int,int);
static int mark_point (View *,int,int);
static int get_point2_poly (double *,double *);
static int keyboard(void);
static int _keyboard(void);
static int digitizer(void);
static int screen (int,int,int);
static int cancel_return(void);
static int cancel(void);

/*---------------------------------------------------------------------*/
int mark_control()
{
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
        TITLE("<MARK-CONTROL>", &use),
	MENU ("ZOOM",   zoom, &use),
	INFO ("Mark Control Point on Imagery", &use),
	OPTITLE ("<Input method>", &from_flag),
	OPTION("DIGITIZER",2,&from_digitizer),
	OPTION("KEYBOARD",2,&from_keyboard),
	OPTION("SCREEN",2,&from_screen),
	OTHER(_mark, &use),
	{0}
    };

    Input_pointer (objects);
    Menu_msg ("");
    
    return 0 ; /* return but don't make main driver quit */
}


/*---------------------------------------------------------------------*/
/* Check that currect button was pressed and that the mouse was in the */
/* Source imagery location (VIEW_MAP1 or VIEW_MAP1_ZOOM).              */
/*---------------------------------------------------------------------*/
static int _mark(int x,int y,int button)
{
    /* Button one to set point, Button 2 & 3 return location */
    if (button != 1)
	return where (x,y);

    /* Button press must be in the source imagery */
    if (VIEW_MAP1->cell.configured && In_view (VIEW_MAP1, x, y))
	mark_point (VIEW_MAP1, x, y);
    else if (VIEW_MAP1_ZOOM->cell.configured && In_view (VIEW_MAP1_ZOOM, x, y))
	mark_point (VIEW_MAP1_ZOOM, x, y);

    return 0 ; /* return but don't quit */
}

/*-----------------------------------------------------------------------*/
/* Converts (x,y) screen pixel from VIEW_MAP1 or VIEW_MAP1_ZOOM to       */
/* source image cordinates.  Calls get_point2() to get the target coords */
/* Display a dot on the screen and saves the control point               */
/*-----------------------------------------------------------------------*/
static int mark_point (View *view,int x,int y)
{
/** TODO -- Need generic Points struct **/
Control_Points     *points;
double e1,n1;
double e2,n2;
double lat, lon;
int row,col;
int status;
int elevation;
char buf[100];


    /* make points visiable */
    points = (Control_Points *) group.points;

    /* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e1 = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    n1 = row_to_northing (&view->cell.head, row, 0.5);

/**    Curses_clear_window (MENU_WINDOW);
 **    sprintf (buf, "Point %d marked on image at", points->points_temp.count+1);
 **    Curses_write_window (MENU_WINDOW, 1, 1, buf);
 **    sprintf (buf, "East:  %10.2lf", e1);
 **    Curses_write_window (MENU_WINDOW, 3, 3, buf);
 **    sprintf (buf, "North: %10.2lf", n1);
 **    Curses_write_window (MENU_WINDOW, 4, 3, buf);
 **    Curses_clear_window (INFO_WINDOW);
**/
    R_standard_color (I_COLOR_ORANGE);
    save_under_dot (x,y);
    dot(x,y);


    /* Get the target location (coords) for the point */
    /* get_point2() returns: */
    /*     1 - second point was marked */
    /*     0 - means ignore second point */
    status = get_point2_poly(&e2, &n2);

    if (!status) {
      /* ignore point */
      /** Curses_clear_window (MENU_WINDOW); **/
      restore_under_dot();
    }

    else  {

      /* Display the coords */
/**      Curses_write_window (MENU_WINDOW, 7, 1, "Point located at");
 **      sprintf (buf, "East:      %10.2lf", e2);
 **      Curses_write_window (MENU_WINDOW, 9, 3, buf);
 **      sprintf (buf, "North:     %10.2lf", n2);
 **      Curses_write_window (MENU_WINDOW,10, 3, buf);
**/
      /*** TODO -- display lat/lons ***/ 

      /* add to control points file */
      I_new_con_point_ll  (&points->points_ll, e1, n1, e2, n2, 1);

      /* conver to ll */
      convert_to_ll (&points->points_ll, &points->points_temp);
      I_put_con_points_ll (group.name, &points->points_ll);

      /* display the new points */
      display_points(1);
    }

    release_under_dot();

    return 0;
}


/*-----------------------------------------------------------------------*
* Get the target coordinates of the marked point.
* Calls either screen(), keyboard(), or digitizer() based on input option
*
* RETURNS: 
*      1 - second point entered
*      0 - canceled second point so ignore the first (source) point enterd
*-----------------------------------------------------------------------*/
static int get_point2_poly (double *east,double *north)
{
    static int use = 1;
    int stat;
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel_return, &use),
	INFO ("Mark point on target image", &use),
	OTHER (screen, &use),
	{0}
    };


    /* Use the digitizer */
    if (from_digitizer > 0)
    {
	stat = Input_other (digitizer, "Digitizer");
    }

    /* Use the mouse */
    else if (from_screen > 0)
    {
	set_colors (&VIEW_MAP2->cell.colors);
	stat = Input_pointer(objects) > 0;
	set_colors (&VIEW_MAP1->cell.colors);
    }

    /* Use the keyboard */
    else
	stat = Input_other (keyboard, "Keyboard");

    /* stat == 1 means target coords are good */
    /* stat == 0 means ignore the second point */

    if(stat)  {
	*east = E;
	*north = N;
    }

    return stat ;
}


/*-----------------------------------------------------------------------*
* Use the keyboard to enter the target coordinates
*
* RETRUNS: 
*      1 - second point entered
*      0 - canceled second point so ignore the first (source) point enterd
*-----------------------------------------------------------------------*/
static int keyboard()
{
    int ok;
    /** Curses_clear_window (INFO_WINDOW); **/
    ok = _keyboard ();
    /** Curses_clear_window (INFO_WINDOW); **/
    return ok;
}

static int _keyboard()
{ 
    char buf[100];
    char t1[80], t2[80], t3[80];
    char s_east[16], s_north[16];
    int  status = 0;

    sprintf (s_east,  "0.0");
    sprintf (s_north, "0.0");

    sprintf (t1, "Enter control point coordinates as East North:");
    sprintf (t2, "East and North are in Target Location coordinate system.");
    sprintf (t3, "   East:                        North: ");

    V_clear(); 
    V_line (3, t1);
    V_line (5, t2);
    V_line (7, t3);
    V_ques (s_east,  's', 7, 10, 15);
    V_ques (s_north, 's', 7, 40, 15);

    for (;;) 
    {
	V_intrpt_ok();
	if (!V_call())
		goto end;


	G_strip (s_east);
	if (*s_east == 0)
	{
	    continue;
	}

	if (sscanf (s_east, "%lf", &E) != 1)
	{
	    /** Beep(); **/
	    continue;
	}

	G_strip (s_north);
	if (*s_north == 0)
	{
	    continue;
	}

	if (sscanf (s_north, "%lf", &N) != 1)
	{
	    /** Beep(); **/
	    continue;
	}

	break;

   } /* for */
   status = 1;

end: 
   	return (status);
}


/*-----------------------------------------------------------------------*
* Use the digitizer to enter the target coordinates
*
* RETURNS: 
*      1 - second point entered
*      0 - canceled second point so ignore the first (source) point enterd
*-----------------------------------------------------------------------*/
static int digitizer()
{
    return digitizer_point (&E, &N);
}



/*-----------------------------------------------------------------------*
* Mark the target coordinates using the mouse.
*
* RETRUNS: 
*      1 - second point entered
*      0 - canceled second point so ignore the first (source) point enterd
*-----------------------------------------------------------------------*/
static int screen (int x,int y,int button)
{
    int row,col;
    char buf[50];
    View *view;

    /* User must mark in target VIEWS, and they must be configured */
    if (In_view (VIEW_MAP2, x, y) && 
	(VIEW_MAP2->cell.configured || VIEW_MAP2->vect.configured))
	view = VIEW_MAP2;
    else if (In_view (VIEW_MAP2_ZOOM, x, y) && 
	     (VIEW_MAP2_ZOOM->cell.configured || VIEW_MAP2_ZOOM->vect.configured))
	view = VIEW_MAP2_ZOOM;
    else
	return 0; /* ignore mouse event */


    /* convert from screen to target coordinates */
    col = view_to_col (view, x);
    if (view->cell.configured)
       E = col_to_easting (&view->cell.head, col, 0.5);
    else if (view->vect.configured)
       E = col_to_easting (&view->vect.head, col, 0.5);
  
    row = view_to_row (view, y);
    if (view->cell.configured)
       N = row_to_northing (&view->cell.head, row, 0.5);
    else if (view->vect.configured)
       N = row_to_northing (&view->vect.head, row, 0.5);

    /* Set target coordinates */
    if (button == 1)
	return 1;

    /* buttons 2 or 3 - print target location, but ignore point */

/**    sprintf (buf, "East:   %10.2lf\n", E);
**    Curses_write_window (INFO_WINDOW, 2, 2, buf);
**    sprintf (buf, "North:  %10.2lf\n", N);
**    Curses_write_window (INFO_WINDOW, 3, 2, buf);
**/
    return 0;
}

/*-----------------------------------------------------------------------*/
static int cancel_return()
{
    return -1;  /** 0 ? **/
}

/*-----------------------------------------------------------------------*/
static int cancel()
{
    return 1;
}
