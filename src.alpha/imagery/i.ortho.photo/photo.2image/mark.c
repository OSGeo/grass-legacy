#include "globals.h"
#include "camera_ref.h"

static char buf[300];

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
    double e1,n1;
    double e2,n2;
    int row,col;

    char buf[100];

/* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e1 = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    n1 = row_to_northing (&view->cell.head, row, 0.5);

    Curses_clear_window (MENU_WINDOW);
    sprintf (buf, "Point %d marked at IMAGE COORDINATES:", group.photo_points.count+1);
    Curses_write_window (MENU_WINDOW, 1, 1, buf);
    sprintf (buf, "East:  %10.2lf", e1);
    Curses_write_window (MENU_WINDOW, 3, 3, buf);
    sprintf (buf, "North: %10.2lf", n1);
    Curses_write_window (MENU_WINDOW, 4, 3, buf);
    Curses_clear_window (INFO_WINDOW);

    R_standard_color (ORANGE);
    save_under_dot (x,y);
    dot(x,y);

    if (!get_point2(&e2, &n2))
    {
	Curses_clear_window (MENU_WINDOW);
	restore_under_dot(); 
    }
    else
    {

        sprintf (buf, "Point %d referenced to PHOTO COORDINATES:", group.photo_points.count+1);
        Curses_write_window (MENU_WINDOW, 7, 1, buf);
	sprintf (buf, "X:  %10.2lf", e2);
	Curses_write_window (MENU_WINDOW, 9, 3, buf);
	sprintf (buf, "Y:  %10.2lf", n2);
	Curses_write_window (MENU_WINDOW,10, 3, buf);
	I_new_ref_point (&group.photo_points, e1, n1, e2, n2, 1);
	I_put_ref_points (group.name, &group.photo_points);
	Compute_equation();
	display_ref_points(1);
    }
    release_under_dot(); 
}

static double N,E;

static
get_point2 (east, north)
    double *east, *north;
{
    int fromfile();
    int keyboard();
    int pick();
    int stat;
    int screen();
    int cancel();
    static int use = 1;
    static Objects objects[] =
    {
	INFO  ("Choose input method", &from_flag),        
        OTHER (fromfile, &from_screen),
	OTHER (keyboard, &from_keyboard),  
 	OTHER (cancel, &use), 
	{0}
    };
	if (from_screen < 0)
	{
	    from_flag = 1;
	    from_screen = 0;
	    if (from_keyboard < 0)
	    {
		from_keyboard = 0;
		from_screen = 1;
	    }
	}

    if (from_screen > 0) {
	stat = Input_other (fromfile, "CAMERA FILE");
        set_colors (&VIEW_MAP1->cell.colors);
    }
    else
	stat = Input_other (keyboard, "KEYBOARD");

    if(stat)
    {
	*east = E;
	*north = N;
    }

    return stat ;
}

static
keyboard()
{
    int ok;
    Curses_clear_window (INFO_WINDOW);
    ok = _keyboard ();
    Curses_clear_window (INFO_WINDOW);
    return ok;
}

static
_keyboard()
{
    char buf[100];

    while(1)
    {
	Curses_prompt_gets ("Enter PHOTO COORDINATES as X Y: ", buf);
	G_strip (buf);
	if (*buf == 0)
	{
	    return 0;
	}
	if (sscanf (buf, "%lf %lf", &E, &N) != 2)
	{
	    Beep();
	    continue;
	}
	Curses_clear_window (INFO_WINDOW);
	sprintf (buf, "X:  %lf\n", E);
	Curses_write_window (INFO_WINDOW, 3, 2, buf);
	sprintf (buf, "Y:  %lf\n", N);
	Curses_write_window (INFO_WINDOW, 4, 2, buf);
	Curses_write_window (INFO_WINDOW, 5, 2, "Look ok? (y/n) ");

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
}

/* get point 2 from camera file */
static
screen (x,y,button)
{
    int row,col;
    char buf[50];

    if (button == 1)
	return 1;

    sprintf (buf, "X:  %10.2lf\n", E);
    Curses_write_window (INFO_WINDOW, 3, 2, buf);
    sprintf (buf, "Y:  %10.2lf\n", N);
    Curses_write_window (INFO_WINDOW, 4, 2, buf);

    return 0;
}

static
cancel()
{
    return -1;
}

static
fromfile()
{
   /*  int ok; */
    Curses_clear_window (INFO_WINDOW);
    _drawcam();
    Curses_clear_window (INFO_WINDOW);
    return ok;
}

static
_drawcam ()
{
    static int use = 1;
    int pick();
    int done();
    int cont();
    static Objects objects[]=
    {
	MENU("CANCEL", done, &use),
 	INFO(" Double click on point to be referenced", &use),
	OTHER(pick,&use),   
	{0}
    };

/* to give user a response of some sort */
    Menu_msg ("Preparing Camera Reference File...");

/*
 * more, less, and report boxes defined in use_camera.c
 *
 */

/* allocate predicted values */
    Xf = (double *) G_calloc (group.camera_ref.num_fid, sizeof (double));
    Yf = (double *) G_calloc (group.camera_ref.num_fid, sizeof (double));

/*  redraw current report */
/****
    R_standard_color (GREY);
    R_box_abs (report.left, report.top, report.right, report.bottom);
    R_standard_color (BACKGROUND);
****/

/* lets do it */
/*    curp = first_point = 0;   */
    pager = 0;
    while(1)
    {
        if (pager)  {
           R_standard_color (GREY);
           R_box_abs (report.left, report.top, report.right, report.bottom);
           R_standard_color (BACKGROUND);
           line = 0; 
           curp = first_point;
	 }
 
	R_text_size (tsize, tsize);
        cury = report.top ;
        /* line = 0; */

	while(1)
	{
	    if (line >= nlines || curp >= group.camera_ref.num_fid)
		break;
	    line++;
        	color = BLACK;
	    if (pager)
	    {
		FMT1(buf, group.camera_ref.fiducials[curp].fid_id, group.camera_ref.fiducials[curp].Xf, group.camera_ref.fiducials[curp].Yf);
		dotext (buf, cury, cury+height, left, right -1, 0, color);
	    }
	    cury += height;
	    curp++;
            report.bottom = cury;
	}

	downarrow (&more, curp < group.camera_ref.num_fid ? BLACK : BACKGROUND);
	uparrow   (&less, first_point > 0  ? BLACK : BACKGROUND);

	pager = 0;
	which = -1;
	if(Input_pointer(objects) < 0)
		break;
    }
    return 1; 
}


static
uparrow (box, color)
    struct box *box;
{
    R_standard_color (color);
    Uparrow (box->top+edge, box->bottom-edge, box->left+edge, box->right-edge);
}

static
downarrow (box, color)
    struct box *box;
{
    R_standard_color (color);
    Downarrow (box->top+edge, box->bottom-edge, box->left+edge, box->right-edge);
}

static
pick(x,y)
{
    int n;
    int cur;

    cur = which;
    cancel_which();
    if (inbox(&more,x,y))
    {
	if (curp >= group.camera_ref.num_fid)
	    return 0;
	first_point = curp;
	pager = 1;
	return 1;
    }
    if (inbox(&less,x,y))
    {
	if (first_point == 0)
	    return 0;
	first_point -= nlines;
	if (first_point < 0)
	    first_point = 0;
	pager = 1;
	return 1;
    }
    if (!inbox (&report,x,y))
    {
	return 0;
    }

    n = (y - report.top)/height;
/* debug ("n = %d",n);
   debug ("cur = %d",cur); */

    if (n == cur) /* second click! */
    {

/* debug ("Geting point %d   E = %f  N = %f",n, E, N);*/

        E = group.camera_ref.fiducials[first_point+n].Xf;
        N = group.camera_ref.fiducials[first_point+n].Yf;
/* debug ("Got point %d   E = %f  N = %f",n, E, N);*/
	Curses_clear_window (INFO_WINDOW);
	sprintf (buf, "X:  %lf\n", E);
	Curses_write_window (INFO_WINDOW, 3, 2, buf);
	sprintf (buf, "Y:  %lf\n", N);
	Curses_write_window (INFO_WINDOW, 4, 2, buf);
	Curses_write_window (INFO_WINDOW, 5, 1, "Look ok? (y/n) ");
	Curses_write_window (PROMPT_WINDOW, 1, 1, "Keyboard Input Required ");

	while(1)
	{
	    int c;
	    c = Curses_getch(0);
	    if (c == 'y' || c == 'Y')
               { ok = 1;
                 return -1;
               }
            if (c == 'n' || c == 'N')
               {  ok = 0;
                  break;
               }
	    Beep();
	}

	/* show_point (first_point+n, 1);*/
        Curses_clear_window (INFO_WINDOW);
        Curses_write_window (PROMPT_WINDOW, 1, 1, "Use Mouse Now \n");
	return 1;
    }
    which = n;
    /* show_point (first_point+n, 0);*/
    R_standard_color (RED);
    Outline_box (report.top + n*height, report.top +(n+1)*height,
		         report.left, report.right-1);
    Curses_write_window (PROMPT_WINDOW, 1, 1, "Use Mouse Now \n");
    return 0; /* ignore first click */
    
}

static
done()
{
    cancel_which();
    ok = 0;
    return -1;
}

static
cancel_which()
{
    if (which >= 0)
    {
	R_standard_color (BACKGROUND);
	Outline_box (report.top + which*height, report.top +(which+1)*height,
		         report.left, report.right-1);
	/* show_point (first_point+which, 1); */
    }
    which = -1;
}

static
inbox (box,x,y)
    struct box *box;
{
    return (x>box->left && x <box->right && y>box->top && y<box->bottom);
}

static
dotext (text, top, bottom, left, right, centered, color)
    char *text;
{
    R_standard_color (BACKGROUND);
    R_box_abs (left, top, right, bottom);
    R_standard_color (color);
    R_move_abs (left+1+edge, bottom-1-edge);
    if (centered)
	R_move_rel ((right-left-strlen(text)*size)/2,0);
    R_set_window (top, bottom, left, right);	/* for text clipping */
    R_text (text);
    R_set_window (SCREEN_TOP, SCREEN_BOTTOM, SCREEN_LEFT, SCREEN_RIGHT);
}

static
printcentered (fd, buf, width)
    FILE *fd;
    char *buf;
{
    int len;
    int n;
    int i;

    len = strlen (buf);
    n = (width -len)/2;

    for (i = 0; i < n; i++)
	fprintf (fd, " ");
    fprintf (fd, "%s", buf);
    i += len;
    while (i++ < width)
	fprintf (fd, " ");
}

static
show_point (n, true_color)
{
    if (!true_color)
	R_standard_color (ORANGE);
    else if(group.photo_points.status[n])
	R_standard_color (GREEN);
    else
	R_standard_color (RED);
}
static
debug (msg) char *msg;
{
    R_stabilize();
    Curses_write_window (PROMPT_WINDOW, 1, 1, msg);
    Curses_getch(0);
}
