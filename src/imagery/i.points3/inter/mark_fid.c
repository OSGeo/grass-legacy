#include <string.h>
#include "globals.h"
#include "raster.h"
#include "camera_ref.h"
#define BACKGROUND I_COLOR_GREY

static int use = 1;
static double N,E;

static int _mark(int,int,int);
static int mark_point (View *, int,int);
static int get_point2_fiducial (double *,double *);
static int keyboard(void);
static int _keyboard(void);
static int screen (int,int,int);
static int cancel(void);
static int fromfile(void);
static int _drawcam (void);
static int uparrow (struct box *,int);
static int downarrow (struct box *,int);
static int pick(int,int);
static int done(void);
static int cancel_which(void);
static int inbox (struct box *,int,int);
static int dotext (char *,int,int,int,int,int,int);
static int printcentered (FILE *,char *,int);
static int show_point (int,int);
static int debug (char *);

int mark_fiducial()
{
  static Objects objects[] =
    {
      MENU ("CANCEL", cancel, &use),
      TITLE("<MARK-FIDUCIAL>", &use),
      MENU ("ZOOM",   zoom, &use),
      INFO ("Mark Fiducial Point on Imagery.", &use),
      
      OPTITLE("<Input method>", &from_flag_fid),
      OPTION ("KEYBOARD",2,&from_keyboard_fid),
      OPTION ("CAMERA",2,  &from_camera_fid),
      OTHER(_mark, &use),
      {0}
    };
  
  /* ask if user wants to use camera file */
  setup_camera_file();

  /* we user wants to use the camera file - draw it */
  if (from_camera_fid) {
    draw_camera_file();
  }

  
  Input_pointer (objects);
  Menu_msg ("");

  /* TODO erase camera file */
  if (from_camera_fid) {
    R_panel_restore (tempfile_camera);
  }

  return 0 ; /* return but don't quit */
}


static int _mark(int x,int y,int button)
{
    if (button != 1)
	return where (x,y);

    if (VIEW_MAP1->cell.configured && In_view (VIEW_MAP1, x, y))
	mark_point (VIEW_MAP1, x, y);
    else if (VIEW_MAP1_ZOOM->cell.configured && In_view (VIEW_MAP1_ZOOM, x, y))
	mark_point (VIEW_MAP1_ZOOM, x, y);
    return 0 ; /* return but don't quit */
}

static int mark_point (View *view, int x, int y)
{
Auxillary_Photo *auxil;

    double e1,n1;
    double e2,n2;
    int row,col;
    int status;
    int elevation;

    char buf[100];

/* make auxil visiable */
   auxil = (Auxillary_Photo *) group.auxil;


/* convert x,y to east,north at center of cell */
    col = view_to_col (view, x);
    e1 = col_to_easting (&view->cell.head, col, 0.5);
    row = view_to_row (view, y);
    n1 = row_to_northing (&view->cell.head, row, 0.5);

/**    Curses_clear_window (MENU_WINDOW);
**    sprintf (buf, "Point %d marked on image at", auxil->points_fid.count+1);
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



    status = get_point2_fiducial(&e2, &n2);



    if (!status) {
      /** Curses_clear_window (MENU_WINDOW); **/
      restore_under_dot();
    }

    else {

/**      sprintf (buf, "Point %d referenced to PHOTO COORDINATES:", 
**	       auxil->points_fid.count+1);
**      Curses_write_window (MENU_WINDOW, 7, 1, buf);
**      sprintf (buf, "X:  %10.2lf", e2);
**      Curses_write_window (MENU_WINDOW, 9, 3, buf);
**      sprintf (buf, "Y:  %10.2lf", n2);
**      Curses_write_window (MENU_WINDOW,10, 3, buf);
**/

      I_new_ref_point  (&auxil->points_fid, e1, n1, e2, n2, 1);
      I_put_ref_points (group.name, &auxil->points_fid);

      /** Compute_fiducial_equation(); **/
      display_fiducial_points(1);
    }

    release_under_dot();

    return 0;
}

static int get_point2_fiducial (double *east,double *north)
{
    int stat;
    static int use = 1;

    static Objects objects[] =
    {
	INFO  ("Choose input method", &from_flag_fid),        
        OTHER (fromfile, &from_camera_fid),
	OTHER (keyboard, &from_keyboard),  
 	OTHER (cancel, &use), 
	{0}
    };

    if (from_camera_fid < 0)
      {
	from_flag_fid = 1;
	from_camera_fid = 0;
	if (from_keyboard_fid < 0)
	  {
	    from_keyboard_fid = 0;
	    from_camera_fid = 1;
	  }
      }

    if (from_camera_fid > 0) {
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

    while(1)
    {
	/** Curses_prompt_gets ("Enter PHOTO COORDINATES as X Y: ", buf); **/
	G_strip (buf);
	if (*buf == 0)
	{
	    return 0;
	}
	if (sscanf (buf, "%lf %lf", &E, &N) != 2)
	{
	    /** Beep(); **/
	    continue;
	}

/**	Curses_clear_window (INFO_WINDOW);
 **	sprintf (buf, "X:  %lf\n", E);
 **	Curses_write_window (INFO_WINDOW, 3, 2, buf);
 **	sprintf (buf, "Y:  %lf\n", N);
 **	Curses_write_window (INFO_WINDOW, 4, 2, buf);
 **	Curses_write_window (INFO_WINDOW, 5, 2, "Look ok? (y/n) ");

 **	while(1)
 **	{
 **	    int c;
 **	    c = Curses_getch(0); 
 **	    if (c == 'y' || c == 'Y')
 **		return 1;
 **	    if (c == 'n' || c == 'N')
 **		break;
 **	    Beep();
 **	}
**/

    }

    return 0;
}

/* get point 2 from camera file */
static int screen (int x,int y,int button)
{
    int row,col;
    char buf[50];

    if (button == 1)
	return 1;

/**    sprintf (buf, "X:  %10.2lf\n", E);
 **    Curses_write_window (INFO_WINDOW, 3, 2, buf);
 **    sprintf (buf, "Y:  %10.2lf\n", N);
 **    Curses_write_window (INFO_WINDOW, 4, 2, buf);
**/
    return 0;
}

static int cancel()
{
    return -1;
}

static int fromfile()
{
    int ok = 1; 

/**    Curses_clear_window (INFO_WINDOW); **/
       _drawcam();
/**    Curses_clear_window (INFO_WINDOW); **/
    return ok;
}

static int _drawcam ()
{
Auxillary_Photo  *auxil;

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

/* make auxil data visiable */
   auxil = (Auxillary_Photo *) group.auxil;


/* to give user a response of some sort */
    Menu_msg ("Preparing Camera Reference File...");

/*
 * more, less, and report boxes defined in use_camera.c
 *
 */

/* allocate predicted values */
    Xf = (double *) G_calloc (auxil->camera.num_fid, sizeof (double));
    Yf = (double *) G_calloc (auxil->camera.num_fid, sizeof (double));

/*  redraw current report */
/****
    R_standard_color (I_COLOR_GREY);
    R_box_abs (report.left, report.top, report.right, report.bottom);
    R_standard_color (BACKGROUND);
****/

/* lets do it */
/*    curp = first_point = 0;   */
    pager = 0;
    while(1)
    {
        if (pager)  {
           R_standard_color (I_COLOR_GREY);
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
	    if (line >= nlines || curp >= auxil->camera.num_fid)
		break;
	    line++;
        	color = I_COLOR_BLACK;
	    if (pager)
	    {
		FMT1(buf, auxil->camera.fiducials[curp].fid_id, auxil->camera.fiducials[curp].Xf, auxil->camera.fiducials[curp].Yf);
		dotext (buf, cury, cury+height, left, right -1, 0, color);
	    }
	    cury += height;
	    curp++;
            report.bottom = cury;
	}

	downarrow (&more, curp < auxil->camera.num_fid ? I_COLOR_BLACK : BACKGROUND);
	uparrow   (&less, first_point > 0  ? I_COLOR_BLACK : BACKGROUND);

	pager = 0;
	which = -1;
	if(Input_pointer(objects) < 0)
		break;
    }
    return 1; 
}


static int uparrow (struct box *box,int color)
{
    R_standard_color (color);
    Uparrow (box->top+edge, box->bottom-edge, box->left+edge, box->right-edge);

    return 0;
}

static int downarrow (struct box *box,int color)
{
    R_standard_color (color);
    Downarrow (box->top+edge, box->bottom-edge, box->left+edge, box->right-edge);

    return 0;
}

static int pick(int x,int y)
{
Auxillary_Photo *auxil;

    int n;
    int cur;

/* make auxil visiable */
   auxil = (Auxillary_Photo * ) group.auxil;


    cur = which;
    cancel_which();
    if (inbox(&more,x,y))
    {
	if (curp >= auxil->camera.num_fid)
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

        E = auxil->camera.fiducials[first_point+n].Xf;
        N = auxil->camera.fiducials[first_point+n].Yf;
/* debug ("Got point %d   E = %f  N = %f",n, E, N);*/


/**	Curses_clear_window (INFO_WINDOW);
 **	sprintf (buf, "X:  %lf\n", E);
 **	Curses_write_window (INFO_WINDOW, 3, 2, buf);
 **	sprintf (buf, "Y:  %lf\n", N);
 **	Curses_write_window (INFO_WINDOW, 4, 2, buf);
**/

	ok = 1;
	return -1;
	
    }
    which = n;
    /* show_point (first_point+n, 0);*/
    R_standard_color (I_COLOR_RED);
    Outline_box (report.top + n*height, report.top +(n+1)*height,
		         report.left, report.right-1);
    /** Curses_write_window (PROMPT_WINDOW, 1, 1, "Use Mouse Now \n"); **/
    return 0; /* ignore first click */
    
}

static int done()
{
    cancel_which();
    ok = 0;
    return -1;
}

static int cancel_which()
{
    if (which >= 0)
    {
	R_standard_color (BACKGROUND);
	Outline_box (report.top + which*height, report.top +(which+1)*height,
		         report.left, report.right-1);
	/* show_point (first_point+which, 1); */
    }
    which = -1;

    return 0;
}

static int inbox (struct box *box,int x,int y)
{
    return (x>box->left && x <box->right && y>box->top && y<box->bottom);
}

static int dotext (char *text,int top,int bottom,int left,int right,int centered,int color)
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

    return 0;
}

static int printcentered (FILE *fd,char *buf,int width)
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

    return 0;
}

static int show_point (int n,int true_color)
{
Auxillary_Photo *auxil;

    /* make visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    if (!true_color)
	R_standard_color (I_COLOR_ORANGE);
    else if(auxil->points_fid.status[n])
	R_standard_color (I_COLOR_GREEN);
    else
	R_standard_color (I_COLOR_RED);

    return 0;
}

static int debug (char *msg)
{
    R_stabilize();
    /** Curses_write_window (PROMPT_WINDOW, 1, 1, msg); **/
    /** Curses_getch(0); **/

    return 0;
}
