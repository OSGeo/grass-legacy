/*======================================================================
				i.points
  analyze_contol.c --
    void analyze_control(void)

       Routine to allow user to view the current status of the 
       transformation coeffients.

       User may turn on/off control points or delete selected 
       control points.

       User can switch between the different transfomrations, provided
       that all the required auxillary data already exist.


       The menu contains the following options:
       
           CANCEL    -  
           PRINT   -  
	   FILE    -  

	   -- Delete mode OPTIONS:  
	   Delete Off -  selected points will be ignored in transformation.
	   DELETE ON  -  selected points will be deleted from file.


	   -- Transformation OPTIONS:  
	   Polynomial -  select polynomial transfomation order
	   OrthoPhoto -  select ortho photo transformation
	   LandsatTM  -  select LandSat TM transformation 

======================================================================*/


#include <string.h>
#include <unistd.h>
#include "globals.h"
#include "raster.h"
#include "crs.h"


/*-------------------------------------------------------------------------*/
#define NLINES 18

struct box
{
    int top, bottom, left,right;
};

static int which;
static struct box more, less, report;
static int height, size, edge, nlines;
static int curp, first_point;
static int pager;
static char buf[300];

static char delete_msg[11] = "DELETE_OFF";
static char pick_msg[42]   = "Double click on pt. to include-exclude";
static int delete_mode;

static int do_del() { return (1); }
static int dont_del() { return (-1); }
static int delete_mark(void);
static int delete_control_point(int);
static int uparrow (struct box *,int);
static int downarrow (struct box *,int);
static int pick(int,int);
static int done(void);
static int cancel_which(void);
static int inbox (struct box *,int,int);
static int dotext (char *,int,int,int,int,int,int);
static int to_file(void);
static int askfile(void);
static int to_printer(void);
static int do_report (FILE *);
static int printcentered (FILE *,char *,int);
static int show_point (int,int);
static int print_order_mes(void);
static int ask_which_trans(void);

/*-------------------------------------------------------------------------*/

#define FMT0(buf,n) \
	sprintf (buf, "%3d ", n)

#define FMT1(buf,xres,yres,gnd) \
	sprintf (buf, "%7.1f \t%7.1f \t%7.1f ", xres,yres,gnd)

#define LHEAD1 "              error             "
#define LHEAD2 "  #    east   north    distance "

#define FMT2(buf, e1, n1, e2, n2) sprintf (buf, "\t\t%7.0f \t%7.0f \t\t%9.1f \t%9.1f ", e1, n1, e2, n2)


#define RHEAD1 "         image                    target            "
#define RHEAD2 "     east    north      east      north      elev.  "


/* background color for pop-up screen */
#define BACKGROUND I_COLOR_GREY


/*-------------------------------------------------------------------------*/

Residuals    resid;



/*-------------------------------------------------------------------------*/
int anal_control()
{
Control_Points  *points;

    static int use = 1;

    static Objects objects[]=
    {
	MENU ("CANCEL", done, &use),
	TITLE("<ANALYZE>", &use),
	MENU ("PRINT", to_printer, &use),
	MENU ("FILE", to_file, &use),
	MENU (delete_msg, delete_mark, &use), 
	INFO (pick_msg, &use),

        OPTITLE("<Trans>", &use), 
        MENU(order_msg,  ask_which_trans, &use),  
        /** MENU(order_msg,  ask_transform, &use), **/
	OTHER(pick,&use),
	{0}
    };


    int color;
    int tsize;
    int cury;
    int len;
    int line;
    int top, bottom, left, right, width, middle, nums;

/* make visiable */
    points = (Control_Points *) group.points;


/* get the current transformation mesage */
    print_order_mes();

/* to give user a response of some sort */
    Menu_msg ("Preparing analysis ...");

/*
 * build a popup window at center of the screen.
 * 35% the height and wide enough to hold the report
 *
 */

/* height of 1 line, based on NLINES taking up 35% vertical space */
    height = (.35 * (SCREEN_BOTTOM - SCREEN_TOP))/NLINES + 1;

/* size of text, 80% of line height */
    tsize = .8 * height;
    size = tsize-2; /* fudge for computing pixels width of text */

/* indent for the text */
    edge = .1 * height + 1;

/* determine the length, in chars, of printed line */
    FMT0 (buf,0);
    nums = strlen(buf) * size;
    FMT1 (buf, 99999.9, 99999.9, 99999.9);
    len = strlen(buf);
    middle = len * size;
    FMT2 (buf, 99999.0, 99999.0, 9999999.0, 9999999.0);
    len += strlen(buf) ;

/* width is for max chars plus sidecar for more/less */
    width = len * size + nums + 2*height;
    if ((SCREEN_RIGHT - SCREEN_LEFT) < width)
	width = SCREEN_RIGHT - SCREEN_LEFT;


/* define the window */
    /*** Make the window centerd on lower half os screen **/
    bottom = VIEW_MENU->top - (.075 * (SCREEN_BOTTOM-SCREEN_TOP));
    top = bottom - height*NLINES;


    left = SCREEN_LEFT + ((SCREEN_RIGHT - width) / 2);
    right = left + width;
    middle += left + nums;
    nums += left;



/* save what is under this area, so it can be restored */
    R_panel_save (tempfile1, top, bottom, left, right);


/* fill it with white */
    R_standard_color (BACKGROUND);
    R_box_abs (left, top, right, bottom);

    right -= 2*height;	/* reduce it to exclude sidecar */

/* print messages in message area */
    R_text_size (tsize, tsize);


/* setup the more/less boxes in the sidecar */
    R_standard_color (I_COLOR_BLACK);
    less.top = top;
    less.bottom = top + 2*height;
    less.left = right;
    less.right = right + 2*height;
    Outline_box (less.top, less.bottom, less.left, less.right);

    more.top = bottom - 2*height;
    more.bottom = bottom;
    more.left = right;
    more.right = right + 2*height;
    Outline_box (more.top, more.bottom, more.left, more.right);

/*
 * top two lines are for column labels
 * last two line is for overall rms error.
 */
    nlines = NLINES - 3;
    first_point = 0;

/* allocate predicted values */
    resid.count = points->points_temp.count;
    resid.xres = (double *) G_calloc (points->points_temp.count, sizeof (double));
    resid.yres = (double *) G_calloc (points->points_temp.count, sizeof (double));
    resid.gnd  = (double *) G_calloc (points->points_temp.count, sizeof (double));

/* compute transformation for the first time */
    group.anal_points(&group, &resid);


/* put head on the report */
    cury = top;
    dotext (LHEAD1, cury, cury+height, left, middle, 0, I_COLOR_BLACK);
    dotext (RHEAD1, cury, cury+height, middle, right-1, 0, I_COLOR_BLACK);
    cury += height;
    dotext (LHEAD2, cury, cury+height, left, middle, 0, I_COLOR_BLACK);
    dotext (RHEAD2, cury, cury+height, middle, right-1, 0, I_COLOR_BLACK);
    cury += height;
    R_move_abs (left, cury-1);
    R_cont_abs (right, cury-1);

/* isolate the sidecar */
    R_move_abs (right, top);
    R_cont_abs (right, bottom);

/* define report box */
    report.top = cury;
    report.left = left;
    report.right = right;

/* lets do it */

    pager = 1;
    while(1)
    {
	R_text_size (tsize, tsize);
	line = 0;
	curp = first_point;
	cury = top + 2*height;
	while(1)
	{
	    if (line >= nlines || curp >= points->points_temp.count)
		break;
	    line++;

	    if(!delete_mode)
   		color = I_COLOR_BLACK;
	    else
   		color = I_COLOR_BLUE;

	    if(group.stat > 0 && points->points_temp.status[curp] > 0)
	    {
		/* color = I_COLOR_BLACK; */
		FMT1(buf, resid.xres[curp], resid.yres[curp], resid.gnd[curp]);
		if (curp == resid.large_x ||
		    curp == resid.large_y ||
		    curp == resid.large_gnd)
		  color = I_COLOR_RED;
		dotext (buf, cury, cury+height, nums, middle, 0, color);
	    }
	    else if (points->points_temp.status[curp] > 0)
		dotext ("?", cury, cury+height, nums, middle, 1, color);
	    else
		dotext ("not used", cury, cury+height, nums, middle, 1, color);

	    if (pager)
	    {
		FMT0 (buf, curp+1);
		dotext (buf, cury, cury+height, left, nums, 0, color);
		FMT2(buf,
		    points->points_temp.e1[curp],
		    points->points_temp.n1[curp],
		    points->points_temp.e2[curp],
		    points->points_temp.n2[curp]);
		dotext (buf, cury, cury+height, middle, right-1, 0, color);
	    }
	    cury += height;
	    curp++;
	}
	report.bottom = cury;
	downarrow (&more, curp < points->points_temp.count ? color : BACKGROUND);
	uparrow   (&less, first_point > 0  ? color : BACKGROUND);
	R_standard_color (BACKGROUND);
	R_box_abs (left, cury, right-1, bottom);
	if (group.stat < 0)
	{

		if(group.stat == -1)
		{
	    		color = I_COLOR_RED;
	    		strcpy (buf, "Poorly placed control points");
		}
		else
		{
   		if(group.stat == -2)
      			G_fatal_error("NOT ENOUGH MEMORY");
   		else
      			G_fatal_error("PARAMETER ERROR");
		}

	}
	else if (group.stat == 0)
	{
	    color = I_COLOR_RED;
	    strcpy (buf, "Not enough control points");
	}
	else
	{
	    color = I_COLOR_BLACK; 
	    sprintf (buf, "Overall rms error: %.2f", resid.gnd_rms);
	}
	dotext (buf, bottom-height, bottom, left, right-1, 0, color);
	R_standard_color (I_COLOR_BLACK);
	R_move_abs (left, bottom-height);
	R_cont_abs (right-1, bottom-height);

	pager = 0;
	which = -1;
	if(Input_pointer(objects) < 0)
		break;
    display_points(1);
    }

/* all done. restore what was under the window */
    right += 2*height;	/* move it back over the sidecar */
    R_standard_color (BACKGROUND);
    R_box_abs (left, top, right, bottom);
    R_panel_restore (tempfile1);
    R_panel_delete (tempfile1);
    R_flush();


    /** TODO - clean up free **/
    G_free (resid.xres); G_free (resid.yres); G_free (resid.gnd);

    convert_to_ll (&points->points_ll, &points->points_temp);
    I_put_con_points_ll (group.name, &points->points_ll);


    display_points(1);
    return 0; /* return but don't QUIT */
}





static int delete_mark()
{
   static int use =1;

   static Objects objects[]=
   {
	TITLE("<ANALZE>", &use),
	INFO ("Do you really want the DELETE mode ?", &use),
	MENU ("YES", do_del, &use),   /* returns 1 */
	MENU ("NO",  dont_del, &use), /* returns -1 */
	{0}
   };


   if(delete_mode)  {
       strcpy(pick_msg,  "Double click on pt. to include-exclude");
       strcpy(delete_msg,"DELETE_OFF");
       delete_mode = 0;     
   }
   else {
      /** TODO bring up a yes/no menu **/
      if (Input_pointer(objects) == 1) {
         strcpy(pick_msg,   "Double click on point to DELETE.");
         strcpy(delete_msg, "DELETE_ON");
         delete_mode = 1;
      }
   }
   pager = 1; /* redisplay entire form */

   return 0;
}



static int delete_control_point(int n)
{
    int i;
    char msg[80];
 
Control_Points *points;

    /* make visiable */
    points = (Control_Points *) group.points;

    if(n < 0 | n > points->points_temp.count -1)
        {
        sprintf(msg,"%d is an invalid control point index value.",n);
	G_warning(msg);
	return -1;
	}
    for(i = n; i < points->points_temp.count -1; i++)
	{
	G_copy(&points->points_temp.e1[i], &points->points_temp.e1[i+1], sizeof(points->points_temp.e1[0]));
	G_copy(&points->points_temp.n1[i], &points->points_temp.n1[i+1], sizeof(points->points_temp.n1[0]));

	G_copy(&points->points_temp.n2[i], &points->points_temp.n2[i+1], sizeof(points->points_temp.n2[0])); 
	G_copy(&points->points_temp.e2[i], &points->points_temp.e2[i+1], sizeof(points->points_temp.e2[0]));


	G_copy(&points->points_temp.status[i], &points->points_temp.status[i+1], sizeof(points->points_temp.status[0]));
	}

    points->points_temp.count -= 1; 
    
    /* convert to lat/lon */
    convert_to_ll (&points->points_ll, &points->points_temp);
    if(I_put_con_points_ll(group.name,&points->points_ll) < 0)
	{
	G_fatal_error("bad return on I_put_control_points");
	return 1;
	}

   return 0;
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
    int n;
    int cur;

Control_Points *points;

    /* make visiable */
    points = (Control_Points *) group.points;

    cur = which;
    cancel_which();
    if (inbox(&more,x,y))
    {
	if (curp >= points->points_temp.count)
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
    if (n == cur) /* second click! */
    {
    
    if(!delete_mode) 
       {
       points->points_temp.status[first_point+n] = !points->points_temp.status[first_point+n];

/***       compute_transformation_poly(); ***/
       group.anal_points(&group, &resid);

       show_point (first_point+n, 1);


       /* write out control points before switching transforms */
       convert_to_ll (&points->points_ll, &points->points_temp);
       I_put_con_points_ll (group.name, &points->points_ll);


       return 1;
       }
    else
      {
      delete_control_point(first_point+n);       
      first_point = 0;

/***      compute_transformation_poly(); ****/
      group.anal_points(&group, &resid);

      pager = 1;
      return 1;
      }
   }
    which = n;
    show_point (first_point+n, 0);
    if(!delete_mode)
        R_standard_color (I_COLOR_RED);
    else  
        R_standard_color(I_COLOR_WHITE);
    Outline_box (report.top + n*height, report.top +(n+1)*height,
		         report.left, report.right-1);



    return 0; /* ignore first click */

}

static int done()
{
    cancel_which();
    return -1;
}

static int cancel_which()
{
    if (which >= 0)
    {
	R_standard_color (BACKGROUND);
	Outline_box (report.top + which*height, report.top +(which+1)*height,
		         report.left, report.right-1);
	show_point (first_point+which, 1);
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

static int to_file()
{
    FILE *fd;
    char msg[1024];

    cancel_which();
    if (Input_other (askfile, "Keyboard") < 0)
    {
	return 0;
    }

    fd = fopen (buf, "w");
    if (fd == NULL)
    {
	sprintf (msg, "** Unable to create file %s\n", buf);
	/** Beep(); **/
	/** Curses_write_window (PROMPT_WINDOW, 2, 1, msg); **/
    }
    else
    {
	do_report (fd);
	fclose (fd);
	sprintf (msg, "Report saved in file %s\n", buf);
	/** Curses_write_window (PROMPT_WINDOW, 2, 1, msg); **/
    }
    return 0;
}

static int askfile()
{
    char file[100];

    while (1)
    {
	/** Curses_prompt_gets ("Enter file to hold report: ", file); **/
	G_strip (file);
	if (*file == 0) return -1;
	if (G_index (file, '/'))
	    strcpy (buf, file);
	else
	    sprintf (buf, "%s/%s", G_home(), file);
	if (access (buf, 0) != 0)
	    return 1;
	sprintf (buf, "** %s already exists. choose another file", file);
	/** Beep(); **/
	/** Curses_write_window (PROMPT_WINDOW, 2, 1, buf); **/
    }
}

static int to_printer()
{
    FILE *fd;

    cancel_which();
    Menu_msg ("Sending report to printer ...");
/** Curses_write_window(PROMPT_WINDOW, 1, 1, "Sending report to printer ..."); **/
    fd = popen ("lp", "w");
    do_report (fd);
    pclose (fd);
    return 0;
}

static int do_report (FILE *fd)
{
    char buf[100];
    int n;
    int width;

Control_Points *points;

    /* make visiable */
    points = (Control_Points *) group.points;


    fprintf (fd, "LOCATION: %-20s GROUP: %-20s MAPSET: %s\n\n",
	G_location(), group.name, G_mapset());
    fprintf (fd, "%15sAnalysis of control point registration\n\n", "");
    fprintf (fd, "%s   %s\n", LHEAD1, RHEAD1);
    fprintf (fd, "%s   %s\n", LHEAD2, RHEAD2);

    FMT1(buf,0.0,0.0,0.0);
    width = strlen (buf);

    for (n = 0; n < points->points_temp.count; n++)
    {
	FMT0(buf,n+1);
	fprintf (fd, "%s", buf);
	if(group.stat > 0 && points->points_temp.status[n] > 0)
	{
	    FMT1(buf, resid.xres[n], resid.yres[n], resid.gnd[n]);
	    fprintf (fd, "%s", buf);
	}
	else if (points->points_temp.status[n] > 0)
	    printcentered (fd, "?", width);
	else
	    printcentered (fd, "not used", width);
	FMT2(buf,
	    points->points_temp.e1[n],
	    points->points_temp.n1[n],
	    points->points_temp.e2[n],
	    points->points_temp.n2[n]);
	fprintf (fd, "   %s\n", buf);
    }
    fprintf (fd, "\n");
    if (group.stat < 0)
	fprintf (fd, "Poorly place control points_temp\n");
    else if (group.stat == 0)
	fprintf (fd, "No active control points_temp\n");
    else
	fprintf (fd, "Overall rms error: %.2f                %s Transformation\n", rms,order_msg);

   return 0;
}

static int printcentered (FILE *fd, char *buf,int width)
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
Control_Points *points;

    /* make visiable */
    points = (Control_Points *) group.points;

    if (!true_color)
	R_standard_color (I_COLOR_WHITE);  /* ORANGE); */
    else if(points->points_temp.status[n])
	R_standard_color (I_COLOR_YELLOW);  /* GREEN); */
    else
	R_standard_color (I_COLOR_RED);
    display_one_point (VIEW_MAP1, 
		       points->points_temp.e1[n], 
		       points->points_temp.n1[n]);

   return 0;
}


static int print_order_mes()
{

  switch (group.trans_type) {
  case POLY1:
    sprintf (order_msg, "POLY1");
    break;
  case POLY2:
    sprintf (order_msg, "POLY2");
    break;
  case POLY3:
    sprintf (order_msg, "POLY3");
    break;
  case PHOTO:
    sprintf (order_msg, "PHOTO");
    break;
  case LAND_TM:
    sprintf (order_msg, "LAND_TM");
    break;
  default:
	/* ZERO_T, PROJ, NONE_T */
    sprintf (order_msg, "Bad Type");
  }

   return 0;
}


static int ask_which_trans()
{

  /* ask for transformation */
  ask_transform();

  /* recalculate residuals */
  group.anal_points(&group, &resid);


   return 0;
}
