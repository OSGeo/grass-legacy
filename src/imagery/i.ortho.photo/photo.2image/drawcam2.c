#include "globals.h"
#define NLINES 18
struct box
{
    int top, bottom, left,right;
};


static int which;
static struct box more, less, report;
static int height, size, edge, nlines;
static int curp, first_point;
static double rms;
static char cam_name[30], cam_id[30];
static double Xp, Yp, cfl;
static int num_fid;
static char fid_id[5];
static double *Xf, *Yf;
static int pager;
static int xmax, ymax, gmax;
static char buf[300];

#define FMT0(buf,n) \
	sprintf (buf, "%3d ", n)
#define FMT1(buf,fid_id,Xf,Yf) \
	sprintf (buf, "%10s  %10.4lf %10.4lf ", fid_id,Xf,Yf)
#define FMT2(buf,cam_name) \
	sprintf (buf, "CAMERA NAME   %10s", cam_name)
#define FMT3(buf,cam_id) \
	sprintf (buf, "CAMERA ID     %10s", cam_id)
#define FMT4(buf,cfl) \
	sprintf (buf, "CAMERA CFL    %10.4f", cfl)
#define FMT5(buf, Xp) \
	sprintf (buf, "CAMERA XP     %10.4f", Xp)
#define FMT6(buf, Yp) \
	sprintf (buf, "CAMERA YP     %10.4f", Yp)
#define FMT7(buf, num_fid) \
	sprintf (buf, "number of fid.  %5d", num_fid)
#define LHEAD1 "         CAMERA REFERENCE FILE          "
#define LHEAD3 "                                        "
#define LHEAD4 "Num       Fid Id       Xf         Yf    " 
#define LHEAD2 "----------------------------------------"

#define BACKGROUND GREY
int ii;  /* return int for cont or quit */

drawcam ()
{
    static int use = 1;
    int pick();
    int to_printer();
    int done();
    int cont();
    static Objects objects[]=
    {
	MENU("DONE", done, &use),
	MENU("CONTINUE", cont, &use),
	MENU("PRINT", to_printer, &use),
 /*	INFO(" Double click on point to be referenced", &use),
	OTHER(pick,&use),   */
	{0}
    };

    int color;
    int tsize;
    int cury;
    int len;
    int line;
    int top, bottom, left, right, width, middle, nums;

/* to give user a response of some sort */
    Menu_msg ("Preparing Camera Reference File...");

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
    FMT0 (buf,0.0);
    nums = strlen(buf) * size;
    strcpy (buf,LHEAD1);
    len = strlen(buf);
    middle = len * size;

/* width is for max chars plus sidecar for more/less */
    width = len * size + nums + 2*height;
    if ((SCREEN_RIGHT - SCREEN_LEFT) < width)
	width = SCREEN_RIGHT - SCREEN_LEFT;


/* define the window */
    bottom = VIEW_MENU->top-1;
    top = bottom - height*NLINES;


    left = SCREEN_LEFT;
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
    R_standard_color (BLACK);
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
 * top eight lines are for column labels
 * last two line is for overall rms error.
 */
    nlines = NLINES - 9;
    first_point = 0;

/* allocate predicted values */
    Xf = (double *) G_calloc (group.camera.num_fid, sizeof (double));
    Yf = (double *) G_calloc (group.camera.num_fid, sizeof (double));


/* put head on the report */
    cury = top;
    dotext (LHEAD1, cury, cury+height, left, middle, 0, BLACK);
    cury += height;
    dotext (LHEAD2, cury, cury+height, left, middle, 0, BLACK);
    cury += height;
    
    FMT2(buf, group.camera.cam_name);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    FMT3(buf, group.camera.cam_id);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    FMT4(buf, group.camera.CFL);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    FMT5(buf, group.camera.Xp);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    FMT6(buf, group.camera.Yp);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    FMT7(buf, group.camera.num_fid);
    dotext (buf, cury, cury+height, left, middle, 0, color);
    cury += height;
    
    dotext (LHEAD3, cury, cury+height, left, middle, 0, BLACK);
    cury += height;
    dotext (LHEAD4, cury, cury+height, left, middle, 0, BLACK);
    cury += height;
    
    R_move_abs (left, cury-1);
    R_cont_abs (right, cury-1);

/* isolate the sidecar */
    R_move_abs (right, top);
    R_cont_abs (right, bottom);

/* define report box */
    report.top = top + (10*height);
    report.left = left;
    report.right = right;

/* lets do it */
    pager = 1;
    while(1)
    {
	R_text_size (tsize, tsize);
	line = 0;
	curp = first_point;
	cury = top + 10*height;
	while(1)
	{
	    if (line >= nlines || curp >= group.camera.num_fid)
		break;
	    line++;
        	color = BLACK;
		FMT1(buf, group.camera.fiducials[curp].fid_id, group.camera.fiducials[curp].Xf, group.camera.fiducials[curp].Yf);
		dotext (buf, cury, cury+height, nums, middle, 0, color);
	    if (pager)
	    {
		FMT0 (buf, curp+1);
		dotext (buf, cury, cury+height, left, nums, 0, BLACK);
	    }
	    cury += height;
	    curp++;
	}
	report.bottom = cury;
	downarrow (&more, curp < group.camera.num_fid ? BLACK : BACKGROUND);
	uparrow   (&less, first_point > 0  ? BLACK : BACKGROUND);
	R_standard_color (BACKGROUND);
	R_box_abs (left, cury, right-1, bottom);
	R_standard_color (BLACK);
	R_move_abs (left, bottom-height);
	R_cont_abs (right-1, bottom-height);

	pager = 0;
	which = -1;
	if(Input_pointer(objects) < 0)
		break;
    }

/* all done. restore what was under the window */
  
    if (ii == -1)
    { 
       right += 2*height;	/* move it back over the sidecar */
       R_standard_color (BACKGROUND);
       R_box_abs (left, top, right, bottom);
       R_panel_restore (tempfile1);
       R_panel_delete (tempfile1);
       R_flush();
    }
    free (Xf); free (Yf); 
    /* I_put_ref_points (group.name, &group.ref_points); */
    return 0; /* return but don't QUIT */
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
	if (curp >= group.points.count)
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
	group.points.status[first_point+n] = !group.points.status[first_point+n];
	show_point (first_point+n, 1);
	return 1;
    }
    which = n;
    show_point (first_point+n, 0);
    R_standard_color (RED);
    Outline_box (report.top + n*height, report.top +(n+1)*height,
		         report.left, report.right-1);
    return 0; /* ignore first click */

}

static
cont()
{
    cancel_which();
    ii = -1;
    return -1;
}

static
done()
{
    cancel_which();
    ii = -1;
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
	show_point (first_point+which, 1);
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
debug (msg) char *msg;
{
    R_stabilize();
    Curses_write_window (PROMPT_WINDOW, 1, 1, msg);
    Curses_getch(0);
}

static
to_file()
{
    int askfile();
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
	Beep();
	Curses_write_window (PROMPT_WINDOW, 2, 1, msg);
    }
    else
    {
	do_report (fd);
	fclose (fd);
	sprintf (msg, "Report saved in file %s\n", buf);
	Curses_write_window (PROMPT_WINDOW, 2, 1, msg);
    }
    return 0;
}

static
askfile()
{
    char file[100];
    char *G_index();
    char *G_home();

    while (1)
    {
	Curses_prompt_gets ("Enter file to hold report: ", file);
	G_strip (file);
	if (*file == 0) return -1;
	if (G_index (file, '/'))
	    strcpy (buf, file);
	else
	    sprintf (buf, "%s/%s", G_home(), file);
	if (access (buf, 0) != 0)
	    return 1;
	sprintf (buf, "** %s already exists. choose another file", file);
	Beep();
	Curses_write_window (PROMPT_WINDOW, 2, 1, buf);
    }
}

static
to_printer()
{
    FILE *fd;
    cancel_which();
    Menu_msg ("sending camera file to printer ...");

    fd = popen ("lpr", "w");
    do_report (fd);
    pclose (fd);
    return 0;
}

static
do_report (fd)
    FILE *fd;
{
    char buf[100];
    int n;
    int width;

    fprintf (fd, "LOCATION: %-20s GROUP: %-20s MAPSET: %s\n\n",
	G_location(), group.name, G_mapset());
    fprintf (fd, "CAMERA REFERENCE FILE\n\n", "");
    fprintf (fd, "%s   %s\n", LHEAD1);
    fprintf (fd, "%s   %s\n", LHEAD2);

    FMT1 (buf,"     ",0.0,0.0);
    width = strlen (buf);

    for (n = 0; n < group.camera.num_fid; n++)
    {
	FMT0(buf,n+1);
	fprintf (fd, "%s", buf);
        FMT1(buf, group.camera.fiducials[n].fid_id,
                  group.camera.fiducials[n].Xf,
                  group.camera.fiducials[n].Yf);
	fprintf (fd, "%s", buf);
	fprintf (fd, "   %s\n", buf);
    }
    fprintf (fd, "\n");
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
    else if(group.points.status[n])
	R_standard_color (GREEN);
    else
	R_standard_color (RED);
}
