/* Written by Bill Brown, USACERL (brown@zorro.cecer.army.mil)
 * May 2-12, 1994
 * Upgraded for floating point grass Oct 1995
 *
 * This code is in the public domain. Specifically, we give to the public
 * domain all rights for future licensing of the source code, all resale
 * rights, and all publishing rights.
 * 
 * We ask, but do not require, that the following message be included in
 * all derived works:
 *     "Portions developed at the US Army Construction Engineering 
 *     Research Laboratories, Champaign, Illinois."
 * 
 * USACERL GIVES NO WARRANTY, EXPRESSED OR IMPLIED,
 * FOR THE SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT
 * LIMITATION, WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A
 * PARTICULAR PURPOSE.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include "gis.h"
#include "gui.h"

#define COLOR_OFFSET 0
#define MAXIMAGES 400
#define DEF_MAX 900
#define DEF_MIN 600
#define MAXVIEWS    4 
#define BORDER_W    2


static Boolean do_run();
void change_label();
short _get_Xlookup();
char **gee_wildfiles();
char *G__mapset_name ();
void parse_command();
/*char *strchr();*/



Widget 	toplevel, mainwin, canvas, flabel; 
Display *theDisplay;
XImage *pic_array[MAXIMAGES];
GC           invertGC, drawGC;
int nrows, ncols, numviews;
int MaxColors, Top=0, Left=0;
char    frame[MAXIMAGES][4];
char 	*vfiles[MAXVIEWS][MAXIMAGES];
int	LabelPos[MAXVIEWS][2];

float 	vscale, scale;  /* resampling scale factors */
int 	irows, icols, vrows, vcols;
int 	frames;

int depth;

extern Display *dpy;
extern Window grwin;
extern int scrn;
extern Visual *use_visual;
extern Colormap fixedcmap;

int main (argc, argv)
    int  argc;
    char **argv;
{
    int	     	i, j;
    int       	*sdimp, longdim;
    unsigned long blackPix, whitePix;

    struct gui_data cd;

    XtAppContext AppC;
    Arg          wargs[15];
    int          n;
    Widget       trc;

    
    toplevel = XtAppInitialize(&AppC, "xganimate", NULL, 0,
			      &argc, argv, NULL, wargs, 0);

    theDisplay = XtDisplay(toplevel);
   

    G_gisinit (argv[0]);
    parse_command(argc, argv, vfiles, &numviews, &frames);

    /* debug */
    for(i=0; i<numviews; i++){
	fprintf(stderr,"\nVIEW %d: ", i+1);
	for(j=0; j< frames; j++){
	    fprintf(stderr,"%s ", vfiles[i][j]);
	}
    }
    fprintf(stderr,"\n");

    vrows = G_window_rows();
    vcols = G_window_cols();
    nrows = vrows;
    ncols = vcols;

    /* short dimension */
    sdimp = nrows>ncols? &ncols: &nrows;

    /* these proportions should work fine for 1 or 4 views, but for
    2 views, want to double the narrow dim & for 3 views triple it */
    if(numviews == 2)
	*sdimp *= 2;
    else if(numviews == 3)
	*sdimp *= 3;

    longdim = nrows>ncols? nrows: ncols;

    scale = 1.0;

    { /* find animation image size */
    int max, min;
    char *p;

    max = DEF_MAX;
    min = DEF_MIN;

    if(p = getenv ("XGANIM_SIZE"))
	max = min = atoi(p);

    if(longdim > max)      /* scale down */
	scale = (float)max/longdim;
    else if(longdim < min) /* scale up */
	scale = (float)min/longdim;
    }
    
    vscale = scale;
    if(numviews == 4)
	vscale = scale / 2.;

    nrows *= scale;
    ncols *= scale;
    /* now nrows & ncols are the size of the combined - views image */
    vrows *= vscale;
    vcols *= vscale;
    /* now vrows & vcols are the size for each sub-image */

    /* add to nrows & ncols for borders */
    /* irows, icols used for vert/horizontal determination in loop below */
    irows = nrows;
    icols = ncols;
    nrows += (1 + (nrows/vrows)) * BORDER_W;
    ncols += (1 + (ncols/vcols)) * BORDER_W;


    n = 0;
    if(ncols>nrows){
	XtSetArg(wargs[n], XmNwidth, ncols); n++;
	XtSetArg(wargs[n], XmNheight, nrows+60); n++;
    }
    else{
	XtSetArg(wargs[n], XmNwidth, ncols+80); n++;
	XtSetArg(wargs[n], XmNheight, nrows); n++;
    }
    mainwin = XtCreateManagedWidget("GRASS Animate", xmFormWidgetClass,
	    toplevel, wargs, n);

    cd.speed = 100;
    cd.direction = 1;
    cd.shownames = 1;

    n=0;
    XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
    XtSetArg(wargs[n],XmNwidth,ncols); n++;
    XtSetArg(wargs[n],XmNheight,nrows); n++;
    canvas = XtCreateManagedWidget("canvas", xmDrawingAreaWidgetClass,
		  mainwin, wargs, n);

    n=0;
    if(ncols > nrows){
	XtSetArg(wargs[n],XmNorientation,XmHORIZONTAL); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNtopWidget,canvas); n++;
    }
    else{
	XtSetArg(wargs[n],XmNorientation,XmVERTICAL); n++;
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n],XmNleftWidget,canvas); n++;
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_FORM); n++;
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_FORM); n++;
    }
    XtSetArg(wargs[n],XmNbackground,WhitePixelOfScreen(XtScreen(toplevel)));n++;
    XtSetArg(wargs[n],XmNadjustMargin,False); n++;
    trc = XtCreateManagedWidget("controls_rc",
		xmRowColumnWidgetClass, mainwin, wargs, n);

    make_buttons(&cd, trc, XtScreen(toplevel));

    n = 0;
    XtSetArg(wargs[n],XmNalignment, XmALIGNMENT_END); n++;
    flabel = XtCreateManagedWidget("cfr", xmLabelWidgetClass, 
		trc, wargs,n);

    XtRealizeWidget(toplevel);
    set_buttons_pixmap(theDisplay, XtWindow(canvas));

    /**************************************************************/

    dpy = XtDisplay(canvas);
    grwin = XtWindow(canvas);
    scrn = DefaultScreen(dpy);
    use_visual = DefaultVisual(dpy, scrn);
#if 1
    fixedcmap = XCreateColormap(dpy, grwin,
				use_visual, AllocNone);
#else
    fixedcmap = DefaultColormap(dpy, scrn);
#endif
    fixedcmap = InitColorTableFixed(fixedcmap);

    XtVaGetValues(canvas, XmNdepth, &depth, NULL);

    XtVaSetValues(toplevel, XmNcolormap, fixedcmap, NULL);
    XtSetWMColormapWindows(toplevel, &canvas, 1);

    /**************************************************************/

    blackPix = _get_lookup_for_color(0, 0, 0);
    whitePix = _get_lookup_for_color(255, 255, 255);

    drawGC = XCreateGC(XtDisplay(canvas), XtWindow(canvas), 0, NULL);
    XSetFunction(theDisplay, drawGC, GXcopy);
    XSetForeground(theDisplay, drawGC, blackPix);
    XSetBackground(theDisplay, drawGC, whitePix);

    invertGC = XCreateGC(XtDisplay(canvas), XtWindow(canvas), 0, NULL);
    XSetFunction(theDisplay, invertGC, GXcopy);
    XSetForeground(theDisplay, invertGC, whitePix);
    XSetBackground(theDisplay, invertGC, blackPix);


    for(j=0; j<MAXIMAGES; j++)
	sprintf(frame[j],"%2d",j+1);
    
   
    while(1) { /* wait for window */
	XEvent      xev;

	XNextEvent(theDisplay, &xev);
	if(xev.type == MapNotify && xev.xmap.event == XtWindow(mainwin))
	    break;
    }

    XtAppAddWorkProc(AppC, do_run, &cd);
    XtAppMainLoop(AppC);

    return 0;
}

int load_files()
{
CELL 	*cell;
FCELL 	*fcell;
DCELL 	*dcell;
void    *voidc;
unsigned char	*tr, *tg, *tb, *tset;
int     tsiz, coff;
register int rowoff, row, col, vxoff, vyoff;
int 	cnt, ret, fd;
int	vnum;
XImage  *pa;
char	*mapset, name[BUFSIZ];
struct Colors colors;
int     rtype;

    cell = G_allocate_c_raster_buf();
    fcell = G_allocate_f_raster_buf();
    dcell = G_allocate_d_raster_buf();
     
    tsiz = G_window_cols();

    if(NULL == (tr = malloc(tsiz * sizeof(char)))){
	fprintf(stderr,"Unable to malloc.\n");
	exit (0);
    }
    if(NULL == (tg = malloc(tsiz * sizeof(char)))){
	fprintf(stderr,"Unable to malloc.\n");
	exit (0);
    }
    if(NULL == (tb = malloc(tsiz * sizeof(char)))){
	fprintf(stderr,"Unable to malloc.\n");
	exit (0);
    }
    if(NULL == (tset = malloc(tsiz * sizeof(char)))){
	fprintf(stderr,"Unable to malloc.\n");
	exit (0);
    }

    for (cnt = 0; cnt < frames; cnt++)
    {
	
        if (cnt > MAXIMAGES)
	{
	    cnt--;
	    break;
	}

	pa = XCreateImage(theDisplay, use_visual, depth, ZPixmap,
			  0, NULL,  ncols, nrows, 8, 0);
	pa->data = G_malloc(nrows * pa->bytes_per_line);
	pic_array[cnt] = pa;

	for(vnum = 0; vnum < numviews; vnum++){
	    if(icols == vcols){
		vxoff =  BORDER_W;
		vyoff = (irows == vrows)? BORDER_W : 
			    BORDER_W + vnum*(BORDER_W+vrows);
	    }
	    else if (irows == vrows){
		vxoff = (icols == vcols)? BORDER_W : 
			    BORDER_W + vnum*(BORDER_W+vcols);
		vyoff =  BORDER_W;
	    }
	    else{ /* 4 views */
		/* assumes we want :
		    view1	view2

		    view3	view4   
		*/
		vxoff = vnum%2? BORDER_W: vcols+2*BORDER_W;
		vyoff = vnum>1? vrows+2*BORDER_W: BORDER_W; 
	    }
	    if(!cnt){
		LabelPos[vnum][0] = vxoff;
		LabelPos[vnum][1] = vyoff+vrows-1;
	    }

	    strcpy(name,vfiles[vnum][cnt]);
	    fprintf (stderr, "\rReading file '%s'\n", name);
	    mapset = G_find_cell2 (name, "");
	    if (mapset == NULL){
		char msg[100];	
		sprintf (msg, "%s: <%s> cellfile not found\n", 
					    G_program_name(), name);
		G_fatal_error (msg);
		exit(1);
	    }
	    fd = G_open_cell_old (name, mapset);
	    if (fd < 0)
		exit(1);
    /*
	    strcpy(title[cnt],G_get_cell_title(name, mapset));
    */

	    rtype = G_raster_map_type(name, mapset);
	    if (rtype == CELL_TYPE)
		voidc = (CELL *)cell;
	    else if (rtype == FCELL_TYPE)
		voidc = (FCELL *)fcell;
	    else if (rtype == DCELL_TYPE)
		voidc = (DCELL *)dcell;
	    else
		exit(0);

	    ret = G_read_colors(name, mapset, &colors);
	    if (ret < 0)
		exit(1);

	    for (row = 0; row < vrows; row++){
		if (G_get_raster_row (fd, (void *)voidc,
				      (int)(row/vscale), rtype) < 0)
		    exit(1);

		rowoff = (vyoff+row)*ncols;
		G_lookup_raster_colors((void *)voidc, tr, tg, tb, tset, tsiz,
		    &colors, rtype);
		for (col = 0; col < vcols; col++){
		    coff= (int)(col/vscale);
		    if(!tset[coff])
			tr[coff] = tg[coff] = tb[coff] = 255;
		    XPutPixel(pa, vxoff+col, vyoff+row,
			      _get_lookup_for_color(tr[coff],
						    tg[coff],
						    tb[coff]));
		}
	    }

	    G_close_cell(fd);
	}

	XPutImage(theDisplay, XtWindow(canvas), drawGC, pa, 0, 0, 
		  Left, Top, ncols, nrows);
	change_label(flabel, frame[cnt]);

    }
    free(cell);
    free(fcell);
    free(dcell);
    free(tr);
    free(tg);
    free(tb);
    free(tset);

    return(cnt);

}

/* ###################################################### */
static Boolean do_run(cd)
struct gui_data *cd;
{
int i, cnt;
static int first=1;
Drawable dr;
    
    if(first){
	first=0;
	cnt = load_files();
	cd->curframe = cd->direction > 0? 0: cnt-1;
	cd->prevframe = cd->curframe;
	cd->step = cd->stop = 0;
	cd->loop = cd->swing = 0;
	cd->nframes = cnt;

    }

    if(cd->rewind){
	cd->rewind = 0;
	cd->curframe = 0;
	cd->direction = 1;
	cd->step = 1;
    }
    if(cd->swing){
	if(cd->curframe==cd->nframes || cd->curframe<0){
	     cd->direction = -cd->direction;
	     cd->curframe += cd->direction;
	}
    }
    else if(cd->loop){
	if(cd->curframe==cd->nframes)
	     cd->curframe = 0;
	else if(cd->curframe<0)
	     cd->curframe = cd->nframes-1;
    }
    else if(cd->curframe == cd->nframes || cd->curframe < 0)
	cd->stop = 1;

    if(cd->stop && !cd->step)
	return (False);

    if(cd->curframe < cd->nframes && cd->curframe >= 0){
	/* little pause */
	{
	float tf;
	for(tf=0.0; tf < cd->speed; tf += .01);
	}

	dr = XtWindow(canvas);
	XPutImage(theDisplay, dr, drawGC, pic_array[cd->curframe], 0, 0, 
		    Left, Top, ncols, nrows);
	/* draw labels */
	if(cd->shownames == 1)
	    for(i=0; i < numviews; i++){
		XDrawString(theDisplay, dr, drawGC,
			LabelPos[i][0]+5, LabelPos[i][1]-5,
			vfiles[i][cd->curframe],
			strlen(vfiles[i][cd->curframe]));
	    }
	else if(cd->shownames == 2)
	    for(i=0; i < numviews; i++){
		XDrawString(theDisplay, dr, invertGC,
			LabelPos[i][0]+5, LabelPos[i][1]-5,
			vfiles[i][cd->curframe],
			strlen(vfiles[i][cd->curframe]));
	    }
	change_label(flabel, frame[cd->curframe]);

	cd->prevframe = cd->curframe;
    }

    cd->curframe += cd->direction;

    if (cd->step){
	cd->step = 0;
	cd->stop = 1;
    }

    return False; /* to keep it running */
}

/* ###################################################### */
char **gee_wildfiles(wildarg, element, num)
char *wildarg, *element;
int *num;
{
int n, cnt=0;
char path[1000], *mapset, cmd[1000], buf[512];
char *p, *tfile;
static char *newfiles[MAXIMAGES];
FILE *tf;
   
    *num = 0;
    tfile = G_tempfile();
    /* build list of filenames */
    for(n=0; (mapset = G__mapset_name (n)); n++){
	if (strcmp (mapset,".") == 0)
	    mapset = G_mapset();
	G__file_name (path, element, "", mapset);
	if(access(path, 0) == 0) {
	    sprintf(cmd, "cd %s; \\ls %s >> %s 2> /dev/null", 
		path, wildarg, tfile);
	    system(cmd);
	}
    }
    if(NULL == (tf = fopen(tfile, "r"))){
	fprintf(stderr, "Error reading wildcard\n");
    }
    else{
	while(NULL != fgets(buf,512,tf)){
	    /* replace newline with null */
	    if( p = strchr(buf, '\n'))
		*p = '\0';
	    /* replace first space with null */
	    else if( p = strchr(buf, ' '))
		*p = '\0';
	    if(strlen(buf) > 1){
		newfiles[cnt++] = G_store (buf);
	    }
	}
	fclose(tf);
    }
    *num = cnt;
    free (tfile);
    return(newfiles);

}



/********************************************************************/
/* to change label in label widget */

void
change_label(wid, str)
Widget wid;
char *str;
{
Arg wargs[1];
XmString xmstr;

    xmstr = XmStringCreateSimple(str);
    XtSetArg (wargs[0], XmNlabelString,  xmstr);
    XtSetValues (wid, wargs, 1);
    XmStringFree (xmstr);
}

/********************************************************************/
void parse_command(argc, argv, vfiles, numviews, numframes)
int argc;
char *argv[];
char *vfiles[MAXVIEWS][MAXIMAGES];
int *numframes, *numviews;
{
    struct Option *viewopts[MAXVIEWS]; 
    char buf[BUFSIZ], **wildfiles;
    int i,j,k, numi, wildnum;

    *numviews = *numframes = 0;
    for(i=0; i<MAXVIEWS; i++){
	viewopts[i] = G_define_option();
	sprintf(buf,"view%d", i+1);
	viewopts[i]->key		= G_store(buf);
	viewopts[i]->type 		= TYPE_STRING;
	viewopts[i]->required 		= (i? NO: YES);
	viewopts[i]->multiple 		= YES;
	viewopts[i]->gisprompt 		= "old,cell,Raster";;
	sprintf(buf,"Raster file(s) for View%d", i+1);
	viewopts[i]->description 	= G_store(buf);
    }
    if (G_parser (argc, argv))
	    exit (-1);

    for(i=0; i<MAXVIEWS; i++){
	if(viewopts[i]->answers){
	    (*numviews)++;
	    for (j = 0, numi=0 ; viewopts[i]->answers[j] ; j++){
		if((NULL != strchr(viewopts[i]->answers[j], '*')) || 
		   (NULL != strchr(viewopts[i]->answers[j], '?')) || 
		   (NULL != strchr(viewopts[i]->answers[j], '['))){
		    wildfiles = gee_wildfiles(viewopts[i]->answers[j],
				"cell", &wildnum);
		    for(k=0; k<wildnum; k++){
			vfiles[i][numi++] = wildfiles[k];
		    }
		}
		else
		    vfiles[i][numi++] = G_store(viewopts[i]->answers[j]);
	    }
	    /* keep track of smallest number of frames */
	    *numframes = *numframes? *numframes > numi? numi: *numframes: numi;
	}
    }

}

/*********************************************************************/
/*********************************************************************/
