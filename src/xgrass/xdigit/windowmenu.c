/*
** Written Fall 1992 by Terry Baker
** US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include "digit.h"


make_win_menu (parent)
    Widget parent;
{
    Widget marker, alabs, llabs, slabs, labline;
    Widget site, line, nodes, clear, scale;
    Widget where, overlay, backdrop, sep, frame;
    int    x, y;
    int    n = 0;
    Arg    wargs[10];
    Pixel  fg, bg;
    Pixmap pix;


    n = 0;
    XtSetArg (wargs[n], XtNforeground, &fg); n++;
    XtSetArg (wargs[n], XtNbackground, &bg); n++;
    XtGetValues (parent, wargs, n);

    x = 15;
    y = 5;
 
    marker = 
    make_button (parent, "", NULL, NULL, "markers", fg, bg,
						"Display area markers");
    XtAddCallback (marker, XmNactivateCallback, win_men, MWC_CENT);


    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (marker, wargs, n);
   
    x += 6;
 
    alabs = 
    make_button (parent, "", NULL, NULL, "arealabs", fg, bg,
						"Display area labels");
    XtAddCallback (alabs, XmNactivateCallback, win_men, MWC_LABELS);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (alabs, wargs, n);
   
    x += 6;
 
    site = 
    make_button (parent, "", NULL, NULL, "site", fg, bg,
						"Display sites");
    XtAddCallback (site, XmNactivateCallback, win_men, MWC_SITES);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (site, wargs, n);
   
    x += 6;
 
    slabs = 
    make_button (parent, "", NULL, NULL, "sitelabs", fg, bg,
						"Display site labels");
    XtAddCallback (slabs, XmNactivateCallback, win_men, MWC_SLABELS);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (slabs, wargs, n);

    x += 6;

    line = 
    make_button (parent, "", NULL, NULL, "line", fg, bg,
						"Display lines"); 
    XtAddCallback (line, XmNactivateCallback, win_men, MWC_LINES);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (line, wargs, n);


    x += 6;
    llabs = 
    make_button (parent, "", NULL, NULL, "linelabs", fg, bg,
						"Display line labels"); 
    XtAddCallback (llabs, XmNactivateCallback, win_men, MWC_LLABELS);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (llabs, wargs, n);

    x += 6;
    labline = 
    make_button (parent, "", NULL, NULL, "labline", fg, bg,
						"Display labeled lines"); 
    XtAddCallback (labline, XmNactivateCallback, win_men, MWC_LLINES);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (labline, wargs, n);



    x += 6;
    nodes = 
    make_button (parent, "", NULL, NULL, "node", fg, bg, "Display nodes"); 

    XtAddCallback (nodes, XmNactivateCallback, win_men, MWC_NODES);
    n = 0;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNspacing, 1); n++;
    XtSetValues (nodes, wargs, n);

    x += 8;
    scale =
    make_button (parent, "", NULL, NULL, "scale", fg, bg,
						"Display scale");
    XtAddCallback (scale, XmNactivateCallback, win_men, MWC_SCALE);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (scale, wargs, n); 
  
    x += 6; 

    overlay =
    make_button (parent, "", win_men, MWC_OVERLAY, "overlay", fg, bg,
						"Display overlay vector map");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (overlay, wargs, n); 
    
    x += 6; 
    backdrop =
    make_button (parent, "",win_men, MWC_BACKDROP, "backdrop", fg, bg,
						"Display backdrop raster map");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (backdrop, wargs, n); 
   
    x += 8; 
    clear =
    make_button (parent, "", win_men, MWC_CLEAR, "clear", fg, bg,
						"Clear window");
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (clear, wargs, n); 
   
    x += 6; 
    where =
    make_button (parent, "", NULL, NULL, "where", fg, bg, "Where am I?");

    XtAddCallback (where, XmNactivateCallback, where_am_i, CM);
    n = 0;
    XtSetArg (wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNleftPosition, x); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
    XtSetArg (wargs[n], XmNtopPosition, y); n++;
    XtSetValues (where, wargs, n); 
}
