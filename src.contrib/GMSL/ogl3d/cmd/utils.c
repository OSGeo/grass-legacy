/* general utilities for Motif stuff */

#include "interface.h"
#include "coldefs.h"
#include "string.h"


int rgb_int_from_pix (data_cell *dc, unsigned long pix)
{
int r, g, b, ret;

    get_pixel_color(dc, pix, &r, &g, &b);

    RGB_TO_INT(r,g,b,ret);

    return(ret);

}

/**********************************************************************/
/* Sets foreground color (black or white) to contrast with background */

int 
set_button_colors (Widget w, data_cell *dc, int id)
{
int tr, tg, tb;
Arg wargs[3];


    get_pixel_color(dc, dc->cells[id], &tr, &tg, &tb);
    if((tr+tg+tb) > 382)
	XtSetArg(wargs[0], XmNforeground, 
			BlackPixel(dc->dpy, dc->scr));
    else
	XtSetArg(wargs[0], XmNforeground,
			WhitePixel(dc->dpy, dc->scr));
    XtSetArg(wargs[1], XmNbackground, dc->cells[id]);
    XtSetValues(w, wargs, 2);

}

/**********************************************************************/
/* Sets foreground & background colors to match those of it's parent  */

int 
unset_button_colors (Widget w, data_cell *dc)
{
unsigned int bgpix, fgpix;
Arg wargs[3];
Widget parent;

    XtSetArg(wargs[0], XmNforeground, &fgpix);
    XtSetArg(wargs[1], XmNbackground, &bgpix);
    XtGetValues(XtParent(w), wargs, 2);
    XtSetArg(wargs[0], XmNforeground, fgpix);
    XtSetArg(wargs[1], XmNbackground, bgpix);
    XtSetValues(w, wargs, 2);

}

/********************************************************************/
/* to change label in label widget */

extern void 
change_label (Widget wid, char *str)
{
Arg wargs[1];
XmString xmstr;

    xmstr = XmStringCreateSimple(str);
    XtSetArg (wargs[0], XmNlabelString,  xmstr);
    XtSetValues (wid, wargs, 1);
    XmStringFree (xmstr);
}

/********************************************************************/

void
unmanage_cb(w, target, call_data)
Widget    w;
Widget    target;
XmAnyCallbackStruct call_data;
{

    XtUnmanageChild(target);
}


void
update_cb(w, dc, call_data)
Widget    w;
data_cell    *dc;
XmAnyCallbackStruct call_data;
{

    XmUpdateDisplay(w);

}


void
destroy_cb(w, target, call_data)
Widget    w;
Widget    target;
XmAnyCallbackStruct call_data;
{

    XtDestroyWidget(target);

}


void 
set_pixel_color (data_cell *dc, unsigned long pix, int r, int g, int b)
{
XColor tmp;

    tmp.pixel = pix;
    tmp.flags = DoRed | DoGreen | DoBlue;
    tmp.red = CHARVAL(r);
    tmp.green = CHARVAL(g);
    tmp.blue = CHARVAL(b);
    /*XStoreColor(dc->dpy, dc->cmap, &tmp);*/

#ifdef DEBUG
fprintf(stderr, "set_pixel_color called: R%d G%d B%d\n", 
			    tmp.red, tmp.green, tmp.blue);
#endif

}

void 
get_pixel_color (data_cell *dc, unsigned long pix, int *r, int *g, int *b)
{
XColor tmp;

    tmp.pixel = pix;
    tmp.flags = DoRed | DoGreen | DoBlue;
    XQueryColor(dc->dpy, dc->cmap, &tmp);
    *r = TOCHARVAL(tmp.red);
    *g = TOCHARVAL(tmp.green);
    *b = TOCHARVAL(tmp.blue);

#ifdef DEBUG
fprintf(stderr, "get_pixel_color called: R%d G%d B%d\n", 
			    tmp.red, tmp.green, tmp.blue);
#endif

}

unsigned long 
get_default_draw_color (data_cell *dc)
{
int r,g,b;
unsigned long i;

    get_pixel_color(dc, dc->cells[BG_CELL], &r, &g, &b);
    RGB_TO_INT(r,g,b,i);

    return(~i);

}



/* If positions != -1, sets position argument AND attachment to ATTACH_POSITION
 * Otherwise sets attachment to default_attachment 
*/
void 
SetPositionArgs (Arg wargs[], int *pn, int top, int bottom, int left, int right, XtArgVal default_attachment)
{
int n;

    n = *pn;

    if(top != -1){
	XtSetArg(wargs[n],XmNtopAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNtopPosition,top); n++;
    }
    else{
	XtSetArg(wargs[n],XmNtopAttachment,default_attachment); n++;
    }


    if(bottom != -1){
	XtSetArg(wargs[n],XmNbottomAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNbottomPosition,bottom); n++;
    }
    else{
	XtSetArg(wargs[n],XmNbottomAttachment,default_attachment); n++;
    }


    if(left != -1){
	XtSetArg(wargs[n],XmNleftAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNleftPosition, left); n++;
    }
    else{
	XtSetArg(wargs[n],XmNleftAttachment,default_attachment); n++;
    }


    if(right != -1){
	XtSetArg(wargs[n],XmNrightAttachment,XmATTACH_POSITION); n++;
	XtSetArg(wargs[n],XmNrightPosition, right); n++;
    }
    else{
	XtSetArg(wargs[n],XmNrightAttachment,default_attachment); n++;
    }

    *pn = n;
}


int 
make_red_yellow_ramp (int *ramp, int num, int minval, int maxval)
{
int g, i, incr;

    incr = (maxval - minval)/(num-1); 
    for(i=0; i<num; i++){
	g = minval + incr * i;
	RGB_TO_INT(maxval,g,0,ramp[i]);
    }

}


/***********************************************************************/
/* called by pack_surfinfo, pack_vectinfo */

int 
pack_handles (int *handles, int max)
{
int i, j;

    for(i=0; i < max; i++){
	if(!handles[i]){
	    for(j=i; j<max-1; j++){
		if (handles[j+1])
		    handles[j] = handles[j+1];	
	    }
	}
    }

}


/***********************************************************************/

/* checks name length to fit in 40 char surfname */
int 
_set_shortname (char *surfname, char *name)
{
    
    if(name){
	if(strlen(name) < 40)
	    strcpy(surfname, name);
	else{
	    strncpy(surfname, name, 38);
	    strcat(surfname, "");
	}
    }
    else
	strcpy(surfname, "");

}
