
#include "interface.h"
#include <math.h>

/*#ifdef USE_GLX*/
/*
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/GLW/GlwMDrawingArea.h>
*/
#include <GL/GLwMDrawA.h>
/* #include <GL/GLwDrawAP.h> */
#include <Xm/XmP.h>
/*#endif*/

#ifdef USE_OGL
#include <GL/gl.h>
#include <GL/glx.h>
#endif

/* trying to use the values below for reading in pixel data from the function 
   gsd_getimage, so had to change to extern. This function makes a call to 
   glGetIntegerv(GL_VIEWPORT, tmp). For reasons unknown, this function call 
   clears the graphics window, and takes alot of time to complete. (E. Cline 1997) */
   
int W_width, W_height;


static Display *Dpy;
static Window Win;


void 
swap_buffers (void)
{


    glXSwapBuffers(Dpy, Win);

}

/* init:
** this function is used to initialize any GL stuff
** that is not configured by the call to GLXgetconfig
*/


void
init(w, dc, call_data)
Widget w;
data_cell *dc;
GLwDrawingAreaCallbackStruct *call_data;
{
XVisualInfo *vis;
Arg wargs[3];
GLXContext glxcontext;

	XtSetArg(wargs[0], GLwNvisualInfo, &vis);

	XtGetValues(dc->monitor, wargs, 1);

	Dpy = XtDisplay(dc->monitor);
	Win = XtWindow(dc->monitor);
	GS_set_swap_func(swap_buffers);

	glxcontext = glXCreateContext(Dpy, vis, NULL,True);

        glXMakeCurrent( Dpy, Win, glxcontext); 	 	
	GS_init_view();
	GS_set_focus_center_map(dc->hSurf[0]);

	XtCallCallbacks(w, GLwNresizeCallback, call_data);

	if (glXIsDirect(Dpy, glxcontext))
		fprintf(stderr, "Using hardware rendering! ..... or Mesa.\n");
	else
		fprintf(stderr, "Using software rendering!\n");

}


/********************************************************************/
/* resized:
** used to resize the GL image when it needs resizing
*/



void
resized(w,dc, call_data)
Widget w;
data_cell *dc;
GLwDrawingAreaCallbackStruct *call_data;
{
	W_width = call_data->width;
	W_height = call_data->height;

#ifdef USE_OGL

	glViewport(0,  0, ( (GLuint)call_data->width-1)-(0)+1, ( (GLuint)call_data->height-1)-( 0)+1); glScissor(0,  0, ( (GLuint)call_data->width-1)-(0)+1, ( (GLuint)call_data->height-1)-( 0)+1);

#endif

	quick_draw(dc);

}


/********************************************************************/

/* add_me:
** adds or removes a callback to the GL window
** depending on the status of the "whats here" button
*/


#ifdef USE_GLX

void
add_me(w, dc, data)
Widget w;
data_cell *dc;
XmToggleButtonCallbackStruct  data;
{


    if(XmToggleButtonGetState(w)){
	inform(dc,"Done");
	XtRemoveCallback(dc->monitor, GLwNinputCallback, show_it,dc);
	dc->Wset = 1;	
    }
		    
    else if(dc->Wset){  /* don't add it twice */
	inform(dc,"Click left mouse button on surface for surface info");
	XtAddCallback(dc->monitor, GLwNinputCallback, show_it,dc);
	dc->Wset = 0;
    }
}

#else

void
add_me(w, dc, data)
Widget w;
data_cell *dc;
XmToggleButtonCallbackStruct  data;
{

}

#endif		


/********************************************************************/

/* exposed:
** used to redraw the GL image whenever the window becomes exposed
*/

#ifdef USE_GLX

void
exposed(w, dc, call_data)
Widget w;
data_cell *dc;
GLwDrawingAreaCallbackStruct *call_data;
{

	quick_draw(dc);

}

#endif

/********************************************************************/

/*#ifdef USE_GLX
*/
void
add_look(w, dc, data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct  data;
{

    inform(dc,"Click left mouse button in display window to set center");
    XtAddCallback(dc->monitor, GLwNinputCallback, look_here,dc);
}

void
look_here(w, dc, call_data)
Widget w;
data_cell *dc;
GLwDrawingAreaCallbackStruct *call_data;
{
    Arg wargs[5];
    int n;
    char str[80];

    int sx, sy;
    int i, id;
    float x, y, z;
    
    /* avoid double response */
    if(call_data->event->type == ButtonRelease) return;

    sx = call_data->event->xbutton.x;
    sy = W_height - call_data->event->xbutton.y;
    if(GS_look_here(sx, sy)){
	inform(dc, "New center of view has been set");
    }

    /* remove itself as callback to glx window */
    XtRemoveCallback(dc->monitor, GLwNinputCallback, look_here,dc);

    quick_draw(dc);

}

/*#endif*/


/********************************************************************/

/* show_it:
** prints to a text widget the mouse coordinates when clicked
** while the "whats here" button is active
*/
/*
*  GlxNinputCallback
*  
*  Specifies the list of callbacks that is called when the
*  widget receives a keyboard or mouse event.  By default,
*  the input callback is called on each key press and key
*  release, on each mouse button press and release, and
*  whenever the mouse is moved while a button is pressed.
*  However this can be changed by providing a different
*  translation table. The callback structure also includes
*  the input event.  The callback reason is GlxCR_INPUT.
*  
*  The input callback is provided as a programming
*  convenience, as it provides a convenient way to catch
*  all input events.  However, a more modular program can
*  often be obtained by providing specific actions and
*  translations in the application rather than using a
*  single catch all callback.  Use of explicit
*  translations can also provide for more customizability.
*/

#ifdef USE_GLX

void
show_it(w, dc, call_data)
Widget w;
data_cell *dc;
GLwDrawingAreaCallbackStruct *call_data;
{
    Arg wargs[5];
    int n;
    char str[80];

    int sx, sy, i;
    float dist, x, y, z, pt[3];
    static int id, pid;
    static float px, py, pz; 
    static int first=1;

    if(call_data->event->type == ButtonRelease) return;

    sx = call_data->event->xbutton.x;
    sy = W_height - call_data->event->xbutton.y;
    if(GS_get_selected_point_on_surface(sx, sy, &id, &x, &y, &z)){
    /* returns x, y, z position on surface id */
    /* may also use GS_query_surf(id, x, y, ATT) for more info */ 

	if(0 <= (i = surf_indexof_handle(dc, id))){
	    sprintf(str,"\nSurface: %s\n", dc->Surf_Settings[i].surfname);
	    XmTextInsert(dc->Wtxt, dc->current_position, str);
	    dc->current_position += strlen(str);
	}

	sprintf(str,"\teasting:  %15.4f\n", x);	
	XmTextInsert(dc->Wtxt, dc->current_position, str);
	dc->current_position += strlen(str);

	sprintf(str,"\tnorthing: %15.4f\n", y);	
	XmTextInsert(dc->Wtxt, dc->current_position, str);
	dc->current_position += strlen(str);

	sprintf(str,"\televation:%15.4f\n", z);	
	XmTextInsert(dc->Wtxt, dc->current_position, str);
	dc->current_position += strlen(str);

	if(!first){
	    sprintf(str,"xy distance from previous:   %15.4f\n", 
		    (float)sqrt((px-x)*(px-x)+(py-y)*(py-y)));	
	    XmTextInsert(dc->Wtxt, dc->current_position, str);
	    dc->current_position += strlen(str);
	    sprintf(str,"xyz distance from previous:  %15.4f\n", 
		    (float)sqrt((px-x)*(px-x)+(py-y)*(py-y)+(pz-z)*(pz-z)));	
	    XmTextInsert(dc->Wtxt, dc->current_position, str);
	    dc->current_position += strlen(str);
	    if(pid == id){
		if(GS_get_distance_alongsurf(id, x, y, px, py, &dist, 0)){
		    sprintf(str,"distance along surface:      %15.4f\n", dist);
		    XmTextInsert(dc->Wtxt, dc->current_position, str);
		    dc->current_position += strlen(str);
		}
		if(GS_get_distance_alongsurf(id, x, y, px, py, &dist, 1)){
		    sprintf(str,"distance along exag. surface:%15.4f\n", dist);
		    XmTextInsert(dc->Wtxt, dc->current_position, str);
		    dc->current_position += strlen(str);
		}
		GS_set_draw(GSD_FRONT);
		GS_draw_line_onsurf(id, x, y, px, py);
		GS_set_draw(GSD_BACK);
	    }
	}
	px = x; py = y; pz = z;
	pid = id;
	first=0;

	pt[X] = x;
	pt[Y] = y;
	GS_set_draw(GSD_FRONT);
	GS_draw_X(id, pt);
	GS_set_draw(GSD_BACK);
    }
    else{
	sprintf(str,"\nselected point not on surface\n");	
	XmTextInsert(dc->Wtxt, dc->current_position, str);
	dc->current_position += strlen(str);
    }

    n = 0;
    XtSetArg(wargs[n], XmNcursorPosition, dc->current_position); n++;
    XtSetValues(dc->Wtxt, wargs, n);

    XmTextShowPosition(dc->Wtxt, dc->current_position);

    
}

#else

void 
show_it (Widget w, data_cell *dc, caddr_t *call_data)
{


}

#endif
