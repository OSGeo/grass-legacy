/* togl_cb.c */

#include <stdlib.h>
#include <string.h>
#include "togl.h"
#include "interface.h"

#ifndef WIN32
#define X11
#endif

#ifdef X11
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <GL/glx.h>
#endif /*X11 */

/*
static struct Togl *Togl_wins[NV_MAXVIEWS];
static int Togl_num=0;
*/

static struct Togl *Togl_cur = NULL;

/*
 * Togl widget create callback.  This is called by Tcl/Tk when the widget has
 * been realized.  Here's where one may do some one-time context setup or
 * initializations.
 */
void create_cb(struct Togl *togl)
{
/*   fprintf (stdout,"create_cb\n");*/
    Togl_cur = togl;
    GS_init_view();
}


/*
 * Togl widget reshape callback.  This is called by Tcl/Tk when the widget
 * has been resized.  Typically, we call glViewport and perhaps setup the
 * projection matrix.
 */
void reshape_cb(struct Togl *togl)
{
    int width = Togl_Width(togl);
    int height = Togl_Height(togl);

    GS_set_viewport(0, width, 0, height);

    GS_set_draw(GSD_BACK);
/*   GS_clear(0x0000FF); *//* causes red flash - debug only */

    GS_ready_draw();
    GS_alldraw_wire();
    GS_done_draw();

/*   
fprintf (stdout,"resize_cb %s %d %d\n", Togl_Ident(togl), width, height );
*/

#ifdef OLD

    glViewport(0, 0, width, height);

    /* Set up projection transform */
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-aspect, aspect, -1.0, 1.0, 1.0, 10.0);

    /* Change back to model view transform for rendering */
    glMatrixMode(GL_MODELVIEW);

#endif
}


void display_cb(struct Togl *togl)
{
    GS_set_draw(GSD_BACK);
/*   GS_clear(0x0000FF); *//* debug only */

    GS_ready_draw();
    GS_alldraw_wire();
    GS_done_draw();
}

void swap_togl(void)
{

    if (!Togl_cur) {
	fprintf(stderr, "set Togl_CreateFunc\n");
	return;
    }
#ifdef DEBUG_MSG
    fprintf(stderr, "calling Togl_SwapBuffers...\n");
#endif

    Togl_SwapBuffers(Togl_cur);

#ifdef DEBUG_MSG
    fprintf(stderr, "Togl_SwapBuffers returns.\n");
#endif
}

GLuint load_font(char font[100])
{

    if (!Togl_cur) {
	fprintf(stderr, "set Togl_CreateFunc\n");
	return (0);
    }

    FontBase = Togl_LoadBitmapFont(Togl_cur, font);
    if (!FontBase) {
	fprintf(stderr, "Cannot Load Font\n");
	fprintf(stderr, "Trying default Font\n");
	FontBase = Togl_LoadBitmapFont(Togl_cur, TOGL_BITMAP_HELVETICA_12);
	if (!FontBase) {
	    fprintf(stderr, "Cannot Load Default Font\n");
	    return (0);
	}
    }
    return (FontBase);
}

int unload_font(Nv_data * data, Tcl_Interp * interp,	/* Current interpreter. */
		int argc,	/* Number of arguments. */
		char **argv)
{
    int font_base;

    font_base = (int) atoi(argv[1]);

    if (!Togl_cur) {
	fprintf(stderr, "set Togl_CreateFunc\n");
	return (0);
    }

    Togl_UnloadBitmapFont(Togl_cur, font_base);
    return (TCL_OK);
}
