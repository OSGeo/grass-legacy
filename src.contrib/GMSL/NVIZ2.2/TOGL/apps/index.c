/* updated to tcl/tk 8.0  8/99 MN */
/* index.c */

/*
 * An example Tk/OpenGL program using color-index mode.
 * Brian Paul  (brianp@ssec.wisc.edu)
 */


#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include "togl.h"


/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;


/* Our color indexes: */
unsigned long black, red, green, blue;



/*
 * Togl widget create callback.  This is called by Tcl/Tk when the widget has
 * been realized.  Here's where one may do some one-time context setup or
 * initializations.
 */
void create_cb( struct Togl *togl )
{
   /* allocate color indexes */
   black = Togl_AllocColor( togl, 0.0, 0.0, 0.0 );
   red   = Togl_AllocColor( togl, 1.0, 0.0, 0.0 );
   green = Togl_AllocColor( togl, 0.0, 1.0, 0.0 );
   blue  = Togl_AllocColor( togl, 0.0, 0.0, 1.0 );

   /* If we were using a private read/write colormap we'd setup our color
    * table with something like this:
    */
/*
   black = 1;   Togl_SetColor( togl, black, 0.0, 0.0, 0.0 );
   red   = 2;   Togl_SetColor( togl, red,   1.0, 0.0, 0.0 );
   green = 3;   Togl_SetColor( togl, green, 0.0, 1.0, 0.0 );
   blue  = 4;   Togl_SetColor( togl, blue,  0.0, 0.0, 1.0 );
*/
}


/*
 * Togl widget reshape callback.  This is called by Tcl/Tk when the widget
 * has been resized.  Typically, we call glViewport and perhaps setup the
 * projection matrix.
 */
void reshape_cb( struct Togl *togl )
{
   int width = Togl_Width( togl );
   int height = Togl_Height( togl );
   float aspect = (float) width / (float) height;

   glViewport( 0, 0, width, height );

   /* Set up projection transform */
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho( -aspect, aspect, -1.0, 1.0, -1.0, 1.0 );

   /* Change back to model view transform for rendering */
   glMatrixMode(GL_MODELVIEW);
}



/*
 * Togl widget display callback.  This is called by Tcl/Tk when the widget's
 * contents have to be redrawn.  Typically, we clear the color and depth
 * buffers, render our objects, then swap the front/back color buffers.
 */
void display_cb( struct Togl *togl )
{
   glClearIndex( black );
   glClear(GL_COLOR_BUFFER_BIT);

   glLoadIdentity();

   glBegin( GL_TRIANGLES );

   glIndexi( red );
   glVertex2f( -0.5, -0.3 );
   glVertex2f(  0.5, -0.3 );
   glVertex2f(  0.0,  0.6 );

   glIndexi( green );
   glVertex2f( -0.5+0.2, -0.3-0.2 );
   glVertex2f(  0.5+0.2, -0.3-0.2 );
   glVertex2f(  0.0+0.2,  0.6-0.2 );

   glIndexi( blue );
   glVertex2f( -0.5+0.4, -0.3-0.4 );
   glVertex2f(  0.5+0.4, -0.3-0.4 );
   glVertex2f(  0.0+0.4,  0.6-0.4 );

   glEnd();

   glFlush();
}



/*
 * Called by Tk_Main() to let me initialize the modules (Togl) I will need.
 */
int my_init( Tcl_Interp *interp )
{
   /*
    * Initialize Tcl, Tk, and the Togl widget module.
    */
   if (Tcl_Init(interp) == TCL_ERROR) {
      return TCL_ERROR;
   }
   if (Tk_Init(interp) == TCL_ERROR) {
      return TCL_ERROR;
   }
   if (Togl_Init(interp) == TCL_ERROR) {
      return TCL_ERROR;
   }

   /*
    * Specify the C callback functions for widget creation, display,
    * and reshape.
    */
   Togl_CreateFunc( create_cb );
   Togl_DisplayFunc( display_cb );
   Togl_ReshapeFunc( reshape_cb );

   /*
    * Make a new Togl widget command so the Tcl code can set a C variable.
    */
   /* NONE */

   /*
    * Call Tcl_CreateCommand for application-specific commands, if
    * they weren't already created by the init procedures called above.
    */
   /*NOTHING*/

   /*
    * Specify a user-specific startup file to invoke if the application
    * is run interactively.  Typically the startup file is "~/.apprc"
    * where "app" is the name of the application.  If this line is deleted
    * then no user-specific startup file will be run under any conditions.
    */
#if ((TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION >= 5))
   Tcl_SetVar( interp, "tcl_rcFileName", "./index.tcl", TCL_GLOBAL_ONLY );
#else
/*   tcl_RcFileName = "./index.tcl";*/
   Tcl_SetVar( interp, "tcl_rcFileName", "./index.tcl", TCL_GLOBAL_ONLY );
#endif

   return TCL_OK;
}



int main( int argc, char *argv[] )
{
   Tk_Main( argc, argv, my_init );
   return 0;
}
