/* double.c */

/*
 * An example Tk/OpenGL program which compares single vs double buffering.
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



static float xAngle = 0.0, yAngle = 0.0, zAngle = 0.0;





/*
 * Togl widget create callback.  This is called by Tcl/Tk when the widget has
 * been realized.  Here's where one may do some one-time context setup or
 * initializations.
 */
void create_cb( struct Togl *togl )
{
/*   fprintf (stdout,"create_cb\n");*/
   glEnable(GL_DEPTH_TEST);    /* Enable depth buffering */
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

/*   fprintf (stdout,"resize_cb %s %d %d\n", Togl_Ident(togl), width, height );*/
   glViewport( 0, 0, width, height );

   /* Set up projection transform */
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glFrustum(-aspect, aspect, -1.0, 1.0, 1.0, 10.0);

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
/*   fprintf (stdout,"display_cb %s\n", Togl_Ident(togl) );*/

   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   glLoadIdentity();	/* Reset modelview matrix to the identity matrix */
   glTranslatef(0.0, 0.0, -3.0);      /* Move the camera back three units */
   glRotatef(xAngle, 1.0, 0.0, 0.0);  /* Rotate by X, Y, and Z angles */
   glRotatef(yAngle, 0.0, 1.0, 0.0);
   glRotatef(zAngle, 0.0, 0.0, 1.0);
    
   /* Front face */
   glBegin(GL_QUADS);
   glColor3f(0.0, 0.7, 0.1);	/* Green */
   glVertex3f(-1.0, 1.0, 1.0);
   glVertex3f(1.0, 1.0, 1.0);
   glVertex3f(1.0, -1.0, 1.0);
   glVertex3f(-1.0, -1.0, 1.0);
   /* Back face */
   glColor3f(0.9, 1.0, 0.0);   /* Yellow */
   glVertex3f(-1.0, 1.0, -1.0);
   glVertex3f(1.0, 1.0, -1.0);
   glVertex3f(1.0, -1.0, -1.0);
   glVertex3f(-1.0, -1.0, -1.0);
   /* Top side face */
   glColor3f(0.2, 0.2, 1.0);   /* Blue */
   glVertex3f(-1.0, 1.0, 1.0);
   glVertex3f(1.0, 1.0, 1.0);
   glVertex3f(1.0, 1.0, -1.0);
   glVertex3f(-1.0, 1.0, -1.0);
   /* Bottom side face */
   glColor3f(0.7, 0.0, 0.1);   /* Red */
   glVertex3f(-1.0, -1.0, 1.0);
   glVertex3f(1.0, -1.0, 1.0);
   glVertex3f(1.0, -1.0, -1.0);
   glVertex3f(-1.0, -1.0, -1.0);
   glEnd();

   Togl_SwapBuffers( togl );
}




int setXrot_cb( struct Togl *togl, int argc, char *argv[] )
{
   Tcl_Interp *interp = Togl_Interp(togl);

   /* error checking */
   if (argc != 3) {
      Tcl_SetResult( interp,
                     "wrong # args: should be \"pathName setXrot ?angle?\"",
                     TCL_STATIC );
      return TCL_ERROR;
   }

   xAngle = (float)atof( argv[2] );
   
/* fprintf (stdout, "before %f ", xAngle ); */

   if ( xAngle < 0.0 ) {
     xAngle += 360.0;
   } else if ( xAngle > 360.0 ) {
     xAngle -= 360.0;
   }

/* fprintf (stdout, "after %f \n", xAngle ); */

   Togl_PostRedisplay( togl );

   /* Let result string equal value */
   strcpy( interp->result, argv[2] );
   return TCL_OK;
}



int setYrot_cb( struct Togl *togl, int argc, char *argv[] )
{
   double atof();
   Tcl_Interp *interp = Togl_Interp(togl);

   /* error checking */
   if (argc != 3) {
      Tcl_SetResult( interp,
                     "wrong # args: should be \"pathName setYrot ?angle?\"",
                     TCL_STATIC );
      return TCL_ERROR;
   }

   yAngle = (float)atof( argv[2] );
   
   if ( yAngle < 0.0 ) {
     yAngle += 360.0;
   } else if ( yAngle > 360.0 ) {
     yAngle -= 360.0;
   }

   Togl_PostRedisplay(togl);

   /* Let result string equal value */
   strcpy( interp->result, argv[2] );
   return TCL_OK;
}

int getXrot_cb( ClientData clientData, Tcl_Interp *interp,
		int argc, char *argv[])
{
  sprintf( interp->result, "%d", (int)xAngle );
  return TCL_OK;
}

int getYrot_cb( ClientData clientData, Tcl_Interp *interp,
		int argc, char *argv[])
{
  sprintf( interp->result, "%d", (int)yAngle );
  return TCL_OK;
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
   
   Togl_CreateCommand( "setXrot", setXrot_cb );
   Togl_CreateCommand( "setYrot", setYrot_cb );

   /*
    * Call Tcl_CreateCommand for application-specific commands, if
    * they weren't already created by the init procedures called above.
    */
   
   Tcl_CreateCommand( interp, "getXrot", getXrot_cb, (ClientData)NULL,
                                                     (Tcl_CmdDeleteProc *)NULL );
   Tcl_CreateCommand( interp, "getYrot", getYrot_cb, (ClientData)NULL,
                                                     (Tcl_CmdDeleteProc *)NULL );
   /*
    * Specify a user-specific startup file to invoke if the application
    * is run interactively.  Typically the startup file is "~/.apprc"
    * where "app" is the name of the application.  If this line is deleted
    * then no user-specific startup file will be run under any conditions.
    */
#if ((TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION >= 5))
   Tcl_SetVar( interp, "tcl_rcFileName", "./double.tcl", TCL_GLOBAL_ONLY );
#else
/*   tcl_rcFileName = "./double.tcl";*/
   Tcl_SetVar( interp, "tcl_rcFileName", "./double.tcl", TCL_GLOBAL_ONLY );
#endif

   return TCL_OK;
}



int main( int argc, char *argv[] )
{
   Tk_Main( argc, argv, my_init );
   return 0;
}
