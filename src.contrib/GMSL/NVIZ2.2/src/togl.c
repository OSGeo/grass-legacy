/* togl.c */

/*
 * Togl - a Tk OpenGL widget
 * Version:  1.0
 * Copyright (C) 1996  Brian Paul (brianp@ssec.wisc.edu) and
 * Ben Bederson (bederson@cs.unm.edu)  See the LICENSE file for details.
 */




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* for XA_RGB_DEFAULT_MAP atom */
#if defined(__vms)
#include <X11/StdCmap.h>  /* for XmuLookupStandardColormap */
#else
#include <X11/Xmu/StdCmap.h>  /* for XmuLookupStandardColormap */
#endif
#include <GL/glx.h>
#include <string.h>


#include "togl.h"

				/* Default widget configure options */
#define DEFAULT_WIDTH            "400"
#define DEFAULT_HEIGHT           "400"
#define DEFAULT_IDENT            ""



#define MAX(a,b)	(((a)>(b))?(a):(b))	

#define TCL_ERR(interp, string) {Tcl_ResetResult(interp); \
			         Tcl_AppendResult(interp, string, NULL); \
				 return(TCL_ERROR);}

#define ALL_EVENTS_MASK \
    KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask| \
    EnterWindowMask|LeaveWindowMask|PointerMotionMask|ExposureMask| \
    VisibilityChangeMask|FocusChangeMask|PropertyChangeMask|ColormapChangeMask



/*
 * Prototypes for functions local to this file
 */
static int  Togl_Cmd(ClientData clientData, Tcl_Interp *interp, 
		    int argc, char **argv);
static void Togl_EventProc(ClientData clientData, XEvent *eventPtr);
static int  Togl_MakeWindowExist(struct Togl *togl);


/*
 * Setup Togl widget configuration options:
 */

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
     DEFAULT_HEIGHT, Tk_Offset(struct Togl, height), 0, NULL},
  
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
     DEFAULT_WIDTH, Tk_Offset(struct Togl, width), 0, NULL},
  
    {TK_CONFIG_STRING, "-ident", "ident", "Ident",
     DEFAULT_IDENT, Tk_Offset(struct Togl, ident), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-rgba", "rgba", "Rgba",
     "true", Tk_Offset(struct Togl, rgba_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-double", "double", "Double",
     "false", Tk_Offset(struct Togl, double_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-depth", "depth", "Depth",
     "false", Tk_Offset(struct Togl, depth_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-accum", "accum", "Accum",
     "false", Tk_Offset(struct Togl, accum_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-alpha", "alpha", "Alpha",
     "false", Tk_Offset(struct Togl, alpha_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-stencil", "stencil", "Stencil",
     "false", Tk_Offset(struct Togl, stencil_flag), 0, NULL},

    {TK_CONFIG_BOOLEAN, "-privatecmap", "privateCmap", "PrivateCmap",
     "false", Tk_Offset(struct Togl, privatecmap_flag), 0, NULL},

    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
     (char *) NULL, 0, 0, NULL}
};


static Togl_Callback *create_proc = NULL;
static Togl_Callback *render_proc = NULL;
static Togl_Callback *resize_proc = NULL;
static Tcl_HashTable command_table;



/*
 * Return an X colormap to use for OpenGL RGB-mode rendering.
 * Input:  dpy - the X display
 *         scrnum - the X screen number
 *         visinfo - the XVisualInfo as returned by glXChooseVisual()
 * Return:  an X Colormap or 0 if there's a _serious_ error.
 */
static Colormap
get_rgb_colormap( Display *dpy, int scrnum, XVisualInfo *visinfo )
{
   Atom hp_cr_maps;
   Status status;
   int numCmaps;
   int i;
   XStandardColormap *standardCmaps;
   Window root = RootWindow(dpy,scrnum);
   int using_mesa;

   /*
    * First check if visinfo's visual matches the default/root visual.
    */
   if (visinfo->visual==DefaultVisual(dpy,scrnum)) {
      /* use the default/root colormap */
      return DefaultColormap( dpy, scrnum );
   }

   /*
    * Check if we're using Mesa.
   if (strstr(glXQueryServerString( dpy, scrnum, GLX_VERSION ), "Mesa")) {
      using_mesa = 1;
   }
   else {
      using_mesa = 0;
   }
    */
      using_mesa = 0;

   /*
    * Next, if we're using Mesa and displaying on an HP with the "Color
    * Recovery" feature and the visual is 8-bit TrueColor, search for a
    * special colormap initialized for dithering.  Mesa will know how to
    * dither using this colormap.
    */
   if (using_mesa) {
      hp_cr_maps = XInternAtom( dpy, "_HP_RGB_SMOOTH_MAP_LIST", True );
      if (hp_cr_maps
	  && visinfo->visual->class==TrueColor
	  && visinfo->depth==8) {
	 status = XGetRGBColormaps( dpy, root, &standardCmaps,
				    &numCmaps, hp_cr_maps );
	 if (status) {
	    for (i=0; i<numCmaps; i++) {
	       if (standardCmaps[i].visualid == visinfo->visual->visualid) {
                  Colormap cmap = standardCmaps[i].colormap;
                  XFree( standardCmaps );
		  return cmap;
	       }
	    }
            XFree(standardCmaps);
	 }
      }
   }

   /*
    * Next, try to find a standard X colormap.
    */
#ifndef SOLARIS_BUG
   status = XmuLookupStandardColormap( dpy, visinfo->screen,
				       visinfo->visualid, visinfo->depth,
				       XA_RGB_DEFAULT_MAP,
				       /* replace */ False, /* retain */ True);
   if (status == 1) {
      status = XGetRGBColormaps( dpy, root, &standardCmaps,
				 &numCmaps, XA_RGB_DEFAULT_MAP);
      if (status == 1) {
         for (i = 0; i < numCmaps; i++) {
	    if (standardCmaps[i].visualid == visinfo->visualid) {
               Colormap cmap = standardCmaps[i].colormap;
	       XFree(standardCmaps);
	       return cmap;
	    }
	 }
         XFree(standardCmaps);
      }
   }
#endif

   /*
    * If we get here, give up and just allocate a new colormap.
    */
   return XCreateColormap( dpy, root, visinfo->visual, AllocNone );
}




/*
 * Togl_Init
 *
 *   Called upon system startup to create Togl command.
 */
int Togl_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "togl", Togl_Cmd, Tk_MainWindow(interp), NULL);
    Tcl_InitHashTable(&command_table, TCL_STRING_KEYS);

    return TCL_OK;
}


/*
 * Register a C function to be called when an Togl widget is realized.
 */
void Togl_CreateFunc( Togl_Callback *proc )
{
   create_proc = proc;
}


/*
 * Register a C function to be called when an Togl widget must be redrawn.
 */
void Togl_DisplayFunc( Togl_Callback *proc )
{
   render_proc = proc;
}


/*
 * Register a C function to be called when an Togl widget is resized.
 */
void Togl_ReshapeFunc( Togl_Callback *proc )
{
   resize_proc = proc;
}


/*
 * Togl_CreateCommand
 *
 *   Declares a new C sub-command of Togl callable from Tcl.
 *   Every time the sub-command is called from Tcl, the
 *   C routine will be called with all the arguments from Tcl.
 */
void Togl_CreateCommand(char *cmd_name, Togl_CmdProc *cmd_proc)
{
    int new_item;
    Tcl_HashEntry *entry;

    entry = Tcl_CreateHashEntry(&command_table, cmd_name, &new_item);
    Tcl_SetHashValue(entry, cmd_proc);
}



/*
 * Togl_MakeCurrent
 *
 *   Bind the OpenGL rendering context to the specified
 *   Togl widget.
 */
void Togl_MakeCurrent(struct Togl *togl)
{
   glXMakeCurrent(Tk_Display(togl->tkwin), Tk_WindowId(togl->tkwin), togl->cx);
}



/*
 * Called when the widget's contents must be redrawn.  Basically, we
 * just call the user's render callback function.
 *
 * Note that the parameter type is ClientData so this function can be
 * passed to Tcl_DoWhenIdle().
 */
static void Togl_Render( ClientData clientData )
{
   struct Togl *togl = (struct Togl *)clientData;

   if (togl->render_proc) {
      Togl_MakeCurrent(togl);
      togl->render_proc(togl);
   }
   togl->update_pending = GL_FALSE;
}



static int Togl_Configure(Tcl_Interp *interp, struct Togl *togl, 
			 int argc, char *argv[], int flags) 
{
   int old_double_buffer;

    old_double_buffer = togl->double_flag;
    if (Tk_ConfigureWidget(interp, togl->tkwin, configSpecs,
			   argc, argv, (char *)togl, flags) == TCL_ERROR) 
      {
	  return(TCL_ERROR);
      }

    Tk_GeometryRequest(togl->tkwin, togl->width, togl->height);

    if (togl->double_flag != old_double_buffer) {
       if (Togl_MakeWindowExist(togl)==TCL_ERROR) {
          return TCL_ERROR;
       }
    }

    return(TCL_OK);
}


int Togl_Widget(ClientData clientData, Tcl_Interp *interp,
	       int argc, char *argv[]) 
{
    struct Togl *togl = (struct Togl *)clientData;
    int result = TCL_OK;
    Tcl_HashEntry *entry;
    Tcl_HashSearch search;
    Togl_CmdProc *cmd_proc;

    if (argc < 2) 
      {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " ?options?\"", NULL);
	return TCL_ERROR;
      }

    Tcl_Preserve((ClientData)togl);

   if (!strncmp(argv[1], "configure", MAX(1, strlen(argv[1])))) {
      if (argc == 2) {
         /* Return list of all configuration parameters */
         result = Tk_ConfigureInfo(interp, togl->tkwin, configSpecs,
                                   (char *)togl, (char *)NULL, 0);
      }
      else if (argc == 3) {
         if (strcmp(argv[2],"-extensions")==0) {
            /* Return a list of OpenGL extensions available */
            char *extensions;
            extensions = (char *) glGetString(GL_EXTENSIONS);
            Tcl_SetResult( interp, extensions, TCL_STATIC );
            result = TCL_OK;
         }
         else {
            /* Return a specific configuration parameter */
            result = Tk_ConfigureInfo(interp, togl->tkwin, configSpecs,
                                      (char *)togl, argv[2], 0);
         }
      }
      else {
         /* Execute a configuration change */
         result = Togl_Configure(interp, togl, argc-2, argv+2, 
                                TK_CONFIG_ARGV_ONLY);
      }
   }
   else if (!strncmp(argv[1], "render", MAX(1, strlen(argv[1])))) {
      /* force the widget to be redrawn */
      Togl_Render(togl);
   }
   else {
      /* Probably a user-defined function */
	  entry = Tcl_FindHashEntry(&command_table, argv[1]);
	  if (entry != NULL) {
	      cmd_proc = (Togl_CmdProc *)Tcl_GetHashValue(entry);
	      result = cmd_proc(togl, argc, argv);
	  } else {
	      Tcl_AppendResult(interp, "Togl: Unknown option: ", argv[1], "\n", 
			       "Try: configure or render\n",
			       "or one of the user-defined commands:\n",
			       NULL);
	      entry = Tcl_FirstHashEntry(&command_table, &search);
	      while (entry) {
		  Tcl_AppendResult(interp, "  ", Tcl_GetHashKey(&command_table, entry), "\n", NULL);
		  entry = Tcl_NextHashEntry(&search);
	      }
	      result = TCL_ERROR;
	  }
      }

    Tcl_Release((ClientData)togl);
    return result;
}



/*
 * Togl_Cmd
 *
 *   Called when Togl is executed - creation of an Togl.
 *     * Creates a new window
 *     * Creates an 'Togl' data structure
 *     * Creates an event handler for this window
 *     * Creates a command that handles this object
 *     * Configures this Togl for the given arguments
 */
static int Togl_Cmd(ClientData clientData, Tcl_Interp *interp, 
		   int argc, char **argv)
{
    char *name;
    Tk_Window main = (Tk_Window)clientData;
    Tk_Window tkwin;
    struct Togl *togl;
    
    if (argc <= 1) {
	TCL_ERR(interp, "wrong # args: should be \"pathName read filename\"");
    }

				/* Create the window. */
    name = argv[1];
    tkwin = Tk_CreateWindowFromPath(interp, main, name, (char *) NULL);
    if (tkwin == NULL) {
	return TCL_ERROR;
    }
    
    Tk_SetClass(tkwin, "Togl");

				/* Create Togl data structure */
    togl = (struct Togl *)malloc(sizeof(struct Togl));
    togl->tkwin = tkwin;
    togl->interp = interp;
    togl->cx = NULL;
    togl->width = 0;
    togl->height = 0;
    togl->rgba_flag = 1;
    togl->double_flag = 0;
    togl->depth_flag = 0;
    togl->accum_flag = 0;
    togl->alpha_flag = 0;
    togl->stencil_flag = 0;
    togl->update_pending = GL_FALSE;
    togl->create_proc = create_proc;
    togl->render_proc = render_proc;
    togl->resize_proc = resize_proc;
    togl->ident = NULL;  /* Per Benjamin on March 6, 1996 */

				/* Create command event handler */
    Tcl_CreateCommand(interp, Tk_PathName(tkwin), Togl_Widget, 
		      (ClientData)togl, (void (*)(ClientData)) NULL);
    Tk_CreateEventHandler(tkwin, 
			  ExposureMask | StructureNotifyMask,
			  Togl_EventProc, 
			  (ClientData)togl);
    
				/* Configure Togl widget */
    if (Togl_Configure(interp, togl, argc-2, argv+2, 0) == TCL_ERROR) {
	Tk_DestroyWindow(tkwin);
        goto error;
    }

				/* Force creation of window now to */
				/* be sure that we make it an OpenGL window */
    if (Togl_MakeWindowExist(togl) == TCL_ERROR) {
      goto error;
    }
      
				/* If defined, call create callback */
    if (togl->create_proc) {
	togl->create_proc(togl);
    }
    /*NEW*/
    if (togl->resize_proc) {
       togl->resize_proc(togl);
    }

    Tcl_AppendResult(interp, Tk_PathName(tkwin), NULL);
    return TCL_OK;

  error:
    Tcl_DeleteCommand(interp, "togl");
    /*free(togl);   Don't free it, if we do a crash occurs later...*/
    return TCL_ERROR;
}



/*
 * Togl_MakeWindowExist
 *
 *   Modified version of Tk_MakeWindowExist.
 *   Creates an OpenGL window for the Togl widget.
 */
static int Togl_MakeWindowExist(struct Togl *togl) 
{
    XVisualInfo    *visual;
    Display        *dpy;
    int		    dummy;
    int attrib_list[1000];
    int attrib_count;
    TkWindow *winPtr = (TkWindow *) togl->tkwin;
    TkWindow *winPtr2;
    Window parent;
    Colormap cmap;
    XSetWindowAttributes swa;
    Tcl_HashEntry *hPtr;
    int new_flag;
    int scrnum;
    int attempt;

#define MAX_ATTEMPTS 12
    static int ci_depths[MAX_ATTEMPTS]
      = { 8, 4, 2, 1, 12, 16, 8, 4, 2, 1, 12, 16 };
    static int dbl_flags[MAX_ATTEMPTS]
      = { 0, 0, 0, 0,  0,  0, 1, 1, 1, 1,  1,  1 };


    dpy = Tk_Display(togl->tkwin);

    if (winPtr->window != None) {
	XDestroyWindow(dpy, winPtr->window);
    }

    /* Make sure OpenGL's GLX extension supported */
    if (!glXQueryExtension(dpy, &dummy, &dummy)) {
      TCL_ERR(togl->interp, "Togl: X server has no OpenGL GLX extension");
    }

    /* It may take a few tries to get a visual */
    for (attempt=0; attempt<MAX_ATTEMPTS; attempt++) {
       attrib_count = 0;
       attrib_list[attrib_count++] = GLX_USE_GL;
       if (togl->rgba_flag) {
          attrib_list[attrib_count++] = GLX_RGBA;
          attrib_list[attrib_count++] = GLX_RED_SIZE;
          attrib_list[attrib_count++] = 1;
          attrib_list[attrib_count++] = GLX_GREEN_SIZE;
          attrib_list[attrib_count++] = 1;
          attrib_list[attrib_count++] = GLX_BLUE_SIZE;
          attrib_list[attrib_count++] = 1;
          if (togl->alpha_flag) {
             attrib_list[attrib_count++] = GLX_ALPHA_SIZE;
             attrib_list[attrib_count++] = 1;
          }
       }
       else {
          int depth;
          attrib_list[attrib_count++] = GLX_BUFFER_SIZE;
          depth = ci_depths[attempt];
          attrib_list[attrib_count++] = depth;
       }
       if (togl->depth_flag) {
          attrib_list[attrib_count++] = GLX_DEPTH_SIZE;
          attrib_list[attrib_count++] = 1;
       }
       if (togl->double_flag || dbl_flags[attempt]) {
          attrib_list[attrib_count++] = GLX_DOUBLEBUFFER;
       }
       if (togl->stencil_flag) {
          attrib_list[attrib_count++] = GLX_STENCIL_SIZE;
          attrib_list[attrib_count++] = 1;
       }
       if (togl->accum_flag) {
          attrib_list[attrib_count++] = GLX_ACCUM_RED_SIZE;
          attrib_list[attrib_count++] = 1;
          attrib_list[attrib_count++] = GLX_ACCUM_GREEN_SIZE;
          attrib_list[attrib_count++] = 1;
          attrib_list[attrib_count++] = GLX_ACCUM_BLUE_SIZE;
          attrib_list[attrib_count++] = 1;
          if (togl->alpha_flag) {
             attrib_list[attrib_count++] = GLX_ACCUM_ALPHA_SIZE;
             attrib_list[attrib_count++] = 1;
          }
       }
       attrib_list[attrib_count++] = None;

       visual = glXChooseVisual( dpy, DefaultScreen(dpy), attrib_list );
       if (visual) {
          /* found a GLX visual! */
          break;
       }
    }
    if (visual==NULL) {
       TCL_ERR(togl->interp, "Togl: couldn't get visual");
    }


    /* Create an OpenGL rendering context */
    /* No sharing of display lists */
    /* Direct rendering if possible */
    togl->cx = glXCreateContext(dpy, visual, None, GL_TRUE);
    if (togl->cx == NULL) {
      TCL_ERR(togl->interp, "could not create rendering context");
    }

				/* Find parent of window */
				/* Necessary for creation */
    if ((winPtr->parentPtr == NULL) || (winPtr->flags & TK_TOP_LEVEL)) {
	parent = XRootWindow(winPtr->display, winPtr->screenNum);
    } else {
	if (winPtr->parentPtr->window == None) {
           Tk_MakeWindowExist((Tk_Window) winPtr->parentPtr);
	}
	parent = winPtr->parentPtr->window;
    }

    /*
     * find a colormap
     */
    scrnum = DefaultScreen(dpy);
    if (togl->rgba_flag) {
       /* Colormap for RGB mode */
       cmap = get_rgb_colormap( dpy, scrnum, visual );
    }
    else {
       /* Colormap for CI mode */
       if (togl->privatecmap_flag) {
          /* need read/write colormap so user can store own color entries */
          cmap = XCreateColormap(dpy, RootWindow(dpy, visual->screen), 
                                 visual->visual, AllocAll);
       }
       else {
          if (visual->visual==DefaultVisual(dpy, scrnum)) {
             /* share default/root colormap */
             cmap = DefaultColormap(dpy,scrnum);
          }
          else {
             /* make a new read-only colormap */
             cmap = XCreateColormap(dpy, RootWindow(dpy, visual->screen), 
                                    visual->visual, AllocNone);
          }
       }
    }

				/* Make sure Tk knows to switch to the new
				   colormap when the cursor is over this window
				   when running in color index mode. */
    Tk_SetWindowVisual(togl->tkwin, visual->visual, 8, cmap);

    swa.colormap = cmap;
    swa.border_pixel = 0;
    swa.event_mask = ALL_EVENTS_MASK;
    winPtr->window = XCreateWindow(dpy, parent,
				   0, 0, togl->width, togl->height,
				   0, visual->depth,
				   InputOutput, visual->visual, 
				   CWBorderPixel | CWColormap | CWEventMask, 
				   &swa);

    hPtr = Tcl_CreateHashEntry(&winPtr->dispPtr->winTable,
	    (char *) winPtr->window, &new_flag);
    Tcl_SetHashValue(hPtr, winPtr);
 
    winPtr->dirtyAtts = 0;
    winPtr->dirtyChanges = 0;
#ifdef TK_USE_INPUT_METHODS
    winPtr->inputContext = NULL;
#endif /* TK_USE_INPUT_METHODS */

    if (!(winPtr->flags & TK_TOP_LEVEL)) {
	/*
	 * If any siblings higher up in the stacking order have already
	 * been created then move this window to its rightful position
	 * in the stacking order.
	 *
	 * NOTE: this code ignores any changes anyone might have made
	 * to the sibling and stack_mode field of the window's attributes,
	 * so it really isn't safe for these to be manipulated except
	 * by calling Tk_RestackWindow.
	 */

	for (winPtr2 = winPtr->nextPtr; winPtr2 != NULL;
		winPtr2 = winPtr2->nextPtr) {
	    if ((winPtr2->window != None) && !(winPtr2->flags & TK_TOP_LEVEL)) {
		XWindowChanges changes;
		changes.sibling = winPtr2->window;
		changes.stack_mode = Below;
		XConfigureWindow(winPtr->display, winPtr->window,
			CWSibling|CWStackMode, &changes);
		break;
	    }
	}

	/*
	 * If this window has a different colormap than its parent, add
	 * the window to the WM_COLORMAP_WINDOWS property for its top-level.
	 */

	if ((winPtr->parentPtr != NULL) &&
		(winPtr->atts.colormap != winPtr->parentPtr->atts.colormap)) {
	    TkWmAddToColormapWindows(winPtr);
	}
    }
    /*
     * Issue a ConfigureNotify event if there were deferred configuration
     * changes (but skip it if the window is being deleted;  the
     * ConfigureNotify event could cause problems if we're being called
     * from Tk_DestroyWindow under some conditions).
     */

    if ((winPtr->flags & TK_NEED_CONFIG_NOTIFY)
	    && !(winPtr->flags & TK_ALREADY_DEAD)){
	XEvent event;

	winPtr->flags &= ~TK_NEED_CONFIG_NOTIFY;

	event.type = ConfigureNotify;
	event.xconfigure.serial = LastKnownRequestProcessed(winPtr->display);
	event.xconfigure.send_event = False;
	event.xconfigure.display = winPtr->display;
	event.xconfigure.event = winPtr->window;
	event.xconfigure.window = winPtr->window;
	event.xconfigure.x = winPtr->changes.x;
	event.xconfigure.y = winPtr->changes.y;
	event.xconfigure.width = winPtr->changes.width;
	event.xconfigure.height = winPtr->changes.height;
	event.xconfigure.border_width = winPtr->changes.border_width;
	if (winPtr->changes.stack_mode == Above) {
	    event.xconfigure.above = winPtr->changes.sibling;
	} else {
	    event.xconfigure.above = None;
	}
	event.xconfigure.override_redirect = winPtr->atts.override_redirect;
	Tk_HandleEvent(&event);
    }

				/* Request the X window to be displayed */
    XMapWindow(dpy, Tk_WindowId(togl->tkwin));

    /* Bind the context to the window and make it the current context. */
    Togl_MakeCurrent(togl);

    /* Check for a single/double buffering snafu */
    {
       int dbl_flag;
       if (glXGetConfig( dpy, visual, GLX_DOUBLEBUFFER, &dbl_flag )) {
          if (togl->double_flag==0 && dbl_flag) {
             /* We requested single buffering but had to accept a */
             /* double buffered visual.  Set the GL draw buffer to */
             /* be the front buffer to simulate single buffering. */
             glDrawBuffer( GL_FRONT );
          }
       }
    }

    return TCL_OK;
}




/*
 * Togl_Destroy
 *
 * Gets called when an Togl widget is destroyed.
 */
static void Togl_Destroy( char *clientData )
{
    struct Togl *togl = (struct Togl *)clientData;
    
    Tk_FreeOptions(configSpecs, (char *)togl, Tk_Display(togl->tkwin), 0);
    
    free(togl);
}



/*		
 * This gets called to handle Togl window configuration events
 */
static void Togl_EventProc(ClientData clientData, XEvent *eventPtr) 
{
    struct Togl *togl = (struct Togl *)clientData;

    switch (eventPtr->type) 
      {
	case Expose:
	  if ((eventPtr->xexpose.count == 0) && !togl->update_pending) 
	    {
		Togl_Render(togl);
	    }
	  break;
	case ConfigureNotify:
	  if ( 1 /*Tk_IsMapped(togl->tkwin)*/ ) {
	      togl->width = Tk_Width(togl->tkwin);
	      togl->height = Tk_Height(togl->tkwin);
	      XResizeWindow(Tk_Display(togl->tkwin), Tk_WindowId(togl->tkwin), 
			    togl->width, togl->height);
	      Togl_MakeCurrent(togl);
	      if (togl->resize_proc) {
		  togl->resize_proc(togl);
	      } else {
		  glViewport(0, 0, togl->width, togl->height);
	      }
              Togl_PostRedisplay(togl);
	  }
	  break;
	case MapNotify:
	  break;
	case DestroyNotify:
          Tcl_EventuallyFree( (ClientData) togl, (Tcl_FreeProc *)Togl_Destroy );
	  break;
      }
}




void Togl_SwapBuffers( struct Togl *togl )
{
   if (togl->double_flag) {
      glXSwapBuffers( Tk_Display(togl->tkwin), Tk_WindowId(togl->tkwin) );
   }
   else {
      glFlush();
   }
}



void Togl_PostRedisplay( struct Togl *togl )
{
   if (!togl->update_pending) {
      Tcl_DoWhenIdle( Togl_Render, togl );
      togl->update_pending = GL_TRUE;
   }
}



char *Togl_Ident( struct Togl *togl )
{
   return togl->ident;
}


int Togl_Width( struct Togl *togl )
{
   return togl->width;
}


int Togl_Height( struct Togl *togl )
{
   return togl->height;
}


Tcl_Interp *Togl_Interp( struct Togl *togl )
{
   return togl->interp;
}



/*
 * A replacement for XAllocColor.  This function should never
 * fail to allocate a color.  When XAllocColor fails, we return
 * the nearest matching color.  If we have to allocate many colors
 * this function isn't too efficient; the XQueryColors() could be
 * done just once.
 * Written by Michael Pichler, Brian Paul, Mark Kilgard
 * Input:  dpy - X display
 *         cmap - X colormap
 *         cmapSize - size of colormap
 * In/Out: color - the XColor struct
 * Output:  exact - 1=exact color match, 0=closest match
 */
static void
noFaultXAllocColor( Display *dpy, Colormap cmap, int cmapSize,
                    XColor *color, int *exact )
{
  XColor *ctable, subColor;
  int i, bestmatch;
  double mindist;       /* 3*2^16^2 exceeds long int precision. 
                         */

  for (;;) {
    /* First try just using XAllocColor. */
    if (XAllocColor(dpy, cmap, color)) {
       *exact = 1;
       return;
    }

    /* Retrieve color table entries. */
    /* XXX alloca candidate. */
    ctable = (XColor *) malloc(cmapSize * sizeof(XColor));
    for (i = 0; i < cmapSize; i++)
      ctable[i].pixel = i;
    XQueryColors(dpy, cmap, ctable, cmapSize);

    /* Find best match. */
    bestmatch = -1;
    mindist = 0.0;
    for (i = 0; i < cmapSize; i++) {
      double dr = (double) color->red - (double) ctable[i].red;
      double dg = (double) color->green - (double) ctable[i].green;
      double db = (double) color->blue - (double) ctable[i].blue;
      double dist = dr * dr + dg * dg + db * db;
      if (bestmatch < 0 || dist < mindist) {
        bestmatch = i;
        mindist = dist;
      }
    }

    /* Return result. */
    subColor.red = ctable[bestmatch].red;
    subColor.green = ctable[bestmatch].green;
    subColor.blue = ctable[bestmatch].blue;
    free(ctable);
    if (XAllocColor(dpy, cmap, &subColor)) {
      *color = subColor;
      *exact = 0;
      return;
    }
    /* Extremely unlikely, but possibly color was deallocated
       and reallocated by someone else before we could
       XAllocColor the color cell we located.  If so, loop
       again... */
  }
}



unsigned long Togl_AllocColor( struct Togl *togl,
                              float red, float green, float blue )
{
   XColor xcol;
   int exact;

   if (togl->rgba_flag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return 0;
   }
   /* TODO: maybe not... */
   if (togl->privatecmap_flag) {
      fprintf(stderr,"Error: Togl_FreeColor illegal with private colormap\n");
      return 0;
   }

   xcol.red   = (short) (red   * 65535.0);
   xcol.green = (short) (green * 65535.0);
   xcol.blue  = (short) (blue  * 65535.0);

   noFaultXAllocColor( Tk_Display(togl->tkwin), Tk_Colormap(togl->tkwin),
                       Tk_Visual(togl->tkwin)->map_entries, &xcol, &exact );

   return xcol.pixel;
}



void Togl_FreeColor( struct Togl *togl, unsigned long pixel )
{
   if (togl->rgba_flag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return;
   }
   /* TODO: maybe not... */
   if (togl->privatecmap_flag) {
      fprintf(stderr,"Error: Togl_FreeColor illegal with private colormap\n");
      return;
   }

   XFreeColors( Tk_Display(togl->tkwin), Tk_Colormap(togl->tkwin),
                &pixel, 1, 0 );
}



void Togl_SetColor( struct Togl *togl,
                   unsigned long index, float red, float green, float blue )
{
   XColor xcol;

   if (togl->rgba_flag) {
      fprintf(stderr,"Error: Togl_AllocColor illegal in RGBA mode.\n");
      return;
   }
   if (!togl->privatecmap_flag) {
      fprintf(stderr,"Error: Togl_SetColor requires a private colormap\n");
      return;
   }

   xcol.pixel = index;
   xcol.red   = (short) (red   * 65535.0);
   xcol.green = (short) (green * 65535.0);
   xcol.blue  = (short) (blue  * 65535.0);
   xcol.flags = DoRed | DoGreen | DoBlue;

   XStoreColor( Tk_Display(togl->tkwin), Tk_Colormap(togl->tkwin), &xcol );
}



