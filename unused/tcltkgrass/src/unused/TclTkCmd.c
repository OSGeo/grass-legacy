#include "tk.h"



/*
   X include files
*/
#include <X11/Xlib.h>


int viewCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char *argv[];
{
   Tk_Window mainWindow;
   Tk_Window newWindow;
   XColor *blackcol;
   XSetWindowAttributes setwinattr;


   if (argc < 2)
   {
       Tcl_AppendResult(interp, "Wrong # args", 0);
       return TCL_ERROR;
   }

   mainWindow = (Tk_Window) clientData;

   newWindow = Tk_CreateWindowFromPath(interp, mainWindow, argv[1], NULL);
   if (newWindow == NULL)
   {
       Tcl_AppendResult(interp, "Could not create new window", 0);
       return TCL_ERROR;
   }

   Tk_SetClass(newWindow, "View");
   blackcol = Tk_GetColor(interp, newWindow, Tk_Colormap(newWindow),
                                             Tk_GetUid("black"));
   Tk_SetWindowBackground(newWindow, blackcol->pixel);   
   Tk_GeometryRequest(newWindow, 350, 350);
   Tk_MakeWindowExist(newWindow);

   /*
      patch for backing storage
   */
   setwinattr.backing_store = Always;
   XChangeWindowAttributes(Tk_Display(newWindow), Tk_WindowId(newWindow),
                                                  CWBackingStore, &setwinattr);

   /*
      let's create a command for this view
   
   Tcl_CreateCommand(interp, argv[1], viewMethods, (ClientData) view, NULL);
   */

   
      let's create an event handler for this view
   
   unsigned long mask = ExposureMask | StructureNotifyMask | SubstructureNotifyMask |
                        ButtonPressMask | ButtonReleaseMask | PointerMotionMask;

   Tk_CreateEventHandler(newWindow, mask, viewEventHandler, (ClientData) view);
   

   Tcl_SetResult(interp, argv[1], TCL_VOLATILE);

   return TCL_OK;
}   




