static char rcsid[] = "@(#)XGRASS $Id: prop.c,v 0.0 1992/05/05 14:56:27 sink Exp sink $";
/*
* File: prop.c
*
* Desc: code for setting properties
*
* Auth: Eric W. Sink
*
* Date: Tue Nov  5 16:21:47 CST 1991
*
* Modification History:
*
*
*/

#include "xgrass_lib.h"
#include <pwd.h>
 
Window
#ifdef _NO_PROTO
XgGetMenuWindow(display)
   Display *display;
#else
XgGetMenuWindow(Display *display)
#endif
{
  Atom property;
  Window *prop;
  Atom actual_type;
  Atom type;
  int screen;
  int actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  int err;
  int pid;
  char buf[64];

  pid = getppid();
  sprintf(buf,"XG_PARENT.%d\n",pid);

  property = XInternAtom(display,buf,False);
  type = XInternAtom(display,"window",False);
  screen = DefaultScreen(display);
  err = XGetWindowProperty(display, RootWindow(display,screen), property,
	0L, 8192L, False, type, &actual_type, &actual_format, &nitems,
	&bytes_after, (unsigned char **) &prop);
  if ( prop  && (err == Success)) {
    return (*prop);
  } else {
    return (Window) NULL;
  }
}

char *
#ifdef _NO_PROTO
XgSetCommandString(display,window,s)
Display *display;
Window window;
char *s;
#else
XgSetCommandString(Display *display, Window window, char *s)
#endif
{
  Atom property;

  int err;
  if (!window) return NULL;
  property = XInternAtom(display,"XGRASS_COMMAND",False);
  err = XChangeProperty(display,window,property,XA_STRING,
		  8, PropModeReplace, s, strlen(s)+1);
  if (err != Success) {
    fprintf(stderr,"Failed in XgSetCommandString\n");
  }
}

#ifdef _NO_PROTO
char *
XgGetCommandString(display,window)
Display *display;
Window window;
#else
char *
XgGetCommandString(Display *display,Window window)
#endif
{
  Atom property;
  char *prop;
  Atom actual_type;
  int *actual_format;
  unsigned long nitems;
  unsigned long bytes_after;
  int err;

  if (!window) return NULL;
  property = XInternAtom(display,"XGRASS_COMMAND",False);
  err = XGetWindowProperty(display, window, property,
	0, 8192, False, XA_STRING, &actual_type, &actual_format, &nitems,
	&bytes_after, &prop);
  return prop;
}

