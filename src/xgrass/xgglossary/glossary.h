/* $Log:	glossary.h,v $
 * Revision 1.3  92/02/28  13:45:36  jinyuan
 * *** empty log message ***
 * 
 * Revision 1.2  92/02/26  09:19:23  jinyuan
 * *** empty log message ***
 * 
 * Revision 1.1  92/02/24  09:24:39  jinyuan
 * Initial revision
 *  */

#include <stdio.h>
#include <xgrass_lib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <Xm/MainW.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>
#include <Xm/ScrolledW.h>
#include <Xm/Frame.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/ToggleB.h>
#include <Xm/Text.h>
#include <Xm/SelectioB.h>
#include <Xm/List.h>
#include <Interact.h>

#include "Help.h"

#define MAXCHAR 300

typedef struct _ListItem {
    char term[MAXCHAR];
    char where[MAXCHAR];
} ListItem;


#ifdef MAIN
   XtAppContext				appContext;
   Widget					top;
   Display					*dpy;
#else
   extern XtAppContext	appContext;
   extern Widget			top;
   extern Display			*dpy;
#endif
