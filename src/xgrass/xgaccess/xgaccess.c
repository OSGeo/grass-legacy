static char rcsid[] = "@(#)XGRASS $Id: xgaccess.c,v 0.0.0.1 1992/05/05 14:59:18 kurt Exp kurt $";
/*
 * File: xgaccess.c
 *
 * Desc: Top level program for controlling mapset acces
 *
 * Auth: Eric W. Sink
 *
 * Date: 1 Feb 1992
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"
#include <X11/Xresource.h>
#include <Xm/TextF.h>
#include "Interact.h"
#include "Region.h"
#ifndef mips
#include "unistd.h"
#include "stdlib.h"
#include "sys/stat.h"
#endif
#include "sys/types.h"
#include "gis.h"

Display        *display;
Widget          mainshell;
Widget          xgi;
Widget          theToggles, groupToggle, otherToggle;
Widget          theForm;
Widget          theFrame;
Widget          theLabel;
int theGroup;
int theOther;
char mapsetPath[512];
int mapsetPerms;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Access"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

XtAppContext    appContext;

static void     GroupToggle();
static void     OtherToggle();

Widget          locDialog;
Widget          LocSetupToggles[16];
int             CountLocSetupToggles;
char           *G__projection_name();

static void
#ifdef _NO_PROTO
XgaccessOk(w,data)
Widget w;
XtPointer data;
#else
XgaccessOk(Widget w, XtPointer data)
#endif
{
  char buf[512];
  set_perms(mapsetPath,mapsetPerms,theGroup,theOther);
      if (theGroup) {
	if (theOther) {
	  sprintf(buf,"xgaccess +group +other");
	}
	else {
	  sprintf(buf,"xgaccess +group -other");
	}
      }
      else {
	if (theOther) {
	  sprintf(buf,"xgaccess -group +other");
	}
	else {
	  sprintf(buf,"xgaccess -group -other");
	}
      }
  XgSetCommandString(display,XgGetMenuWindow(display),buf);
  XFlush(display);
  exit(0);
}

static void
#ifdef _NO_PROTO
XgaccessCancel(w,data)
Widget w;
XtPointer data;
#else
XgaccessCancel(Widget w, XtPointer data)
#endif
{
  XFlush(display);
  exit(1);
}

static void
#ifdef _NO_PROTO
GroupToggle(w, data)
    Widget          w;
    XtPointer       data;
#else
GroupToggle(Widget w, XtPointer data)
#endif
{
    Widget          xgi = (Widget) data;
    theGroup = !theGroup;
}

static void
#ifdef _NO_PROTO
OtherToggle(w, data)
    Widget          w;
    XtPointer       data;
#else
OtherToggle(Widget w, XtPointer data)
#endif
{
    Widget          xgi = (Widget) data;
    theOther = !theOther;
}

static int
#ifdef _NO_PROTO
CreateLayout(w)
    Widget          w;
#else
CreateLayout(Widget w)
#endif
{
    Arg             al[16];
    int             ac = 0;
    XmString mesg;
    char mesg2[256];

    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    xgi = XgCreateInteractor(w, "Set Current Mapset Permissions", al, ac);
    XtManageChild(xgi);

    XtAddCallback(xgi, XmNokCallback, XgaccessOk, xgi);
    XtAddCallback(xgi, XmNcancelCallback, XgaccessCancel, xgi);

    theForm = XtVaCreateManagedWidget("xgaccess_form", xmFormWidgetClass, xgi,
                                      NULL);
    mesg = (XmString)XgCreateXmStringFromFile("xgaccess_mesg");
    sprintf(mesg2,"\nYour current mapset is %s.",G_mapset());
    mesg = XmStringConcat(mesg,XmStringCreateLtoR(mesg2,XmSTRING_DEFAULT_CHARSET));
    
    theLabel = XtVaCreateManagedWidget("xgaccess_label", xmLabelWidgetClass,theForm,
				       XmNtraversalOn, False,
				       XmNlabelString,mesg,
                                       XmNtopAttachment, XmATTACH_FORM,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       NULL);
    theFrame = XtVaCreateManagedWidget("xgaccess_frame", xmFrameWidgetClass, theForm,
                                       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget,theLabel,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNbottomAttachment, XmATTACH_FORM,
                                       NULL);

    theToggles = XtVaCreateManagedWidget("xgaccess_toggles", xmRowColumnWidgetClass, theFrame,
                                        NULL);

    groupToggle = XtVaCreateManagedWidget("xgacc_g_toggle", xmToggleButtonWidgetClass, theToggles,
                           XmNlabelString, XmStringCreateSimple("Access allowed for group members"),
                                   XmNset, theGroup,
                                             NULL);
    XgAddHelpCallBackFromFile(groupToggle, "xgacc_g_toggle");
    XtAddCallback(groupToggle, XmNvalueChangedCallback, GroupToggle, xgi);

    otherToggle = XtVaCreateManagedWidget("xgacc_o_toggle", xmToggleButtonWidgetClass, theToggles,
                           XmNlabelString, XmStringCreateSimple("Access allowed for others"),
                                   XmNset, theOther,
                                             NULL);
    XtAddCallback(otherToggle, XmNvalueChangedCallback, OtherToggle, xgi);
    XgAddHelpCallBackFromFile(otherToggle, "xgacc_o_toggle");

}

static XrmOptionDescRec options[] = {
{"-title", "*title", XrmoptionSepArg, (caddr_t)"Set Current Mapset Permissions"},
};

int
#ifdef _NO_PROTO
main(argc, argv)
    unsigned int    argc;
    char          **argv;
#else
main(unsigned int argc, char **argv)
#endif
{
    Widget          shell;
    Arg             al[16];
    int             ac;
    Boolean bong;

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    G_gisinit(argv[0]);
    shell = mainshell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    strcpy(mapsetPath,G_location_path());
    strcat(mapsetPath,"/");
    strcat(mapsetPath,G_mapset());

    get_perms(mapsetPath,&mapsetPerms,&theGroup,&theOther);

    bong = False;
    if (argc == 3 ) {
      if (!strcmp(argv[1],"-group")) {
	theGroup = False;
	if (!strcmp(argv[2],"-other")) {
	  theOther = False;
	}
	else if (!strcmp(argv[2],"+other")) {
	  theOther = True;
	}
	else {
	  bong = True;
	}
      }
      else if (!strcmp(argv[1],"+group")) {
	theGroup = True;
	if (!strcmp(argv[2],"-other")) {
	  theOther = False;
	}
	else if (!strcmp(argv[2],"+other")) {
	  theOther = True;
	}
	else {
	  bong = True;
	}
      }
      else {
	bong = True;
      }
    }
    else {
	bong = True;
    };
    if (bong) {
	CreateLayout(mainshell);

	XtRealizeWidget(shell);
	XtAppMainLoop(appContext);
    }
    else {
      char buf[512];
      set_perms(mapsetPath,mapsetPerms,theGroup,theOther);
      if (theGroup) {
	if (theOther) {
	  sprintf(buf,"xgaccess +group +other");
	}
	else {
	  sprintf(buf,"xgaccess +group -other");
	}
      }
      else {
	if (theOther) {
	  sprintf(buf,"xgaccess -group +other");
	}
	else {
	  sprintf(buf,"xgaccess -group -other");
	}
      }
      XgSetCommandString(display,XgGetMenuWindow(display),buf);
    }

    return 0;
}

#include "access.h"

get_perms (path, perms, group, other)
    char *path;
    int *perms;
    int *group;
    int *other;
{
    struct stat buf;

    if (stat (path, &buf) != 0)
	return -1;
    
    *perms = buf.st_mode;
    *group = (*perms & GROUP_PERMS) ? 1 : 0 ;
    *other = (*perms & OTHER_PERMS) ? 1 : 0 ;
    return 0;
}
	
set_perms (path, perms, group, other)
    char *path;
{
    char *explain_perms();

    perms |= OWNER_PERMS;

    perms &= ~GROUP_BITS;
    perms &= ~OTHER_BITS;

    if (group)
	perms |= GROUP_PERMS;
    if (other)
	perms |= OTHER_PERMS;

#ifdef Undefined
    if(chmod (path, perms) == 0)
	printf ("%s\n", explain_perms (group, other, 0));
    else
	G_fatal_error ("unable to change mapset permissions");
#else
	chmod(path,perms);
#endif
}

char *
explain_perms (group, other, will)
{
    static char buf[128];
    char *who;
    char *verb;
    char *read;

    verb="have";
    read="read ";
    read="";	/* remove this to have "read" appear */
    if (group && other)
    {
	who  = "Everyone";
	verb = "has";
    }
    else if (group)
    {
	who = "Only users in your group";
    }
    else if (other)
    {
	who = "Only users outside your group";
    }
    else
    {
	who  = "Only you";
	read = "";
    }
    if (will) verb = "have";

    sprintf (buf, "%s %s %s %saccess to mapset %s",
	who, will?"will":"now", verb, read, G_mapset());
    return buf;
}
