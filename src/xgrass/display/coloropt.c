#include "xgdisp.h"

typedef struct _colorOptMenuData {
    Widget optMenu;
    Widget *widgets;
    struct _colorOptMenuData *next;
} ColorOptMenuRec;

static ColorOptMenuRec *optMenuList = NULL;

Widget
#ifdef _NO_PROTO
CreateColorOptionMenu(parent, omenu, string, function)
Widget parent;
Widget *omenu;
char *string;
void (*function)();
#else
CreateColorOptionMenu(
Widget parent,
Widget *omenu,
char *string,
void (*function)())
#endif
{
    Widget frame;
    Widget menuPane;
    Arg al[15];
    int ac = 0;
    char **col = XgdGetVectColorNames();
    XmString xms;
    int i;
    static int count = 0;
    char buf[256];
    ColorOptMenuRec *optMenuPtr;
    ColorOptMenuRec *curOptMenuPtr;
    ColorOptMenuRec *optMenuData;

    optMenuData = (ColorOptMenuRec*)XtMalloc(sizeof(ColorOptMenuRec));
    optMenuData->widgets = 
        (Widget *)XtCalloc(Global.numVectColors,sizeof(Widget));

    if ( optMenuList == NULL ) {
	optMenuList = optMenuData;
	curOptMenuPtr = optMenuData;
    } else {
	for ( optMenuPtr = optMenuList; optMenuPtr->next; 
	      optMenuPtr = optMenuPtr->next );
	optMenuPtr->next = optMenuData;
	curOptMenuPtr = optMenuData;
    }

    frame = 
        XtVaCreateManagedWidget("frame", xmFrameWidgetClass, parent, NULL);

    menuPane = (Widget)XmCreatePulldownMenu(frame, "pulldown", NULL, 0);

    sprintf(buf, "option_menu%04d", count++);
    xms = XmStringCreateSimple(string);
    XtSetArg(al[ac], XmNsubMenuId, menuPane); ac++;
    XtSetArg(al[ac], XmNlabelString, xms); ac++;
    curOptMenuPtr->optMenu = XmCreateOptionMenu(frame, buf, al, ac);
    XmStringFree(xms);

    for ( i = 0; i < Global.numVectColors; i++ ) {
       Pixel fgcolor = SelectBestLabelColor(Global.vectColors[i]);

       if ( i == 0 ) {
           XtVaSetValues(curOptMenuPtr->optMenu, 
               XmNbackground, Global.vectColors[i],
               XmNforeground, fgcolor, NULL);
       }
       curOptMenuPtr->widgets[i] = XtVaCreateManagedWidget(col[i],
           xmPushButtonWidgetClass, menuPane,
           XmNbackground, Global.vectColors[i],
           XmNforeground, fgcolor, 
           NULL);
       XtAddCallback(curOptMenuPtr->widgets[i], 
           XmNactivateCallback, UpdateColorOptionMenu, curOptMenuPtr->optMenu);
       XtAddCallback(curOptMenuPtr->widgets[i], 
           XmNactivateCallback, function, (XtPointer)Global.vectColors[i]);
    }
    XtManageChild(curOptMenuPtr->optMenu);
    curOptMenuPtr->next = NULL;
    *omenu = curOptMenuPtr->optMenu;
    return frame;
}


void
#ifdef _NO_PROTO
SetOptMenuToItem(optMenu, item)
Widget optMenu;
char *item;
#else
SetOptMenuToItem(Widget optMenu, char *item)
#endif
{
    char **col = XgdGetVectColorNames();
    int i;
    ColorOptMenuRec *oPtr;

    if ( optMenu == NULL || !XtIsManaged(optMenu) ) return;

    for ( oPtr = optMenuList; oPtr; oPtr = oPtr->next ) {
        if ( oPtr->optMenu == optMenu ) {
	    for ( i = 0; i < Global.numVectColors; i++) {
		if ( !strcmp(col[i], item) ) {
                   XmString xms = XmStringCreateSimple(col[i]);
		   Pixel fgcolor = 
                       SelectBestLabelColor(Global.vectColors[i]);

		    XtVaSetValues(optMenu, 
                        XmNmenuHistory, oPtr->widgets[i], 
                        XmNoptionLabel, xms,
                        XmNbackground, Global.vectColors[i],
                        XmNforeground, fgcolor, NULL);    
                    XmStringFree(xms);
		}
	    }
	} 
    } 
}

Pixel 
#ifdef _NO_PROTO
SelectBestLabelColor(pixel)
Pixel pixel;
#else
SelectBestLabelColor(Pixel pixel)
#endif
{
       XColor query;
       double x;
       Pixel fgcolor;

       query.pixel = pixel;

       XQueryColor(Global.display, Global.cmap, &query);
       x = .299*query.red + .114*query.blue + .587*query.green;
       if ( x < 65536/2 ) {
           fgcolor = WhitePixel(Global.display, Global.screenNo);
       } else {
           fgcolor = BlackPixel(Global.display, Global.screenNo);
       }
       return fgcolor;
}

void
#ifdef _NO_PROTO
UpdateColorOptionMenu(w, cld, cad)
Widget w;
Widget cld;
XtPointer cad;
#else
UpdateColorOptionMenu( Widget w, Widget cld, XtPointer cad)
#endif
{
    XmString xms;
    char *text;

    XtVaGetValues(w, XmNlabelString, &xms, NULL);
    XmStringGetLtoR(xms,XmSTRING_DEFAULT_CHARSET, &text);
    SetOptMenuToItem(cld, text);
}

void
#ifdef _NO_PROTO
SetOptMenuToPixel(optMenu, item)
Widget optMenu;
Pixel  item;
#else
SetOptMenuToPixel(Widget optMenu, Pixel item)
#endif
{
  char **col = XgdGetVectColorNames();
  int i;
  ColorOptMenuRec *oPtr;
  
  if ( optMenu == NULL || !XtIsManaged(optMenu) ) return;
  
  for ( oPtr = optMenuList; oPtr; oPtr = oPtr->next ) {
    if ( oPtr->optMenu == optMenu ) {
      for ( i = 0; i < Global.numVectColors; i++) {
	if ( Global.vectColors[i] == item) {
	  XmString xms = XmStringCreateSimple(col[i]);
	  Pixel fgcolor = SelectBestLabelColor(Global.vectColors[i]);
	  
	  XtVaSetValues(optMenu, 
			XmNmenuHistory, oPtr->widgets[i], 
			XmNoptionLabel, xms,
                        XmNbackground, Global.vectColors[i],
                        XmNforeground, fgcolor, NULL);    
	  XmStringFree(xms);
	}
      }
    } 
  } 
}

