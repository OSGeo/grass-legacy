#include <X11/cursorfont.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/Text.h>

#include "InteractP.h"

static void _XgFontListOkCB();
static Widget _xg_d;

Widget
#ifdef _NO_PROTO
XgSetFontList(parent, child)
     Widget parent, child;
#else
XgSetFontList(Widget parent, Widget child);
#endif
{
  _xg_d = XgCreateInteractorFontListDialog(parent, "fontlist", NULL, 0);

  XtAddCallback(_xg_d, XmNokCallback, _XgFontListOkCB, child);

  return(_xg_d);
}

static void
#ifdef _NO_PROTO
_XgFontListOkCB(w, cli, call)
     Widget w;
     Widget cli;
     caddr_t call;
#else
_XgFontListOkCB(Widget w, Widget cli, caddr_t call)
#endif
{
  Widget text;
  char   *font;
  ArgList args;
  int     numargs;
  XFontStruct *f;
  
  text = XgInteractorGetChild(_xg_d, XmINTERACT_LIST_TEXT);
  font = XmTextGetString(text);
  f = XLoadQueryFont(XtDisplay(cli), font);
  XtVaSetValues(cli, XmNfontList, XmFontListCreate(f, XmSTRING_DEFAULT_CHARSET),
						      NULL);
}

