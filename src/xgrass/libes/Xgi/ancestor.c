#include <X11/Intrinsic.h>

Widget 
_XgFindRootAncestor(w)
Widget w;
{
    int foundOne = 0;
    while (w) {
      Widget parent = XtParent(w);
      if (!parent) return w;
      if (XtIsShell(w))  {
	if (foundOne) return w;
	else foundOne = 1;
      }
      w = parent;
    }
    return w;
}
