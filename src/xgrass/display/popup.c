
#include "xgdisp.h"

void
#ifdef _NO_PROTO
RefreshCallBack(w, cld, cad)
    Widget w;
    XtPointer cld, cad;
#else
RefreshCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{
  XClearWindow(Global.display, XtWindow(Global.drawArea));
  DrawObjectsInList(Global.objectList, NULL);
}

void
#ifdef _NO_PROTO
ObjectToBackCallBack(w, cld, cad)
    Widget w;
    XtPointer cld, cad;
#else
ObjectToBackCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{

    if ( Global.selectedObjects == NULL ) return;
    if ( SelectedObjectCount() != 1 ) {
        XgWarningDialog(Global.applShell, "Select one (1) object only.");
        return;
    }
    ObjectToBack(&Global.objectList, Global.selectedObjects->object);
    UnDrawObjectsInList(Global.objectList);
    DrawObjectsInList(Global.objectList, NULL);

}

void
#ifdef _NO_PROTO
ObjectToFrontCallBack(w, cld, cad)
    Widget w;
    XtPointer cld, cad;
#else
ObjectToFrontCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{
  if ( Global.selectedObjects == NULL ) return;
  if ( SelectedObjectCount() != 1 ) {
    XgWarningDialog(Global.applShell, "Select one (1) object only.");
    return;
  }
  ObjectToFront(&Global.objectList,Global.selectedObjects->object);
  UnDrawObjectsInList(Global.objectList);
  DrawObjectsInList(Global.objectList, NULL);
}

void 
#ifdef _NO_PROTO
CreatePopup()
#else
CreatePopup(void)
#endif
{
    Widget widget;

    Global.popup=XmCreatePopupMenu(Global.drawArea,"popup", NULL, 0);

    widget = XtVaCreateManagedWidget("Select Mode", xmPushButtonGadgetClass,
        Global.popup, NULL);
    XtAddCallback(widget, XmNactivateCallback, TAct, XGD_MODE_SELECT);

    widget = XtVaCreateManagedWidget("Front", xmPushButtonGadgetClass,
        Global.popup, NULL);
    XtAddCallback(widget, XmNactivateCallback, ObjectToFrontCallBack, NULL);

    widget = XtVaCreateManagedWidget("Back", xmPushButtonGadgetClass,
        Global.popup, NULL);
    XtAddCallback(widget, XmNactivateCallback, ObjectToBackCallBack, NULL);

    widget = XtVaCreateManagedWidget("Delete", xmPushButtonGadgetClass,
        Global.popup, NULL);
    XtAddCallback(widget, XmNactivateCallback, DeleteObjects, NULL);

    widget = XtVaCreateManagedWidget("Refresh", xmPushButtonGadgetClass,
        Global.popup, NULL);
    XtAddCallback(widget, XmNactivateCallback, RefreshCallBack, NULL);
}
