#include "xgdisp.h"

static XtCallbackRec gapvalueCB[] = {
    {(XtCallbackProc)GridGapTextCallback, (XtPointer) NULL} ,
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec spacevalueCB[] = {
    {(XtCallbackProc)GridSpacingTextCallback, (XtPointer) NULL} ,
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec lwvalueCB[] = {
    {(XtCallbackProc)GridLineWidthTextCallback, (XtPointer) NULL} ,
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec toggleCB[] = {
    {(XtCallbackProc)GridToggleCallBack, (XtPointer)NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec labeltoggleCB[] = {
    {(XtCallbackProc)GridLabelToggleCallBack, (XtPointer)NULL},
    {(XtCallbackProc) NULL, NULL}
};

removeCallbacks()
{
XtRemoveCallbacks(gridgapw, XmNvalueChangedCallback, gapvalueCB);

XtRemoveCallbacks(gridspacew, XmNvalueChangedCallback, spacevalueCB);

XtRemoveCallbacks(gridlww, XmNvalueChangedCallback, lwvalueCB);

XtRemoveCallbacks(gridonw, XmNvalueChangedCallback, toggleCB);

XtRemoveCallbacks(gridlabelonw, XmNvalueChangedCallback, labeltoggleCB);


}

addCallbacks()
{
XtAddCallback(gridgapw, XmNvalueChangedCallback, GridGapTextCallback, (XtPointer)NULL);

XtAddCallback(gridspacew, XmNvalueChangedCallback, GridSpacingTextCallback, (XtPointer)NULL);

XtAddCallback(gridlww, XmNvalueChangedCallback, GridLineWidthTextCallback, (XtPointer)NULL);

XtAddCallback(gridonw, XmNvalueChangedCallback, GridToggleCallBack, (XtPointer)NULL);

XtAddCallback(gridlabelonw, XmNvalueChangedCallback, GridLabelToggleCallBack, (XtPointer)NULL);
}

void 
#ifdef _NO_PROTO
UpdateGridBox()
#else
UpdateGridBox(void)
#endif
{
  char value[30];

  if (Global.selectedObjects != NULL){
    removeCallbacks();
    if ( Global.selectedObjects->object->Obj.GeoFrame.gridOn){
      Global.gridattr = Global.selectedObjects->object->Obj.GeoFrame.grid;

      XmToggleButtonSetState(gridonw, True, True);

      if (Global.selectedObjects->object->Obj.GeoFrame.grid.labelon &&
	                                           XtIsManaged(gridlabelonw) )
	XmToggleButtonSetState(gridlabelonw, True, True); 
      else
	XmToggleButtonSetState(gridlabelonw, False, True); 
    } else {
      XmToggleButtonSetState(gridonw, False, True);
      XmToggleButtonSetState(gridlabelonw, False, True); 
    }

    sprintf (value, "%8.2f", Global.gridattr.gridgap);
    XmTextFieldSetString(gridgapw, value);

    sprintf (value, "%d", Global.gridattr.spacing);
    XmTextFieldSetString(gridspacew,value);
    
    sprintf (value, "%d", Global.gridattr.linewidth);
    XmTextFieldSetString(gridlww,value);
    
    addCallbacks();
  }
  
}
