#include "xgdisp.h"

static XtCallbackRec lenvalueCB[] = {
    {(XtCallbackProc) BarLengthTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec lenmodCB[] = {
    {(XtCallbackProc) BarLengthTextVerifyCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec itvlvalueCB[] = {
    {(XtCallbackProc) BarIntervalTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec lwvalueCB[] = {
    {(XtCallbackProc) BarThickTextCallBack, (XtPointer) NULL},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec dashedtoggleCB[] = {
    {(XtCallbackProc) BarStyleToggleCallBack, (XtPointer) DASHED},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec tickedtoggleCB[] = {
    {(XtCallbackProc) BarStyleToggleCallBack, (XtPointer) TICKED},
    {(XtCallbackProc) NULL, NULL}
};

static XtCallbackRec mtoggleCB[] = {
    {(XtCallbackProc) BarUnitToggleCallBack, (XtPointer) XGD_METERS},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec kmtoggleCB[] = {
    {(XtCallbackProc) BarUnitToggleCallBack, (XtPointer) XGD_KILOMETERS},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec mitoggleCB[] = {
    {(XtCallbackProc) BarUnitToggleCallBack, (XtPointer) XGD_MILES},
    {(XtCallbackProc) NULL, NULL}
};
static XtCallbackRec fttoggleCB[] = {
    {(XtCallbackProc) BarUnitToggleCallBack, (XtPointer) XGD_FEET},
    {(XtCallbackProc) NULL, NULL}
};







removeBarCallbacks()
{
    XtRemoveCallbacks(barlengthw, XmNvalueChangedCallback, lenvalueCB);
    XtRemoveCallbacks(barlengthw, XmNmodifyVerifyCallback, lenmodCB);

    XtRemoveCallbacks(barintervalw, XmNvalueChangedCallback, itvlvalueCB);

    XtRemoveCallbacks(barthickw, XmNvalueChangedCallback, lwvalueCB);

    XtRemoveCallbacks(bardashedw, XmNvalueChangedCallback, dashedtoggleCB);

    XtRemoveCallbacks(bartickedw, XmNvalueChangedCallback, tickedtoggleCB);

    XtRemoveCallbacks(barmw, XmNvalueChangedCallback, mtoggleCB);
    XtRemoveCallbacks(barkmw, XmNvalueChangedCallback, kmtoggleCB);
    XtRemoveCallbacks(barmiw, XmNvalueChangedCallback, mitoggleCB);
    XtRemoveCallbacks(barftw, XmNvalueChangedCallback, fttoggleCB);
}

addBarCallbacks()
{
    XtAddCallback(barlengthw, XmNvalueChangedCallback, 
        BarLengthTextCallBack, (XtPointer) NULL);
    XtAddCallback(barlengthw, XmNmodifyVerifyCallback, 
        BarLengthTextVerifyCallBack, (XtPointer) NULL);

    XtAddCallback(barintervalw, XmNvalueChangedCallback, 
        BarIntervalTextCallBack, (XtPointer) NULL);

    XtAddCallback(barthickw, XmNvalueChangedCallback, 
        BarThickTextCallBack, (XtPointer) NULL);

    XtAddCallback(bardashedw, XmNvalueChangedCallback, 
        BarStyleToggleCallBack, (XtPointer) DASHED);

    XtAddCallback(bartickedw, XmNvalueChangedCallback, 
        BarStyleToggleCallBack, (XtPointer) TICKED);

    XtAddCallback(barmw, XmNvalueChangedCallback, 
        BarUnitToggleCallBack, (XtPointer) XGD_METERS);
    XtAddCallback(barkmw, XmNvalueChangedCallback, 
        BarUnitToggleCallBack, (XtPointer) XGD_KILOMETERS);
    XtAddCallback(barmiw, XmNvalueChangedCallback, 
        BarUnitToggleCallBack, (XtPointer) XGD_MILES);
    XtAddCallback(barftw, XmNvalueChangedCallback, 
        BarUnitToggleCallBack, (XtPointer) XGD_FEET);
}

void 
UpdateBarBox()
{
    char        value[30];

    if (Global.selectedObjects != NULL) {
        if (Global.selectedObjects->object->type == XGD_BARSCALE) {
            removeBarCallbacks();
            Global.barattr = Global.selectedObjects->object->Obj.Barscale;

            if (Global.selectedObjects->object->Obj.Barscale.style == DASHED)
                XmToggleButtonSetState(bardashedw, True, True);
            else
                XmToggleButtonSetState(bartickedw, True, True);

            switch (Global.selectedObjects->object->Obj.Barscale.unit) {
            case XGD_KILOMETERS:
                XmToggleButtonSetState(barkmw, True, True);
                break;
            case XGD_METERS:
                XmToggleButtonSetState(barmw, True, True);
                break;
            case XGD_MILES:
                XmToggleButtonSetState(barmiw, True, True);
                break;
            case XGD_FEET:
                XmToggleButtonSetState(barmiw, True, True);
                break;
            }

            sprintf(value, "%8.2f", Global.barattr.length);
            XmTextFieldSetString(barlengthw, value);

            sprintf(value, "%d", Global.barattr.intervals);
            XmTextFieldSetString(barintervalw, value);

            sprintf(value, "%d", Global.barattr.linewidth);
            XmTextFieldSetString(barthickw, value);

            SetOptMenuToPixel(barfgw, Global.barattr.color);
            SetOptMenuToPixel(bartcw, Global.barattr.textcolor);

            addBarCallbacks();
        }
    }
}
