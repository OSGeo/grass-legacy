static char                     rcsid[] = "@(#)XGRASS $Id: xc.create.c,v 0.0.0.1 1992/05/05 14:59:11 kurt Exp kurt $";
/*
 * File: create.c
 * 
 * Desc: Does all widget creation and most operations (too lazy to split it
 *         all out at the moment...)
 * 
 * Auth: Kurt Buehler
 * 
 * Date: Sun Mar  8 12:05:48 CST 1992
 * 
 * Modification History:
 * 
 * 
 */

#include <ctype.h>
#include <string.h>
#include <errno.h>
#include "xc.xclip.h"
#include "menu_item.h"
#include "xgbitmaps.h"
#include "xgrass.h"

Widget CreateWorkArea();

char *
#ifdef _NO_PROTO
_XcEnumKeyToString(modifier,key)
    XCTypeModifier modifier;
    char *key;
#else
_XcEnumKeyToString( XCTypeModifier modifier, char *key)
#endif
{
    Boolean end = False;
    int i;

    for (i = 0; i < modifier.enumNItems && !end; i++ ) {
	if ( modifier.enumKeys[i] != NULL ) {
	    if ( !strcmp(key,modifier.enumKeys[i]) ) {
		if ( modifier.enumStrings[i] != NULL ) {
		    return(modifier.enumStrings[i]);
		} else {
		    return(key);
		}
	    }
	}
    }
    return(key);
}

char *
#ifdef _NO_PROTO
_XcEnumStringToKey(modifier,string)
    XCTypeModifier modifier;
    char *string;
#else
_XcEnumStringToKey( XCTypeModifier modifier, char *string)
#endif
{
    Boolean end = False;
    int i;

    for (i = 0; i < modifier.enumNItems && !end; i++ ) {
	if ( !strcmp(string,modifier.enumStrings[i]) ) {
	    if ( modifier.enumKeys[i] != NULL ) {
		return(modifier.enumKeys[i]);
	    } else {
		return(string);
	    }
	}
    }
    return(string);
}

void
XClipCancelCallBack(w,
                    cld,
                    cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    if (Global->standAlone) {
        XClipExit(Global->applShell, 0,Global);
    }
    else {
	XtDestroyWidget(Global->interactor);
    }
}

void
XClipResetCallBack(w,
                   cld,
                   cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCInterfaceData *cur = Global->_xc_Data.data;

    _XcResetInterface(cur);
    XmTextSetString(Global->_xc_Data.commandText, _XcBuildCommandString(Global));

    return;
}

double
Ten(n)
    int                             n;
{
    if (n == 1 || n == 0)
        return (10.0);
    return (10.0 * Ten(n - 1));
}

_XcUnsetParmWidget(cur)
XCParmData *cur;
{
	    switch (cur->type) {
	    case XC_TYPE_CHARACTER:
		XmTextSetString(cur->widget,"");
		break;
	    case XC_TYPE_ENUMERATE:
		XmListDeselectAllItems(cur->widget);
		cur->parmSet = False;
		break;
	    case XC_TYPE_INTEGER:
		cur->parmSet = False;
		if (cur->hasModifier) {
		    XtVaSetValues(cur->widget, XmNvalue,
			     ((cur->def.ival) ? cur->def.ival :
			      (int) cur->modifier.start), NULL);
		} else {
		    XmTextSetString(cur->widget,"");
		}
		break;
	    case XC_TYPE_FLOAT:
	    case XC_TYPE_DOUBLE:
		if (cur->hasModifier) {
		    XtVaSetValues(cur->widget, XmNvalue,
			     ((cur->def.dval != 0.0) ? 
			      (int) (cur->value.dval *
			       Ten(cur->modifier.decimalPoints)) :
			      (int) cur->modifier.start), NULL);
		    cur->parmSet = False;
		} else {
		    XmTextSetString(cur->widget,"");
		    cur->parmSet = False;
		}
	    case XC_TYPE_DB_ELEMENT:
		XmTextSetString(cur->widget,"");
		cur->parmSet = False;
		break;
	    case XC_TYPE_LOGICAL:
		cur->parmSet = False;
		XtVaSetValues(cur->widget, XmNset, False, NULL);
	    case XC_TYPE_FILENAME:
		XmTextSetString(cur->widget,"");
		cur->parmSet = False;
	    }
}

_XcHandlePrecludes(ptr,Global)
XCParmData *ptr;
XclipGlobalData *Global;
{
    XCRequireData *rPtr;
    XCInterfaceData *cur;
    /* XXX */
    cur = _XcGetParmByParmData(Global->_xc_Data.data,
                               ptr);
    if ( cur == NULL ) return;
    rPtr = _XcReturnAppropriatePrecludeData(cur);
    while (rPtr) {
        /*
         * this is not the flag or parm that changed, unset it
         */
        if (strcmp(cur->name,
                   rPtr->name)) {
            if ( rPtr->isFlag) {
                XCInterfaceData                *ptr =
                _XcGetFlagByName(Global->_xc_Data.data,
                                 rPtr->name);

                ptr->flag->answer = False;
                XmToggleButtonSetState(ptr->flag->toggle,
                                       False,
                                       False);
            } else {
                XCInterfaceData                *ptr =
                _XcGetParmByName(Global->_xc_Data.data,
                                 rPtr->name);

                ptr->parm->parmSet = False;
                _XcUnsetParmWidget(ptr->parm);
            }
        }
        rPtr = rPtr->next;
    }
}

_XcResetInterface(cur)
XCInterfaceData *cur;
{
    while ( cur ) {
	switch ( cur->type ) {
	case XC_DIALOG: 
	    _XcResetInterface(cur->right);
	    break;
	case XC_FLAG:
		XtVaSetValues(cur->flag->toggle, XmNset, cur->flag->def, NULL);
		cur->flag->answer = False;
		break;
	case XC_PARM:
	    switch (cur->parm->type) {
	    case XC_TYPE_CHARACTER:
		XmTextSetString(cur->parm->widget,"");
		cur->parm->parmSet = False;
		if ( cur->parm->def.cval ) {
		    XmTextSetString(cur->parm->widget,cur->parm->def.cval);
		    cur->parm->value.cval = cur->parm->def.cval;
		    cur->parm->parmSet = True;
		}
		break;
	    case XC_TYPE_ENUMERATE:
		XmListDeselectAllItems(cur->parm->widget);
		cur->parm->parmSet = False;
		if ( cur->parm->def.cval ) {
                    XmString xms;
		    char *string;

		    string = _XcEnumKeyToString(cur->parm->modifier, 
                                                cur->parm->def.cval);
		    xms = XmStringCreateSimple(string);
		    XmListSelectItem(cur->parm->widget, xms, False);
		    cur->parm->value.cval = cur->parm->def.cval;
		    cur->parm->parmSet = True;
                    XmStringFree(xms);
		}
		break;
	    case XC_TYPE_INTEGER:
		cur->parm->parmSet = False;
		if (cur->parm->hasModifier) {
		    XtVaSetValues(cur->parm->widget, XmNvalue,
			     ((cur->parm->def.ival) ? cur->parm->def.ival :
			      (int) cur->parm->modifier.start), NULL);
		    cur->parm->parmSet = True;
		    cur->parm->value.ival = cur->parm->def.ival;
		} else {
		    if ( cur->parm->def.cval ) {
			XmTextSetString(cur->parm->widget,cur->parm->def.cval);
			cur->parm->value.cval = cur->parm->def.cval;
			cur->parm->parmSet = True;
		    }
		}
		break;
	    case XC_TYPE_FLOAT:
	    case XC_TYPE_DOUBLE:
		cur->parm->parmSet = False;
		if (cur->parm->hasModifier) {
		    XtVaSetValues(cur->parm->widget, XmNvalue,
			     ((cur->parm->def.dval != 0.0) ? 
			      (int) (cur->parm->value.dval *
			       Ten(cur->parm->modifier.decimalPoints)) :
			      (int) cur->parm->modifier.start), NULL);
		    cur->parm->parmSet = True;
		    cur->parm->value.ival = cur->parm->def.ival;
		} else {
		    if ( cur->parm->def.cval ) {
			XmTextSetString(cur->parm->widget,cur->parm->def.cval);
			cur->parm->value.cval = cur->parm->def.cval;
			cur->parm->parmSet = True;
		    }
		}
		break;
	    case XC_TYPE_DB_ELEMENT:
		XmTextSetString(cur->parm->widget,"");
		cur->parm->parmSet = False;
		if ( cur->parm->def.cval ) {
		    XmTextSetString(cur->parm->widget,cur->parm->def.cval);
		    cur->parm->value.cval = cur->parm->def.cval;
		cur->parm->parmSet = True;
		}
		break;
	    case XC_TYPE_LOGICAL:
		cur->parm->parmSet = False;
		if ( cur->parm->def.bval ) {
		    XtVaSetValues(cur->parm->widget,  
			XmNset, cur->parm->def.bval, NULL);
		}
		break;
	    case XC_TYPE_FILENAME:
		XmTextSetString(cur->parm->widget,"");
		cur->parm->parmSet = False;
		if ( cur->parm->def.cval ) {
		    XmTextSetString(cur->parm->widget,cur->parm->def.cval);
		    cur->parm->value.cval = cur->parm->def.cval;
		    cur->parm->parmSet = True;
		}
		break;
	    }
	    break;
	}
	cur = cur->left;
    }
}

void
XClipExecuteCallBack(w,
                     cld,
                     cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    Widget xclipShell = (Widget)theBunch->data;
    XclipGlobalData *Global = theBunch->theGlobal;
    Status                          resBlack, resWhite;
    char *com;
    XgMenuItemRec *mi;

    if (_XcCheckRequirements(Global->_xc_Data.data,Global) && 
	_XcCheckOptional(Global->_xc_Data.data,Global)) {

        XgDoHourGlass(Global->applShell);
        XgSystem(xclipShell, com = _XcBuildCommandString(Global), 
	    !(Global->_xc_Data.capture), Global->_xc_Data.errorcodes, Global->_xc_Data.nerrcodes);
/* KAB KAB
        XgSetCommandString(Global->display,XgGetMenuWindow(Global->display),
            buf);
*/

          mi = (XgMenuItemRec *)_XgMalloc(sizeof(XgMenuItemRec));
          bzero((char *)mi, sizeof(XgMenuItemRec));
	mi->arglist = com;

        if ( Global->standAlone == False ) 
	    __XgHistoryAddItem(mi,XG_HISTORY_EXEC);
        XgUndoHourGlass(Global->applShell);
    }
}

void
RadioCallBack(w,
              cld,
              cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    XCFlagData                     *f = (XCFlagData *) theBunch->data;
    XCInterfaceData                *cur;
    XCRequireData                  *rPtr;

    f->flagSet = True;
    f->answer = cbs->set;
    if (cbs->set == False) {
        XmTextSetString(Global->_xc_Data.commandText,
                        _XcBuildCommandString(Global));
        return;
    }
    /*
     * the flag has been turned on, check the flags and parms that it precludes
     * unsetting any that are set.
     */
    cur = _XcGetFlagByFlagData(Global->_xc_Data.data,
                               f);
    rPtr = _XcReturnAppropriatePrecludeData(cur);
    while (rPtr) {
        /*
         * this is not the flag or parm that changed, unset it
         */
        if (strcmp(cur->name,
                   rPtr->name)) {
	    if ( rPtr->isFlag) {
		XCInterfaceData                *ptr =
		_XcGetFlagByName(Global->_xc_Data.data,
				 rPtr->name);

		ptr->flag->answer = False;
		XmToggleButtonSetState(ptr->flag->toggle,
				       False,
				       False);
	    } else {
		XCInterfaceData                *ptr =
		_XcGetParmByName(Global->_xc_Data.data,
				 rPtr->name);

		ptr->parm->parmSet = False;
		_XcUnsetParmWidget(ptr->parm);
	    }
        }
        rPtr = rPtr->next;
    }
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
ScaleToggleCallBack(w,
               cld,
               cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    XCParmData                     *p = (XCParmData *) theBunch->data;

    p->togSet = cbs->set;
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
ToggleCallBack(w,
               cld,
               cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    XCFlagData                     *f = (XCFlagData *) theBunch->data;

    f->answer = cbs->set;
    f->flagSet = True;
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
ParmToggleCallBack(w,
                   cld,
                   cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmToggleButtonCallbackStruct   *cbs = (XmToggleButtonCallbackStruct *) cad;
    XCParmData                     *p = (XCParmData *) theBunch->data;

    p->parmSet = True;
    p->value.bval = cbs->set;
    _XcHandlePrecludes(p,Global);
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
ScaleCallBack(w,
              cld,
              cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmScaleCallbackStruct          *cbs = (XmScaleCallbackStruct *) cad;
    XCParmData                     *p = (XCParmData *) theBunch->data;

    p->parmSet = True;
    if (p->type == XC_TYPE_INTEGER)
        p->value.ival = cbs->value;
    else {
        p->value.dval = (double) cbs->value / Ten(p->modifier.decimalPoints);
    }
    _XcHandlePrecludes(p,Global);
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
BrowserCancelCallBack(w,
                      cld,
                      cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XtDestroyWidget(w);
}

void
BrowserOkCallBack(w,
                  cld,
                  cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCParmData                     *curp = (XCParmData *) theBunch->data;
    char                           *result;
    XmString                        xms;

    XtVaGetValues(w,
                  XmNresultString,
                  &xms,
                  NULL);
    XmStringGetLtoR(xms,
                    XmSTRING_DEFAULT_CHARSET,
                    &result);
    _XcHandlePrecludes(curp,Global);
    if (result != NULL) {
        if (curp->parmSet) {
            _XgFree(curp->value.dbval.desc);
        }
        curp->parmSet = True;
        curp->value.dbval.desc = result;
        XmTextSetString(curp->widget,
                        result);
        XmTextSetString(Global->_xc_Data.commandText,
                        _XcBuildCommandString(Global));
    } else {
        if (curp->parmSet) {
            _XgFree(curp->value.dbval.desc);
            curp->value.dbval.desc = NULL;
        }
        XmTextSetString(curp->widget,
                        "");
        XmTextSetString(Global->_xc_Data.commandText,
                        _XcBuildCommandString(Global));
        curp->parmSet = False;
    }
}

void
DBElementCallBack(w,
                  cld,
                  cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCParmData                     *curp = (XCParmData *) theBunch->data;
    char                           *result = NULL;
    Arg                             al[20];
    int                             ac = 0;
    XmString                        xms, xms1, xms2;
    Widget                          xgb;

    xms1 = XmStringCreateSimple(G_mapset());
    XtSetArg(al[ac],
             XmNinitialMapset1,
             xms1);
    ac++;
    XtSetArg(al[ac],
             XmNnumLists,
             1);
    ac++;
    XtSetArg(al[ac],
             XmNenableWorkAreaStretch,
             True);
    ac++;

    switch (curp->value.dbval.type) {
    case XC_DB_TYPE_RASTER:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_RASTER);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more raster maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a raster map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_ASCII_DLG:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_ASCII_DLG);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more ASCII DLG maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select an ASCII DLG map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_DLG:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_DLG);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more binary DLG maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a binary DLG map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_ASCII_VECTOR:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_ASCII_VECTOR);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more ASCII vector maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select an ASCII vector map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_VECTOR:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_VECTOR);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more vector maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a vector map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_SITES:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_SITE);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more sites maps");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a sites map");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_REGION:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_REGION);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more regions");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a saved region");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_ICON:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_ICON);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more icon files");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select an icon file");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_LABEL:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_LABEL);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more label files");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select a label file");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_IMAGE_GROUP:
        XtSetArg(al[ac],
                 XmNbrowseMode,
                 XG_GROUP);
        ac++;
        if (curp->multiple) {
            xms = XmStringCreateSimple("Please select one or more image groups");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_MULTIPLE_SELECT);
            ac++;
        } else {
            xms = XmStringCreateSimple("Please select an image group");
            XtSetArg(al[ac],
                     XmNselMode,
                     XG_SINGLE_SELECT);
            ac++;
        }
        XtSetArg(al[ac],
                 XmNpromptLabelString,
                 xms);
        ac++;
        break;
    case XC_DB_TYPE_IMAGE_SUBGROUP:
        XgWarningDialog(Global->interactor,
                        "Image Sub Group Browser not yet implemented");
        return;
    case XC_DB_TYPE_USER_DEFINED:
        XgWarningDialog(Global->interactor,
                        "User-defined Browser not yet implemented");
        return;
    }
    xgb = XgCreateBrowserDialog(Global->interactor,
                                "XGRASS Raster Browser",
                                al,
                                ac);
    XtManageChild(XgInteractorGetChild(xgb,
                                       XmINTERACT_PROMPT_LABEL));
    XtUnmanageChild(XgInteractorGetChild(xgb,
                                         XmINTERACT_APPLY_BUTTON));
    XtAddCallback(xgb,
                  XmNokCallback,
                  BrowserOkCallBack,
                  (XtPointer) CreateBunch(Global,curp));
    XtAddCallback(xgb,
                  XmNcancelCallback,
                  BrowserCancelCallBack,
                  CreateBunch(Global,NULL));
    XmStringFree(xms);
    XmStringFree(xms1);
    XtManageChild(xgb);
}

void
FileSelectCallBack(w,
                   cld,
                   cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmFileSelectionBoxCallbackStruct *fsb =
    (XmFileSelectionBoxCallbackStruct *) cad;
    XCParmData                     *curp = (XCParmData *) theBunch->data;
    char                           *file, *directory;
    struct stat                     sbuf;

    XmStringGetLtoR(fsb->value,
                    XmSTRING_DEFAULT_CHARSET,
                    &file);
    XmStringGetLtoR(fsb->dir,
                    XmSTRING_DEFAULT_CHARSET,
                    &directory);

    if (stat(file,
             &sbuf) == -1) {
        /*
         * doesn't exist, check the directory to see if it is writable
         */
        if (stat(directory,
                 &sbuf) != -1) {
            if ((sbuf.st_mode & S_IFMT) == S_IFDIR &&
                    access(directory,
                           W_OK) != -1) {
                /*
                 * looks good, set the file name in the command string
                 */
                curp->parmSet = True;
                curp->value.cval = _XgStrDup(file);
                XmTextSetString(curp->widget,
                                file);
		_XcHandlePrecludes(curp,Global);
                XmTextSetString(Global->_xc_Data.commandText,
                                _XcBuildCommandString(Global));
                XtUnmanageChild(w);
                return;
            }
        }
        sprintf(errorbuf,
                "Directory \"%s\" not writable!",
                directory);
        XgWarningDialog(Global->interactor,
                        errorbuf);
    } else if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
        /* is a directory */
        sprintf(errorbuf,
                "Cannot save to \"%s\", it is a directory!",
                file);
        XgWarningDialog(Global->interactor,
                        errorbuf);
    } else if (!(sbuf.st_mode & S_IFREG) || access(file,
                                                   W_OK) == -1) {
        /* is not a regular file or is unwritable */
        if (!(sbuf.st_mode & S_IFREG))
            sprintf(errorbuf,
                    "Cannot save to \"%s\", not a regular file!",
                    file);
        else
            sprintf(errorbuf,
                    "Cannot save to \"%s\", not writable!",
                    file);
        XgWarningDialog(Global->interactor,
                        errorbuf);
    } else {
        /* a legitimate, but already existing file... */
        curp->parmSet = True;
        curp->value.cval = _XgStrDup(file);
        XmTextSetString(curp->widget,
                        file);
	_XcHandlePrecludes(curp,Global);
        XmTextSetString(Global->_xc_Data.commandText,
                        _XcBuildCommandString(Global));
        XtUnmanageChild(w);
        return;
    }
    if (curp->parmSet == True) {
        curp->parmSet = False;
        _XgFree(curp->value.cval);
        XmTextSetString(curp->widget,
                        "");
        XmTextSetString(Global->_xc_Data.commandText,
                        _XcBuildCommandString(Global));
    }
    XtUnmanageChild(w);
}

void
FileSelectCancelCallBack(w,
                         cld,
                         cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmFileSelectionBoxCallbackStruct *fsb =
    (XmFileSelectionBoxCallbackStruct *) cad;

    XtUnmanageChild(w);
    /*
     * KAB Should we destroy this widget??? XtDestroyWidget(w);
     */
}


void
FilenameInputCallBack(w,
                      cld,
                      cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    Widget                          dialog;
    Widget                          shell;
    Atom                            protocol;
    XCParmData                     *curp = (XCParmData *) theBunch->data;

    dialog = XmCreateFileSelectionDialog(Global->interactor,
                                         curp->desc,
                                         NULL,
                                         0);

    XtVaSetValues(dialog,
                  XmNdialogStyle,
                  XmDIALOG_FULL_APPLICATION_MODAL,
                  NULL);

    shell = XtParent(dialog);
    if (XmIsMotifWMRunning(shell)) {
        unsigned int                    decor_flags;

        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
                      XmNmwmDecorations,
                      decor_flags,
                      NULL);
    }
    XtAddCallback(dialog,
                  XmNokCallback,
                  FileSelectCallBack,
                  (XtPointer) CreateBunch(Global,curp));
    XtAddCallback(dialog,
                  XmNcancelCallback,
                  FileSelectCancelCallBack,
                  CreateBunch(Global,NULL));

    XtUnmanageChild(XmFileSelectionBoxGetChild(dialog,
                                               XmDIALOG_HELP_BUTTON));

    XtManageChild(dialog);
}


/* 
 * Save functionality 
 */

#ifdef _NO_PROTO
__XcSaveToFile(text, file)
Widget text;
char *file;
#else
__XcSaveToFile( Widget text, char *file)
#endif
{
    FILE *fp, *fopen();
    char *string;
    int length = 0;
    char errorbuf[1024];

    string = XmTextGetString(text);
    length = XmTextGetLastPosition(text);
    if ( string == NULL || *string == NULL ) {
	return;
    }

    fp = fopen(file, "w");
    if ( fp == NULL ) {
	sprintf(errorbuf,"\"%s\": %s", file, strerror(errno));
	XgWarningDialog(text,errorbuf);
	return;
    }
    fwrite(string, sizeof(char), length, fp);
    fclose(fp);
}

static void
#ifdef _NO_PROTO
__XcSaveAsFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XcSaveAsFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XCParmData *curp = (XCParmData *) cld;
    InteractorCallbackStruct *xgb =
        (InteractorCallbackStruct *)cad;
    char *ptr, *file, *directory, *path;
    char errorbuf[1024];
    struct stat sbuf;

    XmStringGetLtoR(xgb->value,XmSTRING_DEFAULT_CHARSET,&path);
    /* make directory point to the directory part */
    file = XtNewString(path);
    ptr = (char *)strrchr(path,'/');
    if ( ptr == NULL ) {
	directory = "./";
    } else {
	*ptr = '\0';
	directory = path;
    }

    /* is the directory path writeable? */
    if ( access(directory, W_OK) == -1 ) {
	sprintf(errorbuf,"\"%s\": %s", directory, strerror(errno));
	XgWarningDialog(w,errorbuf);
	return;
    }
    /* does the file already exist? */
    if ( access(file, F_OK) != -1 ) {
	/* a legitimate, but already existing file... */
	if ( access(file, W_OK) != -1 ) {
	    sprintf(errorbuf,"File \"%s\" exists, overwrite?",file);
	    if ( XgYesNo(w, errorbuf) ) {
		XtFree(curp->value.dbval.filename);
		curp->value.dbval.filename = XtNewString(file);
		__XcSaveToFile((Widget) curp->value.dbval.fileText, file);
		/*  set the text field to the latest file name */
		XmTextSetString(curp->widget, curp->value.dbval.filename);
		XtVaSetValues(curp->value.dbval.shell, 
                    XmNtitle, curp->value.dbval.filename, NULL);
	    }
	} else {
	    sprintf(errorbuf,"\"%s\": %s",file, strerror(errno));
	    XgWarningDialog(w,errorbuf);
	}
    } else {
	/* everything is OK, write the file */
        XtFree(curp->value.dbval.filename);
        curp->value.dbval.filename = XtNewString(file);
	__XcSaveToFile((Widget) curp->value.dbval.fileText, file);
	/*  set the text field to the latest file name */
	XmTextSetString(curp->widget, curp->value.dbval.filename);
	XtVaSetValues(curp->value.dbval.shell, 
            XmNtitle, curp->value.dbval.filename, NULL);
    }

}

static void
#ifdef _NO_PROTO
_XcEditorSaveAs(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorSaveAs(Widget w, XtPointer cld, XtPointer cad)
#endif
{
     Widget dialog;
     Widget shell;
     Atom protocol;
     Arg al[10];
     int ac = 0;
     XmString xms;

     xms = XmStringCreateSimple("Enter file name:");
     XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
     dialog = XgCreateInteractorPromptDialog((Widget)w,
	 "XClip Editor Save As", al, ac);

    shell = XtParent(dialog);
    if ( XmIsMotifWMRunning(shell) ) {
	unsigned int decor_flags;

	decor_flags = MWM_DECOR_BORDER;

	XtVaSetValues(shell,
	    XmNmwmDecorations, decor_flags,
	    NULL);
    }

    XtAddCallback(dialog, XmNokCallback, __XcSaveAsFile, cld);
    XtAddCallback(dialog, XmNcancelCallback, XtUnmanageChild, dialog);

    XtUnmanageChild(XgInteractorGetChild(dialog,XmINTERACT_HELP_BUTTON));
    XtManageChild(dialog);
}

static void
#ifdef _NO_PROTO
_XcEditorSave(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorSave(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XCParmData *curp = (XCParmData *) cld;

    if ( curp->value.dbval.filename == NULL ) {
        _XcEditorSaveAs(w, cld, cad);
        return;
    }
    __XcSaveToFile((Widget) curp->value.dbval.fileText, 
                   curp->value.dbval.filename);
}

/* 
 * Load functionality 
 */

static Boolean
#ifdef _NO_PROTO
__XcLoadFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XcLoadFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget text = (Widget)cld;
    XmFileSelectionBoxCallbackStruct *fsb =
	(XmFileSelectionBoxCallbackStruct *)cad;
    char *file;
    int acstat;
    FILE *fp, *fopen();
    struct stat sbuf;
    char errorbuf[1024];
    char *buf;
    int buflen;
    int bytes;
    Boolean first = True;

    XmStringGetLtoR(fsb->value,XmSTRING_DEFAULT_CHARSET,&file);

    if ( stat(file, &sbuf) == -1 || (acstat = access(file, R_OK)) == -1 ) {
	if ( acstat == -1 ) {
	    sprintf(errorbuf,"Can't load from \"%s\", not readable.", file);
	    XgWarningDialog(w,errorbuf);
	    return False;
	} else {
            return True;
	}
    }
    if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
	sprintf(errorbuf,"Cannot load from \"%s\", it is a directory!",file);
	XgWarningDialog(w,errorbuf);
	return False;
    }
    buflen = sbuf.st_size;

    if ( buflen > 0 ) {
	fp = fopen(file, "r");
	buf = XtMalloc(buflen);
	bytes = fread(buf,sizeof(char),buflen,fp);
	buf[bytes] = '\0';
	fclose(fp);
	if ( bytes ) {
	    XmTextReplace(text,0,XmTextGetLastPosition(text),buf);
	}
    }
}

static void
#ifdef _NO_PROTO
_XcEditorLoad(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorLoad(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget fsb;
    XmString xms = XmStringCreateSimple("XClip Editor Load Dialog");;

    fsb = XmCreateFileSelectionDialog(w, "load_dialog", NULL, 0);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XcLoadFile, cld);
    XtManageChild(fsb);
}

/* 
 * Insert functionality 
 */


static void
#ifdef _NO_PROTO
__XcInsertFile(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
__XcInsertFile(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget text = (Widget)cld;
    XmFileSelectionBoxCallbackStruct *fsb =
	(XmFileSelectionBoxCallbackStruct *)cad;
    char *file;
    int acstat;
    FILE *fp, *fopen();
    struct stat sbuf;
    char errorbuf[1024];
    char *buf;
    int buflen;
    int bytes;
    Boolean first = True;

    XmStringGetLtoR(fsb->value,XmSTRING_DEFAULT_CHARSET,&file);

    if ( stat(file, &sbuf) == -1 || (acstat = access(file, R_OK)) == -1 ) {
	if ( acstat == -1 ) {
	    sprintf(errorbuf,"Can't insert from \"%s\", not readable.", file);
	    XgWarningDialog(text,errorbuf);
	} else {
	    sprintf(errorbuf,"Can't insert from \"%s\", try again.",
		file);
	    XgWarningDialog(text,errorbuf);
	}
	return;
    }
    if ((sbuf.st_mode & S_IFMT) == S_IFDIR) {
	sprintf(errorbuf,"Cannot insert from \"%s\", it is a directory!",file);
	XgWarningDialog(text,errorbuf);
	return;
    }
    buflen = sbuf.st_size;

    if ( buflen > 0 ) {
	fp = fopen(file, "r");
	buf = XtMalloc(buflen);
	bytes = fread(buf,sizeof(char),buflen,fp);
	buf[bytes] = '\0';
	fclose(fp);
	if ( bytes ) {
	    XmTextInsert(text,XmTextGetInsertionPosition(text),buf);
	}
    }
}

static void
#ifdef _NO_PROTO
_XcEditorInsert(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorInsert(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget fsb;
    XmString xms = XmStringCreateSimple("XClip Editor Insert Dialog");;

    fsb = XmCreateFileSelectionDialog(w, "insert_dialog", NULL, 0);
    XtVaSetValues(fsb, XmNdialogTitle, xms, XmNautoUnmanage, True, NULL);
    XtAddCallback(fsb, XmNokCallback, __XcInsertFile, cld);
    XtManageChild(fsb);
}

/*
 * Quit functionality
 */

static void
#ifdef _NO_PROTO
_XcEditorReturn(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorReturn(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XCParmData *curp = (XCParmData *) cld;

    if ( curp->value.dbval.filename != NULL ) {
	__XcSaveToFile((Widget) curp->value.dbval.fileText, 
		       curp->value.dbval.filename);
	/*  set the text field to the latest file name */
	XmTextSetString(curp->widget, curp->value.dbval.filename);
	/* set the popup title to the filename */
	XtVaSetValues(curp->value.dbval.shell, 
		XmNtitle, curp->value.dbval.filename, NULL);
    }

    XtPopdown(curp->value.dbval.shell);
    curp->value.dbval.shell = NULL;
}

static void
#ifdef _NO_PROTO
_XcEditorCut(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorCut(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextCut((Widget)cld,CurrentTime) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to cut!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XcEditorCopy(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorCopy(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextCopy((Widget)cld,CurrentTime) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to copy!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XcEditorPaste(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorPaste(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    if ( XmTextPaste((Widget)cld) == False ) {
        XgWarningDialog((Widget)cld,"Nothing to paste!");
        return;
    }
}

static void
#ifdef _NO_PROTO
_XcEditorClear(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorClear(Widget w, XtPointer cld, XtPointer cad)
#endif
{
    XmTextClearSelection((Widget)cld,CurrentTime);
}

static void
#ifdef _NO_PROTO
_XcEditorHelp(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else
_XcEditorHelp(Widget w, XtPointer cld, XtPointer cad)
#endif
{
printf("Help\n");
}

void
FilenameEditCallBack(wid,
                      cld,
                      cad)
    Widget                          wid;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCParmData                     *curp = (XCParmData *) theBunch->data;
    Widget w;
    Widget main_window;
    Widget menu_bar;
    Widget frame;
    Widget textw;
    Widget file_menu;
    Widget file_menu_button;
    Widget save_as;
    Widget save_file;
    Widget load_file;
    Widget insert_file;
    Widget quit;
    Widget edit_menu;
    Widget edit_menu_button;
    Widget cut;
    Widget copy;
    Widget paste;
    Widget clear;
    Widget help_menu;
    Widget help_menu_button;
    char *file;
    XmString xmstring;
    Arg al[5];
    int ac = 0;
    Atom protocol;

    if ( curp->value.dbval.shell != NULL ) {
        XgWarningDialog(Global->applShell,"An editor is already active!!");
	return;
    }
    curp->value.dbval.filename = XmTextGetString(curp->widget);
    if ( curp->value.dbval.filename == NULL || 
         *(curp->value.dbval.filename) == NULL ) {
	file = XtNewString("NoName");
        curp->value.dbval.filename = NULL;
    } else {
	file = curp->value.dbval.filename;
    }

    w = curp->value.dbval.shell =  
        XtVaCreatePopupShell(file, vendorShellWidgetClass, 
        Global->applShell,
	XmNinput,True, 
	XmNwindowGroup, XtWindow(Global->applShell), 
	XmNtransient, True, 
	XmNmappedWhenManaged,False,
	XmNallowShellResize,True, NULL);

    protocol = XmInternAtom(XtDisplay(w), "WM_DELETE_WINDOW", False);
    XmAddWMProtocols(w, &protocol, 1);
    XtAddEventHandler(w, NoEventMask, True, _XcWMClientMessage, w);

    if ( XmIsMotifWMRunning(w) ) {
	unsigned int decor_flags, func_flags;

	decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
	decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

	func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
	func_flags |= MWM_FUNC_MOVE;

	XtVaSetValues(w,
	    XmNmwmDecorations, decor_flags,
	    XmNmwmFunctions, func_flags,
	    NULL);
    }

    main_window = XmCreateMainWindow(w,"main_window",NULL,0);

    menu_bar = XmCreateMenuBar(main_window,"menu_bar",NULL,0);
    XtManageChild(menu_bar);

    /* create the scrolled text window & insert the text */

    frame = XmCreateFrame(main_window, "frame", NULL, 0);
    XtManageChild(frame);

    textw = curp->value.dbval.fileText = 
        XmCreateScrolledText(frame,"editor_text", NULL, 0);

    XtVaSetValues(textw,
        XmNrows, 10,
        XmNcolumns, 24,
        XmNscrollVertical, True,
        XmNscrollHorizontal, True,
        XmNeditMode, XmMULTI_LINE_EDIT, NULL);

    XtManageChild(textw);

    /* load initial file contents */
    if ( curp->value.dbval.filename != NULL ) {
        XmFileSelectionBoxCallbackStruct fsb;

        fsb.value = XmStringCreateSimple(curp->value.dbval.filename);
        if (! __XcLoadFile(Global->applShell, (XtPointer)textw, (XtPointer)&fsb) ) {
            XtDestroyWidget(w);
            return;
        }
    }

    /* create pulldown menu system */

    file_menu = XmCreatePulldownMenu(menu_bar, "File", NULL, 0);

    xmstring = XmStringCreateSimple("File");
    file_menu_button = XtVaCreateManagedWidget("file",
	xmCascadeButtonWidgetClass, menu_bar,
	XmNlabelString, xmstring,
	XmNmnemonic, 'F',
	XmNaccelerator, "Meta<Key>F",
	XmNsubMenuId, file_menu,
	NULL);
    XmStringFree(xmstring);

    xmstring = XmStringCreateSimple("Save As...");
    save_as = XtVaCreateManagedWidget("save_as",
	xmCascadeButtonWidgetClass, file_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'A',
	XmNaccelerator, "<Key>A",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(save_as, XmNactivateCallback,
	_XcEditorSaveAs, (XtPointer)curp);

    xmstring = XmStringCreateSimple("Save");
    save_file = XtVaCreateManagedWidget("save_file",
	xmCascadeButtonWidgetClass, file_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'S',
	XmNaccelerator, "<Key>S",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(save_file, XmNactivateCallback,
	_XcEditorSave, (XtPointer)curp);

    xmstring = XmStringCreateSimple("Load File...");
    load_file = XtVaCreateManagedWidget("load_file",
	xmCascadeButtonWidgetClass, file_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'L',
	XmNaccelerator, "<Key>L",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(load_file, XmNactivateCallback,
	_XcEditorLoad, (XtPointer)textw);

    xmstring = XmStringCreateSimple("Insert...");
    insert_file = XtVaCreateManagedWidget("insert_from_file",
	xmCascadeButtonWidgetClass, file_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'I',
	XmNaccelerator, "<Key>I",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(insert_file, XmNactivateCallback,
	_XcEditorInsert, (XtPointer)textw);

    xmstring = XmStringCreateSimple("Return");
    quit = XtVaCreateManagedWidget("return",
	xmCascadeButtonWidgetClass, file_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'R',
	XmNaccelerator, "Meta<Key>R",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(quit, XmNactivateCallback,
	_XcEditorReturn, (XtPointer)curp);

    edit_menu = XmCreatePulldownMenu(menu_bar, "Edit", NULL, 0);

    xmstring = XmStringCreateSimple("Edit");
    edit_menu_button = XtVaCreateManagedWidget("edit",
	xmCascadeButtonWidgetClass, menu_bar,
	XmNlabelString, xmstring,
	XmNmnemonic, 'E',
	XmNaccelerator, "Meta<Key>E",
	XmNsubMenuId, edit_menu,
	NULL);
    XmStringFree(xmstring);

    xmstring = XmStringCreateSimple("Cut Selection");
    cut = XtVaCreateManagedWidget("cut",
	xmCascadeButtonWidgetClass, edit_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'C',
	XmNaccelerator, "<Key>C",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(cut, XmNactivateCallback,
	_XcEditorCut, (XtPointer)textw);

    xmstring = XmStringCreateSimple("Copy Selection");
    copy = XtVaCreateManagedWidget("copy",
	xmCascadeButtonWidgetClass, edit_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'o',
	XmNaccelerator, "<Key>o",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(copy, XmNactivateCallback,
	_XcEditorCopy, (XtPointer)textw);

    xmstring = XmStringCreateSimple("Paste Selection");
    paste = XtVaCreateManagedWidget("paste",
	xmCascadeButtonWidgetClass, edit_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'P',
	XmNaccelerator, "<Key>P",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(paste, XmNactivateCallback,
	_XcEditorPaste, (XtPointer)textw);

    XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, edit_menu, NULL);

    xmstring = XmStringCreateSimple("Clear Selection");
    clear = XtVaCreateManagedWidget("clear",
	xmCascadeButtonWidgetClass, edit_menu,
	XmNlabelString, xmstring,
	XmNmnemonic, 'l',
	XmNaccelerator, "<Key>l",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(clear, XmNactivateCallback,
	_XcEditorClear, (XtPointer)textw);

    xmstring = XmStringCreateSimple("Help");
    help_menu_button = XtVaCreateManagedWidget("help",
	xmCascadeButtonWidgetClass, menu_bar,
	XmNlabelString, xmstring,
	XmNmnemonic, 'H',
	XmNaccelerator, "Meta<Key>H",
	NULL);
    XmStringFree(xmstring);

    XtAddCallback(help_menu_button, XmNactivateCallback,
	_XcEditorHelp, (XtPointer)main_window);

    XmMainWindowSetAreas(main_window, menu_bar, NULL, NULL, NULL, frame);
    XtManageChild(main_window);

    XtManageChild(w);
    XtPopup(w, XtGrabNone);
}

void
XCListCallBack(w,
             cld,
             cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCInterfaceData                *cur = (XCInterfaceData *) theBunch->data;
    char                           *text;

    if (cur->parm->multiple) {
         /* KAB DEAL WITH THIS... */ ;
    } else {
        XmStringTable                   item;
        int                             count;

        XtVaGetValues(cur->parm->widget,
                      XmNselectedItemCount,
                      &count,
                      XmNselectedItems,
                      &item,
                      NULL);
        if (count) {
            XmStringGetLtoR(item[0],
                            XmSTRING_DEFAULT_CHARSET,
                            &text);
            if (cur->parm->value.cval)
                XtFree(cur->parm->value.cval);
	    if ( cur->parm->type = XC_TYPE_ENUMERATE ) {
		char *key = _XcEnumStringToKey(cur->parm->modifier, text);

		cur->parm->value.cval = _XgStrDup(key);
	    } else {
		cur->parm->value.cval = text;
	    }
            cur->parm->parmSet = True;
        } else {
            cur->parm->value.cval = NULL;
            cur->parm->parmSet = False;
        }
    }
    _XcHandlePrecludes(cur->parm,Global);
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

void
CaptureToggleCallBack(w,
             cld,
             cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmToggleButtonCallbackStruct     *cbs = (XmToggleButtonCallbackStruct *) cad;

    Global->_xc_Data.capture = cbs->set;
}

void
XcTextCallBack(w, cld, cad)
    Widget                          w;
    caddr_t                         cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XmTextVerifyCallbackStruct     *cbs = (XmTextVerifyCallbackStruct *) cad;
    XCParmData                     *p = (XCParmData *) theBunch->data;
    char                           *string;

    string = XmTextGetString(w);
    if (strcmp(string, "")) {
        switch (p->type) {
        case XC_TYPE_INTEGER:
            p->value.ival = atoi(string);
            p->parmSet = True;
	    break;
        case XC_TYPE_FLOAT:
        case XC_TYPE_DOUBLE:
            p->value.dval = atof(string);
            p->parmSet = True;
	    break;
        case XC_TYPE_FILENAME:
            if (p->value.dbval.filename) {
                XtFree(p->value.dbval.filename);
                p->value.dbval.filename = NULL;
            }
            p->value.dbval.filename = string;
            p->parmSet = True;
            break;
        case XC_TYPE_CHARACTER:
            if (p->value.cval) {
                XtFree(p->value.cval);
                p->value.cval = NULL;
            }
            p->value.cval = string;
            p->parmSet = True;
            break;
        case XC_TYPE_DB_ELEMENT:
            if (p->value.dbval.desc) {
                XtFree(p->value.dbval.desc);
                p->value.dbval.desc = NULL;
            }
            p->value.dbval.desc = string;
            p->parmSet = True;
            break;
        }
	_XcHandlePrecludes(p,Global);
    } else {
        p->parmSet = False;
    }
    XmTextSetString(Global->_xc_Data.commandText,
                    _XcBuildCommandString(Global));
}

CreateInterface(Global)
XclipGlobalData *Global;
{
    Widget                          w;
    Widget                          form;
    Widget                          dialog_rc = NULL;
    Widget                          xgi, child;
    XmString                        xms1, xms2, xms3, xms4;
    Arg                             al[10];
    int                             ac = 0;
    Atom                            protocol;
    XCInterfaceData                *cur = Global->_xc_Data.data;
    XCHelpWidgetData *ptr = Global->_xc_Data.helpData;
    int count = 0;
    int i = 0;
    char **helpPaths;
    XmStringTable helpLabels;


    XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
    if ( Global->_xc_Data.helpData ) {

	while ( ptr ) {
	    count++;
	    ptr = ptr->next;
	}

	ptr = Global->_xc_Data.helpData;
	helpLabels = (XmStringTable)XtCalloc(count,sizeof(XmString));
	helpPaths = (char **)XtCalloc(count,sizeof(char *));
	for ( i = 0; i < count; i++, ptr = ptr->next) {
	    helpLabels[i] = XmStringCreateSimple(ptr->label);
	    helpPaths[i] = ptr->path;
	}
	XtSetArg(al[ac],XmNhelpItems, helpPaths); ac++;
	XtSetArg(al[ac],XmNhelpItemLabels, helpLabels); ac++;
	XtSetArg(al[ac],XmNhelpItemCount, count); ac++;

    }
    if (Global->standAlone) {
	Global->interactor = xgi =
	    XgCreateInteractor(Global->applShell, "xclip_dialog", al, ac);
    }
    else {
	Global->interactor = xgi =
	    XgCreateInteractorDialog(Global->applShell, "xclip_dialog", al, ac);
    }

    if ( count ) {
	for ( i = 0; i < count; i++) {
	    XmStringFree(helpLabels[i]);
	}
	XtFree(helpLabels);
	XtFree(helpPaths);
    }

    child = XgInteractorGetChild(xgi,
                                 XmINTERACT_PROMPT_LABEL);
    XtUnmanageChild(child);

    xms1 = XmStringCreateSimple("Execute");
    xms2 = XmStringCreateSimple("Reset");
    if (Global->standAlone) {
        xms3 = XmStringCreateSimple("Exit");
    }
    else {
        xms3 = XmStringCreateSimple("Cancel");
    }
    xms4 = XmStringCreateSimple(Global->_xc_Data.title);
    XtVaSetValues(xgi,
                  XmNokLabelString, xms1,
                  XmNapplyLabelString, xms2,
                  XmNcancelLabelString, xms3,
                  XmNenableWorkAreaStretch, True,
                  XmNdialogTitle, xms4,
                  NULL);
    XmStringFree(xms1);
    XmStringFree(xms2);
    XmStringFree(xms3);
    XmStringFree(xms4);

    child = XgInteractorGetChild(xgi,
                                 XmINTERACT_OK_BUTTON);
    XtAddCallback(child,
                  XmNactivateCallback, XClipExecuteCallBack,
                  CreateBunch(Global,xgi));
    XgAddHelpCallback(child,
	"Clicking on this button will cause the command\n",
	"in the \"Current Command\" area to be executed.\n",
	"The cursor will changed to an hourglass until \n",
	"the command has finished execution.",
	  NULL);

    child = XgInteractorGetChild(xgi,
                                 XmINTERACT_APPLY_BUTTON);
    XtAddCallback(child,
                  XmNactivateCallback,
                  XClipResetCallBack,
                  CreateBunch(Global,NULL));
    XgAddHelpCallback(child,
	"Activating this button will reset the interface",
	"to its original state (i.e., the way it was when",
	"you started).",
	NULL);
    XtManageChild(child);

    child = XgInteractorGetChild(xgi,
                                 XmINTERACT_CANCEL_BUTTON);
    if (Global->standAlone) {
	XtAddCallback(child,
		      XmNactivateCallback,
		      XClipCancelCallBack,
		      CreateBunch(Global,NULL));
	XgAddHelpCallback(child,
			  "Clicking on this button will quit the interface.",
			  NULL);
    }
    else {
	XtAddCallback(child,
		      XmNactivateCallback,
		      XClipCancelCallBack,
		      CreateBunch(Global,NULL));
	XgAddHelpCallback(child,
			  "Clicking on this button will quit the interface\n",
			  "after confirming it with you.",
			  NULL);
    }

    if (Global->_xc_Data.help) {
        XgAddHelpCallback(xgi,
                          Global->_xc_Data.help,
                          NULL);
    } else if ( Global->_xc_Data.helpFile.filename ) {
	_XcDoHelpFile(xgi,Global->_xc_Data.helpFile.filename);
    }
#ifdef Undefined
    {
    Widget f;
    Dimension h,w;

    f = CreateWorkArea(xgi,
                   cur,
                   True,Global);

    XtManageChild(XtParent(xgi)); /* KAB BUG ALERT TOP LEVEL CREATION ROUTINE
	RETURNS THE WRONG THING. BUG BUG BUG BUG BUG BUG BUG */
    XtManageChild(xgi);
    XtVaGetValues(f,XmNheight,&h,XmNwidth,&w,NULL);
    XtVaSetValues(XtParent(xgi),XmNheight,h,XmNwidth,w,NULL);
    XtVaSetValues((xgi),XmNheight,h,XmNwidth,w,NULL);
    }
#else
    CreateWorkArea(xgi,
                   cur,
                   True,Global);

    XtManageChild(xgi);
#endif

}

void
ManageDialog(w,
             cld,
             cad)
    Widget                          w;
    XtPointer                       cld, cad;
{
    struct Bunch *theBunch = (struct Bunch *) cld;
    XclipGlobalData *Global = theBunch->theGlobal;
    XCInterfaceData                *cur = (XCInterfaceData *) theBunch->data;

    XtManageChild(cur->xgi);
    XtManageChild(cur->shell);
    while ( cur ) {
	if ( cur->type == XC_PARM ) {
	    if ( cur->parm->type == XC_TYPE_INTEGER ||
		 cur->parm->type == XC_TYPE_FLOAT ||
		 cur->parm->type == XC_TYPE_DOUBLE ) {
		if ( cur->parm->hasModifier ) {
		    XtVaSetValues(cur->parm->scale_toggle, XmNset, 
			cur->parm->togSet, NULL);
		}
	    }
	}
	cur = cur->left;
    }
}

Widget
CreateWorkArea(interactor, cur, topLevel,Global)
    InteractorWidget                interactor;
    XCInterfaceData                *cur;
    Boolean                         topLevel;
    XclipGlobalData *Global;
{
    Widget                          form;
    Widget                          sep;
    Widget                          button;
    Widget                          child;
    Widget                          frame_t;
    Widget                          w;
    Widget                          capture_toggle;
    Widget                          rc_t;
    Widget                          attach = NULL;
    Widget                          dialog_rc = NULL;
    Widget                          lastAdded = NULL;
    Boolean                         first = True;
    XmString                        xms;
    Atom                        protocol;
    Arg al[10];
    int ac = 0;
    int length;

    form = XmCreateForm((Widget)interactor,
                        "xclip_work_area",
                        NULL,
                        0);

    if ( topLevel  == True && 
	 Global->_xc_Data.description != NULL && *Global->_xc_Data.description != NULL) {
	length = strlen(Global->_xc_Data.description);
	XtSetArg(al[ac], XmNtraversalOn, False); ac++;
	XtSetArg(al[ac], XmNcolumns, length > 50 ? 50:length); ac++;
	XtSetArg(al[ac], XmNrows, length > 50 ? 2:1 ); ac++;
	XtSetArg(al[ac], XmNwordWrap, True); ac++;
	XtSetArg(al[ac], XmNeditable, False); ac++;
	XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
	Global->_xc_Data.descText = XmCreateScrolledText(form, "desc_widget", al, ac);
	XtManageChild(Global->_xc_Data.descText);
	XtManageChild(XtParent(Global->_xc_Data.descText));

	XmTextSetString(Global->_xc_Data.descText,
			Global->_xc_Data.description);
	XtManageChild(Global->_xc_Data.descText);
	XgAddHelpCallback(Global->_xc_Data.descText,
    "This area displays a short description of the\n",
    "command that this interface was designed to execute.",
			  NULL);

	XtVaSetValues(Global->_xc_Data.descText,
		      XmNtopAttachment, XmATTACH_FORM,
		      XmNleftAttachment,
		      XmATTACH_FORM,
		      XmNrightAttachment,
		      XmATTACH_FORM,
		      NULL);

	sep = XtVaCreateManagedWidget("separator",
				      xmSeparatorWidgetClass,
				      form,
				      XmNseparatorType, XmNO_LINE,
				      NULL);

	XtVaSetValues(sep,
		      XmNtopAttachment,
		      XmATTACH_WIDGET,
		      XmNtopWidget,
		      Global->_xc_Data.descText,
		      XmNleftAttachment,
		      XmATTACH_FORM,
		      XmNrightAttachment,
		      XmATTACH_FORM,
		      NULL);
	lastAdded = sep;
	first = False;
    }

    while (cur) {
        /*
         * If we found a dialog, create a button in the dialog_rc, create an
         * interactor dialog, and attach a callback to the button that will
         * manage the new interactor dialog.
         */
        if (cur->type == XC_DIALOG) {
	    char *dotdotdotname;
	    XmString cancelString = XmStringCreateSimple("Done");

            if (dialog_rc == NULL) {
                dialog_rc = XtVaCreateManagedWidget("xclip_dialog_rc",
                                                    xmRowColumnWidgetClass,
                                                    form,
                                                    XmNorientation,
                                                    XmHORIZONTAL,
                                                    XmNpacking,
                                                    XmPACK_COLUMN,
						    XmNisAligned, 
						    True,
						    XmNentryAlignment, 
						    XmALIGNMENT_CENTER,
                                                    NULL);
            }
	    dotdotdotname = (char *)_XgMalloc(strlen(cur->name) + 4);
	    strcpy(dotdotdotname,cur->name);
	    strcat(dotdotdotname,"...");
            button = XtVaCreateManagedWidget(dotdotdotname,
                                             xmPushButtonWidgetClass,
                                             dialog_rc,
                                             XmNnavigationType,
                                             XmSTICKY_TAB_GROUP,
                                             NULL);
	    _XgFree(dotdotdotname);

            if (cur->help) {
                XgAddHelpCallback(button,
                                  cur->help,
                                  NULL);
            } else if ( cur->helpFile.filename ) {
		_XcDoHelpFile(button,cur->helpFile.filename);
	    }
	    ac = 0;
	    XtSetArg(al[ac], XmNcancelLabelString, cancelString); ac++;
            cur->xgi = XgCreateInteractorDialog(Global->interactor,
                                                "xclip_interactor_dialog",
                                                al, ac);
	    XmStringFree(cancelString);

            if (cur->help) {
                XgAddHelpCallback(cur->xgi,
                                  cur->help,
                                  NULL);
            } else if ( cur->helpFile.filename ) {
		_XcDoHelpFile(cur->xgi,cur->helpFile.filename);
	    }
            cur->shell = XtParent(cur->xgi);
	    protocol = XmInternAtom(Global->display, "WM_DELETE_WINDOW", False);
	    XmAddWMProtocols(cur->shell, &protocol, 1);
	    XtAddEventHandler(cur->shell, NoEventMask, True, 
		 _XcWMClientMessage, CreateBunch(Global,cur->shell));

            if (XmIsMotifWMRunning(Global->applShell)) {
                unsigned int                    decor_flags, func_flags;


		decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
		decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

		func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
		func_flags |= MWM_FUNC_MOVE;

                XtVaSetValues(cur->shell,
                              XmNmwmDecorations,
                              decor_flags,
                              XmNmwmFunctions,
                              func_flags,
                              NULL);
            }
            if (cur->desc)
                xms = XmStringCreateSimple(cur->desc);
            else
                xms = XmStringCreateSimple(cur->name);
            XtVaSetValues(cur->xgi,
                          XmNautoUnmanage,
                          True,
                          XmNenableWorkAreaStretch,
                          True,
                          XmNdialogTitle,
                          xms,
                          NULL);
            XmStringFree(xms);

            child = XgInteractorGetChild(cur->xgi,
                                         XmINTERACT_OK_BUTTON);
            XtUnmanageChild(child);
            child = XgInteractorGetChild(cur->xgi,
                                         XmINTERACT_APPLY_BUTTON);
            XtUnmanageChild(child);

            CreateWorkArea(cur->xgi, cur->right, False,Global);
            XtUnmanageChild(cur->shell);
            XtAddCallback(button, XmNactivateCallback, ManageDialog,
                          (XtPointer) CreateBunch(Global,cur));

        } else if (cur->type == XC_FLAG) {
            if (!cur->flag->precludes) {
            /*
             * This widget doesn't preclude any others..
             */
                xms = XmStringCreateSimple(cur->flag->desc);

                cur->flag->toggle = XtVaCreateManagedWidget("toggle",
                                                  xmToggleButtonWidgetClass,
                                                            form,
                                                          XmNnavigationType,
                                                         XmSTICKY_TAB_GROUP,
                                                            XmNindicatorType,
                                                            XmN_OF_MANY,
                                                            XmNset,
                                                            cur->flag->def,
                                                            XmNalignment,
                                                      XmALIGNMENT_BEGINNING,
                                                            XmNlabelString,
                                                            xms,
                                                            NULL);

                XmStringFree(xms);

                XtAddCallback(cur->flag->toggle,
                              XmNvalueChangedCallback,
                              ToggleCallBack,
                              (XtPointer) CreateBunch(Global,cur->flag));
            } else {
                /*
                 * This widget precludes some others..
                 */
                xms = XmStringCreateSimple(cur->flag->desc);

                cur->flag->toggle = XtVaCreateManagedWidget("toggle",
                                                  xmToggleButtonWidgetClass,
                                                            form,
                                                          XmNnavigationType,
                                                         XmSTICKY_TAB_GROUP,
                                                            XmNindicatorType,
                                                            XmONE_OF_MANY,
                                                            XmNset,
                                                            cur->flag->def,
                                                            XmNalignment,
                                                      XmALIGNMENT_BEGINNING,
                                                            XmNlabelString,
                                                            xms,
                                                            NULL);

                XmStringFree(xms);

                XtAddCallback(cur->flag->toggle,
                              XmNvalueChangedCallback,
                              RadioCallBack,
                              (XtPointer) CreateBunch(Global,cur->flag));
            }

            if (cur->flag->help) {
                XgAddHelpCallback(cur->flag->toggle,
                                  cur->flag->help,
                                  NULL);
            } else if ( cur->flag->helpFile.filename ) {
		_XcDoHelpFile(cur->flag->toggle,cur->flag->helpFile.filename);
	    }
            /*
             * Set attachments...
             */
            if (first) {
                XtVaSetValues(cur->flag->toggle,
                              XmNtopAttachment,
                              XmATTACH_FORM,
                              XmNleftAttachment,
                              XmATTACH_FORM,
                              XmNrightAttachment,
                              XmATTACH_FORM,
                              NULL);
                lastAdded = cur->flag->toggle;
                first = False;
            } else {
		XtVaSetValues(cur->flag->toggle,
			      XmNtopAttachment,
			      XmATTACH_WIDGET,
			      XmNtopWidget,
			      lastAdded,
			      XmNleftAttachment,
			      XmATTACH_FORM,
			      XmNrightAttachment,
			      XmATTACH_FORM,
			      NULL);
		lastAdded = cur->flag->toggle;
            }
        } else {                /* XC_PARM */
            switch (cur->parm->type) {
            case XC_TYPE_CHARACTER:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);
                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               XmNorientation,
                                               XmVERTICAL,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);
                w = XtVaCreateManagedWidget("parm_label",
                                            xmLabelWidgetClass,
                                            rc_t,
                                            XmNlabelString,
                                            xms,
                                            XmNtraversalOn,
                                            False,
                                            NULL);
                XmStringFree(xms);

                cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                            rc_t,
                                                          XmNnavigationType,
                                                         XmSTICKY_TAB_GROUP,
                                                            NULL);
                if (cur->parm->parmSet)
                    XmTextSetString(cur->parm->widget,
                                    cur->parm->value.cval);

                if (cur->parm->help) {
                    XgAddHelpCallback(frame_t,
                                      cur->parm->help,
                                      NULL);
                } else if ( cur->parm->helpFile.filename ) {
		    _XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		}
                XtAddCallback(cur->parm->widget,
                              XmNvalueChangedCallback,
                              XcTextCallBack,
                              (XtPointer) CreateBunch(Global,cur->parm));

                break;
            case XC_TYPE_ENUMERATE:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);
                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               XmNorientation,
                                               XmVERTICAL,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);
                w = XtVaCreateManagedWidget("parm_label",
                                            xmLabelWidgetClass,
                                            rc_t,
                                            XmNtraversalOn,
                                            False,
                                            XmNlabelString,
                                            xms,
                                            NULL);
                XmStringFree(xms);

                cur->parm->widget = XmCreateScrolledList(rc_t,
                                                         "parm_widget",
                                                         NULL,
                                                         0);
                XtVaSetValues(cur->parm->widget,
                              XmNnavigationType,
                              XmSTICKY_TAB_GROUP,
                              NULL);

                if (cur->parm->modifier.enumNItems > 8) {
                    XtVaSetValues(cur->parm->widget,
                                  XmNvisibleItemCount,
                                  8,
                                  NULL);
                } else {
                    XtVaSetValues(cur->parm->widget,
                                  XmNvisibleItemCount,
                                  cur->parm->modifier.enumNItems,
                                  NULL);
                }
                if (cur->parm->multiple) {
                    XtVaSetValues(cur->parm->widget,
                                  XmNselectionPolicy,
                                  XmMULTIPLE_SELECT,
                                  NULL);
                } else {
                    XtVaSetValues(cur->parm->widget,
                                  XmNselectionPolicy,
                                  XmSINGLE_SELECT,
                                  NULL);
                }
                XmListAddItems(cur->parm->widget,
                               cur->parm->modifier.enumTable,
                               cur->parm->modifier.enumNItems,
                               0);

                if (cur->parm->parmSet) {
                    xms = XmStringCreateSimple(cur->parm->def.cval);
                    if (XmListItemExists(cur->parm->widget,
                                         xms)) {
                        XmListSelectItem(cur->parm->widget,
                                         xms,
                                         False);
                    } else {
			Boolean ok = False;

		        if ( cur->parm->type == XC_TYPE_ENUMERATE ) {
			     xms = XmStringCreateSimple(
				 _XcEnumKeyToString(cur->parm->modifier,
						    cur->parm->def.cval));
			    if (XmListItemExists(cur->parm->widget,xms)) {
                                XmListSelectItem(cur->parm->widget,
                                         xms,
                                         False);
				ok = True;
			    }
			}
			if ( !ok ) {
			    sprintf(errorbuf,
				    "Specified default [%s] for %s",
				    cur->parm->def.cval,
				    cur->name,
				    "parameter [%s] doesn't exist.");
			    XCFatalError("creating interface",
				       errorbuf);
			}
                    }
                    XmStringFree(xms);
                }
                if (cur->parm->help) {
                    XgAddHelpCallback(frame_t,
                                      cur->parm->help,
                                      NULL);
                } else if ( cur->parm->helpFile.filename ) {
		    _XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		}
                XtAddCallback(cur->parm->widget,
                              XmNsingleSelectionCallback,
                              XCListCallBack,
                              (XtPointer) CreateBunch(Global,cur));
                XtAddCallback(cur->parm->widget,
                              XmNmultipleSelectionCallback,
                              XCListCallBack,
                              (XtPointer) CreateBunch(Global,cur));
                XtAddCallback(cur->parm->widget,
                              XmNextendedSelectionCallback,
                              XCListCallBack,
                              (XtPointer) CreateBunch(Global,cur));
                XtAddCallback(cur->parm->widget,
                              XmNdefaultActionCallback,
                              XCListCallBack,
                              (XtPointer) CreateBunch(Global,cur));

                XtManageChild(cur->parm->widget);

                break;
            case XC_TYPE_INTEGER:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);

                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               XmNorientation,
                                               XmVERTICAL,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);

                if (cur->parm->hasModifier) {
                    cur->parm->togSet = True;
                    cur->parm->scale_toggle = 
			XtVaCreateManagedWidget("parm_scale_toggle",
						     xmToggleButtonWidgetClass,
							    rc_t,
							    XmNset, True,
						XmNtraversalOn,
						False,
						XmNlabelString,
						xms,
                                                            XmNalignment,
                                                      XmALIGNMENT_BEGINNING,
						NULL);
                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
                                                         xmScaleWidgetClass,
                                                                rc_t,
                                                             XmNorientation,
                                                                XmHORIZONTAL,
                                                                XmNminimum,
                                              (int) cur->parm->modifier.min,
                                                                XmNmaximum,
                                              (int) cur->parm->modifier.max,
                                                                XmNvalue,
                                            (int) cur->parm->modifier.start,
                                                                XmNshowValue,
                                                                True,
                                                                NULL);

                    XtVaSetValues(cur->parm->widget,
                                  XmNvalue,
                             ((cur->parm->parmSet) ? cur->parm->value.ival :
                              (int) cur->parm->modifier.start),
                                  NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    XtAddCallback(cur->parm->widget,
                                  XmNdragCallback,
                                  ScaleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                    XtAddCallback(cur->parm->widget,
                                  XmNvalueChangedCallback,
                                  ScaleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                    XtAddCallback(cur->parm->scale_toggle,
                                  XmNvalueChangedCallback,
                                  ScaleToggleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                } else {
		    w = XtVaCreateManagedWidget("parm_label",
						xmLabelWidgetClass,
						rc_t,
						XmNtraversalOn,
						False,
						XmNlabelString,
						xms,
						NULL);
                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                                rc_t,
                                                                NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    if (cur->parm->parmSet) {
                        sprintf(errorbuf,
                                "%d",
                                cur->parm->value.ival);
                        XmTextSetString(cur->parm->widget,
                                        errorbuf);
                    }
                    XtAddCallback(cur->parm->widget,
                                  XmNvalueChangedCallback,
                                  XcTextCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                }
                    if (cur->parm->help) {
                        XgAddHelpCallback(frame_t,
                                          cur->parm->help,
                                          NULL);
		    } else if ( cur->parm->helpFile.filename ) {
			_XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		    }
                XmStringFree(xms);

                break;
            case XC_TYPE_FLOAT:
            case XC_TYPE_DOUBLE:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);
                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);

                if (cur->parm->hasModifier) {
                    cur->parm->togSet = True;
                    cur->parm->scale_toggle = 
			XtVaCreateManagedWidget("parm_scale_toggle",
						     xmToggleButtonWidgetClass,
							    rc_t,
							    XmNset, True,
						XmNtraversalOn,
						False,
						XmNlabelString,
						xms,
                                                            XmNalignment,
                                                      XmALIGNMENT_BEGINNING,
						NULL);
                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
                                                         xmScaleWidgetClass,
                                                                rc_t,
                                                             XmNorientation,
                                                                XmHORIZONTAL,
                                                                XmNminimum,
                                              (int) cur->parm->modifier.min,
                                                                XmNmaximum,
                                              (int) cur->parm->modifier.max,
                                                           XmNdecimalPoints,
                                    (int) cur->parm->modifier.decimalPoints,
                                                                XmNshowValue,
                                                                True,
                                                                NULL);

                    XtVaSetValues(cur->parm->widget,
                                  XmNvalue,
                                  ((cur->parm->parmSet) ?
                                   (int) (cur->parm->value.dval *
                                   Ten(cur->parm->modifier.decimalPoints)) :
                                   (int) cur->parm->modifier.start),
                                  NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    XtAddCallback(cur->parm->widget,
                                  XmNdragCallback,
                                  ScaleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                    XtAddCallback(cur->parm->widget,
                                  XmNvalueChangedCallback,
                                  ScaleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                    XtAddCallback(cur->parm->scale_toggle,
                                  XmNvalueChangedCallback,
                                  ScaleToggleCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                } else {
		    w = XtVaCreateManagedWidget("parm_label",
						xmLabelWidgetClass,
						rc_t,
						XmNtraversalOn,
						False,
						XmNlabelString,
						xms,
						NULL);
                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                                rc_t,
                                                                NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    if (cur->parm->parmSet) {
                        if (cur->parm->type == XC_TYPE_FLOAT)
                            sprintf(errorbuf,
                                    "%f",
                                    cur->parm->value.dval);
                        else
                            sprintf(errorbuf,
                                    "%lf",
                                    cur->parm->value.dval);
                        XmTextSetString(cur->parm->widget,
                                        errorbuf);
                    }
                    XtAddCallback(cur->parm->widget,
                                  XmNvalueChangedCallback,
                                  XcTextCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                }
                    if (cur->parm->help) {
                        XgAddHelpCallback(frame_t,
                                          cur->parm->help,
                                          NULL);
		    } else if ( cur->parm->helpFile.filename ) {
			_XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		    }
                XmStringFree(xms);

                break;
            case XC_TYPE_DB_ELEMENT:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);
                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);
                w = XtVaCreateManagedWidget("parm_label",
                                            xmLabelWidgetClass,
                                            rc_t,
                                            XmNtraversalOn,
                                            False,
                                            XmNlabelString,
                                            xms,
                                            NULL);
                XmStringFree(xms);

                if (!cur->parm->isInput) {
                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                                rc_t,
                                                                NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);
                    if (cur->parm->parmSet) {
                        XmTextSetString(cur->parm->widget,
                                        cur->parm->value.dbval.desc);
                    }
                } else {
                    Widget                          form_t2;
                    Widget                          browserButton;
                    Pixmap                          pixmap;
                    Pixel                           fg, bg;
                    char                           *bitmapBits;
                    int                             bitmapWidth, bitmapHeight;

                    XtVaGetValues(w,
                                  XmNforeground,
                                  &fg,
                                  XmNbackground,
                                  &bg,
                                  NULL);

                    switch (cur->parm->value.dbval.type) {
                    case XC_DB_TYPE_RASTER:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             raster_bm_bits,
                                                             raster_bm_width,
                                                           raster_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_ASCII_DLG:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             dlg_bm_bits,
                                                             dlg_bm_width,
                                                           dlg_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_DLG:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             bdlg_bm_bits,
                                                             bdlg_bm_width,
                                                           bdlg_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_ASCII_VECTOR:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             avector_bm_bits,
                                                             avector_bm_width,
                                                           avector_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_VECTOR:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             vector_bm_bits,
                                                             vector_bm_width,
                                                           vector_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_SITES:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             sites_bm_bits,
                                                             sites_bm_width,
                                                             sites_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_REGION:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             region_bm_bits,
                                                             region_bm_width,
                                                           region_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_ICON:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             icon_bm_bits,
                                                             icon_bm_width,
                                                           icon_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_LABEL:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             label_bm_bits,
                                                             label_bm_width,
                                                           label_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_IMAGE_GROUP:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             images_bm_bits,
                                                             images_bm_width,
                                                           images_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_IMAGE_SUBGROUP:
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                             images_bm_bits,
                                                             images_bm_width,
                                                           images_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
                        break;
                    case XC_DB_TYPE_USER_DEFINED:
		    /* KAB 
                        pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                           user_def_bm_bits,
                                                          user_def_bm_width,
                                                         user_def_bm_height,
                                                             fg,
                                                             bg,
                                       DefaultDepthOfScreen(Global->scrptr));
				       */
                        break;
                    }

                    form_t2 = XtVaCreateManagedWidget("form_t",
                                                    xmFormWidgetClass,
                                                    rc_t,
                                                    NULL);

                    browserButton = XtVaCreateManagedWidget("parm_widget",
                                                    xmPushButtonWidgetClass,
                                                            form_t2,
                                                            XmNlabelType,
                                                            XmPIXMAP,
                                                            XmNlabelPixmap,
                                                            pixmap,
							    XmNleftAttachment,
							    XmATTACH_FORM,
							    XmNtopAttachment,
							    XmATTACH_FORM,
							    XmNbottomAttachment,
							    XmATTACH_FORM,
                                                            NULL);
                    XtVaSetValues(browserButton,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                                form_t2,
                                                                XmNcolumns,
                                                                40,
                                                                XmNrows,
                                                                1,
                                                                XmNwordWrap,
                                                                True,
                                                                XmNeditMode,
                                                          XmMULTI_LINE_EDIT,
							    XmNleftAttachment,
							    XmATTACH_WIDGET,
							    XmNleftWidget,
							    browserButton,
							    XmNrightAttachment,
							    XmATTACH_FORM,
							    XmNtopAttachment,
							    XmATTACH_FORM,
							    XmNbottomAttachment,
							    XmATTACH_FORM,
                                                                NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    XtAddCallback(browserButton,
                                  XmNactivateCallback,
                                  DBElementCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                }

                    if (cur->parm->help) {
                        XgAddHelpCallback(frame_t,
                                          cur->parm->help,
                                          NULL);
		    } else if ( cur->parm->helpFile.filename ) {
			_XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		    }
                if (cur->parm->parmSet) {
                    XmTextSetString(cur->parm->widget,
                                    cur->parm->value.dbval.desc);
                }
                XtAddCallback(cur->parm->widget,
                              XmNvalueChangedCallback,
                              XcTextCallBack,
                              (XtPointer) CreateBunch(Global,cur->parm));

                break;
            case XC_TYPE_LOGICAL:
                xms = XmStringCreateSimple(cur->parm->desc);
                attach = cur->parm->widget =
                    XtVaCreateManagedWidget("parm_widget",
                                            xmToggleButtonWidgetClass,
                                            form,
                                            XmNindicatorType,
                                            XmN_OF_MANY,
                                            XmNset,
                                            cur->parm->def.bval,
                                            XmNalignment,
                                            XmALIGNMENT_BEGINNING,
                                            XmNlabelString,
                                            xms,
                                            NULL);
                XmStringFree(xms);
                XtVaSetValues(cur->parm->widget,
                              XmNnavigationType,
                              XmSTICKY_TAB_GROUP,
                              NULL);

                if (cur->parm->help) {
                    XgAddHelpCallback(cur->parm->widget,
                                      cur->parm->help,
                                      NULL);
                } else if ( cur->parm->helpFile.filename ) {
		    _XcDoHelpFile(cur->parm->widget,cur->parm->helpFile.filename);
		}
                XtAddCallback(cur->parm->widget,
                              XmNvalueChangedCallback,
                              ParmToggleCallBack,
                              (XtPointer) CreateBunch(Global,cur->parm));

                break;
            case XC_TYPE_FILENAME:
                attach = frame_t = XtVaCreateManagedWidget("frame_t",
                                                         xmFrameWidgetClass,
                                                           form,
                                                           NULL);
                rc_t = XtVaCreateManagedWidget("rowcolumn_t",
                                               xmRowColumnWidgetClass,
                                               frame_t,
                                               NULL);

                xms = XmStringCreateSimple(cur->parm->desc);
                w = XtVaCreateManagedWidget("parm_text",
                                            xmLabelWidgetClass,
                                            rc_t,
                                            XmNtraversalOn,
                                            False,
                                            XmNlabelString,
                                            xms,
                                            NULL);
                XmStringFree(xms);

                if (cur->parm->isInput) {
                    if ( cur->parm->hasModifier ) {
			Widget                          form_t2;
			Widget                          browserButton;
			Pixmap                          pixmap;
			Pixel                           fg, bg;

			XtVaGetValues(w,
				      XmNforeground,
				      &fg,
				      XmNbackground,
				      &bg,
				      NULL);

			pixmap = XCreatePixmapFromBitmapData(Global->display,
					      RootWindowOfScreen(Global->scrptr),
							     edit_bm_bits,
							     edit_bm_width,
							     edit_bm_height,
							     fg,
							     bg,
					   DefaultDepthOfScreen(Global->scrptr));

			form_t2 = XtVaCreateManagedWidget("form_t",
							xmFormWidgetClass,
							rc_t,
							NULL);

			browserButton = XtVaCreateManagedWidget("parm_widget",
							xmPushButtonWidgetClass,
								form_t2,
								XmNlabelType,
								XmPIXMAP,
								XmNlabelPixmap,
								pixmap,
								XmNleftAttachment,
								XmATTACH_FORM,
								XmNtopAttachment,
								XmATTACH_FORM,
								XmNbottomAttachment,
								XmATTACH_FORM,
								NULL);
			XtVaSetValues(browserButton,
				      XmNnavigationType,
				      XmSTICKY_TAB_GROUP,
				      NULL);


			cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
					  xmTextFieldWidgetClass,
						    form_t2,
						    XmNcolumns,
						    40,
						    XmNrows,
						    1,
						XmNleftAttachment,
						XmATTACH_WIDGET,
						XmNleftWidget,
						browserButton,
						XmNrightAttachment,
						XmATTACH_FORM,
						XmNtopAttachment,
						XmATTACH_FORM,
						XmNbottomAttachment,
						XmATTACH_FORM,
						    NULL);
			XtVaSetValues(cur->parm->widget,
				      XmNnavigationType,
				      XmSTICKY_TAB_GROUP,
				      NULL);

			if (cur->parm->parmSet) {
			    XmTextSetString(cur->parm->widget,
				    cur->parm->value.dbval.desc);
			}
			XtAddCallback(browserButton,
				     XmNactivateCallback,
				     FilenameEditCallBack,
				     (XtPointer) CreateBunch(Global,cur->parm));
/* XXX
                        cur->parm->value.dbval.filename = (char *)tmpnam(NULL);
                        XmTextSetString(cur->parm->widget, 
                                        cur->parm->value.dbval.filename);
*/

                    } else {
			cur->parm->widget = XtVaCreateManagedWidget(
                             "parm_widget", xmTextFieldWidgetClass, rc_t, NULL);
			XtVaSetValues(cur->parm->widget,
				      XmNnavigationType,
				      XmSTICKY_TAB_GROUP,
				      NULL);
                   }
                } else {
                    Widget                          form_t2;
                    Widget                          browserButton;
                    Pixmap                          pixmap;
                    Pixel                           fg, bg;

                    XtVaGetValues(w,
                                  XmNforeground,
                                  &fg,
                                  XmNbackground,
                                  &bg,
                                  NULL);

                    pixmap = XCreatePixmapFromBitmapData(Global->display,
                                          RootWindowOfScreen(Global->scrptr),
                                                         fs_bm_bits,
                                                         fs_bm_width,
                                                         fs_bm_height,
                                                         fg,
                                                         bg,
                                       DefaultDepthOfScreen(Global->scrptr));

                    form_t2 = XtVaCreateManagedWidget("form_t",
                                                    xmFormWidgetClass,
                                                    rc_t,
                                                    NULL);

                    browserButton = XtVaCreateManagedWidget("parm_widget",
                                                    xmPushButtonWidgetClass,
                                                            form_t2,
                                                            XmNlabelType,
                                                            XmPIXMAP,
                                                            XmNlabelPixmap,
                                                            pixmap,
							    XmNleftAttachment,
							    XmATTACH_FORM,
							    XmNtopAttachment,
							    XmATTACH_FORM,
							    XmNbottomAttachment,
							    XmATTACH_FORM,
                                                            NULL);
                    XtVaSetValues(browserButton,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    cur->parm->widget = XtVaCreateManagedWidget("parm_widget",
						      xmTextFieldWidgetClass,
                                                                form_t2,
                                                                XmNcolumns,
                                                                40,
                                                                XmNrows,
                                                                1,
                                                                XmNwordWrap,
                                                                True,
                                                                XmNeditMode,
                                                          XmMULTI_LINE_EDIT,
							    XmNleftAttachment,
							    XmATTACH_WIDGET,
							    XmNleftWidget,
							    browserButton,
							    XmNrightAttachment,
							    XmATTACH_FORM,
							    XmNtopAttachment,
							    XmATTACH_FORM,
							    XmNbottomAttachment,
							    XmATTACH_FORM,
                                                                NULL);
                    XtVaSetValues(cur->parm->widget,
                                  XmNnavigationType,
                                  XmSTICKY_TAB_GROUP,
                                  NULL);

                    if (cur->parm->parmSet) {
                        XmTextSetString(cur->parm->widget,
                                        cur->parm->value.dbval.desc);
                    }
                    XtAddCallback(browserButton,
                                  XmNactivateCallback,
                                  FilenameInputCallBack,
                                  (XtPointer) CreateBunch(Global,cur->parm));
                }
                if (cur->parm->parmSet) {
                    XmTextSetString(cur->parm->widget,
                                    cur->parm->value.cval);
                }
                if (cur->parm->help) {
                    XgAddHelpCallback(frame_t,
                                      cur->parm->help,
                                      NULL);
                } else if ( cur->parm->helpFile.filename ) {
		    _XcDoHelpFile(frame_t,cur->parm->helpFile.filename);
		}
                XtAddCallback(cur->parm->widget,
                              XmNvalueChangedCallback,
                              XcTextCallBack,
                              (XtPointer) CreateBunch(Global,cur->parm));

                break;
            }
            /*
             * Set attachments...
             */
            if (first) {
                XtVaSetValues(attach,
                              XmNtopAttachment,
                              XmATTACH_FORM,
                              XmNleftAttachment,
                              XmATTACH_FORM,
                              XmNrightAttachment,
                              XmATTACH_FORM,
                              NULL);
                lastAdded = attach;
                first = False;
            } else {
                XtVaSetValues(attach,
                              XmNtopAttachment,
                              XmATTACH_WIDGET,
                              XmNtopWidget,
                              lastAdded,
                              XmNleftAttachment,
                              XmATTACH_FORM,
                              XmNrightAttachment,
                              XmATTACH_FORM,
                              NULL);
                lastAdded = attach;
            }

        }
        cur = cur->left;
    }
    /*
     * Attach dialog row column, or the last widget added...
     */
    if (dialog_rc) {

        xms = XmStringCreateSimple("Other Options Available:");
        w = XtVaCreateManagedWidget("dialog_rc_text",
                                    xmLabelWidgetClass,
                                    form,
                                    XmNtraversalOn,
                                    False,
                                    XmNlabelString,
                                    xms,
                                    NULL);
        XmStringFree(xms);

        if (first) {
            XtVaSetValues(w,
                          XmNtopAttachment,
                          XmATTACH_FORM,
                          XmNleftAttachment,
                          XmATTACH_FORM,
                          XmNrightAttachment,
                          XmATTACH_FORM,
                          NULL);
            XtVaSetValues(dialog_rc,
                          XmNtopAttachment,
                          XmATTACH_WIDGET,
                          XmNtopWidget,
                          w,
                          XmNleftAttachment,
                          XmATTACH_FORM,
                          XmNrightAttachment,
                          XmATTACH_FORM,
                          NULL);
            if (!topLevel)
                XtVaSetValues(dialog_rc,
                              XmNbottomAttachment,
                              XmATTACH_FORM,
                              NULL);
        } else {
            XtVaSetValues(w,
                          XmNtopAttachment,
                          XmATTACH_WIDGET,
                          XmNtopWidget,
                          lastAdded,
                          XmNleftAttachment,
                          XmATTACH_FORM,
                          XmNrightAttachment,
                          XmATTACH_FORM,
                          NULL);
            XtVaSetValues(dialog_rc,
                          XmNtopAttachment,
                          XmATTACH_WIDGET,
                          XmNtopWidget,
                          w,
                          XmNleftAttachment,
                          XmATTACH_FORM,
                          XmNrightAttachment,
                          XmATTACH_FORM,
                          NULL);
            if (!topLevel)
                XtVaSetValues(dialog_rc,
                              XmNbottomAttachment,
                              XmATTACH_FORM,
                              NULL);
        }
    }
    if (!topLevel) {
        XtManageChild(form);
        return;
    }
    xms = XmStringCreateSimple("Current Command");
    w = XtVaCreateManagedWidget("command_label",
                                xmLabelWidgetClass,
                                form,
                                XmNtraversalOn,
                                False,
                                XmNlabelString,
                                xms,
                                NULL);
    XmStringFree(xms);

    ac = 0;
    XtSetArg(al[ac], XmNtraversalOn, False); ac++;
    XtSetArg(al[ac], XmNcolumns, 50); ac++;
    XtSetArg(al[ac], XmNrows, 1); ac++;
    XtSetArg(al[ac], XmNwordWrap, True); ac++;
    XtSetArg(al[ac], XmNeditable, False); ac++;
    XtSetArg(al[ac], XmNeditMode, XmMULTI_LINE_EDIT); ac++;
    Global->_xc_Data.commandText = XmCreateScrolledText(form, "command_widget", al, ac);
    XtManageChild(Global->_xc_Data.commandText);
    XtManageChild(XtParent(Global->_xc_Data.commandText));
    XmTextSetString(Global->_xc_Data.commandText, _XcBuildCommandString(Global));

    xms = XmStringCreateSimple("Capture output into the XGRASS Output Editor");
    ac = 0;
    capture_toggle = XtVaCreateManagedWidget("command_label",
                                xmToggleButtonWidgetClass,
                                form,
                                XmNtraversalOn,
                                True,
				XmNset, 
				Global->_xc_Data.capture,
                                XmNlabelString,
                                xms,
				XmNalignment,
				XmALIGNMENT_BEGINNING,
                                NULL);
    XtAddCallback(capture_toggle, XmNvalueChangedCallback, 
	CaptureToggleCallBack, CreateBunch(Global,NULL));
    XmStringFree(xms);


    XgAddHelpCallback(Global->_xc_Data.commandText,
     "This area displays the current command line as it\n",
     "would be typed at the prompt. It reflects the current\n",
     "values of flags and parameters.",
                      NULL);

    if (dialog_rc)
        attach = dialog_rc;
    else if (!first)
        attach = lastAdded;
    else
        attach = NULL;

    if (attach == NULL) {
        XtVaSetValues(w,
                      XmNtopAttachment,
                      XmATTACH_FORM,
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      NULL);
        XtVaSetValues(XtParent(Global->_xc_Data.commandText),
                      XmNtopAttachment,
                      XmATTACH_WIDGET,
                      XmNtopWidget,
                      w,
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      NULL);
        XtVaSetValues(capture_toggle,
                      XmNtopAttachment,
                      XmATTACH_WIDGET,
                      XmNtopWidget,
                      XtParent(Global->_xc_Data.commandText),
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      XmNbottomAttachment,
                      XmATTACH_FORM,
                      NULL);
    } else {
        XtVaSetValues(w,
                      XmNtopAttachment,
                      XmATTACH_WIDGET,
                      XmNtopWidget,
                      attach,
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      NULL);
        XtVaSetValues(XtParent(Global->_xc_Data.commandText),
                      XmNtopAttachment,
                      XmATTACH_WIDGET,
                      XmNtopWidget,
                      w,
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      NULL);
        XtVaSetValues(capture_toggle,
                      XmNtopAttachment,
                      XmATTACH_WIDGET,
                      XmNtopWidget,
                      XtParent(Global->_xc_Data.commandText),
                      XmNleftAttachment,
                      XmATTACH_FORM,
                      XmNrightAttachment,
                      XmATTACH_FORM,
                      XmNbottomAttachment,
                      XmATTACH_FORM,
                      NULL);
    }

    XtManageChild(form);
    return form;
}

_XcDoHelpFile(w,file)
Widget w;
char *file;
{
    FILE *fp, *fopen();
    struct stat statbuf;
    char *string, *ptr;
    int length;

    if ((fp = fopen(file,"r")) == NULL ) {
	sprintf(errorbuf,"Can't open help file \"%s\"",file);
	XCWarning("adding help file", errorbuf);
	return;
    }
    if ( stat(file,&statbuf) == 0 )
	length = statbuf.st_size;
    else
	length = 10000000;

    if ( length > 0 ) {
	string = (char *)_XgMalloc(length);
	fread(string,sizeof(char), length, fp);
    }
    ptr = string;
    while ( *ptr ) {
	if ( *ptr == '\n' ) 
	    *ptr = '\012';
	else if ( !isprint(*ptr) ) 
	    *ptr = ' ';
	ptr++;
    }
    fclose(fp);
    XgAddHelpCallback(w,string,NULL);
}

_XcParseHelpData(hptr,s)
XCHelpWidgetData *hptr;
char *s;
{
    char *t;

    if ( (t = G_index(s,':')) == NULL ) return 0;
    *t = '\0';
    hptr->path = (char *)_XgMalloc(strlen(s) + 2 );
    strcpy(hptr->path,"@");
    strcat(hptr->path,s);
    t++;
    hptr->label = _XgStrDup(t);
}

_XcParseErrorCodes(s,err)
char *s;
int **err;
{
    char **tokens;
    int num;
    int i;

    tokens = _XgTokenize(s,",");
    num = _XgNumberOfTokens(tokens);

    if ( num == 0 ) return num;
    *err = (int *)XtCalloc(num,sizeof(int));
    for ( i = 0; i < num; i++ ) {
	char *ptr = tokens[i];

	while( *ptr ) {
            if ( !(isdigit(*ptr) || (i == 0 && *ptr == '-')) ) {
		_XgFreeTokens (tokens);
		return -1;
	    }
	    ptr++;
	}
	(*err)[i] = atoi(tokens[i]);
    }
    _XgFreeTokens (tokens);
    return num;
}
