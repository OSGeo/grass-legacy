/**********************************************************************
   toggle.c     - create toggle object
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/
#include "xgen.h"

Widget
CreateToggle(object, widget)
    InterfaceObject                *object;
    Widget                          widget;
{
    static int                      numtoggles = 0;
    char                            togglename[80];
    static int                      numtoggleEls = 0;
    char                            toggleElname[80];
    int                             n;
    Widget                          toggleW;
    Resource                       *resource;
    Resource                       *nresource;
    char                           *sep;
    char                          **tokens = NULL;
    ToggleData                     *toggleData;
    ToggleData                     *curToggle;
    Boolean                         radio = False;
    Boolean                         oneIsOn = False;
    Boolean                         first = True;
    Boolean                         hOrient = False;
    Boolean                         vOrient = True;
    Boolean                         hasToggleState = False;
    Shell                          *shell;
    Resource                       *sres;
    Dimension                       sWidth = 0, sHeight = 0;

    numtoggles++;
    sprintf(togglename, "toggle%03d", numtoggles);
    curToggle = toggleData = (ToggleData *) XtMalloc(sizeof(ToggleData));
    bzero((char *) curToggle, sizeof(ToggleData));

    n = 0;
    SetGlobalArgs(&n, NOFONTS);
    /* KAB - add other resources in here... */
    SetObjectGeometryArgs(object, &n);
    SetObjectColorArgs(object, &n);
    SetObjectAlignmentArgs(object, &n);
    if ((NULL != (resource = IndexResource((char *) object, OBJECT, "orientation")))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        if (!strcmp(resource->val.cval, "horizontal")) {
            XtSetArg(args[n], XmNorientation, XmHORIZONTAL);
            n++;
	    hOrient = True;
	    vOrient = False;
        } else if (!strcmp(resource->val.cval, "vertical")) {
            XtSetArg(args[n], XmNorientation, XmVERTICAL);
            n++;
        } else {

            sprintf(errorbuf, "illegal toggle orientation in object [%s]",
                    object->name);
            XgenWarning("create toggle", errorbuf);
        }
    }
    if ((NULL != (resource = IndexResource((char *) object, OBJECT, "columns")))) {
        if (resource->variable)
            ExpandVariable(resource, (char *) object, OBJECT);
        XtSetArg(args[n], XmNcolumns, resource->val.ival);
        n++;
	shell = IndexShellByObject(object);
	if ((NULL != (sres = IndexResource((char *)shell, SHELL, "width")))) {
	    if ( sres->variable)
			ExpandVariable(sres, (char *) shell, SHELL);
	    sWidth = sres->val.ival;
	}
	if ((NULL != (sres = IndexResource((char *)shell, SHELL, "height")))) {
	    if ( sres->variable)
			ExpandVariable(sres, (char *) shell, SHELL);
	    sHeight = sres->val.ival;
	}
	if ( hOrient && sWidth ) {
	    XtSetArg(args[n], XmNwidth, sWidth); n++;
	    XtSetArg(args[n], XmNresizeWidth, False); n++;
	}
	if ( vOrient && sHeight ) {
	    XtSetArg(args[n], XmNheight, sHeight); n++;
	    XtSetArg(args[n], XmNresizeHeight, False); n++;
	}
	     
    }
    if ((NULL != (resource = IndexResource((char *) object, OBJECT, "toggletype"))) &&
            (!strcmp(resource->val.cval, "radio"))) {
        XtSetArg(args[n], XmNentryClass, xmToggleButtonWidgetClass);
        n++;
        XtSetArg(args[n], XmNpacking, XmPACK_TIGHT);
        n++;
        toggleW = XmCreateRadioBox(widget, togglename, args, n);
        radio = True;
    } else {
        XtSetArg(args[n], XmNpacking, XmPACK_TIGHT);
        n++;
        toggleW = XmCreateRowColumn(widget, togglename, args, n);
    }
    XtManageChild(toggleW);
    XmAddTabGroup(toggleW);
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "togglestate"))) {
	if (resource->variable)
	    ExpandVariable(resource, (char *) object, OBJECT);
	if (NULL != (nresource = IndexResource((char *) object, OBJECT, "listseparator"))) {

	    if (nresource->variable)
		ExpandVariable(nresource, (char *) object, OBJECT);
	    sep = nresource->val.cval;
	} else {
	    sep = ",";
	}
	hasToggleState = True;
        tokens = (char **)Tokenize(resource->val.cval,sep);
    }
    if (NULL != (resource = IndexResource((char *) object, OBJECT, "listelement"))) {
        while (resource) {
            if (!strcmp(resource->name, "listelement")) {
                XmString                        xmstring;
                int                             numTok;

                if (!first) {
                    curToggle->next = (ToggleData *) XtMalloc(sizeof(ToggleData));
                    bzero((char *) curToggle->next, sizeof(ToggleData));
                    curToggle = curToggle->next;
                }
                first = False;
                numtoggleEls++;
                sprintf(toggleElname, "toggle%03d", numtoggleEls);

                n = 0;
                SetGlobalArgs(&n, NOFONTS);
                /* KAB - add other resources in here... */
                SetObjectGeometryArgs(object, &n);
                SetObjectColorArgs(object, &n);
                SetObjectAlignmentArgs(object, &n);
                SetObjectFont(object, &n);
                XtSetArg(args[n], XmNlabelType, XmSTRING);
                n++;
                if (resource->variable)
                    ExpandVariable(resource, (char *) object, OBJECT);
                curToggle->name = XtMalloc(strlen(resource->val.cval) + 1);
                strcpy(curToggle->name, resource->val.cval);
		curToggle->set = False;
		if ( hasToggleState ) {
		    numTok = 0;
		    while ( tokens[numTok] ) {
			if ( !strcmp(tokens[numTok], curToggle->name) ) {
			    if ( (radio && !oneIsOn) || !radio ) {
				curToggle->set = True;
				XtSetArg(args[n], XmNset, True);
				n++;
				oneIsOn = True;
			    }
			}
			numTok++;
		    }
		}
                xmstring = XmStringLtoRCreate(resource->val.cval, SDC);
                XtSetArg(args[n], XmNlabelString, xmstring);
                n++;
                XtSetArg(args[n], XmNfillOnSelect, True);
                n++;
                XtSetArg(args[n], XmNindicatorOn, True);
                n++;
                if (!radio) {
                    curToggle->radioIgnoreOff = False;
                    XtSetArg(args[n], XmNindicatorType, XmN_OF_MANY);
                    n++;
                }
                curToggle->widget = XmCreateToggleButton(toggleW,
                                               resource->val.cval, args, n);
                XtManageChild(curToggle->widget);
                XmAddTabGroup(curToggle->widget);

                if (radio)
                    XtAddCallback(curToggle->widget, XmNvalueChangedCallback,
                                  RadioChangedCB, (caddr_t) object);
                else
                    XtAddCallback(curToggle->widget, XmNvalueChangedCallback,
                                  ToggleChangedCB, (caddr_t) object);

            }
            resource = resource->next;
        }
    } else {
        XgenFatalError("while creating toggle object", "no toggle list elements");
    }
    if ( hasToggleState ) 
	FreeTokens(tokens);
    AddToggleInfo(object->name, toggleData);
    return toggleW;
}
