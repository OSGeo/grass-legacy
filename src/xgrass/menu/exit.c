static char rcsid[] = "@(#)XGRASS $Id: exit.c,v 0.0.0.1 1992/05/05 14:58:31 kurt Exp kurt $";
/*
 * File: exit.c
 *
 * Desc: contains code for exiting XGRASS
 *
 * Auth: Kurt Buehler
 *
 * Date: Tue Oct 22 11:28:35 CDT 1991
 *
 * Modification History:
 *
 *
 */

#include "xgrass.h"

extern char *version;

XgrassExit(exit_code)
int exit_code;
{
    if ( _XG_Global.menuRunning ) 
	_XgSaveSession(&exit_code);
    else 
	_XgExitVerify(&exit_code);
}

extern char theDatabase[], theLocation[], theMapset[], theSession[];

static void
#ifdef _NO_PROTO
ReadCurrentValues()
#else
ReadCurrentValues(void)
#endif
{
    char           *gisrc;
    char          **words;
    char            line[256];
    char           *stat;
    char           *home;
    FILE           *grassrc;

    gisrc = getenv("GISRC");
    if (gisrc ) {
        /* does it exist ? */
        if ( access(gisrc,F_OK) != -1 ) {
            grassrc = fopen(gisrc, "r");
        } else return;
    } else {
        return;
    }
    stat = fgets(line, 255, grassrc);
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    strcpy(theDatabase, words[1]);

    stat = fgets(line, 255, grassrc);
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    strcpy(theLocation, words[1]);

    stat = fgets(line, 255, grassrc);
    _XgStripNewLine(line);
    words = _XgTokenize(line, ": ");
    strcpy(theMapset, words[1]);
    fclose(grassrc);
}


static void 
#ifdef _NO_PROTO
_XgSaveSessionOk(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else 
_XgSaveSessionOk(Widget w,XtPointer cld,XtPointer cad)
#endif
{
    int *exit_code = (int *)cld;
    InteractorCallbackStruct *cbs = (InteractorCallbackStruct *)cad;
    char *text;
    FILE           *fp;

    char file[1024];

    ReadCurrentValues();

    XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET, &text);
    if ( (text == NULL || *text == NULL) ) {
	if ( XgYesNo(_XG_Global.applShell,"No session to save...exit anyway?")) {
	    _XgExitVerify(exit_code);
	    return;
	} else {
	    XtUnmanageChild(w);
	    return;
	}
    } else if (G_legal_filename(text) < 0) {
	if ( XgYesNo(_XG_Global.applShell,"Illegal file name...try again?")) {
	    return;
	}
    }

    sprintf(file,"%s/.xgrass/session/%s", (char *)getenv("HOME"), text);
	
    if ( (fp = fopen(file, "w")) != NULL ) {
	fprintf(fp, "GISDBASE: %s\n", theDatabase);
	fprintf(fp, "LOCATION_NAME: %s\n", theLocation);
	fprintf(fp, "MAPSET: %s\n", theMapset);
	fclose(fp);
        sprintf(file,"%s/.xgrass/histories/%s", (char *)getenv("HOME"), text);
	__XgHistorySaveToFile(file);
    } else {
	char buf[256];

	sprintf(buf,"Couldn't save to session \"%s\"",text);
	XgWarningDialog(_XG_Global.applShell,buf);
	return;
    }
    XtUnmanageChild(w);
    _XgExitVerify(exit_code);
}

static void 
#ifdef _NO_PROTO
_XgSaveSessionApply(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else 
_XgSaveSessionApply(Widget w,XtPointer cld,XtPointer cad)
#endif
{
    int *exit_code = (int *)cld;
    _XgExitVerify(exit_code);
}

static void 
#ifdef _NO_PROTO
_XgSaveSessionCancel(w, cld, cad)
Widget w;
XtPointer cld, cad;
#else 
_XgSaveSessionCancel(Widget w,XtPointer cld,XtPointer cad)
#endif
{
    XtUnmanageChild(w);
}


#ifdef _NO_PROTO
_XgSaveSession(exit_code)
int *exit_code;
#else
_XgSaveSession( int *exit_code)
#endif
{
    Widget xgi;
    XmString xms;
    XmString xms1;
    XmString xms2;
    XmString xms3;
    Arg al[5];
    int ac = 0;
    char buf[256];
    char *gisrc, *base;
    Widget txt;

    sprintf(buf, "%s Save/Exit Session Dialog", version);

    xms = (XmString)XgCreateXmStringFromFile("save_session");
    xms1 = XmStringCreateSimple("Save & Exit");
    xms2 = XmStringCreateSimple("Exit");
    xms3 = XmStringCreateSimple("Cancel");
    XtSetArg(al[ac], XmNpromptLabelString, xms); ac++;
    XtSetArg(al[ac], XmNokLabelString, xms1); ac++;
    XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
    XtSetArg(al[ac], XmNapplyLabelString, xms2); ac++;
    XtSetArg(al[ac], XmNcancelLabelString, xms3); ac++;
    xgi = XgCreateInteractorPromptDialog(_XG_Global.applShell,buf,
	al, ac);
    XtUnmanageChild(XgInteractorGetChild(xgi, XmINTERACT_HELP_BUTTON));
    XtManageChild(XgInteractorGetChild(xgi, XmINTERACT_APPLY_BUTTON));
    txt = XgInteractorGetChild(xgi, XmINTERACT_TEXT);
    gisrc = getenv("GISRC");
    if ( (base = G_rindex(gisrc,'/')) != NULL ) {
	strcpy(theSession, ++base);
    }
    XmTextSetString(txt,theSession);
    XtAddCallback(xgi, XmNokCallback, _XgSaveSessionOk, (int *)exit_code);
    XtAddCallback(xgi, XmNapplyCallback, _XgSaveSessionApply, (int *)exit_code);
    XtAddCallback(xgi, XmNcancelCallback, _XgSaveSessionCancel, (int *)exit_code);
    XtManageChild(xgi);
}
/* Add clean up callbacks to this list...*before* _XgExitOK */
static XtCallbackRec exitCBList[] = {
    {(XtCallbackProc) _XgExitOK, (caddr_t) NULL},
    {(XtCallbackProc) NULL, (caddr_t)NULL}
};

_XgExitVerify(exit_code)
int *exit_code;
{
    Widget w;
    Widget shell;
    Atom protocol;
    char buf[256];

    /* a non-zero exit code means abnormal exit...don't prompt the user */
    if ( *exit_code != 0 ) {
	XgGisrcCleanup();
	XgSystem(_XG_Global.applShell,"clean_temp", False, NULL,0);
	exit(*exit_code);
    }
    exitCBList[0].closure = (caddr_t) exit_code;

    sprintf(buf,"%s Exit Dialog", version);
    w = XmCreateQuestionDialog(_XG_Global.applShell, buf ,NULL,0);
    XtVaSetValues(w, 
	XmNmessageString, XmStringCreateLtoR("Do you really want to exit?",XmSTRING_DEFAULT_CHARSET),
	XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
	XmNcancelLabelString, XmStringCreateSimple("No"),
	XmNokLabelString, XmStringCreateSimple("Yes"),
	XmNokCallback, exitCBList,
	NULL);

    shell = XtParent(w);
    if ( XmIsMotifWMRunning(shell) ) {
	unsigned int decor_flags;

	decor_flags = MWM_DECOR_BORDER;

	XtVaSetValues(shell,
	    XmNmwmDecorations, decor_flags,
	    NULL);
    }

    XtUnmanageChild( XmMessageBoxGetChild(w,XmDIALOG_HELP_BUTTON));

    XtManageChild(w);

}

void
_XgExitOK(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    int *exit_code = (int *)cad;

    XgGisrcCleanup();
    XgSystem(_XG_Global.applShell,"clean_temp", False, NULL,0);
    exit(*exit_code);
}

XgGisrcCleanup()
{
    char *home = (char *)getenv("HOME");
    char *gislock = (char *)getenv("GIS_LOCK");

    char buf[256];

    sprintf(buf,"%s/.xgrass/session/.grassrc%s",home,gislock);
    unlink(buf);
}

XgExitOnIntr()
{
    char            file[256];
    FILE           *fp;

    if ( !_XG_Global.menuRunning ) 
	exit(0);
    ReadCurrentValues();
    sprintf(file,"%s/.xgrass/session/%s", (char *)getenv("HOME"), "abort");
    fprintf(stderr,"Saving session data as \"abort\"\n\n");
    if ( (fp = fopen(file, "w")) != NULL ) {
	fprintf(fp, "GISDBASE: %s\n", theDatabase);
	fprintf(fp, "LOCATION_NAME: %s\n", theLocation);
	fprintf(fp, "MAPSET: %s\n", theMapset);
	fclose(fp);
    }
    sprintf(file,"%s/.xgrass/histories/%s", (char *)getenv("HOME"), "abort");
    __XgHistorySaveToFile(file);
    XgGisrcCleanup();
    exit(0);
}
