
/* pops:
** controls all of the pop up dialogs
*/


#include "interface.h"	


/* OK callback used when getting new filenames for attributes */
static void 
GetAttFileCallback (Widget w, data_cell *dc)
{
char *s;
XmString result;

     XtVaGetValues(w,XmNresultString,&result,NULL);
     XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);

     /* TODO: if(validfile){ } */
    if(s){

	strcpy(dc->Cur_Atts[dc->CurAtt].map_name, s);

	if(dc->Cur_Atts[dc->CurAtt].map_name[0]){
	    dc->Cur_Atts[dc->CurAtt].use_map = 1;
	    curatt_update_status(w, dc, NULL);
	}
    }
}

/***********************************************************/

/* OK callback used when getting new filenames for vectors */
static void 
GetVectFileCallback (Widget w, data_cell *dc)
{
char *s;
XmString result;

     XtVaGetValues(w,XmNresultString,&result,NULL);
     XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);

     /* TODO: if(validfile){ } */
    if(s){
	load_new_vector(dc, s, 0);
    }

}

/***********************************************************/

/* OK callback used when getting new filenames for sites */
static void 
GetSiteFileCallback (Widget w, data_cell *dc)
{
char *s;
XmString result;

     XtVaGetValues(w,XmNresultString,&result,NULL);
     XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);

     /* TODO: if(validfile){ } */
    if(s){
	load_new_site(dc, s, 0);
    }

}

/***********************************************************/

/* OK callback used when getting new filename for sites COLOR (Raster) */
static void 
GetSiteColFileCallback (Widget w, data_cell *dc)
{
char *s;
XmString result;
int ret;

     XtVaGetValues(w,XmNresultString,&result,NULL);
     XmStringGetLtoR(result,XmSTRING_DEFAULT_CHARSET,&s);

     /* TODO: if(validfile){ } */
    if(s){
	ret = GP_attmode_color(dc->hSite[dc->CurSite], s);
	/* check for success! */
	if(ret<=0) inform(dc, "unable to load color file"); 
	else {
	    inform(dc, "color file loaded"); 
	    dc->Site_Settings[dc->CurSite].attrmode = ST_ATT_COLOR;
	}
    }

}

/***********************************************************/

void 
pops (Widget cwid, data_cell *dc, int constant)

{

    static XmString str, str2;
    Widget dialog; 
    Arg wargs[15];
    int n, type, mult=0;
    XtCallbackProc cb;

#ifdef DEBUG
fprintf(stderr,"pops called\n");
#endif

    if(constant > 0 && constant < MAX_PANELS) {	
	str2 = XmStringCreateSimple(" is already open.");
	str = XmStringConcat(dc->panels[constant].name, str2);

	n = 0;
	XtSetArg(wargs[n],XmNmessageString, str); n++;
	XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);
	n++;

	dialog = XmCreateMessageDialog(dc->form_for_main_control_panel,
		    "Whoa", wargs,n);

	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
	XtManageChild(dialog);
    }


    else {
	switch (constant) {
	    case COLOR_FILE :
		/*  mult = 1; */
		type = XG_RASTER;
		str = XmStringCreateSimple("Choose Color Files(s)");
		cb = GetAttFileCallback;
		break;
	    
	    case ELEVATION_FILE :
		type = XG_RASTER;
		str = XmStringCreateSimple("Choose Elevation File");
		cb = GetAttFileCallback;
		break;
	    
	    case VECTOR_FILE :
		type = XG_VECTOR;
		str = XmStringCreateSimple("Choose Vector File");
		cb = GetVectFileCallback;
		break;
	    
	    case SITES_FILE :
		type = XG_SITE;
		str = XmStringCreateSimple("Choose Sites File");
		cb = GetSiteFileCallback;
		break;

	    case SITES_COL_FILE :
		type = XG_RASTER;
		str=XmStringCreateSimple("Choose Raster File for Color Table");
		cb = GetSiteColFileCallback;
		break;

	    case RASTER_FILE :
	    default:
		type = XG_RASTER;
		str = XmStringCreateSimple("Choose Raster File");
		cb = GetAttFileCallback;
		break;
	    
	}

	if(!cwid) cwid = dc->toplevel;

	n = 0;
	if(mult){ 
	    XtSetArg (wargs[n], XmNselMode, XG_MULTIPLE_SELECT); 
	    n++; 
	}
	XtSetArg(wargs[n],XmNdialogStyle,
			    XmDIALOG_FULL_APPLICATION_MODAL); n++;
	XtSetArg (wargs[n], XmNbrowseMode, type); n++;
	XtSetArg(wargs[n], XmNpromptLabelString, str); n++;
	XtSetArg (wargs[n], XmNnumLists, 1); n++;
	dialog = XgCreateBrowserDialog(cwid, "Grazer", wargs, n);
	XtAddCallback(dialog,XmNcancelCallback,destroy_cb, dialog);
	XtAddCallback(dialog,XmNokCallback,cb, dc);
	XtAddCallback(dialog,XmNokCallback,destroy_cb, dialog);

	XtUnmanageChild(XgInteractorGetChild(dialog, XmINTERACT_HELP_BUTTON));
	XtManageChild(XgInteractorGetChild(dialog, XmINTERACT_PROMPT_LABEL));
	XtManageChild(dialog);
    }

    /* These are static! 
    XmStringFree(str);
    XmStringFree(str2);
    */

    return;
}


void 
nyi_pops (Widget cwid, data_cell *dc, caddr_t ignored)

{

    static XmString str;
    Widget dialog; 
    Arg wargs[15];
    int n;

#ifdef DEBUG
fprintf(stderr,"nyi_pops called\n");
#endif

	str = XmStringCreateSimple("This feature not yet implemented!");

	n = 0;
	XtSetArg(wargs[n],XmNmessageString, str); n++;
	XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);
	n++;

	dialog = XmCreateMessageDialog(dc->form_for_main_control_panel,
		    "Oops", wargs,n);

	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
	XtManageChild(dialog);

    XmStringFree(str);
    return;

}


void 
bf_unavail (Widget cwid, data_cell *dc, caddr_t ignored)

{

    static XmString str;
    Widget dialog; 
    Arg wargs[15];
    int n;


	str = XmStringCreateSimple("Transparency unavailable on this machine.");

	n = 0;
	XtSetArg(wargs[n],XmNmessageString, str); n++;
	XtSetArg(wargs[n],XmNdialogStyle,XmDIALOG_FULL_APPLICATION_MODAL);
	n++;

	dialog = XmCreateMessageDialog(dc->form_for_main_control_panel,
		    "Oops", wargs,n);

	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
	XtManageChild(dialog);

    XmStringFree(str);
    return;

}

