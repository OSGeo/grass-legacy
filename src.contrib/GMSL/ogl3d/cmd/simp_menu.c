/* utilities for creating simple pulldown menus for specifying
   which surface is active, which vectors are displayed on which surfaces,
   and which sites are displayed on which surfaces.  There are two
   types:
	SimpleOptionMenu - lets user select a "current" option from
	the choices in the pulldown menu: displays the selected
	option next to the selection area.  Used for:
	    selecting current surface
	    selecting current vector
	    selecting current sites
	SimpleCheckBox - lets user select none, any, or all of
	its subitems.  Places check by selected items.  Used for:
	    selecting surfaces on which to display current vector
	    selecting surfaces on which to display current sites
	    (MAYBE) selecting which surfaces to actually display
*/
/* TODO: should we allow vectors/sites to be displayed on surfaces 
   that are NOT drawn? - sure, why not? */

#include "interface.h"

#define MAX_OPTIONS 15

data_cell *Dc;
/* OK, I admit it - probably should have made this global to begin with */
/* (have to here because simple callbacks only send item # as data) */




int 
init_simple_menudata (data_cell *dc)
{

    Dc = dc;
    init_cp_labels();

}

/******************************************************/
/* callbacks to simple menus go here for access to Dc */

void 
set_cur_cplane (Widget w, int opt, caddr_t call_data)
{

    Dc->CurCplane = opt;
    GS_set_cplane(opt);
    Dc->Cp_on[opt] = 1;
    set_cp_widgets(Dc);
    /* TODO: set active widget if not already */

}

void 
set_active_cplanes (Widget w, int bnum, caddr_t call_data)
{
    if(bnum < MAX_CPLANES){
	if(XmToggleButtonGadgetGetState(w)){
	    GS_set_cplane(bnum);
	    Dc->Cp_on[bnum] = 1;
	}
	else{
	    GS_unset_cplane(bnum);
	    Dc->Cp_on[bnum] = 0;
	}
    }
    else{  /* close button */
	XtUnmanageChild(XtParent(w));
    }

}

/***********************************************************************/

void
set_cur_surf(w, bnum, call_data)
Widget w;
int bnum;
XmAnyCallbackStruct *call_data;
{

    if(Dc->CurSurf >= 0 && bnum < MAX_SURFS){
	Dc->CurSurf = bnum;  

	cursurf_set_curatts(Dc);
	cursurf_update_attlabels(w, Dc, call_data);
	surf_update_displaymode(w, Dc, call_data);
    }

#ifdef DEBUG
{
surf_dm *dm;
dm = &(Dc->Surf_Settings[Dc->CurSurf]);
fprintf(stderr,"--------------prev_surf--------------\n");
fprintf(stderr,"current surface:  %d\n", Dc->CurSurf + 1);
fprintf(stderr,"draw mode:  %x\n", dm->draw_mode);
fprintf(stderr,"wire color: %x\n", dm->wire_color);
fprintf(stderr,"poly res:   %d\n", dm->polycnt);
fprintf(stderr,"wire res:   %d\n", dm->wirecnt);
}
#endif

}

/***********************************************************************/

void
set_cur_vect(w, bnum, call_data)
Widget w;
int bnum;
XmAnyCallbackStruct *call_data;
{

    if(Dc->CurVect >= 0 && bnum < MAX_VECTS){
	Dc->CurVect = bnum;  

	vect_update_displaymode(w, Dc, call_data);
    }

}

/***********************************************************************/
/* callback for surface toggle buttons in vector panel */
void
curvect_setsurf(w, snum, call_data)
Widget w;
int snum;
XmAnyCallbackStruct call_data;
{

    if(GV_surf_is_selected(Dc->hVect[Dc->CurVect], Dc->hSurf[snum]))
	GV_unselect_surf(Dc->hVect[Dc->CurVect], Dc->hSurf[snum]);
    else
	GV_select_surf(Dc->hVect[Dc->CurVect], Dc->hSurf[snum]);
	
}


/***********************************************************************/
/***********************************************************************/

void
set_cur_site(w, bnum, call_data)
Widget w;
int bnum;
XmAnyCallbackStruct *call_data;
{

    if(Dc->CurSite >= 0 && bnum < MAX_SITES){
	Dc->CurSite = bnum;  

	site_update_displaymode(w, Dc, call_data);
    }

}

/***********************************************************************/
/* callback for surface toggle buttons in site panel */
void
cursite_setsurf(w, snum, call_data)
Widget w;
int snum;
XmAnyCallbackStruct call_data;
{

    if(GP_surf_is_selected(Dc->hSite[Dc->CurSite], Dc->hSurf[snum]))
	GP_unselect_surf(Dc->hSite[Dc->CurSite], Dc->hSurf[snum]);
    else
	GP_select_surf(Dc->hSite[Dc->CurSite], Dc->hSurf[snum]);
	
}


/***********************************************************************/
/******************************************************/

/* scb is the simple callback routine which acts according to the button's
   position in the list of options */
Widget 
make_simple_options (Widget parent, char *name, char *mylabel, char *labels[], int num, int init, void (*scb)(void), Arg wargs[], int n)
{
Widget wid;
XmString stab[MAX_OPTIONS];
int i;

    if(!Dc) return(NULL);

    if (num == 0){
	num = 1;
	stab[0] = XmStringCreateSimple("None Loaded");
    }
    else{
	for (i=0; i<num; i++){
	    stab[i] = XmStringCreateSimple(labels[i]);
	}
    }

    /* fill in Arglist */
    /*
    XmNbuttons  XmStringTable (NULL)  - labels
    XmNbuttonSet int (-1) - which one is initially set
    XmNbuttonType XmButtonTypeTable (NULL) - cascade/push
    XmNoptionLabel XmString (NULL) - label to left
    XmNsimpleCallback XtCallbackProc (NULL) - see above
    XmNbuttonCount int (0) - total num to create
    or accelerators & Mnemonics
    */

    XtSetArg(wargs[n], XmNbuttonSet, init); n++;
    XtSetArg(wargs[n], XmNoptionLabel, XmStringCreateSimple(mylabel)); n++;
    XtSetArg(wargs[n], XmNsimpleCallback, scb); n++;
    XtSetArg(wargs[n], XmNbuttonCount, num); n++;
    XtSetArg(wargs[n], XmNbuttons, stab); n++;
    wid = XmCreateSimpleOptionMenu(parent, name, wargs, n);
/*
    for (i=0; i<num; i++){
	XmStringFree(stab[i]);
    }
*/

    XtManageChild(wid);
    return(wid);

}

/******************************************************/

/* scb is the simple callback routine which acts according to the button's
   position in the list.
   Goal here is to have a checkbox that pops up and stays up when
   managed until the button[num], the only pushbutton, is chosen, when
   it is unmanaged (or destroyed), so the simple callback has to know about the
   close button (and seperator?) */

Widget 
make_simple_pupcheckbox (Widget parent, char *name, char *mylabel, char *labels[], int num, int init, void (*scb)(void), Arg wargs[], int n)
{
Widget wid;
XmString stab[MAX_OPTIONS];
XmButtonType btab[MAX_OPTIONS];
int i;

    if(!Dc) return(NULL);


    /* fill in labels */
    for (i=0; i<num; i++){
	stab[i] = XmStringCreateSimple(labels[i]);
    /* fill in button types */
	btab[i] = XmCHECKBUTTON; 
    }
    /* add separator & close button */
    stab[i] = XmStringCreateSimple("");   /* need for separator? */
    btab[i] = XmSEPARATOR; 
    i++;
    stab[i] = XmStringCreateSimple("Close"); 
    btab[i] = XmPUSHBUTTON; 


    /* fill in Arglist */
    /*
    XmNbuttons  XmStringTable (NULL)  - labels
    XmNbuttonSet int (-1) - which one is initially set
    XmNbuttonType XmButtonTypeTable (NULL) - cascade/push
    XmNoptionLabel XmString (NULL) - label to left
    XmNsimpleCallback XtCallbackProc (NULL) - see above
    XmNbuttonCount int (0) - total num to create
    or accelerators & Mnemonics
    XmNrowColumnType (XmWORK_AREA) - XmMENU_PULLDOWN
    */

    XtSetArg(wargs[n], XmNbuttonSet, init); n++;
    XtSetArg(wargs[n], XmNoptionLabel, XmStringCreateSimple(mylabel)); n++;
    XtSetArg(wargs[n], XmNsimpleCallback, scb); n++;
    XtSetArg(wargs[n], XmNbuttonCount, num); n++;
    XtSetArg(wargs[n], XmNbuttonType, btab); n++;
    XtSetArg(wargs[n], XmNbuttons, stab); n++;
    wid = XmCreateSimplePopupMenu(parent, name, wargs, n);
/*
    for (i=0; i<num; i++){
	XmStringFree(stab[i]);
    }

*/
    XtManageChild(wid);

    return(wid);

}



