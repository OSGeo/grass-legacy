
/* close_me:
** this function merely closes a panel when its "close" button is pressed
*/
	
#include "interface.h"


void 
close_me (Widget w, data_cell *dc, caddr_t client_data)
{	
    Widget match, parent;
    int i;

/*
    if(!dc->Wset){
	Arg wargs[3];
	int n;

	n = 0;
	XtSetArg(wargs[n], XmNset, FALSE); n++;
	XtSetValues(dc->Wwhatshere, wargs,n);
	dc->Wset = 1;
	XtRemoveEventHandler(dc->monitor, ButtonPressMask, FALSE, show_it, dc);
    }
*/
    /*-------------------------------------------------------------
    find the parent of the button that called this routine     --
    -------------------------------------------------------------*/
    parent = XtParent(w);

    /*---------------------------------------------------------------
    if a panel is already open, it cannot be opened again. If it --
    is going to be closed, change its open/close identifier      -- 
    to allow further use.                                        --
    ---------------------------------------------------------------*/
    for(i = 0; i < MAX_PANELS ; i++){
	if(parent == dc->panels[i].form) {

	    dc->panels[i].im_already_open = 0;

	    if(dc->panels[i].where == TOP)
		dc->top =  0;

	    else if(dc->panels[i].where == BOTTOM)
		dc->bott =  0;

	    else if(dc->panels[i].where == ALL)
		dc->bott = dc->top =  0;

    /* ---------------------------------------------------------
    the whats here panel has an extra indentifier that needs -
    to be updated but only if that panel is being closed, so -
    this code segment must go inside of this if statement    -
    ----------------------------------------------------------*/
	    if(i == WHAT)
		dc->current_position = 0;

	    if(i == LIGHT || i == SURFACE || i == ANIM)  /* supers */
		dc->previous = 0;
	}
    }


    /*-------------------------------------------------------
    now that the preliminaries are done, close the panel --
    -------------------------------------------------------*/
    XtUnmanageChild(parent);

}



void 
close_destroy_me (Widget w, data_cell *dc, caddr_t client_data)
{	
    Widget match, parent;
    int i;


    if(!dc->Wset){
	Arg wargs[3];
	int n;

	n = 0;
	XtSetArg(wargs[n], XmNset, FALSE); n++;
	XtSetValues(dc->Wwhatshere, wargs,n);
	dc->Wset = 1;
	XtRemoveEventHandler(dc->monitor, ButtonPressMask, FALSE, show_it, dc);
    }

    /*-------------------------------------------------------------
    find the parent of the button that called this routine     --
    -------------------------------------------------------------*/
    parent = XtParent(w);

    /*---------------------------------------------------------------
    if a panel is already open, it cannot be opened again. If it --
    is going to be closed, change its open/close identifier      -- 
    to allow further use.                                        --
    ---------------------------------------------------------------*/
    for(i = 0; i < MAX_PANELS ; i++){
	if(parent == dc->panels[i].form) {

	    dc->panels[i].im_already_open = 0;

	    if(dc->panels[i].where == TOP)
		dc->top =  0;

	    else if(dc->panels[i].where == BOTTOM)
		dc->bott =  0;

	    else if(dc->panels[i].where == ALL)
		dc->bott = dc->top =  0;

    /* ---------------------------------------------------------
    the whats here panel has an extra indentifier that needs -
    to be updated but only if that panel is being closed, so -
    this code segment must go inside of this if statement    -
    ----------------------------------------------------------*/
	    if(i == WHAT)
		dc->current_position = 0;

	    if(i == LIGHT || i == SURFACE)  /* supers */
		dc->previous = 0;
	}
    }


    /*-------------------------------------------------------
    now that the preliminaries are done, close the panel --
    -------------------------------------------------------*/
    XtDestroyWidget(parent);

}

