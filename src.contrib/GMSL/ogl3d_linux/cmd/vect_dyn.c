#include "interface.h"
#include "string.h"
#include "coldefs.h"
/*
#define DEBUG
*/

/***********************************************************************/

char **v_labels (data_cell *dc)
{
static int first = 1;
static char *V_labels[MAX_VECTS];
int i, nvects;
char *map, *p;


    if(first){
	for(i=0; i<MAX_VECTS; i++){
	    V_labels[i] = (char *)malloc((48) * sizeof(char));
	}
	first = 0;
    }
    nvects = GV_num_vects();
    for(i=0; i < nvects; i++){
	/* knock off any GRASS location suffix */
	map = p = dc->Vect_Settings[i].vectname;
	if ((char*)NULL != (p = strrchr (map, '@'))) {
                if (p != map)
                    *p = '\0';
        }
	_set_shortname(V_labels[i], map);
    }
    return(V_labels);

}


/***********************************************************************/
static void 
copy_dm (vect_dm *to, vect_dm *from)
{

	strcpy(to->vectname, from->vectname);
	to->color = from->color;
	to->width = from->width;
	to->pivpt[X] = from->pivpt[X];
	to->pivpt[Y] = from->pivpt[Y];
	to->zrot = from->zrot;
	to->xtrans = from->xtrans;
	to->ytrans = from->ytrans;
	to->ztrans = from->ztrans;

}

/***********************************************************************/

void
delete_vect(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{
int nvects;

    GV_delete_vector(dc->hVect[dc->CurVect]);
    dc->hVect[dc->CurVect] = 0;
    pack_vectinfo(dc->Vect_Settings, dc->hVect);
    nvects = GV_num_vects();
    dc->hVect[nvects] = 0;
    if(dc->CurVect) dc->CurVect--;
    else if (nvects) dc->CurVect++;
    
    update_vector_options(dc);
    vect_update_displaymode(w, dc, call_data);

/* 
    {
    int i;

    for(i=0; i<MAX_VECTS; i++)
	fprintf(stderr,"%d\n", dc->hVect[i]);
    }
DEBUG */
}

/***********************************************************************/

void
new_vect(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{

    pops(w, dc, VECTOR_FILE);

}

/***********************************************************************/
/* If name is not null, loads vector file */
int 
load_new_vector (data_cell *dc, char *name, int startup)
{
int vnum, xres, yres, xwire, ywire;
int snum, i;
char cbuf[40];
	
	vnum = GV_num_vects();
	if(vnum >= MAX_VECTS){
	    inform(dc,"Maximum vectors loaded!");
	    return(0);
	}
	dc->hVect[vnum] = GV_new_vector();

	if(name){
	    inform(dc, "Loading Data");
	    GV_load_vector(dc->hVect[vnum], name);
	    strcpy(dc->Vect_Settings[vnum].vectname, name);
	    dc->CurVect = vnum;
	    inform(dc, "Done");
	}

	GV_set_vectmode(dc->hVect[vnum],dc->Vect_Settings[vnum].use_mem, 
		dc->Vect_Settings[vnum].color, dc->Vect_Settings[vnum].width);

	/* add all defined surfaces to display list */
	snum = GS_num_surfs();
	for(i=0; i<snum; i++){
	    GV_select_surf(dc->hVect[vnum], dc->hSurf[i]); 
	}
	
	if(!startup){
	    update_vector_options(dc);
	    vect_update_displaymode(NULL, dc, NULL);
	}
	return(1);

}

/***********************************************************************/

/***********************************************************************/
/* check state of buttons & arrows, update dm values, call GV routines */ 

void
vect_set_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
vect_dm *dm;
	
	dm = &(dc->Vect_Settings[dc->CurVect]);

	if(XmToggleButtonGetState(dc->toggle_id[VECT_MEM]))
	    dm->use_mem = 1;
	else
	    dm->use_mem = 0;

	dm->width = dc->parrows[VWIDTH_ARWS].val;
	
	GV_set_vectmode(dc->hVect[dc->CurVect],dm->use_mem,dm->color,dm->width);

/* redundant now? (see vect_translate)
	GV_set_trans(dc->hVect[dc->CurVect],dm->xtrans,dm->ytrans,dm->ztrans);
*/

}
/***********************************************************************/


/* updates displaymode widgets in vector panel 
   using settings in dc->Vect_Settings[dc->CurVect]. Should
   be called whenever Current Vector changes. */

void
vect_update_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
vect_dm *curset;
char str[80];
int i,tr,tg,tb;

    curset = &(dc->Vect_Settings[dc->CurVect]);
    
    dc->parrows[VWIDTH_ARWS].val = curset->width;
    sprintf(str, "%d", dc->parrows[VWIDTH_ARWS].val);
    change_label(dc->parrows[VWIDTH_ARWS].txt, str);

    /* the following will also call the toggle button valueChanged
    callback(s) in order to untoggle those in radio boxes, so be careful.
    That's why I'm using the arm callback for these toggles. (in 
    vector_panel.c) */

    if(curset->use_mem)
	XmToggleButtonGadgetSetState(dc->toggle_id[VECT_MEM], TRUE, TRUE);
    else
	XmToggleButtonGadgetSetState(dc->toggle_id[VECT_MEM], FALSE, TRUE);

    i = dc->Vect_Settings[dc->CurVect].color;
    INT_TO_RED(i, tr);
    INT_TO_GRN(i, tg);
    INT_TO_BLU(i, tb);
    set_pixel_color(dc, dc->cells[VECT_CELL], tr, tg, tb);
    set_button_colors(dc->toggle_id[V_COLOR], dc, VECT_CELL);
    
    update_Vscroll_options(dc);


}
/***********************************************************************/

int 
update_Vscroll_options (data_cell *dc)
{
    XtUnmanageChild(dc->panels[VECTOR].form);
    _update_Vscroll_options(dc);
    XtManageChild(dc->panels[VECTOR].form);
}

/***********************************************************************/
int 
_update_Vscroll_options (data_cell *dc)
{
int n, i, nsurfs, nvects;
Arg wargs[12];
Widget wwrc, tw;
char **names;

    XtUnmanageChild(dc->Vscroll);
    XtDestroyWidget(dc->Vscroll);

    n = 0;
    SetPositionArgs(wargs, &n, 40, 80, 45, 95, NULL);
    XtSetArg(wargs[n], XmNscrollingPolicy,XmAUTOMATIC); n++;
    dc->Vscroll = XmCreateScrolledWindow(dc->panels[VECTOR].form, 
	     "on_surfs", wargs, n);	

    n = 0;
    wwrc = XtCreateManagedWidget("surf_buttons", xmRowColumnWidgetClass,
		dc->Vscroll, wargs, n);
    
    names = sf_labels(dc);
    nsurfs = GS_num_surfs();
    nvects = GV_num_vects();
    for(i=0; i < nsurfs; i++){
	n = 0;
	if(nvects)
	    if(GV_surf_is_selected(dc->hVect[dc->CurVect], dc->hSurf[i])){
		XtSetArg(wargs[n],XmNset,TRUE); n++;
	    }
	tw = XtCreateManagedWidget(names[i], xmToggleButtonGadgetClass,
		wwrc, wargs, n);
	XtAddCallback(tw, XmNdisarmCallback, curvect_setsurf, i); 
	/* toggles set state */
    }

    n = 0;
    XtSetArg(wargs[n], XmNworkWindow,wwrc); n++;
    XtSetValues(dc->Vscroll, wargs, n);

    XtManageChild(dc->Vscroll);
	    
}

/***********************************************************************/

void
curvect_change_rgb(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
vect_dm *vdm;
    
    vdm = &(dc->Vect_Settings[dc->CurVect]);
    vdm->color = rgb_int_from_pix(dc, dc->cells[VECT_CELL]);

    set_button_colors(dc->toggle_id[V_COLOR], dc, VECT_CELL);
    GV_set_vectmode(dc->hVect[dc->CurVect],vdm->use_mem,vdm->color,vdm->width);

}

/***********************************************************************/

int 
update_vector_options (data_cell *dc)
{
    XtUnmanageChild(dc->panels[VECTOR].form);
    _update_vector_options(dc);
    XtManageChild(dc->panels[VECTOR].form);
}

/***********************************************************************/
int 
_update_vector_options (data_cell *dc)
{
int n;
Arg wargs[12];

    XtUnmanageChild(dc->RCurVectno);
    XtDestroyWidget(dc->RCurVectno);

    n = 0;
    SetPositionArgs(wargs, &n, 10,-1,2,-1, XmATTACH_NONE);
    dc->RCurVectno = make_simple_options(dc->panels[VECTOR].form,
	    "wcur_v", "Current:", v_labels(dc),
	    GV_num_vects(), dc->CurVect, set_cur_vect, wargs, n);
}

/***********************************************************************/

/***********************************************************************/

void 
vect_translate (Widget w, data_cell *dc, XEvent *event)
{
vect_dm *dm;
   
    dm = &(dc->Vect_Settings[dc->CurVect]);

    GV_set_trans(dc->hVect[dc->CurVect], dm->xtrans, dm->ytrans, dm->ztrans);
/*
    quick_draw(dc);
*/

}
/***********************************************************************/

int 
pack_vectinfo (vect_dm dms[MAX_VECTS], int handles[MAX_VECTS])
{
int i, j;
    
    for(i=0; i < MAX_VECTS; i++){
	if(!handles[i]){
	    for(j=i; j < MAX_VECTS-1; j++){
		if (handles[j+1]){
		    copy_dm(&dms[j], &dms[j+1]);
		}
	    }
	}
    }
    pack_handles(handles, MAX_VECTS);

}
/***********************************************************************/
/***********************************************************************/

