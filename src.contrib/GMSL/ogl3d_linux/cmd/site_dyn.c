#include "interface.h"
#include "string.h"
#include "coldefs.h"
/*
#define DEBUG
*/

/***********************************************************************/

char **s_labels (data_cell *dc)
{
static int first = 1;
static char *S_labels[MAX_SITES];
int i, nsites;
char *map, *p;


    if(first){
	for(i=0; i<MAX_SITES; i++){
	    S_labels[i] = (char *)malloc((48) * sizeof(char));
	}
	first = 0;
    }
    nsites = GP_num_sites();
    for(i=0; i < nsites; i++){
	/* knock off any GRASS location suffix */
	map = p = dc->Site_Settings[i].sitename;
	if ((char*)NULL != (p = strrchr (map, '@'))) {
                if (p != map)
                    *p = '\0';
        }
	_set_shortname(S_labels[i], map);
    }
    return(S_labels);

}


/***********************************************************************/
static void 
copy_dm (site_dm *to, site_dm *from)
{

	strcpy(to->sitename, from->sitename);
	to->color = from->color;
	to->marker = from->marker;
	to->width = from->width;
	to->attrmode = from->attrmode;
	to->size = from->size;
	to->has_z = from->has_z;
	to->use_z = from->use_z;
	to->has_att = from->has_att;
	to->pivpt[X] = from->pivpt[X];
	to->pivpt[Y] = from->pivpt[Y];
	to->zrot = from->zrot;
	to->xtrans = from->xtrans;
	to->ytrans = from->ytrans;
	to->ztrans = from->ztrans;

}

/***********************************************************************/

void
delete_site(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{
int nsites;

    GP_delete_site(dc->hSite[dc->CurSite]);
    dc->hSite[dc->CurSite] = 0;
    pack_siteinfo(dc->Site_Settings, dc->hSite);
    nsites = GP_num_sites();
    dc->hSite[nsites] = 0;
    if(dc->CurSite) dc->CurSite--;
    else if (nsites) dc->CurSite++;
    
    update_site_options(dc);
    site_update_displaymode(w, dc, call_data);

/* 
    {
    int i;

    for(i=0; i<MAX_SITES; i++)
	fprintf(stderr,"%d\n", dc->hSite[i]);
    }
DEBUG */
}

/***********************************************************************/

void
new_site(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{

    pops(w, dc, SITES_FILE);

}

/***********************************************************************/
/* If name is not null, loads site file */
int 
load_new_site (data_cell *dc, char *name, int startup)
{
int stnum, xres, yres, xwire, ywire;
int snum, i;
char cbuf[40];
site_dm *dm;
	
	stnum = GP_num_sites();
	if(stnum >= MAX_SITES){
	    inform(dc,"Maximum sites loaded!");
	    return(0);
	}
	dc->hSite[stnum] = GP_new_site();

	if(name){
	    inform(dc, "Loading Data");
	    GP_load_site(dc->hSite[stnum], name);
	    strcpy(dc->Site_Settings[stnum].sitename, name);
	    dc->CurSite = stnum;
	    inform(dc, "Done");
	}
	
	dm = &(dc->Site_Settings[stnum]);
	dm->use_z = dm->has_z = dm->has_att = 0;
	dm->attrmode = ST_ATT_NONE;
	dm->color = 0xFFFFFF;
	dm->width = 2;
	dm->size = (float)SLIDER_VAL_REAL(dc, SITE_SIZ);
	dm->marker = ST_X;


	GP_set_sitemode(dc->hSite[stnum],dc->Site_Settings[stnum].attrmode, 
		dc->Site_Settings[stnum].color, dc->Site_Settings[stnum].width,
		dc->Site_Settings[stnum].size, dc->Site_Settings[stnum].marker);

	/* add all defined surfaces to display list */
	snum = GS_num_surfs();
	for(i=0; i<snum; i++){
	    GP_select_surf(dc->hSite[stnum], dc->hSurf[i]); 
	}
	
	if(!startup){
	    update_site_options(dc);
	    site_update_displaymode(NULL, dc, NULL);
	}
	return(1);

}

/***********************************************************************/

/***********************************************************************/
/* check state of buttons & arrows, update dm values, call GP routines */ 

void
site_set_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
site_dm *dm;
	
	dm = &(dc->Site_Settings[dc->CurSite]);

	if(XmToggleButtonGetState(dc->toggle_id[SITE_X]))
	    dm->marker = ST_X;
	if(XmToggleButtonGetState(dc->toggle_id[SITE_SPHERE]))
	    dm->marker = ST_SPHERE;
	if(XmToggleButtonGetState(dc->toggle_id[SITE_DIAMOND]))
	    dm->marker = ST_DIAMOND;

	if(XmToggleButtonGetState(dc->toggle_id[SITE_3D])){
	    dm->has_z = (0 < GP_set_zmode(dc->hSite[dc->CurSite], 1));
	    dm->use_z = dm->has_z;
	    if(!dm->has_z)
		XmToggleButtonSetState(dc->toggle_id[SITE_3D], FALSE, TRUE);
	}
	else
	    GP_set_zmode(dc->hSite[dc->CurSite], 0);

	dm->width = dc->parrows[SWIDTH_ARWS].val;
	dm->size = (float)SLIDER_VAL_REAL(dc, SITE_SIZ);
	
	GP_set_sitemode(dc->hSite[dc->CurSite],dm->attrmode, 
		dm->color, dm->width, dm->size, dm->marker);


}
/***********************************************************************/


/* updates displaymode widgets in site panel 
   using settings in dc->Site_Settings[dc->CurSite]. Should
   be called whenever Current Site changes. */

void
site_update_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
site_dm *curset;
char str[80];
int i,tr,tg,tb;

    curset = &(dc->Site_Settings[dc->CurSite]);
    

    dc->parrows[SWIDTH_ARWS].val = curset->width;
    sprintf(str, "%d", dc->parrows[SWIDTH_ARWS].val);
    change_label(dc->parrows[SWIDTH_ARWS].txt, str);

    /* the following will also call the toggle button valueChanged
    callback(s) in order to untoggle those in radio boxes, so be careful.
    That's why I'm using the arm callback for these toggles. (in 
    site_panel.c) */

    if(curset->marker == ST_X){
        XmToggleButtonSetState(dc->toggle_id[SITE_X], TRUE, TRUE);
    }
    if(curset->marker == ST_SPHERE){
        XmToggleButtonSetState(dc->toggle_id[SITE_SPHERE], TRUE, TRUE);
    }
    if(curset->marker == ST_DIAMOND){
        XmToggleButtonSetState(dc->toggle_id[SITE_DIAMOND], TRUE, TRUE);
    }

    if(curset->use_z)
	XmToggleButtonSetState(dc->toggle_id[SITE_3D], TRUE, TRUE);
    else
	XmToggleButtonSetState(dc->toggle_id[SITE_3D], FALSE, TRUE);

    if(curset->attrmode != ST_ATT_COLOR){
	i = dc->Site_Settings[dc->CurSite].color;
	INT_TO_RED(i, tr);
	INT_TO_GRN(i, tg);
	INT_TO_BLU(i, tb);
	set_pixel_color(dc, dc->cells[SITES_CELL], tr, tg, tb);
	set_button_colors(dc->toggle_id[S_COLOR], dc, SITES_CELL);
    }
    else
	unset_button_colors(dc->toggle_id[S_COLOR], dc);
    
    update_Sscroll_options(dc);


}
/***********************************************************************/

int 
update_Sscroll_options (data_cell *dc)
{
    XtUnmanageChild(dc->panels[SITES].form);
    _update_Sscroll_options(dc);
    XtManageChild(dc->panels[SITES].form);
}

/***********************************************************************/
int 
_update_Sscroll_options (data_cell *dc)
{
int n, i, nsurfs, nsites;
Arg wargs[12];
Widget wwrc, tw;
char **names;

    XtUnmanageChild(dc->Sscroll);
    XtDestroyWidget(dc->Sscroll);

    n = 0;
    SetPositionArgs(wargs, &n, 50, 80, 45, 95, NULL);
    XtSetArg(wargs[n], XmNscrollingPolicy,XmAUTOMATIC); n++;
    dc->Sscroll = XmCreateScrolledWindow(dc->panels[SITES].form, 
	     "on_surfs", wargs, n);	

    n = 0;
    wwrc = XtCreateManagedWidget("surf_buttons", xmRowColumnWidgetClass,
		dc->Sscroll, wargs, n);
    
    names = sf_labels(dc);
    nsurfs = GS_num_surfs();
    nsites = GP_num_sites();
    for(i=0; i < nsurfs; i++){
	n = 0;
	if(nsites)
	    if(GP_surf_is_selected(dc->hSite[dc->CurSite], dc->hSurf[i])){
		XtSetArg(wargs[n],XmNset,TRUE); n++;
	    }
	tw = XtCreateManagedWidget(names[i], xmToggleButtonGadgetClass,
		wwrc, wargs, n);
	XtAddCallback(tw, XmNdisarmCallback, cursite_setsurf, i); 
	/* toggles set state */
    }

    n = 0;
    XtSetArg(wargs[n], XmNworkWindow,wwrc); n++;
    XtSetValues(dc->Sscroll, wargs, n);

    XtManageChild(dc->Sscroll);
	    
}

/***********************************************************************/

void
cursite_change_rgb(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
site_dm *dm;
    
    dm = &(dc->Site_Settings[dc->CurSite]);
    dm->color = rgb_int_from_pix(dc, dc->cells[SITES_CELL]);

    if(dm->attrmode != ST_ATT_COLOR)
	set_button_colors(dc->toggle_id[S_COLOR], dc, SITES_CELL);
    else
	unset_button_colors(dc->toggle_id[S_COLOR], dc);

    GP_set_sitemode(dc->hSite[dc->CurSite],dm->attrmode, 
		dm->color, dm->width, dm->size, dm->marker);

}

/***********************************************************************/

int 
update_site_options (data_cell *dc)
{
    XtUnmanageChild(dc->panels[SITES].form);
    _update_site_options(dc);
    XtManageChild(dc->panels[SITES].form);
}

/***********************************************************************/
int 
_update_site_options (data_cell *dc)
{
int n;
Arg wargs[12];

    XtUnmanageChild(dc->RCurSiteno);
    XtDestroyWidget(dc->RCurSiteno);

    n = 0;
    SetPositionArgs(wargs, &n, 10,-1,2,-1, XmATTACH_NONE);
    dc->RCurSiteno = make_simple_options(dc->panels[SITES].form,
	    "wcur_s", "Current:", s_labels(dc),
	    GP_num_sites(), dc->CurSite, set_cur_site, wargs, n);
}

/***********************************************************************/

/***********************************************************************/

void 
site_translate (Widget w, data_cell *dc, XEvent *event)
{
site_dm *dm;
   
    dm = &(dc->Site_Settings[dc->CurSite]);

    GP_set_trans(dc->hSite[dc->CurSite], dm->xtrans, dm->ytrans, dm->ztrans);
/*
    quick_draw(dc);
*/

}
/***********************************************************************/

int 
pack_siteinfo (site_dm dms[MAX_SITES], int handles[MAX_SITES])
{
int i, j;
    
    for(i=0; i < MAX_SITES; i++){
	if(!handles[i]){
	    for(j=i; j < MAX_SITES-1; j++){
		if (handles[j+1]){
		    copy_dm(&dms[j], &dms[j+1]);
		}
	    }
	}
    }
    pack_handles(handles, MAX_SITES);

}
/***********************************************************************/

void
unset_site_colorfile(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
site_dm *dm;

    dm = &(dc->Site_Settings[dc->CurSite]);

    if(dm->attrmode == ST_ATT_COLOR){
	XmToggleButtonSetState(dc->toggle_id[COL_SITEFILE],0,TRUE);
	dm->attrmode = ST_ATT_NONE;
    /* when other dynamic attributes exist, use XOR to turn off ST_ATT_COLOR */
    }
}

/***********************************************************************/

void
cursite_set_colorfile(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    if(XmToggleButtonGetState(dc->toggle_id[COL_SITEFILE])){
	pops(dc->toggle_id[COL_SITEFILE], dc, SITES_COL_FILE);
	/* gets filename, GP_attmode_color */
    }
    else{
        dc->Site_Settings[dc->CurSite].attrmode = ST_ATT_NONE;
    }
}

/***********************************************************************/
/***********************************************************************/

