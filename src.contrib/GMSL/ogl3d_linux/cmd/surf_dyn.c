#include "interface.h"
#include "string.h"
#include "coldefs.h"
/*
#define DEBUG
*/

/***********************************************************************/
static void copy_att (att_info *to, att_info *from)
{

	strcpy(to->name, from->name);
	strcpy(to->status, from->status);
	strcpy(to->map_name, from->map_name);
	to->use_map = from->use_map;
	to->constant = from->constant;
	to->constant2 = from->constant2;
	to->r = from->r;
	to->g = from->g;
	to->b = from->b;

}

/***********************************************************************/
static void 
copy_dm (surf_dm *to, surf_dm *from)
{

	strcpy(to->surfname, from->surfname);
	to->draw_mode = from->draw_mode;
	to->wire_color = from->wire_color;
	to->polycnt = from->polycnt;
	to->wirecnt = from->wirecnt;
	to->zexag = from->zexag;
	to->xtrans = from->xtrans;
	to->ytrans = from->ytrans;
	to->ztrans = from->ztrans;

}

/***********************************************************************/

void
change_cursurf_name(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
char *ctmp;
    
	ctmp = XmTextGetString(w);
	strcpy(dc->Surf_Settings[dc->CurSurf].surfname, ctmp);

	XtFree(ctmp);

}

/***********************************************************************/
/* color (for ATT_COLOR), constant value, and use_map variables should
   be current before calling this to write a status description in the
   current attribute structure.  Also see att_set_status in init.c. */

void
curatt_update_status(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
char *map, *p;

#ifdef DEBUG
fprintf(stderr, "curatt_update_status called\n");
#endif

    if (dc->Cur_Atts[dc->CurAtt].use_map){
	map = p = dc->Cur_Atts[dc->CurAtt].map_name;

	/* knock off any GRASS location suffix */
	if ((char*)NULL != (p = strrchr (map, '@'))) {
		if (p != map)
		    *p = '\0';
	}

	sprintf(dc->Cur_Atts[dc->CurAtt].status, "map: %s", map);
    }
    else if (C2_NOTSET == dc->Cur_Atts[dc->CurAtt].constant2){
	sprintf(dc->Cur_Atts[dc->CurAtt].status, "NOT SET");
    }
    else if(ATT_COLOR == dc->CurAtt){
	sprintf(dc->Cur_Atts[dc->CurAtt].status, "color: R%d,G%d,B%d",
			    (int)dc->Cur_Atts[dc->CurAtt].r,
			    (int)dc->Cur_Atts[dc->CurAtt].g,
			    (int)dc->Cur_Atts[dc->CurAtt].b);
    }
    else {
	sprintf(dc->Cur_Atts[dc->CurAtt].status, "value: %f",
			    dc->Cur_Atts[dc->CurAtt].constant);
    }
    change_label(dc->att_status, dc->Cur_Atts[dc->CurAtt].status);

#ifdef DEBUG
fprintf(stderr, "curatt_update_status done\n");
#endif

}

/***********************************************************************/

void
delete_surf(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{
int i, j, nsurfs;

    GS_delete_surface(dc->hSurf[dc->CurSurf]);
    dc->hSurf[dc->CurSurf] = 0;
    for(j=dc->CurSurf, i=1; i<MAX_ATTS; i++){
	dc->Atts[j][i].use_map = 0;
	dc->Atts[j][i].constant = dc->Atts[j][i].constant2 = 0.0;
	dc->Atts[j][i].map_name[0] = '\0';
	dc->Atts[j][i].r = dc->Atts[j][i].g = dc->Atts[j][i].b = 200;
	strcpy(dc->Atts[j][i].status, dc->Atts[j][i].name);
	strcat(dc->Atts[j][i].status, " not set");
    }
    pack_surfinfo(dc->Atts, dc->Surf_Settings, dc->hSurf);
    nsurfs = GS_num_surfs();

    dc->hSurf[nsurfs] = 0;
    if(dc->CurSurf) dc->CurSurf--;
    else if (nsurfs) dc->CurSurf++;

    cursurf_set_curatts(dc);
    surf_update_displaymode(w, dc, call_data);
    cursurf_update_attlabels(w, dc, call_data);
    update_surface_options(dc);

    update_ranges(dc);
/* 
    {
    int i;

    for(i=0; i<MAX_SURFS; i++)
	fprintf(stderr,"%d\n", dc->hSurf[i]);
    }
DEBUG */


}


/***********************************************************************/
/* Gets handle to new surface, copies default attributes, etc. 
   loads default surface at constant elevation = 0.0 
*/

void
new_surf(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{
int numsurfs;

    numsurfs = GS_num_surfs();
    if(numsurfs < MAX_SURFS){
	dc->CurSurf = numsurfs;
	if(load_new_surface(dc, NULL, 0.0)){
	    cursurf_set_curatts(dc);
	    cursurf_update_attlabels(w, dc, call_data);
	    surf_update_displaymode(w, dc, call_data);
	    pop_attr(dc->Watt[ATT_TOPO],dc,NULL);
	}

    }


}

/***********************************************************************/
/* If name is not null, loads map for elevation, otherwise sets constant k */
int 
load_new_surface (data_cell *dc, char *name, double k) 
{
int snum, xres, yres, xwire, ywire;
char cbuf[40];
	
	snum = GS_num_surfs();
	if(snum >= MAX_SURFS){
	    inform(dc,"Maximum surfaces loaded!");
	    return(0);
	}
	dc->hSurf[snum] = GS_new_surface();

	if(name){
	    inform(dc, "Loading Data");
	    GS_load_att_map(dc->hSurf[snum], name, ATT_TOPO);
	    inform(dc, "Done");
	    strcpy(dc->Atts[snum][ATT_TOPO].map_name, name);
	    dc->Atts[snum][ATT_TOPO].use_map = 1;
	}
	else{
	    dc->Atts[snum][ATT_TOPO].use_map = 0;
	    dc->Atts[snum][ATT_TOPO].constant = k;
	    GS_set_att_const(dc->hSurf[snum], ATT_TOPO, (int)k);
	    sprintf(cbuf, "%f", k);
	    name = cbuf;
	}

	/* set display mode defaults here */
	GS_get_drawres(dc->hSurf[snum], &xres, &yres, &xwire, &ywire);
	surf_init_displaymode(dc, snum, name, DM_GOURAUD | DM_POLY,
		    DEFAULT_WIRE_COLOR, xres, xwire, 1.0, 0., 0., 0.);
	GS_set_drawmode(dc->hSurf[snum], DM_GOURAUD | DM_POLY);
	att_set_status(dc,snum,ATT_TOPO);

	update_ranges(dc);

	return(1);

}

/***********************************************************************/
/* sets status labels and nozero buttons to represent current surface */
/* called when current surface is changed */

void
cursurf_update_attlabels(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct *call_data;
{
int i, tr, tg, tb;
Arg wargs[3];
int tog;
char newlabel[25];

    for(i=1; i<MAX_ATTS; i++){
	tog = (C2_NOZEROS == (int)dc->Cur_Atts[i].constant2? 1: 0);
	change_label(dc->att_label[i],dc->Cur_Atts[i].status);	
	if(i == ATT_TOPO)
	    XmToggleButtonSetState(dc->toggle_id[NZ_TOPO], tog,FALSE);
	if(i == ATT_COLOR){
	    XmToggleButtonSetState(dc->toggle_id[NZ_COLOR], tog,FALSE);

	    if (!dc->Cur_Atts[ATT_COLOR].use_map)
		set_button_colors(dc->Watt[ATT_COLOR], dc, SURF_CELL);
	    /* set back to default background if use_map */
	    else
		unset_button_colors(dc->Watt[ATT_COLOR], dc);
   	} 
    }
    /* TODO: change TMP to SELECT (from resource file) */
    if(0.0 != dc->Surf_Settings[dc->CurSurf].xtrans ||
       0.0 != dc->Surf_Settings[dc->CurSurf].ytrans ||
       0.0 != dc->Surf_Settings[dc->CurSurf].ztrans )
	set_button_colors(dc->Stranslate, dc, TMP_CELL);
    else
	unset_button_colors(dc->Stranslate, dc);

    if(WC_COLOR_ATT != dc->Surf_Settings[dc->CurSurf].wire_color)
	set_button_colors(dc->Swirecolor, dc, GRID_CELL);
    else
	unset_button_colors(dc->Swirecolor, dc);

    if(dc->Wtrans_pop)
	if(XtIsManaged(dc->Wtrans_pop))
	    set_trans_widgets(dc);

}




/* Copies attributes of Current Surface to Cur_Atts, in preparation for
   changing attributes of surface.  This way, Cur_Atts may change before
   final acceptance & actual surface attributes are changed.  */
/* Also updates colors mapped to dc->cells[SURF_CELL] */

void 
cursurf_set_curatts (data_cell *dc)
{
int i, c,tr,tg,tb;

    for(i=1; i<MAX_ATTS; i++){
	copy_att(&(dc->Cur_Atts[i]), &(dc->Atts[dc->CurSurf][i]));
    }
    set_pixel_color(dc, dc->cells[SURF_CELL], dc->Cur_Atts[ATT_COLOR].r,
	    dc->Cur_Atts[ATT_COLOR].g, dc->Cur_Atts[ATT_COLOR].b);
    i = dc->Surf_Settings[dc->CurSurf].wire_color;
    if(i != WC_COLOR_ATT){
	INT_TO_RED(i, tr);
	INT_TO_GRN(i, tg);
	INT_TO_BLU(i, tb);
	set_pixel_color(dc, dc->cells[GRID_CELL], tr, tg, tb);
    }
}

void
curatt_change_rgb(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
att_info *ai;
#ifdef DEBUG
fprintf(stderr, "curatt_change_rgb called\n");
#endif
    
    ai = &(dc->Cur_Atts[dc->CurAtt]);
    ai->use_map = 0;

    ai->r = (int)SLIDER_VAL_REAL(dc, COL_RED); 
    ai->g = (int)SLIDER_VAL_REAL(dc, COL_GRN); 
    ai->b = (int)SLIDER_VAL_REAL(dc, COL_BLU); 

    RGB_TO_INT(ai->r,ai->g,ai->b,ai->constant);

}

void
unset_gridcolor(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    if(dc->Surf_Settings[dc->CurSurf].wire_color == WC_COLOR_ATT)
	XmToggleButtonSetState(dc->toggle_id[COL_WIRE_RND],0,TRUE);
}

void
cursurf_gridcolor_surface(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    if(XmToggleButtonGetState(dc->toggle_id[COL_WIRE_RND])){
	dc->Surf_Settings[dc->CurSurf].wire_color = WC_COLOR_ATT;
	GS_set_wire_color(dc->hSurf[dc->CurSurf], WC_COLOR_ATT);
    }
    else{
	dc->Surf_Settings[dc->CurSurf].wire_color = 0;
	/* will be reset correctly in cursurf_change_gridcolor (OK Callback) */
    }
}


void
cursurf_change_gridcolor(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
int tr,tg,tb, *wc;
    
    wc = &(dc->Surf_Settings[dc->CurSurf].wire_color);
    
    if(*wc != WC_COLOR_ATT){
	get_pixel_color(dc, dc->cells[GRID_CELL], &tr, &tg, &tb);
	RGB_TO_INT(tr, tg, tb, (*wc));
	set_button_colors(dc->Swirecolor, dc, GRID_CELL);
    }
    /* otherwise need to set wire color button to regular background */
    else
	unset_button_colors(dc->Swirecolor, dc);
    
    GS_set_wire_color(dc->hSurf[dc->CurSurf], *wc);

}


/* changes both Cur_Atts and Atts[Cur_Surf] since automatically accepted */
void
set_const2(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

/* The below if statement solves a problem with ogl3d dying on a 
   segmentation fault when selecting "No Zeroes" from the surface panel
   when no elevation map was specified on the command line --- TODO: Check
   effect on default values (E. Cline 1997)


    if (strlen(dc->Cur_Atts[ATT_TOPO].map_name) > 0) {

*/   
    if(w == dc->toggle_id[NZ_TOPO]){
	dc->Cur_Atts[ATT_TOPO].constant2 = 
		    dc->Atts[dc->CurSurf][ATT_TOPO].constant2 = 
		    XmToggleButtonGetState(w)? C2_NOZEROS: 0;
	GS_set_nozero(dc->hSurf[dc->CurSurf], ATT_TOPO, 
		    dc->Cur_Atts[ATT_TOPO].constant2);
	GS_update_curmask(dc->hSurf[dc->CurSurf]);
    }
    else if(w == dc->toggle_id[NZ_COLOR]){
	dc->Cur_Atts[ATT_COLOR].constant2 = 
		    dc->Atts[dc->CurSurf][ATT_COLOR].constant2 =
		    XmToggleButtonGetState(w)? C2_NOZEROS: 0;
	GS_set_nozero(dc->hSurf[dc->CurSurf], ATT_COLOR, 
		    dc->Cur_Atts[ATT_COLOR].constant2);
	GS_update_curmask(dc->hSurf[dc->CurSurf]);
    }
    else if(w == dc->toggle_id[INV_MASK]){
	if(C2_NOTSET != dc->Cur_Atts[ATT_MASK].constant2){
	    /* don't overwrite NOTSET */
	    dc->Cur_Atts[ATT_MASK].constant2 = 
		    dc->Atts[dc->CurSurf][ATT_MASK].constant2 =
		    XmToggleButtonGetState(w)? C2_INVMASK: 0;
	    GS_set_att_const(dc->hSurf[dc->CurSurf], ATT_MASK, 
		    dc->Cur_Atts[ATT_MASK].constant2);
	    GS_update_curmask(dc->hSurf[dc->CurSurf]);
	}
    
}
}

void
curatt_set_const(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
char *ctmp;

    if(xmTextWidgetClass == XtClass(w)){
	ctmp = XmTextGetString(w);
	dc->Cur_Atts[dc->CurAtt].constant = atof(ctmp);
	XtFree(ctmp);
    }
    else if(xmScaleWidgetClass == XtClass(w)){
	dc->Cur_Atts[dc->CurAtt].constant = SLIDER_VAL_REAL(dc, ATTR_CON);
    }

}


void
curatt_reset(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

    copy_att( &(dc->Cur_Atts[dc->CurAtt]),
		    &(dc->Atts[dc->CurSurf][dc->CurAtt]));
    curatt_update_status(w, dc, call_data);
    if(ATT_COLOR == dc->CurAtt)
	set_pixel_color(dc, dc->cells[SURF_CELL], dc->Cur_Atts[ATT_COLOR].r,
		dc->Cur_Atts[ATT_COLOR].g, dc->Cur_Atts[ATT_COLOR].b);

}

/********************************************************************/
/*  load files as necessary, free old data,
    This is the callback for the Accept button when changing an attribute
    Basically, there are only a few types of changes:
	1) new map replacing old map   (new_map)
	2) new map replacing constant  (now_map)
	3) new constant replacing old constant (new_const)
	4) new constant replacing map (now_const)
	5) attribute is unset
	6) mask attribute is inverted/uninverted
    Plus, when user functions are supported, will be a few more.
    update Atts structure, update labels on Surface Panel 
*/
void
change_att(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
att_info *oldattp, *newattp;
int new_map, now_map, new_const, now_const, unset, mode;
    
    unset = new_map = now_map = new_const = now_const = 0;

    oldattp = &(dc->Atts[dc->CurSurf][dc->CurAtt]);
    newattp = &(dc->Cur_Atts[dc->CurAtt]);

    if(newattp->use_map){    /* probably new map */
	now_map = !(oldattp->use_map);
	new_map = now_map? 0: strcmp(newattp->map_name, oldattp->map_name);
    }
    
    else if(newattp->constant2 == C2_NOTSET){
	unset = 1;
    }
    
    else if(dc->CurAtt != ATT_MASK){   /* new constant */
	new_const = !(oldattp->use_map);
	now_const = oldattp->use_map;
    }

    if(ATT_MASK == dc->CurAtt){
	mode = ((int)newattp->constant2 == C2_INVMASK);
	GS_set_maskmode(dc->hSurf[dc->CurSurf], mode);
    }

    if(now_map){
	inform(dc, "Loading Data");
	GS_load_att_map(dc->hSurf[dc->CurSurf], newattp->map_name, dc->CurAtt);
	inform(dc, "Done");
    }
    else if(new_map){
	/* free old or let library handle it? */ /* library */
	inform(dc, "Loading Data");
	GS_load_att_map(dc->hSurf[dc->CurSurf], newattp->map_name, dc->CurAtt);
	inform(dc, "Done");
    }
    else if(now_const){
	/* free old or let library handle it? */ /* library */
	GS_set_att_const(dc->hSurf[dc->CurSurf], dc->CurAtt, 
		    (int)newattp->constant);
    }
    else if(new_const){
	GS_set_att_const(dc->hSurf[dc->CurSurf], dc->CurAtt, 
		    (int)newattp->constant);
    }

    if(ATT_TOPO == dc->CurAtt){
	if(now_map || new_map)
	    _set_shortname(dc->Surf_Settings[dc->CurSurf].surfname, 
			newattp->map_name);

	else if(now_const || new_const)
	    sprintf(dc->Surf_Settings[dc->CurSurf].surfname,"%f",
			newattp->constant);
	update_surface_options(dc);
    }
    else if(unset){ /* can't unset topo */
	GS_unset_att(dc->hSurf[dc->CurSurf], dc->CurAtt);
    }

    copy_att(oldattp, newattp);
    cursurf_update_attlabels(w, dc, call_data);

}
/***********************************************************************/

int 
surf_init_displaymode (data_cell *dc, int snum, char *name, int dmode, int wcol, int pcnt, int wcnt, double zg, double xt, double yt, double zt)
{
surf_dm *dm;

    dm = &(dc->Surf_Settings[snum]);

    _set_shortname(dm->surfname, name);

    dm->draw_mode = dmode;
    dm->wire_color = wcol;
    dm->polycnt = pcnt;
    dm->wirecnt = wcnt;
    dm->zexag = zg;
    dm->xtrans = xt;
    dm->ytrans = yt;
    dm->ztrans = zt;

}
/***********************************************************************/

static void 
_set_displaymode (surf_dm *dm, data_cell *dc, int surfnum)
{
int drawmode;

	drawmode = 0;
	if(XmToggleButtonGetState(dc->toggle_id[GOURAUD_RND]))
	    drawmode = drawmode | DM_GOURAUD;
	else
	    drawmode = drawmode | DM_FLAT;

	if(XmToggleButtonGetState(dc->toggle_id[POLY_RND]))
	    drawmode = drawmode | DM_POLY;
	else if(XmToggleButtonGetState(dc->toggle_id[WIRE_RND]))
	    drawmode = drawmode | DM_WIRE;
/*
	else if(XmToggleButtonGetState(dc->toggle_id[COL_WIRE_RND]))
	    drawmode = drawmode | DM_COL_WIRE;
*/
	else if(XmToggleButtonGetState(dc->toggle_id[WIRE_POLY_RND]))
	    drawmode = drawmode | DM_WIRE_POLY;
	dm->draw_mode = drawmode;

	GS_set_drawmode(dc->hSurf[surfnum], drawmode);

	/* TODO: default draw color should change when background changes 
	   Handle this in library? */
/*
	dm->wire_color = 
		XmToggleButtonGetState(dc->toggle_id[COL_WIRE_RND])?
		WC_COLOR_ATT: dm->wire_color;
	cursurf_change_gridcolor(NULL, dc, NULL);
	GS_set_wire_color(dc->hSurf[surfnum], dm->wire_color);
*/

	dm->polycnt = dc->parrows[P_RES_ARWS].val;
	dm->wirecnt = dc->parrows[G_RES_ARWS].val; 
	GS_set_drawres(dc->hSurf[surfnum],dm->polycnt,dm->polycnt,
			dm->wirecnt, dm->wirecnt);

	dm->zexag = 1.0; /* TODO: move zexag to surf panel, call GS_set_exag */ 

	/* 
	dm->xtrans = 0.0;  
	dm->ytrans = 0.0; 
	dm->ztrans = 0.0; 
	*/

	/* redundant now? (see surf_translate) */
	GS_set_trans(dc->hSurf[surfnum], dm->xtrans, dm->ytrans, dm->ztrans);

#ifdef DEBUG
fprintf(stderr,"--------------set_dm--------------\n");
fprintf(stderr,"cur surf:   %d\n", dc->CurSurf +1);
fprintf(stderr,"draw mode:  %x\n", drawmode);
fprintf(stderr,"wire color: %x\n", dm->wire_color);
fprintf(stderr,"poly res:   %d\n", dm->polycnt);
fprintf(stderr,"wire res:   %d\n", dm->wirecnt);
#endif

}
/***********************************************************************/


/* reads displaymode widgets in top half of surface panel and sets the
displaymode structure for current (or all) surface(s), depending on 
scope settings.  Should be called whenever any of these widget values
are changed by the user. */

void
surf_set_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
surf_dm *curset;
int i;

    if(XmToggleButtonGetState(dc->toggle_id[SCOPE_AS])){ /* do all surfs */
	for(i=0; i< MAX_SURFS; i++){
	    curset = &(dc->Surf_Settings[i]);
	    _set_displaymode(curset, dc, i);
	}
    }

    else{
	curset = &(dc->Surf_Settings[dc->CurSurf]);
	_set_displaymode(curset, dc, dc->CurSurf);
    }
}
/***********************************************************************/


/* updates displaymode widgets in top half of surface panel 
   using settings in dc->Surf_Settings[dc->CurSurf]. Should
   be called whenever Current Surface changes. */

void
surf_update_displaymode(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
surf_dm *curset;
char str[80];

    curset = &(dc->Surf_Settings[dc->CurSurf]);
    
    dc->parrows[P_RES_ARWS].val = curset->polycnt;
    sprintf(str, "%d", dc->parrows[P_RES_ARWS].val);
    change_label(dc->parrows[P_RES_ARWS].txt, str);

    dc->parrows[G_RES_ARWS].val = curset->wirecnt;
    sprintf(str, "%d", dc->parrows[G_RES_ARWS].val);
    change_label(dc->parrows[G_RES_ARWS].txt, str);

    /* TODO: set zexag, xtrans, ytrans, ztrans */ 
    
    /* the following will also call the toggle button valueChanged
    callback(s) in order to untoggle those in radio boxes, so be careful.
    That's why I'm using the arm callback for these toggles. (in 
    surface_panel.c) */

    if(curset->draw_mode & DM_GOURAUD)
	XmToggleButtonSetState(dc->toggle_id[GOURAUD_RND], TRUE, TRUE);
    else
	XmToggleButtonSetState(dc->toggle_id[FLAT_RND], TRUE, TRUE);

    if(curset->draw_mode & DM_POLY)
	XmToggleButtonSetState(dc->toggle_id[POLY_RND], TRUE, TRUE);
/*
    if(curset->draw_mode & DM_COL_WIRE)
	XmToggleButtonSetState(dc->toggle_id[COL_WIRE_RND], TRUE, TRUE);
*/
    if(curset->draw_mode & DM_WIRE)
	XmToggleButtonSetState(dc->toggle_id[WIRE_RND], TRUE, TRUE);
    if(curset->draw_mode & DM_WIRE_POLY)
	XmToggleButtonSetState(dc->toggle_id[WIRE_POLY_RND], TRUE, TRUE);


}
/***********************************************************************/

void 
surf_translate (Widget w, data_cell *dc, XEvent *event)
{
surf_dm *dm;
   
    dm = &(dc->Surf_Settings[dc->CurSurf]);

    GS_set_trans(dc->hSurf[dc->CurSurf], dm->xtrans, dm->ytrans, dm->ztrans);
    quick_draw(dc);

}
/***********************************************************************/

int 
pack_surfinfo (att_info atts[MAX_SURFS][MAX_ATTS], surf_dm dms[MAX_SURFS], int handles[MAX_SURFS])
{
int i, j, k;
    
    for(i=0; i < MAX_SURFS; i++){
	if(!handles[i]){
	    for(j=i; j < MAX_SURFS-1; j++){
		if (handles[j+1]){
		    for(k=0; k < MAX_ATTS; k++)
			copy_att(&atts[j][k], &atts[j+1][k]);
		    copy_dm(&dms[j], &dms[j+1]);
fprintf(stderr,"%d = %d \n", j, j+1);
		}
	    }
	}
    }
    pack_handles(handles, MAX_SURFS);

}

int 
update_surface_options (data_cell *dc)
{
    XtUnmanageChild(dc->panels[SURFACE].form);
    _update_surface_options(dc);
    XtManageChild(dc->panels[SURFACE].form);
}

int 
_update_surface_options (data_cell *dc)
{
int n;
Arg wargs[12];

    XtUnmanageChild(dc->RCurSurfno);
    XtDestroyWidget(dc->RCurSurfno);

    n = 0;
    SetPositionArgs(wargs, &n, 54,-1,2,-1, XmATTACH_NONE);
    dc->RCurSurfno = make_simple_options(dc->panels[SURFACE].form,
	    "wcur_sf", "Current:", sf_labels(dc),
	    GS_num_surfs(), dc->CurSurf, set_cur_surf, wargs, n);
/*TRIES
    XtManageChild(XtParent(XtParent(dc->RCurSurfno))); 
    XtRealizeWidget(dc->RCurSurfno);
    XtSetMappedWhenManaged(dc->RCurSurfno, TRUE);
    XtMapWidget(dc->panels[SURFACE].form);

    XtUnmanageChild(dc->panels[SURFACE].form);
    reshow(dc,SURFACE, 1);
    XmUpdateDisplay(dc->panels[SURFACE].form);
*/
    /* force redraw? - NOPE, ask TB */

}


/***********************************************************************/

char **
sf_labels (data_cell *dc)
{
static int first = 1;
static char *Sf_labels[MAX_SURFS];
int i, nsurfs;
char tmp[32];


    if(first){
	for(i=0; i<MAX_SURFS; i++){
	    sprintf(tmp, "%s", dc->Surf_Settings[i].surfname);
	    Sf_labels[i] = (char *)malloc((strlen(tmp) + 32) * sizeof(char));
	    strcpy(Sf_labels[i], tmp);
	}
	first = 0;
	return(Sf_labels);
    }
    nsurfs = GS_num_surfs();
    for(i=0; i < nsurfs; i++){
	sprintf(tmp, "%s", dc->Surf_Settings[i].surfname);
	strcpy(Sf_labels[i], tmp);
    }
    return(Sf_labels);

}


/***********************************************************************/
int 
surf_indexof_handle (data_cell *dc, int hsurf)
{
int i;

    for(i=0; i<MAX_SURFS; i++){
	if(dc->hSurf[i] == hsurf)
	    return(i);
    }
    return(-1);

}
/***********************************************************************/


