
/* initialize variables, slider values, etc.
*/

#include "interface.h"
#include "coldefs.h"

/* Intitialize the stop animation flag */

int stop_anim = 0;
int Numkeys = 0;

int 
init1 (data_cell *dc)
{
int i;
    
    GS_libinit();

    dc->top = 0;
    dc->bott = 0;
    dc->Wset = 1;
    dc->y= 59.0;
    dc->x = 59.0;
    dc->current_position = 0;
    dc->constant = 500;

    /* flag for determining if primary_controls was called ? */ 


    /*----------      initialize the arrays     ---------*/
    for(i = 0; i < MAX_SLIDERS; i++){
	    dc->sliders[i] = NULL;
	    dc->slider_values[i] = 0.0;
	    dc->slider_txt[i] = NULL;
	    dc->slider_min[i] = 0;
	    dc->slider_max[i] = 1.;
    }


    for(i = 0; i < MAX_TOGGLES; i++) {
	    dc->toggle_id[i] = NULL;
	    
   /*--------removed the variables structure from dc (E. Cline 1997)---------*/      
            dc->toggles[i] = FALSE;
    }

    for(i = 0; i < MAX_PANELS; i++)
	    dc->panels[i].im_already_open = 0;

    for(i = 0; i < MAX_SURFS; i++)
	    dc->hSurf[i] = 0;
    
    init_atts(dc);
    init_dm_settings(dc);
    init_simple_menudata(dc);


}

int 
init2 (data_cell *dc)
{
    init_default_slider_vals1(dc, dc->slider_min, dc->slider_max, 
			    dc->slider_values);
}

/* init sliders that need to reference data ranges */
int 
init3 (data_cell *dc)
{
    init_default_slider_vals2(dc, dc->slider_min, dc->slider_max, 
			    dc->slider_values);
}

void 
init_atts (data_cell *dc)
{
int i, j;

    for(j=0; j<MAX_SURFS; j++){
	strcpy(dc->Atts[j][ATT_TOPO].name,"topography") ;
	strcpy(dc->Atts[j][ATT_COLOR].name,"color") ;
	strcpy(dc->Atts[j][ATT_MASK].name,"mask") ;
	strcpy(dc->Atts[j][ATT_SHINE].name,"shininess") ;
	strcpy(dc->Atts[j][ATT_TRANSP].name,"alpha") ;
	strcpy(dc->Atts[j][ATT_EMIT].name,"emission") ;

	for(i=1; i<MAX_ATTS; i++){
	    dc->Atts[j][i].use_map = 0;
	    dc->Atts[j][i].constant = dc->Atts[j][i].constant2 = 0.0;
	    dc->Atts[j][i].map_name[0] = '\0';
	    dc->Atts[j][i].r = dc->Atts[j][i].g = dc->Atts[j][i].b = 200;
	    strcpy(dc->Atts[j][i].status, dc->Atts[j][i].name);
	    strcat(dc->Atts[j][i].status, " not set");
	}
    }
    dc->CurSurf = 0;
    dc->CurVect = 0;

}

int 
att_set_status (data_cell *dc, int surf, int att)
{
char *map, *p;

    if (dc->Atts[surf][att].use_map){
	map = p = dc->Atts[surf][att].map_name;

	/* knock off any location suffix */
	if ((char*)NULL != (p = strrchr (map, '@'))) {
		if (p != map)
		    *p = '\0';
	}

	sprintf(dc->Atts[surf][att].status, "map: %s", map);
    }
    else if(ATT_COLOR == att){
	sprintf(dc->Atts[surf][att].status, "color: R%d,G%d,B%d",
			    (int)dc->Atts[surf][att].r,
			    (int)dc->Atts[surf][att].g,
			    (int)dc->Atts[surf][att].b);
    }
    else {
	sprintf(dc->Atts[surf][att].status, "value: %f",
			    dc->Atts[surf][att].constant);
    }

}


int 
init_dm_settings (data_cell *dc)
{
surf_dm *sdm;
vect_dm *vdm;
int i;
unsigned long col;
char ctmp[40];


    for(i=0; i< MAX_SURFS; i++){
	sdm = &(dc->Surf_Settings[i]);
	sprintf(ctmp, "Surface %d", i+1);
	strcpy(sdm->surfname, ctmp);
	sdm->draw_mode = DM_GOURAUD | DM_POLY;
	sdm->wire_color = 0xffffff; 
	sdm->polycnt = 2;
	sdm->wirecnt = 2; 
	sdm->zexag = 1.0; /* TODO: move zexag to surf panel ? */ 
	sdm->xtrans = 0.0;  
	sdm->ytrans = 0.0; 
	sdm->ztrans = 0.0;
    }

    for(i=0; i< MAX_VECTS; i++){
	vdm = &(dc->Vect_Settings[i]);
	vdm->color = 0xffffff; 
	vdm->width = 2; 
	vdm->use_mem = 1;   /* FOR NOW */
	vdm->xtrans = 0.0; 
	vdm->ytrans = 0.0;
	vdm->ztrans = 0.0;
    }

}

int 
init_lights (data_cell *dc)
{
float bright, red, green, blue;

    bright = dc->slider_values[LITE_BGT];
    red = bright * dc->slider_values[LITE_RED];
    green = bright * dc->slider_values[LITE_GRN];
    blue = bright * dc->slider_values[LITE_BLU];

    GS_new_light();
    GS_new_light();

    /* NW corner */
    GS_setlight_position(1, -1.0, 1.0, 1.0, 0);
    GS_setlight_color(1, red, green, blue); 
    GS_setlight_ambient(1, 0.2, 0.2, 0.2);

    /* fill light */
    GS_setlight_position(2, 0.0, 0.0, 1.0, 0);
    GS_setlight_color(2, 0.25, 0.25, 0.25);
    GS_setlight_ambient(2, 0.1, 0.1, 0.1);

}

int 
set_default_wirecolors (data_cell *dc, int surfs)
{

#ifdef DO_GREYSCALE
int i, color, greyincr, greyval;

    greyincr = 200/(surfs+1); /* just use upper values */

    for(i = 0; i < surfs; i++){
	greyval = 55 + greyincr*(i +1);
	RGB_TO_INT(greyval,greyval,greyval,color);
	GS_set_wire_color(dc->hSurf[i], color);
	dc->Surf_Settings[i].wire_color = color;
    }

#else

int i, ramp[MAX_SURFS];
int sortSurfs[MAX_SURFS], sorti[MAX_SURFS];

    make_red_yellow_ramp(ramp, surfs, 30, 255);
    sort_surfs_mid(dc->hSurf, sortSurfs, sorti, surfs);

    for(i = 0; i < surfs; i++){
	GS_set_wire_color(sortSurfs[i], ramp[i]);
	dc->Surf_Settings[sorti[i]].wire_color = ramp[i];
    }
    
#endif

}

/******************************************************************/

/* Sorts surfaces by mid elevation, lowest to highest.
   Puts ordered id numbers in id_sort, leaving id_orig unchanged.
   Puts ordered indices of surfaces from id_orig in indices.
*/
int 
sort_surfs_mid (int *id_orig, int *id_sort, int *indices, int num)
{
int i, j;
float midvals[MAX_SURFS];
float tmp, max, tmin, tmax, tmid;

    for(i=0; i < num; i++){
	GS_get_zextents(id_orig[i], &tmin, &tmax, &tmid);
	if(i ==0) max = tmax;
	else max = max < tmax? tmax: max;
	midvals[i] = tmid;
    }
    for(i=0; i < num; i++){
	tmp = midvals[0];
	indices[i] = 0;
	for(j=0; j < num; j++){
	    if(midvals[j] < tmp){
		tmp = midvals[j];
		indices[i] = j;
	    }
	}
	midvals[indices[i]] = max+1;
	id_sort[i] = id_orig[indices[i]];
    }

}

/******************************************************************/

/* Sorts surfaces by max elevation, lowest to highest.
   Puts ordered id numbers in id_sort, leaving id_orig unchanged.
   Puts ordered indices of surfaces from id_orig in indices.
*/
int 
sort_surfs_max (int *id_orig, int *id_sort, int *indices, int num)
{
int i, j;
float maxvals[MAX_SURFS];
float tmp, max, tmin, tmax, tmid;

    for(i=0; i < num; i++){
	GS_get_zextents(id_orig[i], &tmin, &tmax, &tmid);
	if(i ==0) max = tmax;
	else max = max < tmax? tmax: max;
	maxvals[i] = tmax;
    }
    for(i=0; i < num; i++){
	tmp = maxvals[0];
	indices[i] = 0;
	for(j=0; j < num; j++){
	    if(maxvals[j] < tmp){
		tmp = maxvals[j];
		indices[i] = j;
	    }
	}
	maxvals[indices[i]] = max+1;
	id_sort[i] = id_orig[indices[i]];
    }

}

