
/* update_sliders:
** updates the text fields when the sliders have changed
** and also updates the sliders if the text fields have changed
---------------------------------------------------------------

Notice that all calls to XStoreColor have been commented out.
This is because my OpenGL implementation for Linux does not 
support DirectColor visuals---I am still trying to come up with
a workaround for TrueColor visuals. (Eliot Cline Sept. 1997)
*/


#include "interface.h"

double rint();

void
update_sliders (w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
    int  i, ival;
    float real_value, sl_range;
    char   str[80], *ctmp, str2[80];
    double ftmp;
    Arg wargs[4];
    int sl_max, sl_min, str_set=0;
    short decimal;
    XmScaleCallbackStruct tmp_data;


    for(i=0; i < MAX_SLIDERS; i++){
	if(w == dc->sliders[i]){

	    XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	    XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	    XtSetArg (wargs[2], XmNdecimalPoints, &decimal);
	    XtGetValues(w, wargs, 3);
	    sl_range = sl_max - sl_min; 

	    dc->slider_values[i] = (float)((call_data->value)-sl_min)
						    /sl_range;
	    if(w == dc->sliders[COL_RED]){
		dc->colors.red = call_data->value;
		/*XStoreColor(dc->dpy, dc->cmap, &(dc->colors));*/
		sprintf(str, "%d",(int)SLIDER_VAL_REAL(dc, COL_RED));
		str_set = 1;
	    }
	    
	    if(w == dc->sliders[COL_GRN]){
		dc->colors.green = call_data->value;
		/*XStoreColor(dc->dpy, dc->cmap, &(dc->colors));*/
		sprintf(str, "%d",(int)SLIDER_VAL_REAL(dc, COL_GRN));
		str_set = 1;
	    }

	    if(w == dc->sliders[COL_BLU]){
		dc->colors.blue = call_data->value;
		/*XStoreColor(dc->dpy, dc->cmap, &(dc->colors));*/
		sprintf(str, "%d",(int)SLIDER_VAL_REAL(dc, COL_BLU));
		str_set = 1;
	    }

	    if(w == dc->sliders[ATTR_CON]){
		sprintf(str, "%d",(int)SLIDER_VAL_REAL(dc, ATTR_CON));
		str_set = 1;
	    }


	    real_value = SLIDER_VAL_REAL(dc, i);
	    
	    if(!str_set){
		switch(decimal){
		    case 1:
			sprintf (str, "%.1f", real_value); 
			break;
		    case 2:
			sprintf (str, "%.2f", real_value); 
			break;
		    case 3:
			sprintf (str, "%.3f", real_value); 
			break;
		    case 4:
			sprintf (str, "%.4f", real_value); 
			break;
		    case 5:
			sprintf (str, "%.5f", real_value); 
			break;
		    case 6:
			sprintf (str, "%.6f", real_value); 
			break;
		    case 0:
		    default:
			sprintf (str, "%d", (int)rint((double)real_value)); 
			break;
		}
	    }
	    XmTextSetString(dc->slider_txt[i], str);

	}

	else if(w == dc->slider_txt[i]){

	    XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	    XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	    XtGetValues(dc->sliders[i], wargs, 2);
	    sl_range = sl_max - sl_min;

	    ctmp = XmTextGetString(w);
	    ftmp = atof(ctmp);

	    /* allow reset scale of exag slider for val set outside range */
	    if (w == dc->slider_txt[MAIN_ZEX]){
		if(ftmp < (dc->slider_max[i]/10.)){
		    /* reset slider range */
		    while((dc->slider_max[i]/10.) > ftmp){
			dc->slider_max[i] /= 10.;
		    }
		}
		if(ftmp > dc->slider_max[i]){
		    /* reset slider range */
		    while(dc->slider_max[i] < ftmp){
			dc->slider_max[i] *= 10.;
		    }
		}
	    }
	    /* allow reset scale of height slider for val set outside range */
	    else if (w == dc->slider_txt[MAIN_HGT]){
		if(ftmp < (dc->slider_max[i]/10.)){
		    /* reset slider range */
		    while((dc->slider_max[i]/10.) > ftmp){
			dc->slider_max[i] /= 10.;
		    }
		}
		if(ftmp > dc->slider_max[i]){
		    /* reset slider range */
		    while(dc->slider_max[i] < ftmp){
			dc->slider_max[i] *= 10.;
		    }
		}
	    }
	    else{
		if(ftmp < dc->slider_min[i]){
		    ftmp = dc->slider_min[i];
		}
		if(ftmp > dc->slider_max[i]){
		    ftmp = dc->slider_max[i];
		}
	    }
	    

	    dc->slider_values[i] = UNIT_OF(dc->slider_max[i],
				    dc->slider_min[i], ftmp);
	    ival = (int)(sl_min + dc->slider_values[i]*sl_range);
	    XmScaleSetValue(dc->sliders[i], ival); 

	    /* one would think this would be enough - that this callback 
	    would now be called for the slider widget with reason 
	    XmCR_VALUE_CHANGED, but it isn't working that way, 
	    so here I call it myself */ 

	    tmp_data.value = ival; 
	    update_sliders (dc->sliders[i], dc, &tmp_data);

	    XtFree(ctmp);

	}
    } /*-- end of for i --*/

}

int 
reset_slider_range (double newval, data_cell *dc, int id)
{
    if(newval > dc->slider_max[id]){
	while(dc->slider_max[id] < newval)
	    dc->slider_max[id] *= 10.;
    }


}

void 
set_slider_txt (data_cell *dc, int con)
{
Arg wargs[2];
int sl_max, sl_min, sl_range, ival;
char str[20];

    if(XtIsManaged(dc->sliders[con])){
	XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	XtGetValues(dc->sliders[con], wargs, 2);
	sl_range = sl_max - sl_min;

	ival = (int)(sl_min + dc->slider_values[con] * sl_range);
	XmScaleSetValue(dc->sliders[con], ival); 

	sprintf (str, "%d", (int)SLIDER_VAL_REAL(dc, con));
	XmTextSetString(dc->slider_txt[con], str);
    }

}


/* use own system of getting vals from sliders to make it easier to
   change the slider's range dynamically */

int 
init_default_slider_vals1 (data_cell *dc, float *min, float *max, float *val)
{
float longdim;

    GS_get_longdim(&longdim);

    min[MAIN_PSP] = 3;
    max[MAIN_PSP] = 120;
    val[MAIN_PSP] = UNIT_OF(120,3,40);

    min[CPL_TILT] = 0.0;
    max[CPL_TILT] = 360.;
    val[CPL_TILT] = 0.5;

    min[CPL_ROT] = 0.0;
    max[CPL_ROT] = 360.;
    val[CPL_ROT] = 0.5;

    val[VECT_ZEX] = 0.1;
    val[LITE_BGT] = 0.8;
    val[LITE_RED] = 1.0;
    val[LITE_GRN] = 1.0;
    val[LITE_BLU] = 1.0;
    val[LITE_AMB] = 0.3;
    val[LITE_HGT] = 0.8;
    val[LITE_SHN] = 0.8;

    max[COL_RED] = max[COL_GRN] = max[COL_BLU] = 255;
    val[COL_RED] = 0.3;
    val[COL_GRN] = 0.3;
    val[COL_BLU] = 0.3;

    max[ATTR_CON] = 255;
    val[ATTR_CON] = 0.0;

    max[SITE_SIZ] = longdim/20.;
    val[SITE_SIZ] = 0.2;
}

/* after initial data has been loaded, & maybe again later */
int 
init_default_slider_vals2 (data_cell *dc, float *min, float *max, float *val)
{
float longdim, exag, texag, hmin, hmax;
int nsurfs, i;

    nsurfs = GS_num_surfs();
    GS_get_longdim(&longdim);
    GS_get_zrange_nz(&hmin, &hmax);
   
    exag = 0.0;
    for (i=0; i<nsurfs; i++){
	if(GS_get_exag_guess(dc->hSurf[i], &texag) > -1)
	    if(texag) exag = texag > exag? texag: exag;
    }
    if(exag == 0.0) exag = 1.0;

    min[MAIN_HGT] = hmin - (2.*longdim/exag);
    max[MAIN_HGT] = hmin + (3*longdim/exag);
    val[MAIN_HGT] = 0.5;

    min[MAIN_ZEX] = 0.0;
    max[MAIN_ZEX] = 10. * exag;
    val[MAIN_ZEX] = .1;
}



