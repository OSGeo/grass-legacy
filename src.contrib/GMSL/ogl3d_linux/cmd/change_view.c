
/* change_view:
** callbacks for movement & perspective adjustments
*/


#include "interface.h"

/*
#define STABLE_VIEW
*/
/* view Z isn't affected by exaggeration */

void
change_persp (w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
int fov;

    fov = (int)(10 * SLIDER_VAL_REAL(dc, MAIN_PSP));
/*
fprintf(stderr,"field of view: %d\n", fov/10);
*/
    GS_set_fov(fov);
    quick_draw(dc);

}


/**********************************************************************/

void 
change_xypos (Widget w, data_cell *dc, XEvent *event)
{
    Arg wargs[3];
    Dimension tmpx, tmpy;
    int n;
    float xpos, ypos, from[3];

    n = 0;
    XtSetArg(wargs[n], XmNwidth, &tmpx); n++;
    XtSetArg(wargs[n], XmNheight, &tmpy); n++;
    XtGetValues(dc->xy_position, wargs, n);	

    if(dc->x > tmpx || dc->y > tmpy) {
	dc->x = tmpx;
	dc->y = tmpy;
    }

    if(dc->x < 0 || dc->y < 0) {
	dc->x = 0;
	dc->y = 0;
    }


    xpos = dc->x/(float)tmpx;
    ypos = 1. - dc->y/(float)tmpy;  /* top right orig */

    GS_get_from(from);
    from[X] = xpos * RANGE - RANGE_OFFSET;
    from[Y] = ypos * RANGE - RANGE_OFFSET;
    GS_moveto(from);

    quick_draw(dc);

}


/**********************************************************************/

void
change_height(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
    float from[3];

    /*
    GS_get_from(from);
    from[Z] = dc->slider_values[MAIN_HGT] * RANGE - RANGE_OFFSET;
    */
#ifdef STABLE_VIEW
    GS_get_from(from);
    from[Z] = GS_UNIT_SIZE * SLIDER_VAL_REAL(dc, MAIN_HGT)/dc->XYrange ;
    GS_moveto(from);
#else
    GS_get_from_real(from);
    from[Z] = SLIDER_VAL_REAL(dc, MAIN_HGT);
    GS_moveto_real(from);
#endif

    quick_draw(dc);

}

/**********************************************************************/
/* call whenever a new surface is added, deleted, or exag changes */
int 
update_ranges (data_cell *dc)
{
float zmin, zmax;

	GS_get_longdim(&(dc->XYrange));

	dc->Zrange = 0.;
	/* Zrange is based on a minimum of 2*Longdim */
	if(GS_global_exag())
	    dc->Zrange = 2 * dc->XYrange / GS_global_exag();

	GS_get_zrange(&zmin, &zmax, 0); /* actual */
	if((zmax - zmin) > dc->Zrange)
	    dc->Zrange = zmax - zmin;
/*
fprintf (stderr,"global exag: %f\n",GS_global_exag());	
fprintf (stderr,"new zranges: XY: %f    Z: %f\n",dc->XYrange, dc->Zrange);	
*/
}

/**********************************************************************/

void
change_exag(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
    int i;
    float from[3];
/*
    for(i=0; i<MAX_SURFS; i++){
	GS_set_exag(dc->hSurf[i], (float)SLIDER_VAL_REAL(dc, MAIN_ZEX));
    }
*/
    GS_set_global_exag((float)SLIDER_VAL_REAL(dc, MAIN_ZEX));
    
    update_ranges(dc);

#ifdef STABLE_VIEW
    GS_get_from(from);
    from[Z] = GS_UNIT_SIZE * SLIDER_VAL_REAL(dc, MAIN_HGT)/dc->XYrange ;
    GS_moveto(from);
#else
    GS_get_from_real(from);
    from[Z] = SLIDER_VAL_REAL(dc, MAIN_HGT);
    GS_moveto_real(from);
#endif

    quick_draw(dc);

}

/**********************************************************************/
#ifdef OLD
void
change_zsep(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
    int i, id;
    float min, max, mid, xtrans, ytrans, ztrans;

    for(i=0; i<MAX_SURFS; i++){
	id = dc->hSurf[i];
	GS_get_zextents(id, &min, &max, &mid);
	GS_get_trans(id, &xtrans, &ytrans, &ztrans);
	ztrans = mid * (float)SLIDER_VAL_REAL(dc, MAIN_ZSEP);
	GS_set_trans(id,xtrans,ytrans,ztrans);
    }

    quick_draw(dc);

}
#endif
/**********************************************************************/

