


#include "interface.h"
/*
#define DEBUG
*/

static float Xpos, Ypos, Zpos;

/***********************************************************************/

void 
position_light_xy (Widget w, data_cell *dc, XEvent *event)
{
    Arg wargs[3];
    Dimension tmpx, tmpy;
    int n;
    float xpos, ypos, zpos, from[3];

    n = 0;
    XtSetArg(wargs[n], XmNwidth, &tmpx); n++;
    XtSetArg(wargs[n], XmNheight, &tmpy); n++;
    XtGetValues(dc->Lxy_pos, wargs, n);	

    if(dc->x > tmpx || dc->y > tmpy) {
	dc->x = tmpx;
	dc->y = tmpy;
    }

    if(dc->x < 0 || dc->y < 0) {
	dc->x = 0;
	dc->y = 0;
    }


    Xpos = xpos = -1. + 2.0 * dc->x/(float)tmpx;
    Ypos = ypos = -1. + 2.0 * (1. - dc->y/(float)tmpy);   /* top right orig */
    zpos = dc->slider_values[LITE_HGT];

    GS_setlight_position(1, xpos, ypos, zpos, 0);
    GS_set_draw(GSD_FRONT);
    GS_ready_draw();
    GS_draw_lighting_model();
    GS_done_draw();
    GS_set_draw(GSD_BACK);

}

/***********************************************************************/
void
position_light_z(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{

    Zpos = dc->slider_values[LITE_HGT];

    GS_setlight_position(1, Xpos, Ypos, Zpos, 0);
    GS_set_draw(GSD_FRONT);
    GS_ready_draw();
    GS_draw_lighting_model();
    GS_done_draw();
    GS_set_draw(GSD_BACK);
}

/***********************************************************************/
void
update_lightvals(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct    *call_data;
{
float bright, red, green, blue;

    bright = dc->slider_values[LITE_BGT];
    red = bright * dc->slider_values[LITE_RED];
    green = bright * dc->slider_values[LITE_GRN];
    blue = bright * dc->slider_values[LITE_BLU];

    GS_setlight_color(1, red, green, blue); 

    red = green = blue = dc->slider_values[LITE_AMB];
    GS_setlight_ambient(1, red, green, blue);
    GS_setlight_ambient(2, red, green, blue);

    GS_set_draw(GSD_FRONT);
    GS_ready_draw();
    GS_draw_lighting_model();
    GS_done_draw();
    GS_set_draw(GSD_BACK);
}
/***********************************************************************/



