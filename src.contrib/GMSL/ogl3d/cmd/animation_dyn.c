/*--------------------------------------------------------------------------
animation_dyn.c  Callbacks and support functions for the animation panel

Eliot Cline 1997       
---------------------------------------------------------------------------*/
 

#include "interface.h"
#include "../gsf/keyframe.h"
#include "../gsf/kftypes.h"

float mypos = 0.0;
float myprecis = 0.0;
int step = 1;
static int numsteps = 0;

int done;

/* initialized in init.c (E. Cline 1997) */

extern int stop_anim;
extern Widget curframe;
extern int Numkeys;

/*----------------------------------------------------------------

see comments in GK.c for why I had to do this (E. Cline 1997) */

Keylist *Keys = NULL;

/*----------------------------------------------------------------*/



/*--------------------------------------------------------------------------
show_current_frame_number callback for the animation panel (E. Cline 1997)       
---------------------------------------------------------------------------*/

void
show_current_frame_number(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{


}


/*--------------------------------------------------------------------------
run_animation callback for the animation panel     (E. Cline 1997)       
---------------------------------------------------------------------------*/

void
run_animation(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

char filename[128], report[160];
int i;
int singlestep = 1; 
XmString labelString;
String tmpString;
Arg wargs[1];
 
    if (numsteps > 0 && Numkeys > 2) {
    
    for (i=1;i<= numsteps;++i) {
    
    GK_do_framestep(i, 1, singlestep);
    
}

}

}


/*--------------------------------------------------------------------------
stop_animation callback for the animation panel     (E. Cline 1997)       
---------------------------------------------------------------------------*/

void
stop_animation(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

  

}

/*--------------------------------------------------------------------------
set_number_frames callback for the animation panel (E. Cline 1997)       
---------------------------------------------------------------------------*/

void
set_number_frames(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

   Arg wargs[2];
   String value;
   
     XtSetArg (wargs[0], XmNvalue, &value); 
     XtGetValues(w, wargs, 1);

     numsteps = atoi(value);
     GK_set_numsteps(numsteps);
      
}

/*--------------------------------------------------------------------------
step_frame_forward callback for the animation panel         (E. Cline 1997)   
---------------------------------------------------------------------------*/

void
step_frame_forward(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    int singlestep = 1; 
    XmString labelString;
    String tmpString;
    Arg wargs[1];
    
     if (numsteps > 0 && step <= numsteps) {
     sprintf(tmpString, "%d", step);
     labelString = XmStringCreate(tmpString, "tag1");
 
     XtSetArg (wargs[0], XmNlabelString, labelString); 
     XtSetValues(curframe, wargs, 1);
     XmStringFree(labelString);
     
     GK_do_framestep(step, 1, singlestep);
    
     ++step;
     }
}


/*--------------------------------------------------------------------------
step_frame_back callback for the animation panel         (E. Cline 1997)   
---------------------------------------------------------------------------*/

void
step_frame_back(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    int singlestep = 1; 
    XmString labelString;
    String tmpString;
    Arg wargs[1];
         
     if (step > 1) {
     --step;
     
     sprintf(tmpString, "%d", step);
     labelString = XmStringCreate(tmpString, "tag1");
 
     XtSetArg (wargs[0], XmNlabelString, labelString); 
     XtSetValues(curframe, wargs, 1);
     XmStringFree(labelString);
     
     GK_do_framestep(step, 1, singlestep);
    
    }
    
}

/*--------------------------------------------------------------------------
update_frame_slider callback for the animation panel    (E. Cline 1997)     
---------------------------------------------------------------------------*/

void
update_frame_slider(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct call_data;
{

    Arg wargs[4];
    float sl_range;
    int value, sl_max, sl_min, str_set=0;

	    XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	    XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	    XtSetArg (wargs[2], XmNvalue, &value); 
	    XtGetValues(w, wargs, 3);
	    
	    sl_range = 100; 
	    mypos = (float)(value / sl_range);

}

/*--------------------------------------------------------------------------
add_keyframe callback for the animation panel       (E. Cline 1997)     
---------------------------------------------------------------------------*/

void
add_keyframe(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

int test;
unsigned long myfmask;

     if (Keys == NULL)
         myfmask = gk_get_mask_sofar(0.0, Keys);
     else
         myfmask = Keys->fieldmask;
         
     test = GK_add_key(mypos, myfmask, 0, myprecis);

}

/*--------------------------------------------------------------------------
clear_all_keyframes callback for the animation panel   (E. Cline 1997)     
---------------------------------------------------------------------------*/

void
clear_all_keyframes(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

GK_clear_keys();
step = 1;

}

/*--------------------------------------------------------------------------
show_path_vect_site callback for the animation panel (E. Cline 1997) 

maybe use a case statement to call the necessary GK functions    
---------------------------------------------------------------------------*/

void
show_path_vect_site(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

	if(XmToggleButtonGetState(dc->toggle_id[ANIM_P]))
	    GK_show_path(1);
	else
	    GK_show_path(0);
	if(XmToggleButtonGetState(dc->toggle_id[ANIM_V])) 
	    GK_show_vect(1);
	else
	    GK_show_vect(0);
	if(XmToggleButtonGetState(dc->toggle_id[ANIM_S]))
	    GK_show_site(1);
        else
            GK_show_site(0);

}


/*--------------------------------------------------------------------------
set_interpolation_type callback for the animation panel  (E. Cline 1997)     
---------------------------------------------------------------------------*/

void
set_interpolation_type(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

	if(XmToggleButtonGetState(dc->toggle_id[ANIM_SP]))
	    GK_set_interpmode(KF_SPLINE);

	if(XmToggleButtonGetState(dc->toggle_id[ANIM_L])) 
	    GK_set_interpmode(KF_LINEAR);

}

/*--------------------------------------------------------------------------
set_spline_interpolation_level callback for the animation panel (E. Cline 1997)   
---------------------------------------------------------------------------*/

void
set_spline_interpolation_level(w, dc, call_data)
Widget w;
data_cell *dc;
XmScaleCallbackStruct call_data;
{
    Arg wargs[4];
    float sl_range, tension=0.00;
    int value, sl_max, sl_min, str_set=0;

	    XtSetArg (wargs[0], XmNmaximum, &sl_max);	    
	    XtSetArg (wargs[1], XmNminimum, &sl_min);	    
	    XtSetArg (wargs[2], XmNvalue, &value); 
	    XtGetValues(w, wargs, 3);
	    sl_range = 100; 

	    tension = (float)(value / sl_range);					                     
            
            GK_set_tension(tension); 

}

/*--------------------------------------------------------------------------
run_and_save_anim callback for the animation panel (E. Cline 1997)     
---------------------------------------------------------------------------*/

void
run_and_save_anim(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

char filename[128], report[160];
int i;
int singlestep = 1; 
XmString labelString;
String tmpString;
Arg wargs[1];

     
    if (numsteps > 0 && Numkeys > 2) {
    
    for (i=1;i<= numsteps;++i) {
    
    sprintf(filename,"%05d.tga", i);
    GK_do_framestep(i, 1, singlestep);
       
    sprintf(report,"Saving %s...", filename);

    inform(dc, report);
  
    if(filename[0] != '\0'){
	 if(0 > targa_out(filename)) {
	    sprintf(report,"Unable to save %s", filename);
	    XBell(XtDisplay(w), 50);
	}
	else
	
	    sprintf(report,"%s saved.", filename);
    }
    else
	sprintf(report,"<request cancelled>");

   inform(dc, report);
}

}

}











