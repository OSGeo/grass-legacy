/*------------------------------------------------------------
This is a hijaak of the scripting panel. I have added the basic
animation controls as pictured in the maual for NVIZ from CECER

Eliot Cline 1997
------------------------------------------------------------*/



/* animation_panel:
** the animation panel creation routine  
**/

#include "interface.h"

Widget curframe;

void 
animation_panel (Widget w, data_cell *dc, caddr_t call_data)
{
    int n;
    Arg wargs[30];
    static int first = 1;
    Widget wtmp, wtmp1, wtmp2,wwrc, wwrc2, rowcol2;
    Widget wtmp3, wtmp4, wtmp5, wtmp6, wtmp7, wtmp8, wtmp9;
    Widget stop, run, title;
    
    if(first){
	
	inform(dc,"Calculating Positions");

	dc->panels[ANIM].im_already_open = 1;
	dc->panels[ANIM].name = XmStringCreateSimple("Animation Panel");

	check_space(dc,ANIM ,0);

	n = 0;
	
/*------------------------------------------------------------
make the basic form for the animation panel (E. Cline 1997)       
------------------------------------------------------------*/
	
	SetPositionArgs(wargs, &n, TOP,FORM,
			-1,-1, XmATTACH_FORM);
	/*dc->here.botth*/
	dc->panels[ANIM].form = XtCreateManagedWidget("aform", 
		xmFormWidgetClass, dc->form_for_aux_control_panel, wargs,n);		

	title = make_title(ANIM,dc, 0);	
	/*n = 0;
	
	XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n], XmNtopWidget, title); n++;
	XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
	XtSetArg(wargs[n], XmNbottomPosition, 17); n++;
	XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(wargs[n], XmNrightPosition, 50); n++;*/


/*--------------------------------------------------------------------------
make a row/column widget to hold the keyframe slider (E. Cline 1997)       
---------------------------------------------------------------------------*/
	
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;  
        XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNrightPosition, 100); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNtopPosition, 22); n++; 
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 37); n++; 
        XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
        wtmp = XtCreateManagedWidget("key_frames",
                xmRowColumnWidgetClass, dc->panels[ANIM].form, wargs, n);

/*--------------------------------------------------------------------------
make labels for form                            (E. Cline 1997)       
---------------------------------------------------------------------------*/
         
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 15); n++;
        wtmp1 = XtCreateManagedWidget("Step", xmLabelWidgetClass,
                      dc->panels[ANIM].form, wargs, n); 
         
        
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 42); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 12); n++;
        XtCreateManagedWidget("Total  Frames", xmLabelWidgetClass,
                      dc->panels[ANIM].form, wargs, n); 
        
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 37); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 15); n++;
        curframe = XtCreateManagedWidget("0", xmLabelWidgetClass,
                      dc->panels[ANIM].form, wargs, n);
 
/*--------------------------------------------------------------------------
Add show_current_frame_number callback for the label created above (E. Cline 1997)       
---------------------------------------------------------------------------*/
            
       /* XtAddCallback(curframe, 
	    XmNactivateCallback,show_current_frame,dc);  */
	            
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 78); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNtopPosition, 8); n++;
        run = XtCreateManagedWidget("   Run   ", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);    
                      
/*--------------------------------------------------------------------------
Add run_animation callback for the button created above     (E. Cline 1997)       
---------------------------------------------------------------------------*/
            
        XtAddCallback(run, XmNactivateCallback,run_animation,dc);   

        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 78); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNtopPosition, 15); n++;
        stop = XtCreateManagedWidget("   Stop  ", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);
            
/*--------------------------------------------------------------------------
Add stop_animation callback for the button created above     (E. Cline 1997) 
---------------------------------------------------------------------------*/
           
        XtAddCallback(stop, XmNactivateCallback,stop_animation,dc);
	              
        n = 0;                  
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(wargs[n], XmNleftPosition, 50); n++;
	XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 20); n++;
        dc->slider_txt[ANIM_FRMS] = make_text(dc->panels[ANIM].form, "",
                5, "Ssize", NULL, dc, wargs, n);
           
/*--------------------------------------------------------------------------
Add set_number_frames callback for the type-in box created above (E. Cline 1997)       
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->slider_txt[ANIM_FRMS], 
	    XmNvalueChangedCallback,set_number_frames,dc); 
	                             
        n = 0;        
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNleftWidget, wtmp1); n++;
        XtSetArg(wargs[n], XmNwidth, 28); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 15); n++;
        XtSetArg (wargs[n], XmNarrowDirection, XmARROW_LEFT); n++;     
        wtmp2 = XtCreateManagedWidget("StepL", xmArrowButtonWidgetClass,
               dc->panels[ANIM].form, wargs, n);    
               
/*--------------------------------------------------------------------------
Add step_frame callback for the arrow button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp2, XmNactivateCallback, step_frame_back,dc);      
           
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNleftWidget, wtmp2); n++;
        XtSetArg(wargs[n], XmNwidth, 28); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 15); n++;
        XtSetArg (wargs[n], XmNarrowDirection, XmARROW_RIGHT); n++;     
        wtmp3 = XtCreateManagedWidget("StepL", xmArrowButtonWidgetClass,
               dc->panels[ANIM].form, wargs, n); 
               
/*--------------------------------------------------------------------------
Add step_frame callback for the arrow button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp3, XmNactivateCallback,step_frame_forward,dc); 
	    
        n = 0;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp); n++;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNleftWidget, wtmp); n++;   
	XtSetArg(wargs[n], XmNscaleMultiple,10); n++;
	XtSetArg(wargs[n], XmNscaleWidth, 250); n++;
	XtSetArg(wargs[n], XmNscaleHeight, 20); n++;
        dc->sliders[ANIM_FRMS] = make_slider(wtmp, 
		100,0,0,2, "Key Frames", NULL, NULL, 
		dc, 1, XmHORIZONTAL, wargs, n);

/*--------------------------------------------------------------------------
Add update_frame_slider callback for the slider created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->sliders[ANIM_FRMS], XmNvalueChangedCallback,
                      update_frame_slider,dc); 
        
        /*
        n = 0;
	XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	XtSetArg(wargs[n], XmNleftWidget, dc->sliders[KEY_FRMS]); n++;
	XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNtopPosition, 20); n++;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNbottomPosition, 24); n++;
        dc->slider_txt[KEY_FRMS] = make_text(wtmp, "",
                5, "Ssize", update_sliders, dc, wargs, n); */
             
                      
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp); n++;
        wtmp6 = XtCreateManagedWidget("   Add a Keyframe  ", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);  
                           
/*--------------------------------------------------------------------------
Add add_keyframe callback for the button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp6, XmNactivateCallback,
                      add_keyframe,dc);
                          
        n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp6); n++;
        wtmp7 = XtCreateManagedWidget("Clear all Keyframes", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);

/*--------------------------------------------------------------------------
Add clear_all_keyframes callback for the button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp7, XmNactivateCallback,
                      clear_all_keyframes,dc); 
                              
	n = 0;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp); n++;   
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 65); n++;     
        XtSetArg(wargs[n],XmNspacing,0); n++;
        XtSetArg(wargs[n],XmNradioBehavior,FALSE); n++;
        wwrc = XtCreateManagedWidget("anim rc",
                xmRowColumnWidgetClass, dc->panels[ANIM].form, wargs, n);

	n = 0;
	XtSetArg(wargs[n],XmNset,FALSE); n++;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[ANIM_P] = XtCreateManagedWidget("show path", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);

/*--------------------------------------------------------------------------
Add show_path_vect_site callback for the toggle button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->toggle_id[ANIM_P], XmNdisarmCallback,
                      show_path_vect_site,dc); 
                      	
	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[ANIM_V] = 
		    XtCreateManagedWidget("show vect", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n);
		    
/*--------------------------------------------------------------------------
Add show_path_vect_site callback for the toggle button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->toggle_id[ANIM_V], XmNdisarmCallback,
                      show_path_vect_site,dc);
                      	
	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[ANIM_S] = 
		    XtCreateManagedWidget("show site", 
		    xmToggleButtonWidgetClass, wwrc, wargs, n); 
	
	
/*--------------------------------------------------------------------------
Add show_path_vect_site callback for the toggle button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->toggle_id[ANIM_S], XmNdisarmCallback,
                      show_path_vect_site,dc);
                      	
	n = 0;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp7); n++;   
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;     
        XtSetArg(wargs[n],XmNspacing,0); n++;
        XtSetArg(wargs[n],XmNradioBehavior,TRUE); n++;
        wwrc2 = XtCreateManagedWidget("interp rc",
                xmRowColumnWidgetClass, dc->panels[ANIM].form, wargs, n);
                      
	n = 0;
	XtSetArg(wargs[n],XmNset,TRUE); n++;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[ANIM_L] = XtCreateManagedWidget("Linear", 
		    xmToggleButtonWidgetClass, wwrc2, wargs, n);

/*--------------------------------------------------------------------------
Add set_interpolation_type callback for the toggle button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->toggle_id[ANIM_L], XmNdisarmCallback,
                      set_interpolation_type,dc);
                      	
	n = 0;
	XtSetArg(wargs[n],XmNindicatorSize,12); n++;
	XtSetArg(wargs[n],XmNmarginHeight, 0); n++;
	dc->toggle_id[ANIM_SP] = 
		    XtCreateManagedWidget("Spline", 
		    xmToggleButtonWidgetClass, wwrc2, wargs, n);
		
/*--------------------------------------------------------------------------
Add set_interpolation_type callback for the toggle button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->toggle_id[ANIM_SP], XmNdisarmCallback,
                      set_interpolation_type,dc);
                      	
	n = 0;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;  
        XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNrightPosition, 100); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wwrc2); n++; 
        XtSetArg (wargs[n], XmNorientation, XmHORIZONTAL); n++;
        rowcol2 = XtCreateManagedWidget("sp_tens",
                xmRowColumnWidgetClass, dc->panels[ANIM].form, wargs, n);
                
                
	n = 0;
        XtSetArg(wargs[n], XmNbottomAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNbottomWidget, rowcol2); n++;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNleftWidget, rowcol2); n++;   
	XtSetArg(wargs[n], XmNscaleMultiple,10); n++;
	/*XtSetArg(wargs[n], XmNscaleWidth,300); n++;
	XtSetArg(wargs[n], XmNscaleHeight, 20); n++;*/
        dc->sliders[ANIM_SPLN] = make_slider(rowcol2, 
		100,0,1,2, "Spline Tension", NULL, NULL, 
		dc, 1, XmHORIZONTAL, wargs, n);
		
/*--------------------------------------------------------------------------
Add set_spline_interpolation_level callback for the slider created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(dc->sliders[ANIM_SPLN], XmNvalueChangedCallback,
                      set_spline_interpolation_level, dc);
                      		
	n = 0;
	XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNrightPosition, 96); n++;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, rowcol2); n++;
        wtmp8 = XtCreateManagedWidget("   Run and Save Images  ", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);       

/*--------------------------------------------------------------------------
Add run_and_save_anim callback for the button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp8, XmNactivateCallback, run_and_save_anim,dc);
                          
        n = 0;
        XtSetArg(wargs[n], XmNrightAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNrightPosition, 96); n++;
        XtSetArg(wargs[n], XmNleftAttachment, XmATTACH_POSITION); n++;
        XtSetArg(wargs[n], XmNleftPosition, 4); n++;
        XtSetArg(wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
        XtSetArg(wargs[n], XmNtopWidget, wtmp8); n++;
        wtmp9 = XtCreateManagedWidget("Close", xmPushButtonWidgetClass,
                      dc->panels[ANIM].form, wargs, n);

/*--------------------------------------------------------------------------
Add close_me callback for the button created above (E. Cline 1997)     
---------------------------------------------------------------------------*/
            
        XtAddCallback(wtmp9, XmNactivateCallback, close_me,dc);
        			    		
	inform(dc,"Done");

	first = 0;
    }


    else if(dc->panels[ANIM].im_already_open){
	dc->constant = ANIM;
	pops(w, dc, ANIM);
	return;
    }


    else {
	inform(dc,"Calculating Positions");
	reshow(dc,ANIM, 1);
	inform(dc,"Done");
    }

}


