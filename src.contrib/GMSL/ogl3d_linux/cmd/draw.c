
/* draw:
** uses libgsf to draw lighted surfaces
*/

#include "interface.h" 

#define TIMER

#ifdef TIMER
/********************************************/
/********************************************/
/* timing stuff for optimization debugging  */

#include <time.h>

time_t first_time;

int 
bgntime (void)
{
    first_time = time(NULL);
}

int 
endtime (void)
{
    fprintf(stderr,"Elapsed time = %ld seconds.\n", time(NULL) - first_time);
}
    
/********************************************/
/********************************************/
#endif

void check_button();

/* Tried using TimeOuts here to check & set cancel, but apparently
   TimeOuts get put aside during a callback  */
Widget Cxl_w;

int 
enable_cxl (Widget w)
{
    Cxl_w = w; 
    GS_set_cxl_func(check_button);
}

/********************************************/
/* callback for CLEAR button */
void
do_clear(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    GS_set_draw(GSD_BOTH);
    GS_ready_draw();
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_done_draw();
    GS_set_draw(GSD_BACK);
    
    /* GS_set_draw(GSD_BOTH);
    GS_ready_draw();
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_done_draw();
    GS_set_draw(GSD_BACK); */
}

/********************************************/
/* callback for DRAW button */
void
do_draw(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
   /* bgntime(); */
    GS_set_cancel(0);
    surf_all_draw(dc);
    GS_set_cancel(0);
    
   
/* I have no idea why this is happening. When pushing the draw "surface" button
   on the main control panel, the scene is only partially drawn. If the "Clear" button
   is then pressed, the rest of the scene is drawn. Pushing the "Clear" button again then
   actually clears the drawing area with the background color. Copying the code from the 
   "do_clear" function above, completes the scene draw. Subsequent pushes of the "Clear"
   button actually clear the drawing area. (E. Cline 1997) */
      
    /*GS_set_draw(GSD_BOTH);
    GS_ready_draw();
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_done_draw();
    GS_set_draw(GSD_BACK);*/
    
   /* endtime(); */

}

/********************************************/
/* callback for VECT button */
void
do_vectdraw(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    bgntime();
    GS_set_cancel(0);
    vect_all_draw(dc);
    GS_set_cancel(0);
    endtime();

}

/********************************************/
/* callback for SITE button */
void
do_sitedraw(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    GS_set_cancel(0);
    site_all_draw(dc);
    GS_set_cancel(0);
}

/********************************************/
/* callback for VECT POSITION panel */
void
do_fastvectdraw(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{
    GS_set_cancel(0);
    curvect_quick_draw(dc);
    GS_set_cancel(0);
}
/********************************************/
/* callback for CANCEL button */
void
cxl_draw(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

    GS_set_cancel(1);

}


/********************************************/
void 
check_button (void)
{
XEvent event;
Display *display = XtDisplay(Cxl_w);

    XSync (display, 0);
    
    while(XCheckTypedWindowEvent(display, XtWindow(Cxl_w), 
				ButtonPress | ButtonRelease, &event)){
	XtDispatchEvent(&event);
    }

}
/*
#define INDY 1
*/

/********************************************/
/* TEMPORARILY using double buffering for all drawing, should fix to
   better utilize all color bitplanes */
int 
surf_all_draw (data_cell *dc)
{
int i, j, nsurfs;
int sortSurfs[MAX_SURFS], sorti[MAX_SURFS];
int doclear;

    /* documentation for GL says will use front buffer for dest. pixel,
    but doesn't - so need to draw to both when using transparency */
    /* without alpha bit planes, suppose to draw back-to-front -
    for now, maybe just draw bottom surface to top ? (since we don't
    have easy access to a display list of polygons to sort) */ 

    doclear = XmToggleButtonGetState(dc->toggle_id[AUTO_CLEAR]); 

#ifdef INDY
GS_set_draw(GSD_BACK);
fprintf(stderr,"Drawing to back buff\n");
#else
/*
    if(GS_transp_is_set())
	GS_set_draw(GSD_BOTH);
    else
	GS_set_draw(GSD_FRONT);
*/


/* Set the default drawing buffer to GSD_BACK. If I leave it set to GSD_FRONT,
   It seems that the buffers get swapped and I only get wire frame views when writing  
   out omages. How this will affect transparency, as described above, I don't
   know. (E. Cline 1997) */
   
    GS_set_draw(GSD_BACK);
#endif

    nsurfs = GS_num_surfs();
    sort_surfs_max(dc->hSurf, sortSurfs, sorti, nsurfs);

    if(doclear)
	GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));

    GS_ready_draw();

    for(i=0; i<nsurfs; i++){
	if(sortSurfs[i]){      /* and turned on? */
	    GS_draw_surf(sortSurfs[i]);
	}
    }
    /* GS_draw_cplane_fence params will change - surfs aren't used anymore 
    for (i=0; i< MAX_CPLANES; i++){
	if(dc->Cp_on[i] )
	    GS_draw_cplane_fence(sortSurfs[0], sortSurfs[1], i);
    } */


    GS_done_draw(); 

    GS_set_draw(GSD_BACK);

}

/********************************************/
int 
vect_all_draw (data_cell *dc)
{
int i, nvects;
    
    nvects = GV_num_vects();

    GS_set_draw(GSD_FRONT);   /* in case transparency is set */
    GS_ready_draw();
    
    for(i=0; i<nvects; i++){
	GV_draw_vect(dc->hVect[i]);
    }

    GS_done_draw();
    GS_set_draw(GSD_BACK);
}

/********************************************/
int 
site_all_draw (data_cell *dc)
{
int i, nsites;
    
    nsites = GP_num_sites();

    GS_set_draw(GSD_FRONT);   /* in case transparency is set */
    GS_ready_draw();
    
    for(i=0; i<nsites; i++){
	GP_draw_site(dc->hSite[i]);
    }

    GS_done_draw();
    GS_set_draw(GSD_BACK);
}

/********************************************/
int 
curvect_quick_draw (data_cell *dc)
{

    GS_set_draw(GSD_BACK);
    GS_clear(rgb_int_from_pix(dc, dc->cells[BG_CELL]));
    GS_ready_draw();
    
    GV_draw_fastvect(dc->hVect[dc->CurVect]);

    GS_done_draw();
}

/********************************************/
/* callback for Draw Current Surface button */
void
draw_cursurf(w, dc, call_data)
Widget w;
data_cell *dc;
XmAnyCallbackStruct call_data;
{

    GS_set_cancel(0);
/*
changed all BOTHs to FRONT 3/15 BB - should be OK in OpenGL
    if(GS_transp_is_set())
	GS_set_draw(GSD_BOTH);
    else
	GS_set_draw(GSD_FRONT);  
*/
    GS_set_draw(GSD_FRONT);  

    GS_ready_draw();

    GS_draw_surf(dc->hSurf[dc->CurSurf]);

    GS_done_draw();

    GS_set_draw(GSD_BACK);

    GS_set_cancel(0);
}




