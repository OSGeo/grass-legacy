/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"

init_states ()
{
    Dig_Enabled = 1;	  /* Using Digitizer? */
    Extended_Edit = 1;	  /* Have Dig.plus?   */
    Beep_On = 1;		  /* Toggle beep      */
    Terse_On = 0;		  /* Terse Sub-Menus */
    Compress_File = 0;	  /* Remove deleted lines when writing files*/
    Label_Device = MOUSE; /* Labelling device (Mouse,Digitizer) */
    Window_Device = DIGITIZER; /* Windowing device (Mouse,Digitizer) */
    Digtiz_Device = DIGITIZER; /* Digitizing device (Mouse,Digitizer) */
    Point_Device = MOUSE; /* Pointing device (Mouse,Digitizer) */
    Changes_Made = 0;	  /* Changes made, Exit will warn or Not write*/
    Auto_Window = 0;	/* rewindow if chosen area extends outside */
}

set_default_display()
{
    Disp_overlay = 0;	/* Redraw Border Map after ReWindowinng */
    Disp_backdrop = 0;	/* Redraw Cell backdrop after ReWindowinng */
    Disp_lines =   1;	/* all lines */
    Disp_points  = 0;	/* all points in line */
    Disp_nodes =   1;	/* arc endpoints */
    Disp_labels =  0;	/* area labels */
    Disp_outline = 0;	/* area border lines, for labeled areas */
    Disp_markers = 1;	/* area ID markers (dot) */
    Disp_llines =  1;	/* labelled lines */
    Disp_llabels = 0;	/* line labels (category #s)*/
    Disp_thresh  = 0;	/* all snapping thresholds */
    Disp_sites =   1;   /* Sites */
    Disp_slabels = 0;   /* site labels*/
}
