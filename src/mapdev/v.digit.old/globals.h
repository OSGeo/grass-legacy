#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif
/* State flags  Most are Toggles */

GLOBAL char Data_Loaded;	  /* is there data in memory */
				/* used for abnormal exit logic */
GLOBAL char Files_Open;	  /* is there data in memory */


GLOBAL char Dig_Enabled;	  /* Using Digitizer? */
GLOBAL char Extended_Edit;	  /* Have Dig.plus?   */
GLOBAL char Beep_On;		  /* Toggle beep      */
GLOBAL char Terse_On;		  /* Terse Sub-Menus */
GLOBAL char Compress_File;	  /* Remove deleted lines when writing files*/
GLOBAL char Label_Device;	  /* Labelling device (Mouse,Digitizer */
GLOBAL char Window_Device;	  /* Windowing device (Mouse,Digitizer) */
GLOBAL char Digtiz_Device;	  /* Digitizing device (Mouse,Digitizer) */
GLOBAL char Point_Device;	  /* Pointing device (Mouse,Digitizer) */
GLOBAL char Changes_Made;	  /* Changes made, Exit will warn or Not write*/
	 			  /*   if no changes have been made  */
GLOBAL char Auto_Window;	  /* Rewindow if chosen area is outside window*/
GLOBAL int  Contour_Interval;	  /* spacing between contour lines */


GLOBAL char Coor_file[512];	/* argv[2] of digit.  Name of Coord file */


/* Menu control structures */
GLOBAL struct Menu_head M_main;
GLOBAL struct Menu_head M_global;
GLOBAL struct Menu_head Global_mask;
GLOBAL struct Menu_head M_digit;
GLOBAL struct Menu_head M_edit;
GLOBAL struct Menu_head M_custom;
GLOBAL struct Menu_head M_tool;
GLOBAL struct Menu_head M_label;
GLOBAL struct Menu_head M_window;
GLOBAL struct Menu_head M_debug;
GLOBAL struct Menu_head M_display;
GLOBAL struct Menu_head M_color;

GLOBAL struct Map_info       *Current_map;
GLOBAL struct Map_info       Map1;

GLOBAL struct Map_info       Overlay;
/*GLOBAL struct dig_head           overlay_head;*/

GLOBAL struct line_pnts  Gpoints;
GLOBAL P_AREA   Garea;

GLOBAL FILE *digit;

GLOBAL int dcolors[15];

GLOBAL char Disp_overlay;
GLOBAL char Disp_backdrop;
GLOBAL char Disp_lines;
GLOBAL char Disp_points;
GLOBAL char Disp_nodes;
GLOBAL char Disp_labels;
GLOBAL char Disp_outline;
GLOBAL char Disp_markers;
GLOBAL char Disp_llines;
GLOBAL char Disp_llabels;
GLOBAL char Disp_thresh;
GLOBAL char Disp_sites;
GLOBAL char Disp_slabels;
GLOBAL char Disp_over_label;

GLOBAL char CLR_LINE;
GLOBAL char CLR_AREA;
GLOBAL char CLR_SITE;
GLOBAL char CLR_LSITE;
GLOBAL char CLR_LLINE;
GLOBAL char CLR_LAREA;
GLOBAL char CLR_AMARK;
GLOBAL char CLR_ALABEL;
GLOBAL char CLR_LLABEL;
GLOBAL char CLR_HIGHLIGHT;
GLOBAL char CLR_ERASE;
GLOBAL char CLR_UNKNOWN;
GLOBAL char CLR_OVERLAY;
GLOBAL char CLR_0_NODE;
GLOBAL char CLR_1_NODE;
GLOBAL char CLR_2_NODE;


/* hold the names of files etc.  mostly used by main.c */
GLOBAL char *N_dig_file;
GLOBAL char *N_plus_file;
GLOBAL char *N_att_file;
GLOBAL char *N_coor_file;
GLOBAL char *N_digitizer;
GLOBAL char *N_path;
GLOBAL char *N_name;
GLOBAL char *N_PPID;
GLOBAL char *N_overlay;
GLOBAL char *N_label;
GLOBAL char *N_label_mapset;
GLOBAL char *N_backdrop;
GLOBAL char *N_backdrop_mapset;

GLOBAL char *N_subj_file;	/* scs */


/* these are a hack to get the point used to select a line
**  in find_line_with_mouse ()
**  They will contain the last point pointed to with the mouse
**   Currently only works with find_line_with_mouse ()
*/ 
GLOBAL double Point_X, Point_Y;

GLOBAL int In_Debug;
