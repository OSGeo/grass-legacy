#ifdef MAIN
#   define Global
    int ll_flag = 0;
    Pntlist *CurrPL = NULL;
    char EditChanges = 0;
    char Box = 0;
    int Cur_Line = 0;    /* line no. being edited */
#else
#   define Global extern
    extern int ll_flag;
    extern Pntlist *CurrPL;
    extern char EditChanges;
    extern char Box;
    extern int Cur_Line;    /* line no. being edited */
#endif
/* State flags  Most are Toggles */

Global char Data_Loaded;	  /* is there data in memory */
				/* used for abnormal exit logic */
Global char Files_Open;	  /* is there data in memory */


Global char Dig_Enabled;	  /* Using Digitizer? */
Global char Extended_Edit;	  /* Have Dig.plus?   */
Global char Beep_On;		  /* Toggle beep      */
Global char Terse_On;		  /* Terse Sub-Menus */
Global char Compress_File;	  /* Remove deleted lines when writing files*/
Global char Label_Device;	  /* Labelling device (Mouse,Digitizer */
Global char Window_Device;	  /* Windowing device (Mouse,Digitizer) */
Global char Digtiz_Device;	  /* Digitizing device (Mouse,Digitizer) */
Global char Point_Device;	  /* Pointing device (Mouse,Digitizer) */
Global char Changes_Made;	  /* Changes made, Exit will warn or Not write*/
	 			  /*   if no changes have been made  */
Global char Auto_Window;	  /* Rewindow if chosen area is outside window*/
Global int  Contour_Interval;	  /* spacing between contour lines */


Global char Coor_file[512];	/* argv[2] of digit.  Name of Coord file */



Global struct Map_info       *Current_map;
Global struct Map_info       Map1;

Global struct Map_info       Overlay;
/*Global struct dig_head           overlay_head;*/

Global struct line_pnts  Gpoints;
Global P_AREA   Garea;

Global FILE *digit;

Global unsigned long dcolors[15];

Global char Disp_overlay;
Global char Disp_backdrop;
Global char Disp_lines;
Global char Disp_points;
Global char Disp_nodes;
Global char Disp_labels;
Global char Disp_outline;
Global char Disp_markers;
Global char Disp_llines;
Global char Disp_llabels;
Global char Disp_thresh;
Global char Disp_sites;
Global char Disp_slabels;
Global char Disp_over_label;

Global char CLR_LINE;
Global char CLR_AREA;
Global char CLR_SITE;
Global char CLR_LSITE;
Global char CLR_LLINE;
Global char CLR_LAREA;
Global char CLR_AMARK;
Global char CLR_ALABEL;
Global char CLR_LLABEL;
Global char CLR_HIGHLIGHT;
Global char CLR_ERASE;
Global char CLR_UNKNOWN;
Global char CLR_OVERLAY;
Global char CLR_0_NODE;
Global char CLR_1_NODE;
Global char CLR_2_NODE;


/* hold the names of files etc.  mostly used by main.c */
Global char *N_dig_file;
Global char *N_plus_file;
Global char *N_att_file;
Global char *N_coor_file;
Global char *N_digitizer;
Global char *N_path;
Global char *N_name;
Global char *N_PPID;
Global char *N_overlay;
Global char *N_label;
Global char *N_label_mapset;
Global char *N_backdrop;
Global char *N_backdrop_mapset;
Global char *N_lockname;

Global char *N_subj_file;	/* scs */


/* these are a hack to get the point used to select a line
**  in find_line_with_mouse ()
**  They will contain the last point pointed to with the mouse
**   Currently only works with find_line_with_mouse ()
*/ 
Global double Point_X, Point_Y;


#ifdef SCS_MODS
Global char *N_subj_file;
Global double last_prune_thresh;
Global double last_snap_thresh;
#endif /* SCS_MODS */

/* display variables */
Global Display *dpy;
Global Widget  canvas;
Global Widget  Mainform;
Global Widget choose;
Global Widget Cancel;
Global Widget toplevel;
Global int action;
Global int Winx, Winy, Wdth, Hght;
Global int Text;
Global GC gc;
Global GC gcxor;
Global GC gcdotted;
Global Pixmap pix;

/* display variables for line editing (pull stuff) */
Global int Pull_flag;

Global void *PLtoken;
Global int NewPL;
Global Pntlist *PLtop;
Global long *Screen_buf;
Global float Xdim, Ydim;
Global struct line_pnts *Cur_Points; /* Points struct for line being edited */
Global struct line_pnts *Last_Points; /* Points struct for last edit */
Global int SegIndex[3];  /* indices of left, pull, & right points current seg */
Global int Anchors_set;
Global int Closed;
Global int PullIndex;   /* index of pullpoint in Pbuf */
Global int Newseg;
Global int Pulled;
Global int Numppts;
Global double Pbuf[MAX_PULL_PTS][2];
Global XPoint xbuf[MAX_PULL_PTS];
Global double Anchor[2][2];
Global double Pullpoint[2];
Global double Lastmove[2];
Global double SculptRad;
Global double Pi;
Global double (*pullfunc)();
Global int (*fillfunc)();
Global Widget Apickanchors, Apull, Accept;
Global Widget Asculpt, Asculptreset, Asculptsiz, Ablot, Ablotsiz;
Global Widget Aantigrav, Agravextent, Ashowpts, Awhole;
Global Widget Aclean, Acleanres, Ascale;
Global Widget Afillfunc1, Afillfunc2, Afillfunc3, Afillfunc4;
Global Widget Afunc0, Afunc1, Afunc2, Afunc3, Afunc4, Afunc5, Afunc6;
Global Widget Acurfunc, Afunchshape, Afuncvshape, Ashapereset;
Global Widget Adraw, Aquit, Aclear;
Global Widget Atest;
Global char Editing;

Global char  record_name[1024];
Global char  play_name[1024];
Global int   record,play;
FILE  *fd_record,*fd_play;

Global double   Pix_size;



