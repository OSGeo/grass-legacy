

/*------------------------------------------
these are the assignments for the panels:
--------------------------------------------
panels[0] = reserved for special purposes
panels[1] = script panel
panels[2] = vector panel
panels[3] = light panel
panels[4] = rendering panel
panels[5] = colors panel
panels[6] = cplane panel
panels[7] = whats here panel
panels[8] = animation panel

--------------------------------------------
these are the assignments for the sliders:
--------------------------------------------
sliders[0] = reserved for special purposes 
sliders[1] = perspective on main control panel
sliders[2] = height on main control panel
sliders[3] = z exaggeration on main control panel
sliders[4] = vectorZ on vector control panel
sliders[5] = brightness on light control panel
sliders[6] = red on light control panel
sliders[7] = green on light control panel
sliders[8] = blue on light control panel
sliders[9] = ambient on light control panel
sliders[10] = height on light control panel

NOTE: the slider_txt assignments match those of their
respective counterparts.

*/


/*----------------- this is the include file section -----------------*/
#include "gsurf.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/Form.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/ScrolledW.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
#include <Xm/SeparatoG.h>
#include <Xm/Separator.h>
#include <Xm/LabelG.h>
#include <Xm/Label.h>
#include <Xm/CascadeB.h>
#include "Browser.h"
#include "Interact.h"


/*-------------- this is the function declaration section --------------*/

extern update_ranges();
extern char **s_labels();
extern char **sf_labels();
extern char **v_labels();
extern Widget colorbar(); 
extern Widget make_arrows();
extern Widget make_fsbox();
extern Widget make_position();
extern Widget make_simple_options();
extern Widget make_slider(); 
extern Widget make_text(); 
extern Widget make_title(); 
extern unsigned long get_default_draw_color();
extern void SetPositionArgs(); 
extern void _update_arrows(); 
extern void add_me(); 
extern void add_look(); 
extern void look_here(); 
extern void before_pops(); 
extern void change_att();
extern void change_cursurf_name();
extern void change_exag();
extern void change_zsep();
extern void change_label();
extern void change_persp(); 
extern void change_xypos(); 
extern void position_light_xy(); 
extern void position_light_z(); 
extern void update_lightvals(); 
extern void change_height(); 
extern void check_space(); 
extern void cleartxt(); 
extern void close_me(); 
extern void close_destroy_me(); 
extern void closeb(); 
extern void color_panel(); 
extern void cp_alloff(); 
extern void cplane_panel(); 
extern void color_pushed(); 
extern void curatt_change_rgb();
extern void curatt_reset();
extern void curatt_set_const();
extern void curatt_update_status();
extern void cursite_change_rgb();
extern void cursite_setsurf();
extern void unset_site_colorfile();
extern void cursite_set_colorfile();
extern void cursurf_change_gridcolor();
extern void cursurf_gridcolor_surface();
extern void cursurf_set_curatts();
extern void cursurf_update_attlabels();
extern void curvect_change_rgb();
extern void curvect_setsurf();
extern void destroy_cb(); 
extern void do_draw(); 
extern void do_clear(); 
extern void do_fastvectdraw(); 
extern void do_vectdraw(); 
extern void do_sitedraw(); 
extern void cxl_draw(); 
extern void drawpuck(); 
extern void draw_crosshair(); 
extern void draw_cursurf(); 
extern void exit_now(); 
extern void exposed();
extern void exp2(); 
extern void fcolor_above();
extern void fcolor_below();
extern void fcolor_blend();
extern void fcolor_grey();
extern void fcolor_off();
extern void get_pixel_color();
extern void inform();
extern void init();
extern void init_atts();
extern void installcolormap();
extern void lgtexp(); 
extern void light_panel(); 
extern void load_colors(); 
extern void make_main_control_panel(); 
extern void make_primary_controls(); 
extern void next_surf(); 
extern void new_site(); 
extern void new_surf(); 
extern void new_vect(); 
extern void nyi_pops(); 
extern void bf_unavail(); 
extern void delete_site(); 
extern void delete_surf(); 
extern void delete_vect(); 
extern void octo(); 
extern void pop_attr(); 
extern void pop_color(); 
extern void pop_transl(); 
extern void pop_imgdump(); 
extern void pops(); 
extern void prev_surf(); 
extern void rendering_panel(); 
extern void reshow(); 
extern void resized();
extern void save_img();
extern void animation_panel(); 
extern void separate(); 
extern void set_active_cplanes(); 
extern void set_const2();
extern void set_cp_widgets();
extern void set_crosshair();
extern void set_cur_cplane();
extern void set_cur_site();
extern void set_cur_surf();
extern void set_cur_vect();
extern void set_pixel_color();
extern void set_slider_txt();
extern void set_trans_widgets();
extern void show_it(); 
extern void sites_panel(); 
extern void site_set_displaymode();
extern void surf_set_displaymode();
extern void vect_set_displaymode();
extern void site_update_displaymode();
extern void surf_update_displaymode();
extern void vect_update_displaymode();
extern void surf_translate();
extern void surface_panel(); 
extern void toggle();
extern void trackmouse(); 
extern void trackmouse2(); 
extern void unmanage_cb(); 
extern void unset_gridcolor(); 
extern void update_arrows(); 
extern void update_color(); 
extern void update_cb(); 
extern void update_sliders(); 
extern void vector_panel(); 
extern void whats_here_panel(); 
extern void xman_create_menu_items(); 
extern void xypos_redraw(); 

/*--------------These functions are my callbacks for the animation panel (E. Cline 1997)--------------*/

extern void show_current_frame_number();
extern void run_animation();
extern void stop_animation();
extern void set_number_frames();
extern void step_frame_back();
extern void step_frame_forward();
extern void update_frame_slider();
extern void add_keyframe();
extern void clear_all_keyframes();
extern void show_path_vect_site();
extern void set_interpolation_type();
extern void set_spline_interpolation_level();
extern void run_and_save_anim();
extern void close_panel(); 

extern int targa_out();
/*-------------- this is the data define section ---------------*/

#define X 0
#define Y 1
#define Z 2
#define W 3

/* I don't like this - to be consistant, should really do all scaling 
in library, so can send real world coords. (seems like it would make
type-ins & sliders easier, too) */
#define RANGE (5 * GS_UNIT_SIZE)
#define RANGE_OFFSET (2 * GS_UNIT_SIZE)
#define ZRANGE (3 * GS_UNIT_SIZE)
#define ZRANGE_OFFSET (1 * GS_UNIT_SIZE)

#define NOTWIDGET -1

/* defines for attribute constant2 */
#define C2_NOZEROS  1
#define C2_NOTSET   2
#define C2_INVMASK  3

#define DEFAULT_SURF_COLOR 0x33BBFF
#define DEFAULT_WIRE_COLOR 0x999999
#define DEFAULT_WIRE_CNT 10 
#define DEFAULT_POLY_CNT 2

/* -- panel defines -- */
#define SCRIPT 1
#define VECTOR 2 
#define LIGHT 3 
#define SITES  4 
#define COLOR 5 
#define WHAT 6 
#define ANIM 7 
#define SURFACE 8 
#define CPLANE 9 

#define ALL -1 
#define TOP 0
#define MIDDLE 49 
#define BOTTOM 51
#define FORM 100

/* -- file defines -- */
#define COLOR_FILE 1000 
#define ELEVATION_FILE 1001 
#define VECTOR_FILE 1002 
#define SITES_FILE 1003 
#define SITES_COL_FILE 1004 
#define RASTER_FILE 2000 

/* -- button(toggle) defines -- */
#define LOOK_HERE 1
#define LOOK_CENTER 2
#define LOOK_CANCEL 3
#define MAIN_RESET 4
#define SITE_ONSURF 5
#define SITE_3D 6
#define BGN_READ 7
#define END_READ 8
#define LIGHTS_RND 9 
#define FRINGE_RND 10 
#define SRF_ONLY_RND 11 
#define GOURAUD_RND 12 
#define AUTO_CLEAR 13 
#define FLAT_RND 14 
#define COL_SITEFILE 15 
#define WIRE_RND 16 
#define COL_WIRE_RND 17 
#define POLY_RND 18 
#define WIRE_POLY_RND 19 
#define FOLLOW_VIEW_LGT 20 
#define SHOW_MODEL_LGT  21 
#define V_COLOR         22 
#define S_COLOR         23 
#define BG_COLOR        24 
#define SITE_X 25
#define SITE_SPHERE 26
#define SITE_DIAMOND 27
#define VECT_MEM 28
#define SCOPE_CSO 29
#define SCOPE_AS 30
#define NZ_TOPO  31
#define NZ_COLOR 32
#define INV_MASK 33

/*----My toggle button defines (E. Cline)--- */

#define ANIM_P  34
#define ANIM_V  35
#define ANIM_S  36
#define ANIM_SP 37
#define ANIM_L  38

/* -- max amount defines -- increased no sliders and toggles*/
#define MAX_SLIDERS 23  

/* #define MAX_GLOBALS 50 this isn't being used, hence the comments (E. Cline 1997) */

#define MAX_TOGGLES 39 

#define MAX_ARROWS_N 4 

#define MAX_PANELS 17
#define MAX_GLOBALS 50
#define MAX_DYN_COLORS 9 

/* -- arrow defines -- */
#define VWIDTH_ARWS 0
#define P_RES_ARWS  1
#define G_RES_ARWS  2
#define SWIDTH_ARWS 3

/* -- slider defines -- */
#define MAIN_PSP  1 
#define MAIN_HGT  2 
#define MAIN_ZEX  3 
#define SURF_ZTR  4 
#define VECT_ZEX  5 
#define LITE_BGT  6 
#define LITE_RED  7 
#define LITE_GRN  8 
#define LITE_BLU  9 
#define LITE_AMB  10
#define LITE_HGT  11 
#define LITE_SHN  12 
#define COL_RED   13 
#define COL_GRN   14 
#define COL_BLU   15 
#define ATTR_CON  16 
#define CPL_ROT   17 
#define CPL_TILT  18 
#define CPL_ZTR   19 
#define SITE_SIZ  20 

/*----My slider defines (E. Cline)--- */

#define ANIM_FRMS  21
#define ANIM_SPLN  22
 
/* -- select color define -- */
#define SELECT_COLOR   501

/* -- allocated color cell defines -- */
#define RED_CELL  0
#define GRN_CELL  1
#define BLU_CELL  2 
#define VECT_CELL 3
#define SITES_CELL 4
#define BG_CELL   5
#define SURF_CELL 6
#define GRID_CELL 7
#define TMP_CELL 8

#define SLIDER_VAL_REAL(dc, con) (dc->slider_values[con] *      \
			     (dc->slider_max[con] - dc->slider_min[con]) + \
			     dc->slider_min[con])

#define UNIT_OF(max,min,val) (((float)val-min)/(max - min))


/*------------------------------------------------------------------------ 
-            this is the data type declaration section                   -
------------------------------------------------------------------------*/

#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef struct{
	Widget form, close;
	int im_already_open, where;
	XmString name;
}panel_info;

typedef struct{
	int toph, botth;
}place;


typedef struct {
	int toggles[MAX_TOGGLES];
        float slider_value[MAX_SLIDERS]; 
        float globals[MAX_GLOBALS];
}variables;



typedef struct{
	Widget up, down, txt;
	int min, max, val;
}arrow_pair;

typedef struct{
	char name[20], status[128], map_name[128];
	int use_map;
	float constant, constant2;
	char r,g,b;
}att_info;

typedef struct{
	char surfname[40];  /* should alredy be shortened from att_info */ 
	int draw_mode;  /*DM_GOURAUD | 
			    DM_COL_WIRE, DM_POLY, DM_WIRE, DM_WIRE_POLY*/
	long wire_color; /* 0xBBGGRR or WC_COLOR_ATT */
	int polycnt, wirecnt; /* cells per polygon, per wire mesh */
	float zexag;   
	float xtrans, ytrans, ztrans;   
}surf_dm;                            /* surface display mode */	

typedef struct{
	char vectname[128];  
	long color;     /* 0xBBGGRR */
	int width;      /* pixel width */
	int use_mem;      /* load into memory */
	float pivpt[2], zrot;   
	float xtrans, ytrans, ztrans;   
}vect_dm;                            /* vector display mode */	

typedef struct{
	char sitename[128];  
	long color;     /* 0xBBGGRR */
	int marker;      /* ST_X, ST_SPHERE, ST_DIAMOND, etc */
	int width;
	int attrmode;
	int use_z, has_z, has_att;
	float size;
	float pivpt[2], zrot;   
	float xtrans, ytrans, ztrans;   
}site_dm;                            /* sites display mode */	

typedef struct {
	Widget att_status, status, status_frame;
	Widget Wwhatshere, Wtxt, *Wstr, Wframe;
	Widget sliders[MAX_SLIDERS], slider_txt[MAX_SLIDERS];
	Widget Mzex_rc, Mper_rc, Mhgt_rc;
        Widget toplevel, menubar, frm, xy_position;
        Widget Sscript_file, Sbegin_read, Sstop_read, Sreading;
        Widget redraw_holder, redraw_buttons[4];
        Widget look[3], reset;
        Widget monitor, monitor_frame, mouse_read;
        Widget form_for_main_control_panel, frame_for_main_control_panel;
        Widget form_for_aux_control_panel, frame_for_aux_control_panel;
	Widget Lfollow_view, Lshow_model, Lbrightness, Lb_rc, Lbright_rc;
	Widget Lr_rc, Lg_rc, Lheight_rc, Lambient_rc, Lshiney_rc;
	Widget Lxy_pos;
	Widget Vscroll;
	Widget Sscroll;
	Widget RCurSurfno, RCurVectno, RCurSiteno;
	Widget Wcp_act;
	Widget Swirecolor, Stranslate;
	Widget color_pop_lab, showcolor, Wcolor_pop; /*could be local static*/
	Widget fsbox, Watt[MAX_ATTS], Wtrans_pop, Wattr_pop, att_label[MAX_ATTS];
	float debug[50];
	float Zrange, XYrange;
	att_info Atts[MAX_SURFS][MAX_ATTS];
	att_info Cur_Atts[MAX_ATTS];
	int CurSurf, CurAtt;
	int CurCplane, Cp_on[MAX_CPLANES];
	surf_dm Surf_Settings[MAX_SURFS];
	int hSurf[MAX_SURFS];              /* handles to surfaces */

	int CurVect;
	int hVect[MAX_VECTS];              /* handles to vectorsets */
	vect_dm Vect_Settings[MAX_VECTS];

	int CurSite;
	int hSite[MAX_SITES];              /* handles to sitesets */
	site_dm Site_Settings[MAX_SITES];

	arrow_pair parrows[MAX_ARROWS_N];
	XmTextPosition current_position;
	Display *dpy;
	Screen *screen_size;
	GC gc;
	XColor colors;
	Colormap cmap;
	unsigned short cells[MAX_DYN_COLORS]; /* dynamic color cells */
	unsigned int Vcolor, Scolor, Lcolor, BGcolor, GRcolor;
	panel_info panels[MAX_PANELS];
	place here;
	int top,bott;
	int Wset;
	int scr; 
	float x,y;
	int previous,constant; 
	Widget toggle_id[MAX_TOGGLES];   /* TODO: get rid of above dupes -- Done (E. Cline 1997)*/
	int toggles[MAX_TOGGLES];
	float slider_values[MAX_SLIDERS];
	/* variables vars; */
	float slider_min[MAX_SLIDERS];
	float slider_max[MAX_SLIDERS];
}data_cell; 




typedef struct _menu_struct{
  char *name;                     /* Name of the button */
  void (*func)();                 /* Callback to be invoked */
  data_cell *data;                /* Data for the callback */
  struct _menu_struct *sub_menu;  /* Data for the submenu */
  int n_sub_items;                /* Items in the submenu */
  char *sub_menu_title;           /* Title of submenu */
} xman_menu_struct;

