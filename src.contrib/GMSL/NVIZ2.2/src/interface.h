#define TRACE_GS_FUNCS
/*----------------- this is the include file section -----------------*/
#define SGI
/*
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <tk.h>
#include "gis.h"
#ifdef SGI
/*#include <gl.h>*/
#include <GL/gl.h>
/*#include <gl/glws.h>*/
#endif
/*
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
*/
#include "gsurf.h"
#include "gstypes.h"

/* extern int *GV_get_vect_list(int *);
extern int *GS_get_surf_list(int *);
extern int *GP_get_site_list(int *); */


/*-------------- this is the data define section ---------------*/

#define X 0
#define Y 1
#define Z 2
#define W 3

#define SURF 0
#define VECT 1
#define SITE 2
#define VOL  3

/* I don't like this - to be consistant, should really do all scaling 
in library, so can send real world coords. (seems like it would make
type-ins & sliders easier, too) */
#define RANGE (5 * GS_UNIT_SIZE)
#define RANGE_OFFSET (2 * GS_UNIT_SIZE)
#define ZRANGE (3 * GS_UNIT_SIZE)
#define ZRANGE_OFFSET (1 * GS_UNIT_SIZE)

#define DEFAULT_SURF_COLOR 0x33BBFF
#define DEFAULT_WIRE_COLOR 0x999999
#define DEFAULT_WIRE_CNT 10 
#define DEFAULT_POLY_CNT 2

/* Attributes for vector and site files */
#define SV_ATT_WIDTH    -1 
#define SV_ATT_MARKER   -2
#define SV_ATT_SIZE     -3
#define SV_ATT_USEATT   -4
#define SV_ATT_DISPLAY  -5
#define SV_ATT_MAP      -6


/*------------------------------------------------------------------------ 
-            this is the data type declaration section                   -
------------------------------------------------------------------------*/

typedef struct{
        int id;
	float brt;
	float r, g, b;
	float ar, ag, ab;  /* ambient rgb */
	float x, y, z, w; /* position */
} light_data;

typedef struct {
        float Zrange, XYrange;
	
	int NumCplanes;
	int CurCplane, Cp_on[MAX_CPLANES];
	float Cp_trans[MAX_CPLANES][3];
	float Cp_rot[MAX_CPLANES][3];

	light_data light[MAX_LIGHTS];

	int BGcolor;
} Nv_data; 

/* - The following structure is used to associate client data with surfaces.
 * We do this so that we don't have to rely on the surface ID (which is libal to change
 * between subsequent executions of nviz) when saving set-up info to files.
 */

typedef struct {
  /* We use logical names to assign textual names to map objects.
     When Nviz needs to refer to a map object it uses the logical name
     rather than the map ID.  By setting appropriate logical names, we
     can reuse names inbetween executions of Nviz.  The Nviz library
     also provides a mechanism for aliasing between logical names.
     Thus several logical names may refer to the same map object.
     Aliases are meant to support the case in which two logical names
     happen to be the same.  The Nviz library automatically assigns
     logical names uniquely if they are not specified in the creation
     of a map object.  When loading a saved file containing several map
     objects, it is expected that the map 0bjects will be aliased to
     their previous names.  This ensures that old scripts will work.
     */

    char *logical_name;
  
} Nv_clientData;

/* Here are the functions that use these structures */

/* anim_support.c */
int Nset_interp_mode_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_tension_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nshowtension_start_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nupdate_tension_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nshowtension_stop_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nupdate_frames_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_numsteps_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nclear_keys_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nadd_key_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndelete_key_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nmove_key_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndo_framestep_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nshow_site_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nshow_vect_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nshow_path_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nwrite_rgb_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* change_view.c */
int Nchange_persp_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nchange_position_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nchange_height_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_light_to_view_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nchange_exag_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* cutplane_obj.c */
int Nset_fence_color_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_fence_color_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ncutplane_obj_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nnew_cutplane_obj_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nnum_cutplane_obj_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_current_cutplane_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_current_cutplane_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_cutplane_list_cmd(Nv_data *, Tcl_Interp *, int, char **);
void cp_draw(int, Nv_data *, int, int);
int draw_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int on_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int off_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int state_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_set_rot(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_set_trans(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_get_rot(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_get_trans(Nv_data *, Tcl_Interp *, int, char **, int);
int get_cp_idnum(char *);
/* draw.c */
void CancelFunc_Hook(void);
int Nunset_cancel_func_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_cancel_func_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_draw_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ntransp_is_set_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nis_masked_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nhas_transparency_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_def_color_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nclear_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndraw_wire_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndraw_X_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndraw_line_on_surf_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Ndraw_model_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nsurf_draw_one_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nvect_draw_one_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nsite_draw_one_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nsurf_draw_all_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_cancel_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nvect_draw_all_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nsite_draw_all_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nready_draw_cmd(void);
int Ndone_draw_cmd(void);
int check_blank(Tcl_Interp *, int);
/* exag.c */
int Nget_first_exag_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_height_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* glwrappers.c */
int tcl_color_to_int(char *);
int Nresize_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nfinish_cmd(ClientData, Tcl_Interp *, int, char **);
int Nset_background_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* init_commands.c */
int init_commands(Tcl_Interp *, Nv_data *);
/* label.c */
int Nplace_label_cmd(Nv_data *, Tcl_Interp *, int, char **);
void G_site_destroy_struct(void *);
/* lights.c */
int Nlight_obj_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nnew_light_cmd(Nv_data *, Tcl_Interp *);
int Nlights_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* map_obj.c */
int Nliteral_from_logical_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nlogical_from_literal_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nget_surf_list_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nget_vect_list_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nget_site_list_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nnew_map_obj_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int Nmap_obj_cmd(Nv_data *, Tcl_Interp *, int, char *[]);
int get_idnum(char *);
int get_type(char *);
int att_atoi(char *);
/* misc.c */
int Nlibinit_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_cancel_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_SDsurf_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nunset_SDsurf_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_SDscale_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* nviz_init.c */
int Ninit(Tcl_Interp *, Tk_Window);
int Ninitdata(Tcl_Interp *, Nv_data *);
int Ngetargs(Tcl_Interp *, char ***);
int parse_command(Nv_data *, Tcl_Interp *, int, char **);
/* position.c */
int Ninit_view_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_to_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_from_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nlook_here_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nhas_focus_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_focus_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_no_focus_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_focus_map_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nmove_to_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_fov_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_region_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_point_on_surf_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_dist_along_surf_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_cat_at_xy_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_val_at_xy_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_focus_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_longdim_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_zrange_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_xyrange_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_zextents_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nget_exag_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nset_exag_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nsave_3dview_cmd(Nv_data *, Tcl_Interp *, int, char **);
int Nload_3dview_cmd(Nv_data *, Tcl_Interp *, int, char **);
/* quick_draw.c */
int Nquick_draw_cmd(Nv_data *, Tcl_Interp *);
/* script_support.c */
int ScriptAddString_Cmd(ClientData, Tcl_Interp *, int, char **);
int CloseScripting_Cmd(ClientData, Tcl_Interp *, int, char **);
int SetState_Cmd(ClientData, Tcl_Interp *, int, char **);
int SetScriptFile_Cmd(ClientData, Tcl_Interp *, int, char **);
/* tkAppInit.c */
int Tcl_AppInit(Tcl_Interp *);
/* tkBind.c */
int TkCopyAndGlobalEval(Tcl_Interp *, char *);
/* tkSend.c */
char *Tk_SetAppName(Tk_Window, char *);
int Tk_SendCmd(ClientData, Tcl_Interp *, int, char **);
int TkGetInterpNames(Tcl_Interp *, Tk_Window);
/* tkSend_old.c */
int Tk_RegisterInterp(Tcl_Interp *, char *, Tk_Window);
int Tk_SendCmd(ClientData, Tcl_Interp *, int, char **);
int TkGetInterpNames(Tcl_Interp *, Tk_Window);
/* tkSpecial_Wait.c */
int Tk_Tkspecial_waitCmd(ClientData, Tcl_Interp *, int, char **);
int update_ranges(Nv_data *);
void cp_draw(int, Nv_data *, int, int);
int draw_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int on_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int off_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int state_cp_obj(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_set_rot(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_set_trans(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_get_rot(Nv_data *, Tcl_Interp *, int, char **, int);
int cp_get_trans(Nv_data *, Tcl_Interp *, int, char **, int);
int surf_draw_all(Nv_data *, Tcl_Interp *);
int init_default_slider_vals1(Nv_data *, float *, float *, float *);
int set_drawmode(Tcl_Interp *, int, int, Nv_data *, int, char *[]);
int get_drawmode(int, int, Nv_data *, Tcl_Interp *);
int delete_obj(int, int, Nv_data *, Tcl_Interp *);
int set_wirecolor(Tcl_Interp *, int, int, Nv_data *, int, char *[]);
int get_wirecolor(int, int, Nv_data *, Tcl_Interp *);
int set_nozero(Tcl_Interp *, int, int, Nv_data *, int, char *[]);
int get_nozero(int, int, Nv_data *, Tcl_Interp *, int, char *[]);
int load_obj(int, int, Nv_data *, int, char *[], Tcl_Interp *);
int get_att(int, int, Nv_data *, Tcl_Interp *, int, char *[]);
int set_att(int, int, Nv_data *, Tcl_Interp *, int, char *[]);
int get_mask_mode(int, int, Nv_data *, Tcl_Interp *);
int set_mask_mode(int, int, Nv_data *, Tcl_Interp *, int, char *[]);
int set_default_wirecolors(Nv_data *, int);
