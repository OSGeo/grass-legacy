/*
* $Id$
*
****************************************************************************
* 	      NECTEC -- High Performance Computing Center --
*
* MODULE: 	GRASS ogsf library
* FILENAME:	local_proto.h
* AUTHOR(S):	Justin Hickey - Bangkok Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE: 	To list the prototypes for all accessible functions in this
*   	    	library.
* DATE CREATED: Nov 01 2000
* COPYRIGHT:  	(C) 2000 by the GRASS Development Team
*
*   	    	This program is free software under the GPL (>=v2)
*   	    	Read the file COPYING that comes with GRASS for details.
*
*****************************************************************************/

#ifndef _OGSF_LOCAL_PROTO_H
#define _OGSF_LOCAL_PROTO_H

/*========================= Required Include Files =========================*/

#include "gis.h"
#include "gstypes.h"
#include "kftypes.h"

/*===================== User Visible Constants/Defines =====================*/

/* none */

/*====================== Internal Types and Structures =====================*/

/* none */

/*========================== Function Prototypes ===========================*/

/* From GK2.c */
int GK_set_interpmode(int);
void GK_set_tension(float);
void GK_showtension_start(void);
void GK_showtension_stop(void);
void GK_update_tension(void);
void GK_update_frames(void);
void GK_set_numsteps(int);
void GK_clear_keys(void);
int GK_move_key(float, float, float);
int GK_delete_key(float, float, int);
int GK_add_key(float, unsigned long, int, float);
void GK_do_framestep(int, int);
void GK_show_path(int);
void GK_show_vect(int);
void GK_show_site(int);

/* From GP2.c */
int GP_site_exists(int);
int GP_new_site(void);
int GP_num_sites(void);
int *GP_get_site_list(int *);
int GP_delete_site(int);
int GP_load_site(int, char *);
int GP_get_sitename(int, char *);
int GP_get_sitemode(int, int *, int *, int *, float *, int *);
int GP_set_sitemode(int, int, int, int, float, int);
int GP_attmode_color(int, char *);
int GP_attmode_none(int);
int GP_set_zmode(int, int);
int GP_get_zmode(int, int *);
void GP_set_trans(int, float, float, float);
void GP_get_trans(int, float *, float *, float *);
int GP_select_surf(int, int);
int GP_unselect_surf(int, int);
int GP_surf_is_selected(int, int);
void GP_draw_site(int);
void GP_alldraw_site(void);
int GP_Set_ClientData(int, void *);
void *GP_Get_ClientData(int);

/* From GS2.c */
void void_func(void);
void GS_libinit(void);
int GS_get_longdim(float *);
int GS_get_region(float *, float *, float *, float *);
void GS_set_att_defaults(float *, float *);
int GS_surf_exists(int);
int GS_new_surface(void);
int GS_new_light(void);
void GS_setlight_position(int, float, float, float, int);
void GS_setlight_color(int, float, float, float);
void GS_setlight_ambient(int, float, float, float);
void GS_lights_off(void);
void GS_lights_on(void);
void GS_switchlight(int, int);
int GS_transp_is_set(void);
void GS_get_modelposition1(float *);
void GS_get_modelposition(float *, float *);
void GS_draw_X(int, float *);
void GS_draw_line_onsurf(int, float, float, float, float);
int GS_draw_nline_onsurf(int, float, float, float, float, float *, int);
void GS_draw_flowline_at_xy(int, float, float);
void GS_draw_lighting_model1(void);
void GS_draw_lighting_model(void);
int GS_update_curmask(int);
int GS_is_masked(int, float *);
void GS_unset_SDsurf(void);
int GS_set_SDsurf(int);
int GS_set_SDscale(float);
int GS_get_SDsurf(int *);
int GS_get_SDscale(float *);
int GS_update_normals(int);
int GS_get_att(int, int, int *, float *, char *);
int GS_get_cat_at_xy(int, int, char *, float, float);
int GS_get_norm_at_xy(int, float, float, float *);
int GS_get_val_at_xy(int, int, char *, float, float);
int GS_unset_att(int, int);
int GS_set_att_const(int, int, float);
int GS_set_maskmode(int, int);
int GS_get_maskmode(int, int *);
int GS_Set_ClientData(int, void *);
void *GS_Get_ClientData(int);
int GS_num_surfs(void);
int *GS_get_surf_list(int *);
int GS_delete_surface(int);
int GS_load_att_map(int, char *, int);
void GS_draw_surf(int);
void GS_draw_wire(int);
void GS_alldraw_wire(void);
void GS_alldraw_surf(void);
void GS_set_exag(int, float);
void GS_set_global_exag(float);
float GS_global_exag(void);
void GS_set_wire_color(int, int);
int GS_get_wire_color(int, int *);
int GS_setall_drawmode(int);
int GS_set_drawmode(int, int);
int GS_get_drawmode(int, int *);
void GS_set_nozero(int, int, int);
int GS_get_nozero(int, int, int *);
int GS_setall_drawres(int, int, int, int);
int GS_set_drawres(int, int, int, int, int);
void GS_get_drawres(int, int *, int *, int *, int *);
void GS_get_dims(int, int *, int *);
int GS_get_exag_guess(int, float *);
void GS_get_zrange_nz(float *, float *);
void GS_set_trans(int, float, float, float);
void GS_get_trans(int, float *, float *, float *);
unsigned int GS_default_draw_color(void);
unsigned int GS_background_color(void);
void GS_set_draw(int);
void GS_ready_draw(void);
void GS_done_draw(void);
void GS_set_focus(float *);
int GS_get_focus(float *);
void GS_set_focus_center_map(int);
void GS_moveto(float *);
void GS_moveto_real(float *);
int GS_get_zextents(int, float *, float *, float *);
int GS_get_zrange(float *, float *, int);
void GS_get_from(float *);
void GS_get_from_real(float *);
void GS_get_to(float *);
void GS_get_viewdir(float *);
void GS_set_viewdir(float *);
void GS_set_fov(int);
int GS_get_fov(void);
int GS_get_twist(void);
void GS_set_twist(int);
void GS_set_nofocus(void);
void GS_set_viewport(int, int, int, int);
int GS_look_here(int, int);
int GS_get_selected_point_on_surface(int, int, int *, float *, float *,
    float *);
void GS_set_cplane_rot(int, float, float, float);
void GS_set_cplane_trans(int, float, float, float);
void GS_draw_cplane(int);
int GS_draw_cplane_fence(int, int, int);
void GS_alldraw_cplane_fences(void);
void GS_set_cplane(int);
void GS_unset_cplane(int);
void GS_get_scale(float *, float *, float *, int);
void GS_set_fencecolor(int);
int GS_get_fencecolor(void);
int GS_get_distance_alongsurf(int, float, float, float, float, float *, int);
int GS_save_3dview(char *, int);
int GS_load_3dview(char *, int);
void GS_init_view(void);
void GS_clear(int);
double GS_get_aspect(void);
int GS_has_transparency(void);

/* From GSX.c */
int GS_check_cancel(void);
void GS_set_cancel(int);
void GS_set_cxl_func(void (*)(void));
void GS_set_swap_func(void (*)(void));

/* From GS_util.c */
double GS_geodistance (double *, double *, char *);
float GS_distance (float *, float *);
float GS_P2distance (float *, float *);
void GS_v3eq(float *, float *);
void GS_v3add(float *, float *);
void GS_v3sub(float *, float *);
void GS_v3mult(float *, float);
int GS_v3norm(float *);
int GS_v2norm(float *);
int GS_dv3norm(double *);
int GS_v3normalize(float *, float *);
int GS_v3dir(float *, float *, float *);
void GS_v2dir(float *, float *, float *);
void GS_v3cross(float *, float *, float *);
void GS_v3mag(float *, float *);
int GS_coordpair_repeats(float *, float *, int);

/* From GV2.c */
int GV_vect_exists(int);
int GV_new_vector(void);
int GV_num_vects(void);
int *GV_get_vect_list(int *);
int GV_delete_vector(int);
int GV_load_vector(int, char *);
int GV_get_vectname(int, char *);
int GV_set_vectmode(int, int, int, int);
int GV_get_vectmode(int, int *, int *, int *);
void GV_set_trans(int, float, float, float);
int GV_get_trans(int, float *, float *, float *);
int GV_select_surf(int, int);
int GV_unselect_surf(int, int);
int GV_surf_is_selected(int, int);
void GV_draw_vect(int);
void GV_alldraw_vect(void);
void GV_draw_fastvect(int);
int GV_Set_ClientData(int, void *);
void *GV_Get_ClientData(int);

/* From GVL2.c */
int GVL_vol_exists(int);
int GVL_new_volume(void);
int GVL_num_vols(void);
int *GVL_get_vol_list(int *);
int GVL_delete_vol(int);
int GVL_load_volume(int, char *);
int GVL_get_volname(int, char *);
int GVL_set_volmode(int, int, int, int);
int GVL_get_volmode(int, int *, int *, int *);
void GVL_set_exag(int, float);
void GVL_set_trans(int, float, float, float);
int GVL_get_trans(int, float *, float *, float *);
void GVL_draw_vol(int);
void GVL_alldraw_vol(void);
void GVL_draw_fastvol(int);
int GVL_Set_ClientData(int, void *);
void *GVL_Get_ClientData(int);

/* From Gp3.c */
int Gp_set_color(char *, geopoint *);
geopoint *Gp_load_sites(char *, int *, int *, int *);

/* From Gs3.c */
void Gs_warning(char *);
void Gs_status(char *);
double Gs_distance(double *, double *);
int Gs_loadmap_as_float(struct Cell_head *, char *, float *, struct BM *,
    int *);
int Gs_loadmap_as_int(struct Cell_head *, char *, int *, struct BM *, int *);
int Gs_numtype(char *, int *);
int Gs_loadmap_as_short(struct Cell_head *, char *, short *, struct BM *,
    int *);
int Gs_loadmap_as_char(struct Cell_head *, char *, unsigned char *,
    struct BM *, int *);
int Gs_loadmap_as_bitmap(struct Cell_head *, char *, struct BM *);
int Gs_build_256lookup(char *, int *);
void Gs_pack_colors(char *, int *, int, int);
void Gs_pack_colors_float(char *, float *, int *, int, int);
int Gs_get_cat_label(char *, int, int, char *);
int Gs_save_3dview(char *, geoview *, geodisplay *, struct Cell_head *,
    geosurf *);
int Gs_load_3dview(char *, geoview *, geodisplay *, struct Cell_head *,
    geosurf *);
int Gs_update_attrange(geosurf *, int);

/* From Gv3.c */
geoline *Gv_load_vect(char *, int *);
void add_Vectmem(int);
void sub_Vectmem(int);
void show_Vectmem(void);

/* From Gvol.c */
int Gvol_open_dspf(char *, char *, geodsp *);

/* From gk.c */
Keylist *gk_copy_key(Keylist *);
unsigned long gk_get_mask_sofar(float, Keylist *);
int gk_viable_keys_for_mask(unsigned long, Keylist *, Keylist **);
void gk_follow_frames(Viewnode *, int, Keylist *, int, int, int, unsigned long);
void gk_free_key(Keylist *);
Viewnode *gk_make_framesfromkeys(Keylist *, int, int, int, float);
double get_key_neighbors(int, double, double, int, Keylist **, Keylist **,
    Keylist **, Keylist **, Keylist **, double *, double *);
double lin_interp(float, float, float);
double get_2key_neighbors(int, float, float, int, Keylist **, Keylist **,
    Keylist **);
Viewnode *gk_make_linear_framesfromkeys(Keylist *, int, int, int);
void correct_twist(Keylist *);
int gk_draw_path(Viewnode *, int, Keylist *);

/* From gp.c */
geosite *gp_get_site(int);
geosite *gp_get_prev_site(int);
int gp_num_sites(void);
geosite *gp_get_last_site(void);
geosite *gp_get_new_site(void);
void gp_update_drapesurfs(void);
int gp_set_defaults(geosite *);
void print_site_fields(geosite *);
int gp_init_site(geosite *);
void gp_delete_site(int);
int gp_free_site(geosite *);
void gp_free_sitemem(geosite *);
void gp_set_drapesurfs(geosite *, int *, int);

/* From gpd.c */
int gs_point_in_region(geosurf *, float *, float *);
void gpd_obj(geosurf *, int, float, int, Point3);
int gpd_2dsite(geosite *, geosurf *, int);
int gpd_3dsite(geosite *, float, float, int);

/* From gs.c */
void gs_err(char *);
void gs_init(void);
geosurf *gs_get_surf(int);
geosurf *gs_get_prev_surface(int);
int gs_getall_surfaces(geosurf **);
int gs_num_surfaces(void);
int gs_att_is_set(geosurf *, IFLAG);
geosurf *gs_get_last_surface(void);
geosurf *gs_get_new_surface(void);
int gs_init_surf(geosurf *, double, double, int, int, double, double);
int gs_init_normbuff(geosurf *);
void print_frto(float (*)[4]);
void print_realto(float *);
void print_256lookup(int *);
void print_surf_fields(geosurf *);
void print_view_fields(geoview *);
void gs_set_defaults(geosurf *, float *, float *);
void gs_delete_surf(int);
int gs_free_surf(geosurf *);
void gs_free_unshared_buffs(geosurf *);
int gs_num_datah_reused(int);
int gs_get_att_type(geosurf *, int);
int gs_get_att_src(geosurf *, int);
typbuff *gs_get_att_typbuff(geosurf *, int, int);
int gs_malloc_att_buff(geosurf *, int, int);
int gs_malloc_lookup(geosurf *, int);
int gs_set_att_type(geosurf *, int, int);
int gs_set_att_src(geosurf *, int, int);
int gs_set_att_const(geosurf *, int, float);
void gs_set_maskmode(int);
int gs_mask_defined(geosurf *);
int gs_masked(typbuff *, int, int, int);
int gs_mapcolor(typbuff *, gsurf_att *, int);
int gs_get_zextents(geosurf *, float *, float *, float *);
int gs_get_xextents(geosurf *, float *, float *);
int gs_get_yextents(geosurf *, float *, float *);
int gs_get_zrange0(float *, float *);
int gs_get_zrange(float *, float *);
int gs_get_xrange(float *, float *);
int gs_get_yrange(float *, float *);
int gs_get_data_avg_zmax(float *);
int gs_get_datacenter(float *);
int gs_setall_norm_needupdate(void);
int gs_point_is_masked(geosurf *, float *);
int gs_distance_onsurf(geosurf *, float *, float *, float *, int);

/* From gs_bm.c */
struct BM *gsbm_make_mask(typbuff *, float, int, int);
void gsbm_zero_mask(struct BM *);
int gsbm_or_masks(struct BM *, struct BM *);
int gsbm_ornot_masks(struct BM *, struct BM *);
int gsbm_and_masks(struct BM *, struct BM *);
int gsbm_xor_masks(struct BM *, struct BM *);
int gs_update_curmask(geosurf *);
void print_bm(struct BM *);

/* From gs_norms.c */
void init_vars(geosurf *);
int gs_calc_normals(geosurf *);
int calc_norm(geosurf *, int, int, unsigned int);

/* From gs_query.c */
int gs_los_intersect1(int, float (*)[3], float *);
int gs_los_intersect(int, float **, float *);
int RayCvxPolyhedronInt(Point3, Point3, double, Point4 *, int, double *, int *);
void gs_get_databounds_planes(Point4 *);
int gs_setlos_enterdata(Point3 *);

/* From gsd_cplane.c */
void gsd_def_cplane(int, float *, float *);
void gsd_update_cplanes(void);
void gsd_cplane_on(int);
void gsd_cplane_off(int);
void gsd_get_cplanes_state(int *);
int gsd_get_cplanes(Point4 *);
void gsd_update_cpnorm(int);
void gsd_cplane_setrot(int, float, float, float);
void gsd_cplane_settrans(int, float, float, float);
void gsd_draw_cplane_fence(geosurf *, geosurf *, int);
void gsd_draw_cplane(int);

/* From gsd_fonts.c */
void gs_set_font(int, int, int, double);

/* From gsd_img.c */
int GS_write_rgb(char *);

/* From gsd_img_ppm.c */
int GS_write_ppm(char *);

/* From gsd_img_tif.c */
int GS_write_tif(char *);

/* From gsd_label.c */
void gs_put_label(short, short, char *);

/* From gsd_objs.c */
void gsd_plus(float *, int, float);
void gsd_line_onsurf(geosurf *, float *, float *);
int gsd_nline_onsurf(geosurf *, float *, float *, float *, int);
void gsd_x(geosurf *, float *, int, float);
void gsd_diamond(float *, int, float);
void gsd_diamond_lines(void);
void gsd_drawsphere(float *, unsigned long, float);
void gsd_draw_asterisk(float *, unsigned long, float);
void gsd_draw_gyro(float *, unsigned long, float);
void gsd_3dcursor(float *);
void dir_to_slope_aspect(float *, float *, float *, int);
int gsd_arrow(float *, unsigned long, float, float *, float, geosurf *);
int gsd_arrow_onsurf(float *, float *, unsigned long, int, geosurf *);
void gsd_3darrow(float *, unsigned long, float, float, float *, float);
void primitive_cone(unsigned long);
void primitive_cylinder(unsigned long, int);

/* From gsd_prim.c */
void gsd_flush(void);
void gsd_colormode(int);
void show_colormode(void);
void gsd_circ(float, float, float);
void gsd_disc(float, float, float, float);
void gsd_sphere(float *, float);
void gsd_zwritemask(unsigned long);
void gsd_backface(int);
void gsd_linewidth(short);
void gsd_bgnqstrip(void);
void gsd_endqstrip(void);
void gsd_bgntmesh(void);
void gsd_endtmesh(void);
void gsd_bgntstrip(void);
void gsd_endtstrip(void);
void gsd_bgntfan(void);
void gsd_endtfan(void);
void gsd_swaptmesh(void);
void gsd_bgnpolygon(void);
void gsd_endpolygon(void);
void gsd_bgnline(void);
void gsd_endline(void);
void gsd_shademodel(int);
int gsd_getshademodel(void);
void gsd_bothbuffer(void);
void gsd_frontbuffer(int);
void gsd_backbuffer(int);
void gsd_swapbuffers(void);
void gsd_popmatrix(void);
void gsd_pushmatrix(void);
void gsd_scale(float, float, float);
void gsd_translate(float, float, float);
void gsd_rot(float, char);
void gsd_litvert_func(float *, unsigned long, float *);
void gsd_litvert_func2(float *, unsigned long, float *);
void gsd_vert_func(float *);
void gsd_color_func(unsigned int);
void gsd_init_lightmodel(void);
void gsd_set_material(int, int, float, float, int);
void gsd_deflight(int, struct lightdefs *);
void gsd_switchlight(int, int);
int gsd_getimage(unsigned long **, unsigned int *, unsigned int *);
void gsd_blend(int);
void gsd_def_clipplane(int, double *);
void gsd_set_clipplane(int, int);
void gsd_finish(void);
void gsd_viewport(int, int, int, int);
int gsd_makelist(void);
void gsd_bgnlist(int, int);
void gsd_endlist(void);
void gsd_calllist(int);

/* From gsd_surf.c */
int gsd_surf(geosurf *);
int gsd_surf_map(geosurf *);
int gsd_surf_const(geosurf *, float);
int gsd_surf_func(geosurf *, int (*)());
int gsd_triangulated_wall(int, int, geosurf *, geosurf *, Point3 *, Point3 *,
    float *);
void gsd_setfc(int);
int gsd_getfc(void);
int gsd_ortho_wall(int, int, geosurf **, Point3 **, float *);
int gsd_wall(float *, float *, float *);
int gsd_norm_arrows(geosurf *);

/* From gsd_views.c */
int gsd_get_los(float (*)[3], short, short);
void gsd_set_view(geoview *, geodisplay *);
void gsd_check_focus(geoview *);
void gsd_get_zup(geoview *, double *);
int gsd_zup_twist(geoview *);
void gsd_do_scale(int);
void gsd_real2model(Point3);
void gsd_model2real(Point3);
void gsd_model2surf(geosurf *, Point3);
void gsd_surf2real(geosurf *, Point3);
void gsd_real2surf(geosurf *, Point3);

/* From gsd_wire.c */
int gsd_wire_surf(geosurf *);
int gsd_wire_surf_map(geosurf *);
int gsd_wire_surf_const(geosurf *, float);
int gsd_wire_surf_func(geosurf *, int (*)());
int gsd_wire_arrows(geosurf *);

/* From gsdiff.c */
void gsdiff_set_SDscale(float);
float gsdiff_get_SDscale(void);
void gsdiff_set_SDref(geosurf *);
geosurf *gsdiff_get_SDref(void);
float gsdiff_do_SD(float, int);

/* From gsdrape.c */
int gsdrape_set_surface(geosurf *);
int seg_intersect_vregion(geosurf *, float *, float *);
Point3 *gsdrape_get_segments(geosurf *, float *, float *, int *);
Point3 *gsdrape_get_allsegments(geosurf *, float *, float *, int *);
void interp_first_last(geosurf *, float *, float *, Point3, Point3);
int _viewcell_tri_interp(geosurf *, Point3);
int viewcell_tri_interp(geosurf *, typbuff *, Point3, int);
int in_vregion(geosurf *, float *);
int order_intersects(geosurf *, Point3, Point3, int, int, int);
int get_vert_intersects(geosurf *, float *, float *, float *);
int get_horz_intersects(geosurf *, float *, float *, float *);
int get_diag_intersects(geosurf *, float *, float *, float *);
int segs_intersect(float, float, float, float, float, float, float, float,
    float *, float *);
int Point_on_plane(Point3, Point3, Point3, Point3);
int XY_intersect_plane(float *, float *);
int P3toPlane(Point3, Point3, Point3, float *);
int V3Cross(Point3, Point3, Point3);

/* From gsds.c */
int gsds_findh(char *, IFLAG *, IFLAG *, int);
int gsds_newh(char *);
typbuff *gsds_get_typbuff(int, IFLAG);
char *gsds_get_name(int);
int gsds_free_datah(int);
int gsds_free_data_buff(int, int);
int free_data_buffs(dataset *, int);
int gsds_alloc_typbuff(int, int *, int, int);
int gsds_get_changed(int);
int gsds_set_changed(int, IFLAG);
int gsds_get_type(int);

/* From gsget.c */
int get_mapatt(typbuff *, int, float *);

/* From gv.c */
geovect *gv_get_vect(int);
geovect *gv_get_prev_vect(int);
int gv_num_vects(void);
geovect *gv_get_last_vect(void);
geovect *gv_get_new_vect(void);
void gv_update_drapesurfs(void);
int gv_set_defaults(geovect *);
int gv_init_vect(geovect *);
void gv_delete_vect(int);
int gv_free_vect(geovect *);
void gv_free_vectmem(geovect *);
void gv_set_drapesurfs(geovect *, int *, int);

/* From gv_quick.c */
float gv_line_length(geoline *);
int gln_num_points(geoline *);
int gv_num_points(geovect *);
int gv_decimate_lines(geovect *);

/* From gvd.c */
int gs_clip_segment(geosurf *, float *, float *, float *);
int gvd_vect(geovect *, geosurf *, int);
void gvd_draw_lineonsurf(geosurf *, float *, float *, int);

/* From gvl.c */
geovol *gvl_get_vol(int);
geovol *gvl_get_prev_vol(int);
int gvl_num_vols(void);
geovol *gvl_get_last_vol(void);
geovol *gvl_get_new_vol(void);
int gvl_set_defaults(geovol *);
int gvl_init_vol(geovol *);
void gvl_delete_vol(int);
int gvl_free_vol(geovol *);
void gvl_free_volmem(geovol *);
void gvl_set_drapesurf(geovol *, int);
void gvl_unload_dsp(geovol *, struct dspinfo *);
void gvl_load_dsp(geovol *, char *, geodsp *);

/* From trans.c */
void P_scale (float, float, float);
void P_translate (float, float, float);
void P_transform (int, float (*)[4], float (*)[4]);
void P__transform (int, float (*)[4], float (*)[4], float (*)[4]);
void P_matrix_copy (float (*)[4], float (*)[4], int);
int P_pushmatrix (void);
int P_popmatrix (void);
void P_rotate (int, char);
void P_rot (float, char);
void P_rad_rotate (double, char);

#endif /* _OGSF_LOCAL_PROTO_H */
