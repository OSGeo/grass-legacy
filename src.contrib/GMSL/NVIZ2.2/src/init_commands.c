/*	Alex Shevlakov sixote@yahoo.com 02/2000
*	function added to handle postgres queries
*/
#include "interface.h"

extern int
  Nresize_cmd(),
  Nset_background_cmd(),
  Nchange_position_cmd(),
  Nchange_persp_cmd(),
  Nchange_exag_cmd(),
  Nchange_height_cmd(),
  Nget_first_exag_cmd(),
  Nget_height_cmd(),
  Nfinish_cmd(),
  Nlights_cmd(),
  Nlight_obj_cmd(),
  Nnew_light_cmd(),
  Ninit_view_cmd(),
  Nlibinit_cmd(),
  Nhas_transparency_cmd(),
  Ntransp_is_set_cmd(),
  Nis_masked_cmd(),
  Nget_def_color_cmd(),
  Nclear_cmd(),
  Nset_cancel_cmd(),
  Nget_cancel_cmd(),
  Nset_draw_cmd(),
  Nready_draw_cmd(),
  Ndone_draw_cmd(),
  Nnew_map_obj_cmd(),
  Ninit_view_cmd(),
  Nget_to_cmd (),
  Nget_from_cmd (),
  Nlook_here_cmd (),
  Nset_focus_cmd(),
  Nget_focus_cmd(),
  Nhas_focus_cmd(),
  Nset_focus_map_cmd(),
  Nset_no_focus_cmd(),
  Nmove_to_cmd(),
  Nset_fov_cmd(),
  Nget_region_cmd(),
  Nget_point_on_surf_cmd(),
  Nget_point_on_surf_pg_grass(),
  Nget_point_on_surf_pg_site(),
  Nget_longdim_cmd(),
  Nget_zrange_cmd(),
  Nget_zextents_cmd(),
  Nget_exag_cmd(),
  Nset_exag_cmd(),
  Nquick_draw_cmd(),
  Nauto_draw_cmd(),
  Ndraw_all_cmd(),
  Nsurf_draw_all_cmd(),
  Nvect_draw_all_cmd(),
  Nsite_draw_all_cmd(),
  Ndraw_line_on_surf_cmd(),
  Ndraw_model_cmd(),
  Ndraw_wire_cmd(),
  Ndraw_X_cmd(),
  Ndone_draw_cmd(),
  Nready_draw_cmd(),
  Nget_dist_along_surf_cmd(),
  Nget_cat_at_xy_cmd(),
  Nget_val_at_xy_cmd(),
  Nset_light_to_view_cmd(),
  Nset_interp_mode_cmd(),
  Nset_tension_cmd(),
  Nshowtension_start_cmd(),
  Nupdate_tension_cmd(),
  Nshowtension_stop_cmd(),
  Nupdate_frames_cmd(),
  Nset_numsteps_cmd(),
  Nclear_keys_cmd(),
  Nadd_key_cmd(),
  Ndo_framestep_cmd(),
  Nshow_path_cmd(),
  Nshow_site_cmd(),
  Nshow_vect_cmd(),
  Ndelete_key_cmd(),
  Nmove_key_cmd(),
  Nwrite_rgb_cmd(),
  Nwrite_ppm_cmd(),
  Nwrite_tif_cmd(),
  Ncutplane_obj_cmd(),
  Nnew_cutplane_obj_cmd(),
  Nnum_cutplane_obj_cmd(),
  Nset_current_cutplane_cmd(),
  Nget_current_cutplane_cmd(),
  Nget_cutplane_list_cmd(),
  Nset_fence_color_cmd(),
  Nget_fence_color_cmd(),
  Nget_xyrange_cmd(),
  Nset_SDsurf_cmd(),
  Nunset_SDsurf_cmd(),
  Nset_SDscale_cmd(),
  Nget_surf_list_cmd(),
  Nget_vect_list_cmd(),
  Nget_site_list_cmd(),
  Nsave_3dview_cmd(),
  Nload_3dview_cmd(),
  Nset_cancel_func_cmd(),
  Nunset_cancel_func_cmd(),
/*  Tk_Tkspecial_waitCmd(), */
  SetScriptFile_Cmd(),
  SetState_Cmd(),
  CloseScripting_Cmd(),
  ScriptAddString_Cmd(),
  Tk_ScaleCmd(),
  Tk_SendCmd(),
  Nsurf_draw_one_cmd(),
  Nvect_draw_one_cmd(),
  Nsite_draw_one_cmd(),
  Nliteral_from_logical_cmd(),
  Nlogical_from_literal_cmd(),
  Nplace_label_cmd();

extern Tk_Window mainWindow;

int 
init_commands (Tcl_Interp *interp, Nv_data *data)
{
  /* Disabled security version of send */
/*  Tcl_CreateCommand(interp, "send", Tk_SendCmd,
		    (ClientData) mainWindow, (void (*)()) NULL); */
 
  /* Scripting commands */
  Tcl_CreateCommand(interp, "Nv_set_script_file", SetScriptFile_Cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "Nv_set_script_state", SetState_Cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "Nv_close_scripting", CloseScripting_Cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "Nv_script_add_string", ScriptAddString_Cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  
  /* Add our float scale widget */
/*  Tcl_CreateCommand(interp, "scale" , Tk_ScaleCmd, (ClientData) mainWindow,
		    (void (*)()) NULL);
*/
  /* Add the cancel function command */
  Tcl_CreateCommand(interp, "Nset_cancel_func", Nset_cancel_func_cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  Tcl_CreateCommand(interp, "Nunset_cancel_func", Nunset_cancel_func_cmd,
		    (ClientData) mainWindow, (void (*)()) NULL);
  
  /* Add the special tkwait command */
  /* REMOVED 26-Feb-2000 by Philip Warner. Replaced with an Idle handler */
/*
 *  Tcl_CreateCommand(interp, "tkspecial_wait", Tk_Tkspecial_waitCmd,
 *		    (ClientData) mainWindow, (void (*)()) NULL);
 */

  /* Commands for handling logical names */
  Tcl_CreateCommand(interp, "Nliteral_from_logical", Nliteral_from_logical_cmd,
		    (ClientData) mainWindow, (void (*)())NULL);
  Tcl_CreateCommand(interp, "Nlogical_from_literal", Nlogical_from_literal_cmd,
		    (ClientData) mainWindow, (void (*)())NULL);
  
  /* Commands for generating lists of map objects */
  Tcl_CreateCommand(interp, "Nget_surf_list", Nget_surf_list_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_vect_list", Nget_vect_list_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_site_list", Nget_site_list_cmd,
		    data, NULL);

  Tcl_CreateCommand(interp, "Nbackground", Nset_background_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nresize", Nresize_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nchange_position", Nchange_position_cmd,
		      data, NULL);
  Tcl_CreateCommand(interp, "Nchange_persp", Nchange_persp_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nchange_height", Nchange_height_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_first_exag", Nget_first_exag_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_height", Nget_height_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nchange_exag", Nchange_exag_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ngl_finish", Nfinish_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nlights", Nlights_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nnew_light", Nnew_light_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ninit_view", Ninit_view_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nlibinit", Nlibinit_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nhas_transparency", Nhas_transparency_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Ntransp_is_set", Ntransp_is_set_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nis_masked", Nis_masked_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_def_color", Nget_def_color_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nclear", Nclear_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_cancel", Nset_cancel_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_cancel", Nget_cancel_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_draw", Nset_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nready_draw", Nready_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndone_draw", Ndone_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nnew_map_obj", Nnew_map_obj_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_to", Nget_to_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_from", Nget_from_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nlook_here", Nlook_here_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nhas_focus", Nhas_focus_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_focus", Nget_focus_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_focus", Nset_focus_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_focus_map", Nset_focus_map_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_no_focus", Nset_no_focus_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nmove_to", Nmove_to_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_fov", Nset_fov_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_region", Nget_region_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_point_on_surf", Nget_point_on_surf_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_point_on_pg_grass", Nget_point_on_surf_pg_grass, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_point_on_pg_site", Nget_point_on_surf_pg_site, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_longdim", Nget_longdim_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_zrange", Nget_zrange_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_zextents", Nget_zextents_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_exag", Nget_exag_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_exag", Nset_exag_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nquick_draw", Nquick_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nauto_draw", Nauto_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndraw_all", Ndraw_all_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nsurf_draw_all", Nsurf_draw_all_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nsurf_draw_one", Nsurf_draw_one_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nvect_draw_all", Nvect_draw_all_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nvect_draw_one", Nvect_draw_one_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nsite_draw_all", Nsite_draw_all_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nsite_draw_one", Nsite_draw_one_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndraw_line_on_surf", Ndraw_line_on_surf_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Ndraw_model", Ndraw_model_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndraw_wire", Ndraw_wire_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndraw_X", Ndraw_X_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Ndone_draw", Ndone_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nready_draw", Nready_draw_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_dist_along_surf", Nget_dist_along_surf_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_cat_at_xy", Nget_cat_at_xy_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nget_val_at_xy", Nget_val_at_xy_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_light_to_view", Nset_light_to_view_cmd, 
		    data, NULL);
  Tcl_CreateCommand(interp, "Nset_SDsurf", Nset_SDsurf_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nunset_SDsurf", Nunset_SDsurf_cmd, data, NULL);
  Tcl_CreateCommand(interp, "Nset_SDscale", Nset_SDscale_cmd, data, NULL);

  /* Keyframe Animation */
  Tcl_CreateCommand(interp, "Nset_interp_mode", Nset_interp_mode_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nset_tension", Nset_tension_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nshowtension_start", Nshowtension_start_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nupdate_tension", Nupdate_tension_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nshowtension_stop", Nshowtension_stop_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nupdate_frames", Nupdate_frames_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nset_numsteps", Nset_numsteps_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nclear_keys", Nclear_keys_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nadd_key", Nadd_key_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Ndo_framestep", Ndo_framestep_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nshow_path", Nshow_path_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nshow_site", Nshow_site_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nshow_vect", Nshow_vect_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Ndelete_key", Ndelete_key_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nmove_key", Nmove_key_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nwrite_rgb", Nwrite_rgb_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nwrite_ppm", Nwrite_ppm_cmd,
			data, NULL);
  Tcl_CreateCommand(interp, "Nwrite_tif", Nwrite_tif_cmd,
                        data, NULL);

  /* Cutplane Junk */
  Tcl_CreateCommand(interp, "Ncutplane_obj", Ncutplane_obj_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nnew_cutplane_obj", Nnew_cutplane_obj_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nnum_cutplane_obj", Nnum_cutplane_obj_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nset_current_cutplane", Nset_current_cutplane_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_current_cutplane", Nget_current_cutplane_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_cutplane_list", Nget_cutplane_list_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nset_fence_color", Nset_fence_color_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_fence_color", Nget_fence_color_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nget_xyrange", Nget_xyrange_cmd,
		    data, NULL);

  /* Miscellanious */
  Tcl_CreateCommand(interp, "Nsave_3dview", Nsave_3dview_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nload_3dview", Nload_3dview_cmd,
		    data, NULL);
  Tcl_CreateCommand(interp, "Nplace_label", Nplace_label_cmd,
		    data, NULL);
}





