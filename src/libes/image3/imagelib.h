/*  This is a dummy proto for a routine that exists somewhere else */
int mark_control(void);
int select_target_env();
int select_current_env();
int Menu_msg();
int read_elev();

/* ask_camera.c */
int I_ask_camera_old(char *, char *);
int I_ask_camera_new(char *, char *);
int I_ask_camera_any(char *, char *);
/* ask_elev.c */
int ask_elevation(char *, char *, char *, char *);
/* ask_expose.c */
int I_ask_camera_old(char *, char *);
int I_ask_camera_new(char *, char *);
int I_ask_camera_any(char *, char *);
/* error.c */
int m_error(char *);
/* find_camera.c */
int I_find_camera(char *);
/* find_expose.c */
int I_find_initial(char *);
/* get_camera.c */
int I_get_group_camera(char *, Camera *);
int I_put_group_camera(char *, Camera *);
/* get_elev.c */
int I_put_group_elev(char *, Elevation);
int I_get_group_elev(char *, Elevation *);
int I_initialize_group_elev(Elevation *);
/* get_expose.c */
int I_get_group_expose(char *, Camera_Expose *);
int I_put_group_expose(char *, Camera_Expose *);
/* get_sat.c */
int I_get_group_sat(char *, Satellite *);
int I_put_group_sat(char *, Satellite *);
/* get_satexp.c */
int I_get_group_satexp(char *, Satellite_Expose *);
int I_put_group_satexp(char *, Satellite_Expose *);
/* get_trans.c */
int I_put_group_trans(Rectify_Group *);
int I_get_group_trans(Rectify_Group *);
/* list_cam.c */
int I_list_cameras(int);
/* ltm_anal.c */
int ltm_anal_points(Rectify_Group *, Residuals *);
/* ltm_auxil.c */
int I_get_ltm_auxil_data(Rectify_Group *);
/* ltm_coeffs.c */
int I_get_ltm_coefs_data(Rectify_Group *);
int get_ltm_coeffs_data(Rectify_Group *);
/* ltm_fid.c */
int I_compute_fiducial_equations(Auxillary_Photo *, Coeffs_Photo *);
int I_fiducial_ref(double, double, double *, double *, Coeffs_Photo *);
int I_inverse_fiducial_ref(double *, double *, double, double, Coeffs_Photo *);
/* ltm_ortho.c */
int I_ltm_ref(double, double, double, double *, double *, double *, Auxillary_Ltm *, Coeffs_Ltm *);
int I_inverse_ltm_ref(double, double, double, double *, double *, double *, Auxillary_Ltm *, Coeffs_Ltm *);
/* ltm_points.c */
int I_get_ltm_points_data(Rectify_Group *);
int get_ltm_points_data(Rectify_Group *);
/* ltm_trans.c */
int ltm_trans_calculate(Rectify_Group *);
int ltm_trans_forward(Rectify_Group *, double, double, double, double *, double *, double *);
int ltm_trans_inverse(Rectify_Group *, double *, double *, double *, double, double, double);
/* photo_anal.c */
int photo_anal_points(Rectify_Group *, Residuals *);
/* photo_auxil.c */
int I_get_photo_auxil_data(Rectify_Group *);
/* photo_coeffs.c */
int I_get_photo_coefs_data(Rectify_Group *);
int get_photo_coeffs_data(Rectify_Group *);
/* photo_init.c */
int I_init_group_trans_funcs(Rectify_Group *);
int init_photo_trans_funcs(Rectify_Group *);
int init_poly_trans_funcs(Rectify_Group *);
int init_ltm_trans_funcs(Rectify_Group *);
/* photo_points.c */
int I_get_photo_points_data(Rectify_Group *);
int get_photo_points_data(Rectify_Group *);
/* photo_trans.c */
int photo_trans_calculate(Rectify_Group *);
int photo_trans_forward(Rectify_Group *, double, double, double, double *, double *, double *);
int photo_trans_inverse(Rectify_Group *, double *, double *, double *, double, double, double);
/* poly_anal.c */
int poly_anal_points(Rectify_Group *, Residuals *);
/* poly_coeffs.c */
int I_get_poly_coefs_data(Rectify_Group *);
int get_poly_coeffs_data(Rectify_Group *);
/* poly_points.c */
int I_get_poly_points_data(Rectify_Group *);
int get_poly_points_data(Rectify_Group *);
/* poly_trans.c */
int poly_trans_calculate(Rectify_Group *);
int poly_trans_forward(Rectify_Group *, double, double, double, double *, double *, double *);
int poly_trans_inverse(Rectify_Group *, double *, double *, double *, double, double, double);
/* ref_fid.c */
int I_compute_fiducial_equations(Auxillary_Photo *, Coeffs_Photo *);
int I_fiducial_ref(double, double, double *, double *, Coeffs_Photo *);
int I_inverse_fiducial_ref(double *, double *, double, double, Coeffs_Photo *);
/* ref_ortho.c */
int I_ortho_ref(double, double, double, double *, double *, double *, Auxillary_Photo *, Coeffs_Photo *);
int I_inverse_ortho_ref(double, double, double, double *, double *, double *, Auxillary_Photo *, Coeffs_Photo *);
int matrix_error(char *);
int convert_to_ll(Control_Points_LL *, Control_Points_2D *);
int convert_from_ll(Control_Points_LL *, Control_Points_2D *);
int CRS_compute_georef_equations(Control_Points_2D *, double *, double *, double *, double *, int);
int I_get_con_points(char *, Control_Points_3D *);
int I_put_con_points(char *, Control_Points_3D *);
int I_read_con_points(FILE *, Control_Points_3D *);
int I_new_con_point(Control_Points_3D *, double, double, double, double, double, double, int);
int I_write_con_points(FILE *, Control_Points_3D *);
int I_get_ref_points(char *, Control_Points_2D *);
int I_put_ref_points(char *, Control_Points_2D *);
int I_new_ref_point(Control_Points_2D *, double, double, double, double, int);
int I_read_ref_points(FILE *, Control_Points_2D *);
int I_write_ref_points(FILE *, Control_Points_2D *);
int I_get_con_points_ll(char *, Control_Points_LL *);
int I_put_con_points_ll(char *, Control_Points_LL *);
int I_read_con_points_ll(FILE *, Control_Points_LL *);
int I_new_con_point_ll(Control_Points_LL *, double, double, double, double, int);
int I_write_con_points_ll(FILE *, Control_Points_LL *);
int I_compute_ltm_equations(Control_Points_3D *, Auxillary_Ltm *, Coeffs_Ltm *);
int I_compute_ortho_equations(Control_Points_3D *, Auxillary_Photo *, Coeffs_Photo *);
