/*=======================================================================
				i.rectify3
  protodefs.h --
         Contains function protocols.

=======================================================================*/

         /* external function prototypes */

#ifdef _NO_PROTO
          /* group.c */
    int   prepare_group_list( );
    int   choose_groupfile  ( );

          /* target.c */
    int   get_target        ( );
    int   select_current_env( );
    int   select_target_env ( );
    GrassEnv new_environment( );

          /* find.c */
    int   find_taget_files  ( );
    int   find_vector_colors( );

          /*  graphics.c */
    int   Init_graphics     ( );
    int   Outline_box       ( );
    int   Text_width        ( );
    int   Text              ( );
    int   Uparrow           ( );
    int   Downarrow         ( );

          /* title.c */
    int   display_title     ( );

          /* view.c */
    void  Configure_view    ( );
    void  Configure_view_vect  ( );
    int   In_view           ( );
    int   Erase_view        ( );
    double  magnification   ( );

          /* driver.c */
    void  driver            ( );
    int   zoom              ( );

          /* zoom_box.c */
    int   zoom_box          ( );

          /* zoom_pnt.c */
    int   zoom_point        ( );

          /* plot.c */
    int   plotimg           ( );

          /* plotcell.c */
    int   plotcell          ( );

          /* vect.c */
    int   plotvect          ( );
    int   _plotvect         ( );
    int   _plotvect_warp    ( );

          /* transform.c */
    int  get_order          ( );
    int  ortho_photo        ( );
    int  landsat_tm         ( );

#else
    /* ask_files.c */
    int ask_files(struct Ref);
    int dots(char *, int);
    /* ask_files2.c */
    int ask_file_from_list(struct Ref, char *, char *);
    /* ask_wind.c */
    int ask_window(struct Cell_head *);
    /* compress.c */
    int compress(char *);
    /* cp.c */
    int get_control_points(char *, int);
    /* crs.c */
/*
//    void CRS_georef2(int, double [], double [], double [], double [], int);
//    int CRS_georef(double, double, double *, double *, double [], double [], int);
//    int CRS_compute_CP_georef_equations(struct Control_Points *, double [], double [], double [], double [], int);
*/
    /* env.c */
    int select_current_env(void);
    int select_target_env(void);
    int show_env(void);
    /* equ.c */
    int Compute_equation(void);
    int Compute_fiducial_equation(void);
    int Compute_ortho_equation(void);
    /* exec.c */
    int exec_rectify(struct Ref);
    /* georef_wind.c */
    int georef_window(struct Cell_head *, struct Cell_head *);
    /* mail.c */
    int mail(char *);
    int mark_photo(void);
    int mark_control(void);
    int Menu_msg(void);
    int analyze_photo(void);
    int analyze_poly(void);
    /* matrix.c */
    int compute_georef_matrix(int, struct Cell_head *, struct Cell_head *);
    double row_to_northing(struct Cell_head *, int, double);
    double col_to_easting(struct Cell_head *, int, double);
    double northing_to_row(struct Cell_head *, double);
    double easting_to_col(struct Cell_head *, double);
    /* perform.c */
    int perform_georef (int, void *);
    /* rectify.c */
    int rectify(char *, char *, char *);
    /* report.c */
    int report(char *, char *, char *, char *, long, long, int);
    /* target.c */
    int get_target(char *);
    /* write.c */
    int write_matrix(int, int);
    
#endif
