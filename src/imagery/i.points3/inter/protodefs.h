/*=======================================================================
				i.points
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
    int  get_poly_order     ( );
    int  ortho_photo        ( );
    int  landsat_tm         ( );

#else
	/* anal_fid.c */
    int anal_fiducial(void);
    int compute_fiducial_residuals(Rectify_Group *, Residuals *);
	/* anal_control.c */
    int anal_control(void);

	/* camera.c */
    int setup_camera_file(void);
    int draw_camera_file(void);

	/* draw_cell.c */
    int drawcell(View *);

	/* draw_grid.c */
    int grid1(int);
    int grid1_warp(int);
	/* draw_vect.c */
    int plot1(char *, char *, int);
    int plot1_warp(char *, char *, int);

          /* group.c */
    int   prepare_group_list( void );
    int   choose_groupfile  ( char *name, char *mapset );

	/* graph_askmag.c */
    int ask_magnification(int *);
    int draw_mag(void);

          /* target.c */
    int   get_target        ( void );
    int   select_current_env( void );
    int   select_target_env ( void );
    GrassEnv new_environment( GrassEnv newenv );

          /* find.c */
    int   find_taget_files  ( void );
    int   find_vector_colors( void );

          /*  graphics.c */
    int   Init_graphics     ( void );
    int   Outline_box       ( int top, int bottom, \
				  int left, int right );
    int   Text_width        ( char *text );
    int   Text              ( char *text, int top, int bottom, \
				  int left, int right, int edge );
    int   Uparrow           ( int top, int bottom, int left, int right );
    int   Downarrow         ( int top, int bottom, int left, int right );
	/* graph_ask.c */
    int ask_gis_files(char *, char *, char *, char *, int);
	/* graph_dot.c */
    int dot(int, int);
    int save_under_dot(int, int);
    int restore_under_dot(void);
    int release_under_dot(void);

          /* title.c */
    int   display_title     ( View *view );

          /* view.c */
    void  Configure_view    ( View *view, char *name, char *mapset, \
			     double ns_res, double ew_res );
    void  Configure_view_vect  ( View *view, char *name, char *mapset, \
				 double ns_res, double ew_res );
    int   In_view           ( View *, int x, int y );
    int   Erase_view        ( View * );
    double  magnification   ( View *);

          /* driver.c */
    void  driver            ( void );
    int  zoom            ( void );

          /* zoom_box.c */
    int   zoom_box          ( int,int );
	/* mark_fid.c */
    int mark_fiducial(void);

          /* zoom_pnt.c */
    int zoom_point(int, int);

          /* plot.c */
    int   plotimg           ( void );

          /* plotcell.c */
    int   plotcell          ( void );

	/* plot_grid.c */
    int plotgrid(void);
    int _plotgrid(char *, int);
    int _plotgrid_warp(char *, int, int, int, int);

          /* vect.c */
    int   plotvect          ( void );
    int   _plotvect         ( char *name, char *mapset, char *color_name, 
			      int which );
    int   _plotvect_warp    ( char *name, char *mapset, char *color_name, 
			      int which);
	/* input.c */
    int Input_pointer(Objects *);
    int Input_box(Objects *, int, int);
    int Input_other(int (*)(void), char *);
    int Menu_msg(char *);
    int Start_mouse_in_menu(void);

	/* read_elev.c */
    CELL read_elev(int *, double, double);

	/* view_colors.c */
    int set_colors(struct Colors *);
    int get_vector_color(void);

	/* view_clear.c */
    int plot_clear(void);
    int clear_img(void);

	/* view_conv.c */
    int view_to_col(View *, int);
    int view_to_row(View *, int);
    int col_to_view(View *, int);
    int row_to_view(View *, int);
    double row_to_northing(struct Cell_head *, int, double);
    double col_to_easting(struct Cell_head *, int, double);
    double northing_to_row(struct Cell_head *, double);
    double easting_to_col(struct Cell_head *, double);

	/* view_outline.c */
    int Outline_cellhd(View *, struct Cell_head *);

	/* view_points.c */
    int display_points(int);
    int display_fiducial_points(int);
    int display_points_in_view(View *, int, double *, double *, int *, int);
    int display_one_point(View *, double, double);

	/* view_refresh.c */
    int plot_refresh(void);

	/* view_where.c */
    int where(int, int);

	/* overlay.c */
    int _warp_vect(void);
    int _warp_grid(void);

	/* transform.c */
    int ask_transform(void);
    int get_poly_order(void);
    int ortho_photo(void);
    int choose_elev_photo(void);
    int setup_camera(void);
    int setup_expose(void);
    int landsat_tm(void);
    int choose_elev_tm(void);
    int setup_landsat(void);
    int compute_transformation(void);

	/* mouse.c */
    int Mouse_pointer(int *, int *, int *);
    int Mouse_box_anchored(int, int, int *, int *, int *);
    int Get_mouse_xy(int *, int *);
    int Set_mouse_xy(int, int);
	/* list_target.c */
    int find_target_files(void);
	/* digit.c */
    int setup_digitizer(void);
    int digitizer_point(double *, double *);
	/* camera_mod.c */
    int mod_cam_info(int, Camera *);
	/* expose_mod.c */
    int mod_init_info(int, Camera_Expose *);

#endif
