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
          /* group.c */
    int   prepare_group_list( void );
    int   choose_groupfile  ( char *name, char *mapset );

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

          /* title.c */
    int   display_title     ( View *view );

          /* view.c */
    void  Configure_view    ( View *view, char *name, char *mapset, \
			     double ns_res, double ew_res );
    void  Configure_view_vect  ( View *view, char *name, char *mapset, \
				 double ns_res, double ew_res );
    int   In_view           ( View view, int x, int y );
    int   Erase_view        ( View view );
    double  magnification   ( View view );

          /* driver.c */
    void  driver            ( void );

          /* zoom_box.c */
    int   zoom_box          ( void );

          /* zoom_pnt.c */
    int   zoom_point        ( void );

          /* plot.c */
    int   plotimg           ( void );

          /* plotcell.c */
    int   plotcell          ( void );

          /* vect.c */
    int   plotvect          ( void );
    int   _plotvect         ( char *name, char *mapset, char *color_name, 
			      int which );
    int   _plotvect_warp    ( char *name, char *mapset, char *color_name, 
			      int which, double E[], double N[], int order);


#endif
