/* dummies for the driver functions */
int Graph_Close(void);
/*int Graph_Set(int, char **);*/ /*commented due to true XDRIVER 24bit support */
int get_num_colors();
/* Can_do.c */
int can_do_float(void);
/* Clr_table.c */
int reset_color(int, int, int, int);
/* Draw_line.c */
int draw_line(int, int, int, int);
/* Get_w_box.c */
int Get_location_with_box(int, int, int *, int *, int *);
/* Get_w_line.c */
int Get_location_with_line(int, int, int *, int *, int *);
/* Get_w_pnt.c */
int Get_location_with_pointer(int *, int *, int *);
/* Panel.c */
int Panel_save(char *, int, int, int, int);
int Panel_restore(char *);
int Panel_delete(char *);
/* Plylne_abs.c */
int Polyline_abs(int *, int *, int);
int Polyline_rel(int *, int *, int);
/* Polygn_abs.c */
int Polygon_abs(int *, int *, int);
int Polygon_rel(int *, int *, int);
/* Raster.c */
int Raster_int(int, int, int *, int, int);
/* Returns.c */
int Screen_left(int *);
int Screen_rite(int *);
int Screen_bot(int *);
int Screen_top(int *);
int Get_num_colors(int *);
int color(int);

/* lib functions */
int Box_abs(int,int,int,int);
int Box_rel(int,int);
int Color_table_float(void);
int Color_table_fixed(void);
int _get_lookup_for_color(int,int,int);
int get_table_type(void);
int Color(int);
int RGB_color(unsigned char,unsigned char,unsigned char);
int Standard_color(int);
int Color_offset(int);
int get_color_offset(void);
int Cont_abs(int,int);
int Cont_rel(int,int);
int Erase(void);
#ifndef __CYGWIN__
int GFont( char *);
#endif
int Get_text_box( char *,int *t,int *,int *,int *);
int Linemod(void *);
int Move_abs(int,int);
int Get_current_xy(int *,int *);
int Move_rel(int,int);
int Number_of_colors(int *);
int Polydots_abs(int *,int *,int);
int Polydots_rel(int *,int *,int);
int Polyline_abs(int *,int *,int);
int Polyline_rel(int *,int *,int);
int Polygon_rel(int *,int *,int);
int Set_RGB_color(unsigned char *,unsigned char *,unsigned char *);
int RGB_raster(int,int,unsigned char *,unsigned char *,unsigned char *,int);
int Raster_char( int ,int ,unsigned char *,int,int);
int Raster_int_def( int ,int ,int *,int,int);
int Raster_int( int ,int ,int *,int,int);
int Reset_colors( int ,int ,unsigned char *,unsigned char *,unsigned char *);
int Reset_color( unsigned char ,unsigned char ,unsigned char ,int );
int Set_window(int,int,int,int);
int window_clip(double *,double *,double *,double *);
#ifndef __CYGWIN__
int Text( char *);
#endif
int Text_size( int,int );
int Text_rotation( double );
int clip(double,double,double,double,double *,double *,double *,double *);
int assign_fixed_color( int ,int ) ;
int get_fixed_color( int ) ;
int get_fixed_color_array( register int *,register int );
int assign_standard_color( int ,int ) ;
int get_standard_color(int );
int get_max_std_colors(void);
int get_connection(char *,int *,int *);
int prepare_connection(void);
int check_connection( char *,char *);
int init_font(char *) ;
int get_char_vects( unsigned char ,int *,unsigned char **,unsigned char **);
int drawchar (double ,double ,double ,double ,unsigned char );
int soft_text_ext(int x,int ,double ,double ,double ,char *) ;
int get_text_ext (int *,int *,int *,int *);
int soft_text(int ,int ,double ,double ,double ,char *);
int onechar(int ,int ,double ,double ,double ,unsigned char);
