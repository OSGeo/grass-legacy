/* dummies for the driver functions */
/* command.c */
void Respond(void);
int get_command(char *);

int Graph_Close(void);
int Graph_Set(int, char **);
/* Can_do.c */
int can_do_float(void);
/* Clr_table.c */
int reset_color(int, int, int, int);
/* Draw_line.c */
int draw_line(int, int, int, int);
/* Draw_point.c */
int draw_point(int, int);
/* Get_w_box.c */
int Get_location_with_box(int, int, int *, int *, int *);
int Get_location_with_box2(int, int, int *, int *, int *, int);
/* Get_w_line.c */
int Get_location_with_line(int, int, int *, int *, int *);
int Get_location_with_line2(int, int, int *, int *, int *, int);
/* Get_w_pnt.c */
int Get_location_with_pointer(int *, int *, int *);
int Get_location_with_pointer2(int *, int *, int *, int);
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
/* Work.c */
int Has_work(void);
int Work_stream(void);
void Do_work(int);
/* Client.c */
void Client_Open(void);
void Client_Close(void);

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
int Font_get( char *);
int Get_text_box( char *,int *t,int *,int *,int *);
int Linemod(void *);
int Move_abs(int,int);
int Get_current_xy(int *,int *);
int Move_rel(int,int);
int Number_of_colors(int *);
int Polydots_abs(int *,int *,int);
int Polydots_rel(int *,int *,int);
int Set_RGB_color(unsigned char *,unsigned char *,unsigned char *);
int RGB_raster(int,int,unsigned char *,unsigned char *,unsigned char *,unsigned char *);
int Raster_char( int ,int ,unsigned char *,int,int);
int Raster_int_def( int ,int ,int *,int,int);
int Reset_colors( int ,int ,unsigned char *,unsigned char *,unsigned char *);
int Reset_color( unsigned char ,unsigned char ,unsigned char ,int );
int Set_window(int,int,int,int);
int window_clip(double *,double *,double *,double *);
int Text( char *);
int Text_size( int,int );
int Text_rotation( double );
int clip(double,double,double,double,double *,double *,double *,double *);
int assign_fixed_color( int ,int ) ;
int get_fixed_color( int ) ;
int get_fixed_color_array( register int *,register int );
int assign_standard_color( int ,int ) ;
int get_standard_color(int );
int get_max_std_colors(void);
int init_font(char *) ;
int get_char_vects( unsigned char ,int *,unsigned char **,unsigned char **);
int drawchar (double ,double ,double ,double ,unsigned char );
int soft_text_ext(int x,int ,double ,double ,double ,char *) ;
int get_text_ext (int *,int *,int *,int *);
int soft_text(int ,int ,double ,double ,double ,char *);
int onechar(int ,int ,double ,double ,double ,unsigned char);
int _get_color_index(int);
int _get_color_index_array(int *,int);

/* freetype */
int Font_freetype_get(char*);
int Font_freetype_release();
int isFont_freetype();
int init_font_freetype(char*);
char* getFreeTypeName();
int init_font_charset(char* str);
char* getCharset();
int soft_text_freetype(int ,int ,double ,double ,double ,char *);
int soft_text_ext_freetype(int ,int ,double ,double ,double ,char *);
int get_text_ext_freetype(int*,int*,int*,int*);

/* connect_{sock,fifo}.c */
#ifdef USE_G_SOCKS
int get_connection_sock(int,int *,int *,int);
int prepare_connection_sock(char *, char *);
#else
int get_connection_fifo(char *,int *,int *,int);
#endif /* USE_G_SOCKS */
int check_connection(char *,char *);

/* command.c */
void command_init(int,int);
int command_get_input(void);
int process_command(int);

