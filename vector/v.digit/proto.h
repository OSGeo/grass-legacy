/* Driver */
int driver_open (void); 
int driver_close (void); 
int driver_refresh (void);

int update (int, int);
int tool_centre (void);
int end (void);


/* Symbology */
int get_symb_code ( char *); 
char *get_symb_name ( int ); 
void symb_init ( void );
void symb_init_gui ( void );
void symb_set_driver_color ( int );

int symb_line_from_map ( int );
void symb_line_set_from_map ( int );
void symb_lines_init ( void );
void symb_updated_lines_set_from_map ( void );


int symb_node_from_map ( int );
void symb_node_set_from_map ( int );
void symb_nodes_init ( void );
void symb_updated_nodes_set_from_map ( void );

void updated_lines_and_nodes_erase_refresh_display(void); 

/* Edit */
int new_line (int);
int move_vertex (void);
int move_line (void);
int delete_line (void);

/* Display */
void display_points ( struct line_pnts * );
void display_icon ( double, double, int, double, int);
void display_line ( int, int );
void display_updated_lines ( int );
void display_node ( int, int );
void display_updated_nodes ( int );
void display_map ( void );
void display_bg ( void );
void display_erase ( void );
void display_redraw ( void );

/* Zoom */
int zoom_window (void);
int zoom_centre (double factor);

int c_cancel (ClientData , Tcl_Interp *, int, char **);
int c_next_tool (ClientData , Tcl_Interp *, int, char **);
int c_tool_centre (ClientData , Tcl_Interp *, int, char **);
int c_set_color (ClientData , Tcl_Interp *, int, char **);
int c_set_on (ClientData , Tcl_Interp *, int, char **);
int c_set_snap (ClientData , Tcl_Interp *, int, char **);
int c_set_cat (ClientData , Tcl_Interp *, int, char **);
int c_set_cat_mode (ClientData , Tcl_Interp *, int, char **);

int i_prompt (char *);
int i_prompt_buttons (char *, char *, char *);
int i_coor ( double, double);
int i_set_color ( char *, int, int, int);
int i_set_on ( char *, int);
void i_set_snap( void );
int i_update (void);
void i_new_line_options ( int );
void i_set_cat_mode ( void );
void i_new_line_field_set ( int ); 
void i_new_line_cat_set ( int ); 
void i_new_line_cat_set_next ( void ); 

/* Cats */
void cat_init ( void );
int cat_max_get ( int );
void cat_max_set ( int, int);

/* Background */
int bg_add ( char *);

/* Utilities */
char *get_line_type_name ( int type);
