int open_driver (void); 
int close_driver (void); 

int update (int, int);
int tool_centre (void);
int end (void);

/* Edit */
int new_line (void);
int process_line (int);

/* Display */
int display_point ( double , double , int , int );
int display_points ( struct line_pnts *, int, int );
int display_line ( int , int , int );
int display_map ( void );
int blot_point (double *, double *, int );

int c_cancel (ClientData , Tcl_Interp *, int, char **);
int c_next_tool (ClientData , Tcl_Interp *, int, char **);
int c_tool_centre (ClientData , Tcl_Interp *, int, char **);

int i_prompt (char *);
int i_prompt_buttons (char *, char *, char *);
int i_update (void);

