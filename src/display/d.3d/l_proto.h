/* check_opt.c */
int check_options(void);
/* clip.c */
int save_edges(int, int, int, int);
int draw_line(int, int, int, int, int);
int draw_polygon(int *, int *, int);
int draw_polyline(int *, int *, int);
/* command.c */
int set_default_options(void);
/* cube.c */
int copy(int [5], int [8], int, int, int, int);
int Reset_colors(float *, float *, float *, int);
int Reset_color(double, double, double, int);
int _norm(double);
/* draw_face.c */
int draw_north_face(DCELL, DCELL);
int draw_south_face(DCELL, DCELL);
int draw_east_face(DCELL, DCELL);
int draw_west_face(DCELL, DCELL);
/* get_inputs.c */
int get_inputs(int *, char *);
int get_defaults(void);
int save_defaults(void);
/* get_range.c */
int get_range(DCELL *, DCELL *);
/* get_row.c */
int initialize_arrays(void);
int de_initialize_arrays(void);
int get_corners(int, int **, int **, int **, int **);
int avg_rows(DCELL *, DCELL *, DCELL *);
int no_avg_rows(DCELL *, DCELL *);
/* sigint.c */
int set_signals(void);
void sigint(int);
int check_signal(void);
/* threed.c */
int threed(int);
int do_plot(int, int, int, int, int, int, int, struct Colors *);
/* transform.c */
int establish_view(double, double, double, double, double, double, double);
int Screen_calc(DCELL [], double, int [], int [], double, struct Cell_head *, int);
int point_calc(double, double, double, int *, int *);
