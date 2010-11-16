/* ask_elev.c */
int ask_elev_data(void);

/* ask_files.c */
int ask_files(char *);
int dots(char *, int);

/* ask_files2.c */
int ask_file_from_list(char *, char *);

/* ask_wind.c */
int ask_method(void);

/* ask_wind.c */
int ask_window(struct Cell_head *);

/* aver_z.c */
int get_aver_elev(struct Ortho_Control_Points *, double *);

/* conv.c */
int view_to_col(View *, int);
int view_to_row(View *, int);
int col_to_view(View *, int);
int row_to_view(View *, int);
double row_to_northing(struct Cell_head *, int, double);
double col_to_easting(struct Cell_head *, int, double);
double northing_to_row(struct Cell_head *, double);
double easting_to_col(struct Cell_head *, double);

/* cp.c */
int get_conz_points(void);
int get_ref_points(void);

/* elev_data.c */
int elev_data(char *, int);

/* env.c */
int select_current_env(void);
int select_target_env(void);
int show_env(void);

/* equ.c */
int Compute_ortho_equation(void);
int Compute_ref_equation(void);

/* exec.c */
int exec_rectify(void);

/* get_wind.c */
int get_target_window(void);
int georef_window(struct Cell_head *, struct Cell_head *);

/* rectify.c */
int rectify(char *, char *, struct cache *, double, char *);

/* readcell.c */
struct cache *readcell(int, int, int);
block *get_block(struct cache *, int);

#define BKIDX(c,y,x) ((y) * (c)->stride + (x))
#define BKPTR(c,y,x) ((c)->grid[BKIDX((c),(y),(x))])
#define BLOCK(c,y,x) (BKPTR((c),(y),(x)) ? BKPTR((c),(y),(x)) : get_block((c),BKIDX((c),(y),(x))))
#define CPTR(c,y,x) (&(*BLOCK((c),HI((y)),HI((x))))[LO((y))][LO((x))])

/* report.c */
int report(char *, char *, char *, long, long, int);

/* target.c */
int get_target(char *);

/* declare resampling methods */
/* bilinear.c */
extern void p_bilinear(struct cache *, void *, int, double *, double *,
		       struct Cell_head *);
/* cubic.c */
extern void p_cubic(struct cache *, void *, int, double *, double *,
		    struct Cell_head *);
/* nearest.c */
extern void p_nearest(struct cache *, void *, int, double *, double *,
		      struct Cell_head *);
/* bilinear_f.c */
extern void p_bilinear_f(struct cache *, void *, int, double *, double *,
		       struct Cell_head *);
/* cubic_f.c */
extern void p_cubic_f(struct cache *, void *, int, double *, double *,
		    struct Cell_head *);
