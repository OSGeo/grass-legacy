/* event.c */
int list_events(int, char **);
int map_suffix(char *, char *);
int read_events(char *, char *, char ***, FILE **);
int set_event(char **, char *);
int prompt_events(char *, char *, char ***);
int file_list(char *, char *, char ***);
/* geometry.c */
int geominit(void);
/* head_info.c */
int get_head_info(int, struct dig_head *);
/* heap.c */
int PQempty(void);
int PQinitialize(void);
/* memory.c */
char *myalloc(unsigned);
void myfreeall(void);
/* misc.c */
int readsites(int);
/* polygons.c */
int calc_areas(int);
int polygon_area(float [][2], int, double *);
int plot_polygon(char *, float [][2], int);
/* sort.c */
int ask_sort(void);
int check_sort(char *);
int user_add_vertex(int, int, int, double *, double *);
int user_show_buttons(void);
int free_sortlist(void);
char *get_xsect_name(char *);
/* util.c */
int do_init(char *);
int print_date(char *);
int print_title(FILE *, char *);
int mem_exit(void);
int non_blank_line(FILE *, char *);
int blank_pad(char *, int);
int r_justify(char *, char *, int);
int lscan(char *, char *);
int rscan(char *, char *);
int ssort(int, char **);
int ask_yesno(char *);
int point_yesno(char *);
int catindex(char *);
int get_dindex(int, double [], double);
int get_sindex(int, char *[], char *);
int round(double);
double distance(double, double, double, double);
double distance_sq(double, double, double, double);
int free_2d(char **, int);
int value_fmt(double, char *, int, int);
double dist_factor(int, int);
double cvt_dist(double, int, int);
double cvt_area(double, int, int);
double cvt_volume(double, int, int);
int coord_to_cell(struct Cell_head, double, double, int *, int *);
int screen_to_utm(struct Cell_head, int, int, double *, double *);
/* vector.c */
int support_vector(char *);
int get_line_center(double *, double *, struct line_pnts *);
/* voronoi.c */
int sortGLsites(void);
/* vregion.c */
float randr(double, double);
int vdinit(void);
int bboxinit(void);
int load_vsites(int, float **, double, double, double, double);
int find_vregion(int, float [][2]);
int find_adjacent(int, int *);
