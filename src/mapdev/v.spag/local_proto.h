/* break2.c */
int break_lines_at_segs_w_point(struct Map_info *, double, double, int, int, struct line_pnts *, int, int, struct line_pnts *, int);
int break_line_at_seg_w_point(struct Map_info *, double, double, int, int, struct line_pnts *, int);
int break_line_at_segs_w_point(struct Map_info *, double, double, int, int, int, struct line_pnts *, int);
int local_check_dist(struct Map_info *, struct line_pnts *, double, double, double *);
/* compress.c */
int compress(struct Map_info *, int);
/* graphic.c */
int debug_graphic(struct Map_info *, int, int, double, double);
int g_set_window(double, double, double, double);
int g_reset_window(void);
int reset_d_window(void);
int g_graph_line(struct Map_info *, int, int, double, double);
/* intersect.c */
int intersect(struct Map_info *, int);
int cross_lines(struct Map_info *, int, int);
int Same_lines(struct Map_info *, int, int);
int clean_lines(struct Map_info *, double);
double line_length(struct line_pnts *);
/* linecros.c */
int test_for_intersection(double, double, double, double, double, double, double, double);
int find_intersection(double, double, double, double, double, double, double, double, double *, double *);
/* main2.c */
int main(int, char **);
int debugf(char *, ...);
int debugf2(char *, ...);
/* new_line.c */
int new_line(struct Map_info *, int, struct new_node *, struct line_pnts *);
/* remove.c */
int _remove_line(struct Map_info *, int);
/* spag2.c */
int spagetti(struct Map_info *, double);
int process_lines(struct Map_info *, int, int);
int set_window(double, double, double, double);
int reset_window(void);
int bboxes_cross (P_LINE *,P_LINE *);
