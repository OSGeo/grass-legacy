/* draw_point.c */
int draw_points_diamond(struct Cell_head *);
int draw_points_box(struct Cell_head *);
int draw_points_plus(struct Cell_head *);
int draw_points_x(struct Cell_head *);
int next_point(struct Cell_head *, double *, double *);
/* qualify.c */
int parse_set_rules(char *);
int site_qualify(Site *);
int site_in_rule(Site *, Site_rules *);
