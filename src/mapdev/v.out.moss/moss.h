#ifdef GRASS_VECT_H
/* area_to_moss.c */
int area_to_moss(struct Map_info *, struct Categories *, FILE *, int);
/* get_isle_xy.c */
int get_isle_xy(struct Map_info *, int, int *, double **, double **);
struct line_pnts *get__isle_xy(struct Map_info *, int);
/* line_to_moss.c */
int line_to_moss(struct Map_info *, struct Categories *, FILE *, int);
/* pnt_to_moss.c */
int point_to_moss(struct Map_info *, struct Categories *, FILE *, int);
/* store_points.c */
int store_points(double *, double *, int, struct line_pnts *);
#endif 

/* prune_points.c */
int prune_points(double [], double [], int *, double);
/* site_to_moss.c */
int site_to_moss(FILE *, FILE *, int);
/* wr_moss_coor.c */
int write_moss_coordinates(FILE *, int, double [], double [], int, int);
/* wr_moss_head.c */
int write_moss_header(FILE *, int, int, char *, int);
