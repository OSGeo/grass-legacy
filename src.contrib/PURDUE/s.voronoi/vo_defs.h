#ifdef GRASS_VECT_H
/* dt_triangles.c */
void write_triangles(struct Map_info *, char *, char *, int);
/* vo_polygons.c */
void write_polygons(struct Map_info *, char *, int);
#endif

#ifdef GRASS_GIS_H
/* init_head.c */
int init_header(FILE *, struct Cell_head *, struct dig_head *);
#endif

/* vo_cats_att.c */
void write_cats_att(char *, FILE *, FILE *, int, int);
/* vo_extend.c */
int extend_line(double, double, double, double, double, double, double, double, double, double *, double *, double, double, double, double);
/* vo_inregion.c */
int in_region(double, double);
