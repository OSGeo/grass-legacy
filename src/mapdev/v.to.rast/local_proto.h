/* clock.c */
int start_clock(long *);
int stop_clock(long *);
/* inform.c */
int inform(char *);
/* label.c */
int is_labeled(CELL);
int mark_unlabeled(CELL *);

#ifdef GRASS_VECT_H
/* do_areas.c */
int do_areas(struct Map_info *, struct line_pnts *);
int sort_areas(struct Map_info *, struct line_pnts *);
/* do_lines.c */
int do_lines(struct Map_info *, struct line_pnts *);
/* do_sites.c */
int do_sites(struct Map_info *, struct line_pnts *);
/* getformat.c */
int getformat(struct Map_info *);
/* label.c */
int get_area_label(struct Map_info *, int, CELL *);
int get_line_label(struct Map_info *, int, CELL *);
int get_site_label(struct Map_info *, int, CELL *);
#endif

/* mapgraph.c */
int begin_mapgraph(void);
int mapgraph_line(int, int, int, int);
int mapgraph_polygon(double *, double *, int);
int mapgraph_polyline(double *, double *, int);
int mapgraph(double *, double *, int, int);
int end_mapgraph(void);
/* raster.c */
int begin_rasterization(int, int);
int configure_plot(void);
int output_raster(int);
int set_cat(CELL);
int raster_dot(int, int);
/* support.c */
int update_hist(char *, char *, char *, long);
int update_colors(char *);
int update_cats(char *, char *, char *);
/* vect2rast.c */
int vect_to_rast(char *, char *, int);
