#define ISNULL(x)   G_is_c_null_value(x)
#define ISDNULL(x)   G_is_d_null_value(x)
#define SETNULL(x)  G_set_c_null_value(x,1)
#define SETDNULL(x)  G_set_d_null_value(x,1)

#define USE_CELL  1
#define USE_DCELL 2

/* clock.c */
int start_clock(long *);
int stop_clock(long *);
/* inform.c */
int inform(char *);

/* do_areas.c */
/* int do_areas(struct Map_info *, struct line_pnts *, dbCatValArray *, int ); */
int sort_areas(struct Map_info *, struct line_pnts * );
/* do_lines.c */
/* int do_lines(struct Map_info *, struct line_pnts *, dbCatValArray *, int); */

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
int set_dcat(DCELL);
int raster_dot(int, int);
/* support.c */
int update_hist(char *, char *, char *, long);
int update_colors(char *);
int update_cats(char *, char *, char *);
/* vect2rast.c */
int vect_to_rast(char *, char *, char *, int);
