/* add_arc.c */
int dxf_add_arc(FILE *);
/* add_bounds.c */
int dxf_add_boundaries(void);
/* add_circle.c */
int dxf_add_circle(FILE *);
int make_arc(int, double, double, double, double, double, int);
/* add_extents.c */
int dxf_add_extents(void);
/* add_line.c */
int dxf_add_line(FILE *);
/* add_point.c */
int dxf_add_point(FILE *);
/* add_polyline.c */
int dxf_add_polyline(FILE *);
int write_polylines(DXF_DIG *, int);
/* check_ext.c */
int dxf_check_ext(double, double);
/* close_layer.c */
int dxf_close_layer(int);
/* debug.c */
int debuginit(void);
int debugf(char *, ...);
/* entities.c */
int dxf_entities(FILE *);
/* find_lines.c */
int dxf_find_lines(FILE *);
/* header.c */
int dxf_header(FILE *);
/* init_chars.c */
int dxf_init_chars(void);
/* label_box.c */
int dxf_add_labelbox(FILE *);
int dxf_readcode(FILE *);
/* main.c */
int add_line_layer(char *);
int add_att_layer(char *);
int add_layer(char *, char *[][2], int *);
int _add_layer(char *[][2], int *, char *, char *);
char *remap(char *, int);
char *dxf_fgets(char *, int, FILE *);
int extra_help(void);
int big_percent(unsigned long, unsigned long, int);
/* make_header.c */
int dxf_make_header(DXF_DIG *);
/* open_layer.c */
int dxf_open_layer(int, int);
/* reopen_layer.c */
int dxf_reopen_layer(int, int, int);
/* which_layer.c */
DXF_DIG *dxf_which_layer(char *, int);
int set_status(int);
int find_highest_status(void);
