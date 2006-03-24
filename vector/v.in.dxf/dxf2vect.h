/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/Vect.h>

#ifndef DBL_MAX
#define DBL_MAX		9999999999999999999999.9
#define DBL_MIN		-99999999999999999999.9
#endif

#define DXF_ASCII 0
#define DXF_LABEL 1
#define DXF_LABEL_LINE 2

#define MAX_FILES 15

#define POLYFLAG1 1

#define RSTEP  5.0


#define DXF_DIG		struct dxf_dig
DXF_DIG {
    char *name;
    int type;
    FILE *fd;
    struct Map_info *Map;
    int status;
};

#define ARR_INCR	256

#ifdef MAIN
char *dxf_file = NULL;
int num_open_layers = 0;
int num_closed_layers = 0;
int from_table = 0;

#define GLOBAL
#else
extern char *dxf_file;
extern int num_open_layers;
extern int num_closed_layers;
int from_table;
#define GLOBAL extern
#endif

GLOBAL struct line_pnts *Points;
GLOBAL unsigned long file_size;
GLOBAL int percent;
GLOBAL char dig_path[240];
GLOBAL char base_name[100];	/* dpg */
GLOBAL int ARR_MAX;
GLOBAL double n, s, e, w;
GLOBAL long n_off, s_off, e_off, w_off;
GLOBAL char zzero[8];
GLOBAL char eeight[8];
GLOBAL char tten[8];
GLOBAL char ttwenty[8];
GLOBAL char eelev[8];
GLOBAL char ttwentyone[8];
GLOBAL char entitie[12];
GLOBAL char header[12];
GLOBAL char extmin[12];
GLOBAL char extmax[12];
GLOBAL char polyline[12];
GLOBAL char circle[12];
GLOBAL char text[12];	/* dpg */
GLOBAL char line[8];
GLOBAL char point[8];
GLOBAL char arc[8];
GLOBAL char vertex[8];
GLOBAL char seqend[8];
GLOBAL char dxf_line[256];
GLOBAL DXF_DIG layers[MAX_FILES];
GLOBAL DXF_DIG *closed_layers;
GLOBAL double XMAX, XMIN, YMAX, YMIN, ZMAX, ZMIN;
GLOBAL int BOUNDARIES;
GLOBAL struct Flag *txtbox_flag;
GLOBAL struct Map_info dxf_head;
GLOBAL double *xinfo, *yinfo, *zinfo;


/* add_arc.c */
int dxf_add_arc(FILE *);
/* add_bounds.c */
int dxf_add_boundaries(void);
/* add_circle.c */
int dxf_add_circle(FILE *);
int make_arc(int, double, double, double, double, double, double, int);
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
