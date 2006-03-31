#ifndef _GLOBAL_H_
#define _GLOBAL_H_

#include <stdio.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/Vect.h>

struct dxf_file
{
    char *name;
    FILE *fp;
    /* for big_percent() */
    unsigned long size, pos;
    int percent;
};

#define UNIDENTIFIED_LAYER "UNIDENTIFIED"
#define ARR_INCR 256
#define DXF_BUF_SIZE 256

#ifdef _MAIN_C_
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int flag_table;
GLOBAL char dxf_buf[DXF_BUF_SIZE];
GLOBAL int ARR_MAX;
GLOBAL double *xpnts, *ypnts, *zpnts;
GLOBAL struct line_pnts *Points;

/* debug.c */
void debug_init(void);
void debug_msg(char *, ...);

/* dxf_to_vect.c */
int dxf_to_vect(struct dxf_file *, struct Map_info *);
int check_ext(double, double);

/* read_dxf.c */
struct dxf_file *dxf_open(char *);
void dxf_close(struct dxf_file *);
int dxf_find_header(struct dxf_file *);
#define dxf_get_code(a) dxf_read_code(a, dxf_buf, DXF_BUF_SIZE)
int dxf_read_code(struct dxf_file *, char *, int);
int big_percent(unsigned long, unsigned long, int);

/* add_arc.c */
int add_arc(struct dxf_file *, struct Map_info *);
/* add_circle.c */
int add_circle(struct dxf_file *, struct Map_info *);
/* add_text.c */
int add_text(struct dxf_file *, struct Map_info *);
/* add_line.c */
int add_line(struct dxf_file *, struct Map_info *);
/* add_point.c */
int add_point(struct dxf_file *, struct Map_info *);
/* add_polyline.c */
int add_polyline(struct dxf_file *, struct Map_info *);
/* add_lwpolyline.c */
int add_lwpolyline(struct dxf_file *, struct Map_info *);
/* make_arc.c */
int make_arc(int, double, double, double, double, double, double, int);
int make_arc_from_polyline(int, double, double);
/* write_vect.c */
#define write_polyline(a, b, c) write_vect(a, b, c, GV_LINE)
#define write_point(a, b) write_vect(a, b, 2, GV_POINT)
void write_vect(struct Map_info *, char *, int, int);
void write_done(struct Map_info *);

#endif
