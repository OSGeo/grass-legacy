/*
* $Id$
*/

/*  gstypes.h
    Bill Brown, USACERL  
    January 1993
*/
	
#ifndef _GSTYPES_H
#define _GSTYPES_H

#include "gsurf.h"
#include "bitmap.h"
#include <GL/gl.h>

/*
#define TRACE_FUNCS
*/

#define X 0
#define Y 1
#define Z 2
#define W 3
#define FROM 0
#define TO 1

/* colormodes */
#define CM_COLOR               0
#define CM_EMISSION            1
#define CM_AMBIENT             2
#define CM_DIFFUSE             3
#define CM_SPECULAR            4
#define CM_AD                  5
#define CM_NULL                6

#define CM_WIRE CM_COLOR

#define NULL_COLOR 0xFFFFFF

/* attribute sizes - NOT YET USED */
#define GS_CHAR8      char
#define GS_SHORT16   short
#define GS_INT32       int

/* attribute ***types*** */
#define ATTY_NULL       32   /* internal use only */
#define ATTY_MASK       16   /* can't use this one for numbytes */
#define ATTY_FLOAT       8   /* can't use this one for numbytes */
#define ATTY_INT         4 
#define ATTY_SHORT       2 
#define ATTY_CHAR        1
#define ATTY_ANY        63   /* internal use only */
#define LEGAL_TYPE(t)    \
(t==ATTY_MASK || t==ATTY_FLOAT || t==ATTY_INT || t==ATTY_SHORT || t==ATTY_CHAR)

#define MAXDIMS 4

#define FUDGE(gs) ((gs->zmax_nz - gs->zmin_nz)/500.)
#define DOT3( a, b )    ( (a)[X]*(b)[X] + (a)[Y]*(b)[Y] + (a)[Z]*(b)[Z] )

/* changed flags for datasets */
#define CF_NOT_CHANGED 0x000000
#define CF_COLOR_PACKED 0x000001
#define CF_USR_CHANGED 0x000010
#define CF_CHARSCALED  0x000100

#define NAME_SIZ 80
#define MAX_TF 6

#define MASK_TL 0x10000000
#define MASK_TR 0x01000000
#define MASK_BR 0x00100000
#define MASK_BL 0x00010000
#define MASK_NPTS 0x00000007

typedef float Point4[4];
typedef float Point3[3];
typedef float Point2[2];

typedef struct{
    float *fb;
    int *ib;
    short *sb;
    unsigned char *cb;
    struct BM *bm;
    struct BM *nm;     /* null mask: set = null */
    float  (*tfunc)(float, int);
    float k;
} typbuff;

typedef struct{  /* use hash table? */
    int n_elem;  /* if n_elem == 256, index == NULL */
    char *index;
    int *value;
} table256;

typedef struct{  /* applied thusly: offset, mult, if(use_lookup) lookup */
    float offset;
    float mult;
    int use_lookup;
    table256 lookup;
} transform;

/* move this to dataset file? */
typedef struct{
    int data_id;
    int dims[MAXDIMS];
    int ndims;
    int numbytes;
    char unique_name[NAME_SIZ];
    typbuff databuff;
    IFLAG changed;
    int need_reload;
} dataset;

/* maybe add transformation matrix? */
typedef struct{
    IFLAG     att_src;     /* NOTSET_ATT, MAP_ATT, CONST_ATT, FUNC_ATT */
    IFLAG     att_type;    /* ATTY_INT, ATTY_SHORT, ATTY_CHAR, or ATTY_FLOAT */
    int     hdata;      /* handle to dataset */ 
    int     (*user_func)();
    float     constant;     
    int     *lookup;      /* TODO: use transform instead */
    float   min_nz, max_nz, range_nz;
    float   default_null;
} gsurf_att;

typedef struct g_surf{
    int gsurf_id;
    int cols, rows;
    gsurf_att att[MAX_ATTS]; /* mask, topo, color, etc. */
    IFLAG draw_mode; /*DM_GOURAUD | DM_FRINGE | DM_POLY, DM_WIRE, DM_WIRE_POLY*/
    long wire_color;   /* 0xBBGGRR or WC_COLOR_ATT */
    double ox, oy;     /* real world origin (i.e., SW corner) */
    double xres, yres;
    float z_exag;
    float x_trans, y_trans, z_trans;
    float xmin, xmax, ymin, ymax, zmin, zmax, zminmasked;
    float xrange, yrange, zrange; 
    float zmin_nz, zmax_nz, zrange_nz;
    int x_mod, y_mod, x_modw, y_modw; /*cells per viewcell, per wire viewcell*/
    int nz_topo, nz_color;  /* no zero flags */
    int mask_needupdate, norm_needupdate;
    unsigned long *norms;
    struct BM *curmask;
    struct g_surf *next;
    void *clientdata;
} geosurf;

/* maybe put attribute info here instead of in geovect - allow a single
   vector file to have multiple attributes ?   Cached lines should 
   usually be stored as 2d, since they may be draped on multiple
   surfaces & Z will vary depending upon surface. */
typedef struct g_line{
    int dims, npts;
    Point3 *p3;
    Point2 *p2;
    struct g_line *next;
} geoline;

typedef struct g_vect{
    int gvect_id;
    int use_mem, n_lines;
    int drape_surf_id[MAX_SURFS]; /* if you want 'em flat, define the surface */
    int n_surfs;
    int color, width;
    char filename[NAME_SIZ];
    float x_trans, y_trans, z_trans; 
/* also maybe center & rotate? */
    geoline *lines;
    geoline *fastlines;
    int (*bgn_read)(), (*end_read)(), (*nxt_line)();
    struct g_vect *next;
    void *clientdata;
} geovect;

typedef struct g_point{
    int dims;
    Point3 p3;
    float fattr; /* may want to make these pointers or arrays for mult atts */
    int iattr;
    char *cattr;
    struct g_point *next;
} geopoint;

typedef struct g_site{
    int gsite_id;
    int drape_surf_id[MAX_SURFS];  /* ditto */
    int n_surfs, n_sites;
    int color, width, marker, use_z, use_mem;
    int has_z, has_att;  /* set when file loaded */
    int attr_mode; /* ST_ATT_COLOR, ST_ATT_MARKER, ST_ATT_SIZE, ST_ATT_NONE */
    char filename[NAME_SIZ];
    transform attr_trans;
    float size;
    float x_trans, y_trans, z_trans; 
    geopoint *points;
    int (*bgn_read)(), (*end_read)(), (*nxt_site)();
    struct g_site *next;
    void *clientdata;
} geosite;

typedef struct dspinfo{
    int num_levels;
    /* stuff to define which isosurfaces available and which ON,
       file name, etc
       Also maybe where to put fence cuts, normals direction, solid 
       vs. slices, etc */
} geodsp;

typedef struct g_vol{
    char filename[NAME_SIZ];
    int gvol_id;
    int drape_surf_id;
    int n_dsp;
    float x_trans, y_trans, z_trans; 
    struct dspinfo *dspis[MAX_DSP];
    struct g_vol *next;
    void *clientdata;
} geovol;

struct lightdefs{
    float position[4];  /* X, Y, Z, (1=local/0=inf) */
    float color[3];  /* R, G, B */
    float ambient[3];  /* R, G, B */
    float emission[3];  /* R, G, B */
    float shine;  /* 0. to 128. */
};

typedef struct{
    int coord_sys;      /* latlon, equal area, etc */
    int view_proj;     /* perspective, ortho */
    int infocus;       /* fixed center of view - true or false */
    float from_to[2][4];
    int twist, fov, incl, look;  /* 10ths of degrees */
    float real_to[4], vert_exag; /* a global Z exag */
    float scale;
    struct lightdefs lights[MAX_LIGHTS];
} geoview;

typedef struct{        /* need to add elements here for off_screen drawing */
    float nearclip, farclip, aspect;
    short left, right, bottom, top;  /* Screen coordinates */
    int bgcol;
} geodisplay;

void (*Cxl_func)();
void (*Swap_func)();

/* Bring all the function prototypes */
#include "local_proto.h"

#endif /* _GSTYPES_H */
