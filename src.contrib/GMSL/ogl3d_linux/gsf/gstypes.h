/*  gstypes.h
    Bill Brown, USACERL  
    January 1993
*/
	
#include "gsurf.h"
#include "bitmap.h"

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

#define ZERO_COLOR 0xFFFFFF

/* attribute sizes - NOT YET USED */
#define GS_CHAR8      char
#define GS_SHORT16   short
#define GS_INT32       int

/* attribute ***types*** */
#define ATTY_MASK       16   /* can't use this one for numbytes */
#define ATTY_FLOAT       8   /* can't use this one for numbytes */
#define ATTY_INT         4 
#define ATTY_SHORT       2 
#define ATTY_CHAR        1
#define LEGAL_TYPE(t)    \
(t==ATTY_MASK || t==ATTY_FLOAT || t==ATTY_INT || t==ATTY_SHORT || t==ATTY_CHAR)

/* surface attribute **sources**  */
#define NOTSET_ATT   0
#define MAP_ATT      1
#define CONST_ATT    2
#define FUNC_ATT     3
#define LEGAL_SRC(s) (s==NOTSET_ATT||s==MAP_ATT||s==CONST_ATT||s==FUNC_ATT)

#define MAXDIMS 4

#define FUDGE(gs) ((gs->zmax_nz - gs->zmin_nz)/500.)
#define DOT3( a, b )    ( (a)[X]*(b)[X] + (a)[Y]*(b)[Y] + (a)[Z]*(b)[Z] )

/* changed flags for datasets */
#define CF_NOT_CHANGED 0x000000
#define CF_COLOR_PACKED 0x000001
#define CF_USR_CHANGED 0x000010
#define CF_CHARSCALED  0x000100

typedef float Point4[4];
typedef float Point3[3];
typedef float Point2[2];

typedef struct{
    float *fb;
    int *ib;
    short *sb;
    char *cb;
    struct BM *bm;
} typbuff;

typedef struct{  /* use hash table */
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
    char unique_name[80];
    typbuff databuff;
    IFLAG changed;
    int need_reload;
} dataset;

/* maybe add transformation matrix? */
typedef struct{
    IFLAG     att_src;     /* NOTSET_ATT, MAP_ATT, CONST_ATT, FUNC_ATT */
    IFLAG     att_type;    /* ATTY_INT, ATTY_SHORT, or ATTY_CHAR */
    int     hdata;      /* handle to dataset */ 
    int     (*user_func)();
    int     constant;
    int     *lookup;      /* TODO: use transform instead */
    float   min_nz, max_nz, range_nz;
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
    float x_trans, y_trans, z_trans; 
/* also maybe center & rotate? */
    geoline *lines;
    geoline *fastlines;
    int (*bgn_read)(), (*end_read)(), (*nxt_line)();
    struct g_vect *next;
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
    transform attr_trans;
    float size;
    float x_trans, y_trans, z_trans; 
    geopoint *points;
    int (*bgn_read)(), (*end_read)(), (*nxt_site)();
    struct g_site *next;
} geosite;

struct lightdefs{
    float position[4];  /* X, Y, Z, (1=local/0=inf) */
    float color[3];  /* R, G, B */
    float ambient[3];  /* R, G, B */
    float emission[3];  /* R, G, B */
    float shine;  /* 0. to 128. */
};

typedef struct{
    int map_proj;      /* latlon, equal area, etc */
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

float ID_matrix[4][4]; 

void (*Cxl_func)();
void (*Swap_func)();


/* function prototypes */
extern int GS_surf_exists();
extern int GS_new_surface();
extern int GS_delete_surface();
extern float GS_distance();
extern float GS_global_exag(); 
extern double GS_get_aspect();

extern int Gs_loadmap_as_int();
extern int Gs_loadmap_as_short();

extern geoline *Gv_load_vect();

extern geosurf *gs_get_surf();
extern geosurf *gs_get_prev_surface();
extern geosurf *gs_get_last_surf();
extern geosurf *gs_get_new_surf();
extern int gs_num_surfaces();
extern int gs_init_surf();
extern int gs_delete_surf();
extern int gs_free_surf();
extern int gs_free_unshared_buffs();
extern int gs_get_att_type();
extern int gs_get_att_src();
extern float gs_distance();
extern typbuff *gs_get_att_typbuff();
extern int gs_num_datah_reused();

extern int gsds_alloc_typbuff();
extern typbuff *gsds_get_typbuff();	
extern int gsds_geth();	
extern int gsds_findh();
extern int gsds_newh();
extern int gsds_free_datah();
extern int gsds_set_changed();
extern int gsds_get_changed();
extern int gsds_get_type();


