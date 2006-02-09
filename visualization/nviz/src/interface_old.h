#define TRACE_GS_FUNCS
/*----------------- this is the include file section -----------------*/
#define SGI
/*
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <tk.h>
#include <grass/gis.h>
#ifdef SGI
#include <gl.h>
#include <gl/gl.h>
#include <gl/glws.h>
#endif
/*
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
*/
#include <grass/gsurf.h>

extern int *GV_get_vect_list(int *);
extern int *GS_get_surf_list(int *);
extern int *GP_get_site_list(int *);

/*-------------- this is the data define section ---------------*/

#define X 0
#define Y 1
#define Z 2
#define W 3

#define SURF 0
#define VECT 1
#define SITE 2

/* I don't like this - to be consistant, should really do all scaling 
in library, so can send real world coords. (seems like it would make
type-ins & sliders easier, too) */
#define RANGE (5 * GS_UNIT_SIZE)
#define RANGE_OFFSET (2 * GS_UNIT_SIZE)
#define ZRANGE (3 * GS_UNIT_SIZE)
#define ZRANGE_OFFSET (1 * GS_UNIT_SIZE)

#define DEFAULT_SURF_COLOR 0x33BBFF
#define DEFAULT_WIRE_COLOR 0x999999
#define DEFAULT_WIRE_CNT 10 
#define DEFAULT_POLY_CNT 2

/* Attributes for vector and site files */
#define SV_ATT_WIDTH    -1 
#define SV_ATT_MARKER   -2
#define SV_ATT_SIZE     -3
#define SV_ATT_USEATT   -4
#define SV_ATT_DISPLAY  -5

/*------------------------------------------------------------------------ 
-            this is the data type declaration section                   -
------------------------------------------------------------------------*/


typedef struct{
	char name[20], status[128], map_name[128];
	int use_map;
	float constant, constant2;
	char r,g,b;
	int flip_mask;
}att_info;

typedef struct{
        int id;
	char name[40];  /* should alredy be shortened from att_info */ 
	int draw_mode;  /*DM_GOURAUD | 
			    DM_COL_WIRE, DM_POLY, DM_WIRE, DM_WIRE_POLY*/
	long wire_color; /* 0xBBGGRR or WC_COLOR_ATT */
	int polycnt, wirecnt; /* cells per polygon, per wire mesh */
	float zexag;   
	float xtrans, ytrans, ztrans;   
	int cnz, tnz;
} surf_data;                            /* surface display mode */	

typedef struct{
        int id;
	char name[128];  
	long color;     /* 0xBBGGRR */
	int width;      /* pixel width */
	int use_mem;      /* load into memory */
	float pivpt[2], zrot;   
	float xtrans, ytrans, ztrans;   
}vect_data;                            /* vector display mode */	

typedef struct{
        int id;
	char name[128];  
	long color;     /* 0xBBGGRR */
	int marker;      /* ST_X, ST_SPHERE, ST_DIAMOND, etc */
	int width;
	int attrmode;
	int use_z, has_z, has_att;
	float size;
	float pivpt[2], zrot;   
	float xtrans, ytrans, ztrans;   
}site_data;                            /* sites display mode */	

typedef struct{
        int id;
	float brt;
	float r, g, b;
	float ar, ag, ab;  /* ambient rgb */
	float x, y, z, w; /* position */
}light_data;

typedef struct {
        float Zrange, XYrange;
	att_info Atts[MAX_SURFS][MAX_ATTS];
	att_info Cur_Atts[MAX_ATTS];
	int CurAtt;
	
	int NumCplanes;
	int CurCplane, Cp_on[MAX_CPLANES];
	float Cp_trans[MAX_CPLANES][3];
	float Cp_rot[MAX_CPLANES][3];

	int CurSurf;
	surf_data surf[MAX_SURFS];

	int CurVect;
	vect_data vect[MAX_VECTS];

	int CurSite;
	site_data site[MAX_SITES];

	light_data light[MAX_LIGHTS];

	int BGcolor;
}Nv_data; 






