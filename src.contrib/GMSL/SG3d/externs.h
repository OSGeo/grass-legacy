/*
You MUST comment out the next line if compiling under IRIX 3.x */
#define FOUR_OH

/* experimental defines */
/*
#define USE_CHAR
#define USE_SHORT
#define DO_12BIT
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "Vect.h"
/* prevent Vect.h redefined warning - not used anyway */
#ifdef RED
#undef RED
#endif
#ifdef GREEN
#undef GREEN
#endif
#ifdef BLUE
#undef BLUE
#endif
#ifdef MAGENTA
#undef MAGENTA
#endif
#ifdef YELLOW
#undef YELLOW
#endif
#ifdef WHITE
#undef WHITE
#endif
#ifdef BLACK
#undef BLACK
#endif

#include <gl.h>
#include "panel.h"
#include "gis.h"

/* Defines */

#define EP

#define RANGE_FACTOR 1.5
/*
#define RANGE_FACTOR 2.5
*/

#define FROM  0
#define TO    1
#define X 0
#define Y 1
#define Z 2
#define W 3

#define BG_COLOR	0
#define SITE_COLOR	1
#define VECT_COLOR	2
#define LABEL_COLOR	3
#define SCALE_COLOR	4

#define NUM_DRAW_COLORS 5

#define NUM_STANDARD_COLORS 9
#define NUM_CUSTOM_COLORS 5 

#define RED_MASK 0x000000FF
#define GRN_MASK 0x0000FF00
#define BLU_MASK 0x00FF0000

#define RGB_TO_INT(r,g,b,i) (i = (((r) & RED_MASK) +                \
                                 ((int)((g) << 8) & GRN_MASK) +     \
                                 ((int)((b) << 16) & BLU_MASK)))

#define INT_TO_RED(i, r)    (r = (i & RED_MASK))
#define INT_TO_GRN(i, g)    (g = (i & GRN_MASK) >> 8)
#define INT_TO_BLU(i, b)    (b = (i & BLU_MASK) >> 16)

/* uses NTSC greyscale conversion */
#define IS_DARK(i)  (((i & RED_MASK)*.3 +              \
		    ((i & GRN_MASK) >> 8)*.59 +           \
		    ((i & BLU_MASK) >> 16)*.11) < 128. )


/* scale largest dimension to 1000 */
#define LONGDIM 1000.
/* xmark size = 1% of LONGDIM */
#define MARK_SIZ 10. 

#ifndef PI
#define PI  3.14159265358979323846
#endif

#define D_GRID  1
#define D_POLY  2
#define D_GPOLY 3

#define RES_MAX 100

#define XYMAXPOS 0x3ff    /* 1023 */
#define ZMAXPOS 0x3ff    /* 1023 */

#define PNORM(i,nv)  \
  i = ((unsigned int)((nv[X]*XYMAXPOS)+XYMAXPOS) << 21) |           \
  ((unsigned int)((nv[Y]*XYMAXPOS)+XYMAXPOS) << 10) |               \
  (unsigned int)(nv[Z]*ZMAXPOS)


#define NXMASK 0xffe00000/* top 11 bits */
#define NYMASK 0x1ffc00  /* middle 11 bits of packed int */
#define NZMASK 0x3ff     /* lowest 10 bits */

#define MASKDIM 20
#define MASKDIMP 21
#define MASKVERTS 441
/* 21 x 21 */

#ifdef ORIG
#define SP_RES   24  /* keep this an even number */
#define SP_HRES  12  /* half */
#define SP_HRESP 13  /* half + 1 */
#define SP_MEM  936  /* SP_RES * SP_HRESP * 3 */
#else
#define SP_RES   20  /* keep this an even number */
#define SP_HRES  10  /* half */
#define SP_HRESP 11  /* half + 1 */
#define SP_MEM  660  /* SP_RES * SP_HRESP * 3 */
#endif

#define RADIUS  500.

/* reserved site field indexes */
#define S_CAT      0
#define S_X        1
#define S_Y        2
#define S_Z        3

/* last **index** of reserved */
#define S_RESV     3

/* site field indexes into Sfieldno */
#define SF_SIZE    0
#define SF_COLOR   1
#define SF_Z       2
#define SF_SHAPE   3
#define SF_DORDER  4
#define SF_SIZE2    5

#define NUM_ST_ATTS 6
#define MAX_ST_ATTS 20
#define MAX_ST_DIMS 6
#define MAX_ST_FIELDS 24


#define FSET_COLOR(ROW1,COL1,ROW2,COL2)			 		    \
{								            \
	cpack(Blend | visual[Shading? ROW2 + COL2: ROW1 + COL1]);      \
}

#define VERTEX(x,y,z) vector[0] = (x);vector[1]=(y);vector[2]=(z);v3f(vector);

#define NORM3F(i)  \
  fvect[X] = ((int)(((i) & NXMASK) >> 21) - XYMAXPOS)/(float)XYMAXPOS; \
  fvect[Y] = ((int)(((i) & NYMASK) >> 10) - XYMAXPOS)/(float)XYMAXPOS; \
  fvect[Z] = (int)((i) & NZMASK)/(float)ZMAXPOS; \
  n3f(fvect);

#define FNORM(i,nv)  \
  nv[X] = ((int)(((i) & NXMASK) >> 21) - XYMAXPOS)/(float)XYMAXPOS; \
  nv[Y] = ((int)(((i) & NYMASK) >> 10) - XYMAXPOS)/(float)XYMAXPOS; \
  nv[Z] = (int)((i) & NZMASK)/(float)ZMAXPOS; 

#define FVERT(x,y,z,v) v[0] = (x); v[1] = (y); v[2] = (z);

#ifdef FCELL_TYPE
#define EMBNULL1(f) G_is_f_null_value(&(f))
#define EMBNULL(f) (Has_null? G_is_f_null_value(&(f)) : 0)
#else
#define EMBNULL1(f) (0)
#define EMBNULL(f) (0)
#endif

typedef struct view_node{
    float from[3], to[3], fov, twist; 
} Viewnode;

typedef struct key_node{
    float from[3], to[3], fov, twist, pos; 
    struct key_node *next;
} Keylist;

typedef double Point3[3];

/* Extern variables */
#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

typedef int  (Func_ptr)();

GLOBAL int (*vert_func)();
GLOBAL int (*normvert_func)();
GLOBAL int (*v_vert_func)();
GLOBAL int (*vd_vert_func)();
GLOBAL int LatLon; 
GLOBAL float X_Min, X_Max, Y_Min, Y_Max;
GLOBAL float Z_Min_notzero, Z_Min, Z_Max, Z_Span, Z_Span_real;
GLOBAL float X_Min_real, X_Max_real;
GLOBAL float Y_Min_real, Y_Max_real;
GLOBAL float Z_Min_real, Z_Max_real, Z_Mid_real, Zoff;
GLOBAL float X_Mid, Y_Mid, Z_Mid;
GLOBAL float X_Res, Y_Res;

GLOBAL int P_mask[MASKDIMP][MASKDIMP];

GLOBAL int   Site_file;
GLOBAL int   View_file;
GLOBAL int   Vect_file;
GLOBAL int   Pathsteps;      /* for pathlist */
GLOBAL int   Numpathkeys;     
GLOBAL int   Viewsteps;      /* for viewlist */
GLOBAL int   Numkeys;     
GLOBAL FILE *Sitefd;
GLOBAL int   V_Width;
GLOBAL float Vect_z;
GLOBAL struct Map_info Map;
GLOBAL struct line_pnts *Points;
GLOBAL char Cellname[3][200];
GLOBAL char Elevname[200];
GLOBAL char Viewname[200];

GLOBAL int X_Size, Y_Size;		/* number of cells of resolution */

#ifdef USE_SHORT
GLOBAL short *elev_buf;
#else
#ifdef USE_CHAR
GLOBAL unsigned char *elev_buf;
#else
#ifdef FCELL_TYPE
GLOBAL FCELL *elev_buf;
#else
GLOBAL int *elev_buf;
#endif
#endif
#endif
GLOBAL int Has_null;

GLOBAL int *visual;
GLOBAL unsigned int *norm_buf;

GLOBAL float vector[3];
GLOBAL float fvect[3];
GLOBAL int persp;
GLOBAL float farclip, nearclip, aspect;
GLOBAL float Osize;                     /* size for ortho view */
GLOBAL float *normp, *vertp;

GLOBAL int X_Mod, Y_Mod;  /* number of real cells per view cell */
GLOBAL int X_Modr, Y_Modr;  /* number of real cells per view cell,polygon */
GLOBAL Screencoord top, bottom, left, right, xcenter, ycenter;
GLOBAL int Ox, Oy;

GLOBAL float UNIT_FROM_TO[2][4];
GLOBAL float FROM_TO[2][4];
GLOBAL float REAL_TO[4];
GLOBAL float UP_VECT[4];

GLOBAL struct Cell_head  wind;

GLOBAL int InFocus;
GLOBAL int redraw_ok;
GLOBAL int Shading;
GLOBAL int Model_showing;
GLOBAL int Mod_center[2];
GLOBAL float LightPos[4];
GLOBAL float To_Light[3];
GLOBAL int CenterSphere;

GLOBAL float Z_exag;
GLOBAL float XYscale;

GLOBAL int fast_res, slow_res;

GLOBAL int Display_type;
GLOBAL int Fringe_on;

GLOBAL double XBase, YBase, ZBase;
GLOBAL double XRange, YRange, ZRange, ZRange_real;

GLOBAL struct Colors Pcolor;
GLOBAL struct Colors Scolor;
GLOBAL int Three_map;
GLOBAL int New_view;
GLOBAL int Map_Sitecolor;
GLOBAL int Sfieldno[MAX_ST_ATTS];
GLOBAL float Sadd[NUM_ST_ATTS], Smult[NUM_ST_ATTS];
GLOBAL int Snumstrings, Shas_z, Shas_cat, Snumvals, CacheSites;
GLOBAL long Dcolor[NUM_DRAW_COLORS];
GLOBAL long Blend;
GLOBAL Colorindex Ccolori[NUM_CUSTOM_COLORS];
GLOBAL float Scale_Size;


/* Panel defs */

/* Control Panel */
GLOBAL Actuator *Aslow1, *Aslow2, *Afast1, *Afast2, *Altheight, *Alightxy; 
GLOBAL Actuator *Afull, *Aquit, *Ashading, *Adraw, *Areset, *Alight, *Asurface;
GLOBAL Actuator *Apoly, *Agrid, *Agridc, *Agpoly, *Ashowmod, *Aambient, *Awhat;
GLOBAL Actuator *Atransp1, *Atransp2, *Atransp3;
GLOBAL Actuator *Abright, *Atriangle, *Ashine, *Afollow, *Ared, *Agrn, *Ablu;
GLOBAL Actuator *Avect, *Afringe, *Arange1, *Arange2;
GLOBAL Actuator *Acancel;
GLOBAL Actuator *Asfsize_c, *Asfcolor_c, *Asfz_c, *Asfdorder_c;
GLOBAL Actuator *Asfsize2, *Asfsize2_c, *Asfsize2_txt; 
GLOBAL Actuator *AsfcolorRGB, *AsfRGBfields;
GLOBAL Actuator *Asfsize, *Asfcolor, *Asfz, *Asfdorder;
GLOBAL Actuator *Asfsize_txt, *Asfcolor_txt, *Asfz_txt; 
GLOBAL Actuator *Asfdorder_txt;
GLOBAL Actuator *Asfmod_mult[NUM_ST_ATTS], *Asfmod_add[NUM_ST_ATTS];
GLOBAL Actuator *Asitesiz, *Asiteinfo;
GLOBAL Actuator *Asites, *Anewsite, *Anewsitecol, *Aspheresite,*Axsite;
GLOBAL Actuator *Aglyph1site, *Aglyph2site; 
GLOBAL Actuator *Aoctosite, *Aconsite, *Aconetree, *Arndtree, *Acylsite;
GLOBAL Actuator *Ashownorth, *Aputscale, *Ascalesiz, *Ascaleco;
GLOBAL Actuator *Asolidscale, *Awirescale, *A3dscale, *Aflatscale;
GLOBAL Actuator *Aruler, *Anwticks, *Aseticks, *Aneticks, *Aswticks;
GLOBAL Actuator *Ascalez, *Aautoz;
GLOBAL Actuator *Alabelco, *Atext, *Aputlabel, *Aundolabel;
GLOBAL Actuator *Aroman, *Ahelvetica, *Acourier, *Abold, *Aitalic, *Afontsiz;
GLOBAL Actuator *F_font; /* frame for font stuff */
GLOBAL Actuator *Alegend, *Acatvals, *Acatlabs, *Acatall, *Acatinv;
GLOBAL Actuator *Acatrange, *Acatlow, *Acathigh, *Acatlist;
GLOBAL Actuator *Aputsitelabels, *Asitelabcats, *Asitelabstrings, *Asitelabbox;
GLOBAL Actuator *Asitelabvals, *Asitelabdec, *Asitevalfields, *Asitestrfields;
GLOBAL Actuator *Asitelabpie, *Asitepiefields, *Apiegrey, *Apielabel;
GLOBAL Actuator *Avwidth1, *Avwidth2, *Adrape, *Aflat, *Avcolor, *Anewvect;
GLOBAL Actuator *Afocus, *Abgcolor, *Anozero, *Anofocus;
GLOBAL Actuator *AvCcset, *AbgCcset, *AstCcset, *AscaleCcset, *AlabelCcset;
GLOBAL Actuator *Adotype, *Acenter, *Astcolor, *Adump, *Aglobe;
GLOBAL Actuator *Anoclear, *Aclear;

GLOBAL Actuator *AcCpal, *Accred, *Accgrn, *Accblu, *Accok, *Accbox;

GLOBAL Actuator *Amake, *Aframes, *Alevel, *Aterrain, *Apathht;
GLOBAL Actuator *Astepback, *Astepfor, *Arunpath, *Arunsave, *Alookforward;
GLOBAL Actuator *Apathtilt, *Aframecnt, *Apathel, *Ashowpath, *Asmoothing;
GLOBAL Actuator *Alinpath, *Asplpath;
GLOBAL Actuator *Afwdcnt, *Ashowvect, *Arunview, *Akeypts;
GLOBAL Actuator *Akeyframes, *Aaddkey, *Avframecnt, *Arunsavekeys, *Aclearkey;
GLOBAL Actuator *Ashowkpath, *Ashowkvect, *Astepkback, *Astepkfor, *Atension;
GLOBAL Actuator *Alinterp, *Aspline, *Aptension, *Apath2k; 

#ifdef EP
/* exact positioning panel */
GLOBAL Actuator *Avieweast, *Aviewnorth, *Aviewhgt, *Aeppersp;
GLOBAL Actuator *Alooktoeast, *Alooktonorth, *Alooktohgt;
GLOBAL Actuator *Ashowep; 
GLOBAL Panel *P_Exact;
#endif

GLOBAL int Range, Init_Range;

/* Movement Panel */
GLOBAL Actuator *Alook, *Aincl, *Axy, *Aheight, *Aexag, *Apersp, *Avectz;
GLOBAL Actuator *Atwist, *Aexagval, *Aexaglab, *Aortho, *Awhere, *Aplanv;

GLOBAL Panel *P_Lights, *P_Mvmt, *P_Script, *P_Options, *P_Vect;
GLOBAL Panel *P_Path, *P_Keyframe, *P_Scale, *P_Label, *P_Animate, *P_Sites;
GLOBAL Panel *P_CustColor;

#include <stdlib.h>

#ifdef MAIN
    Matrix ID_matrix = {
	1., 0., 0., 0., 
	0., 1., 0., 0., 
	0., 0., 1., 0., 
	0., 0., 0., 1.
    };
#else
extern Matrix ID_matrix;
#endif

typedef int FILEDESC;

GLOBAL char *AUTO_FILE;
GLOBAL int Write_script;
GLOBAL int Site_cat_isZ;

