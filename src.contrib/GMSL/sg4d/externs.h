/*
You MUST comment out the next line if compiling under IRIX 3.x */
#define FOUR_OH

/* our xs24 loaner acts wierd with gouraud shading when not using tmesh */
/*
#define XS24
*/

/* experimental defines */
/*
#define DO_12BIT
#define USE_SHORT
#define USE_CHAR
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include "gl.h"
#include "panel.h"
#include "Vect.h"

#include "gis.h"

/* Defines */


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

/* scale largest dimension to 1000 */
#define LONGDIM 1000.
/* xmark size = 1% of LONGDIM */
#define MARK_SIZ 10. 

#define PI  3.14159265358979323846

#define TOGGLE(x) ((x) = !(x))

#define D_GRID  1
#define D_POLY  2
#define D_GPOLY 3

#define RES_MAX 100

#define XYMAXPOS 0x3ff    /* 1023 */
#define ZMAXPOS 0x3ff    /* 1023 */

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
#endif
#define SP_RES   20  /* keep this an even number */
#define SP_HRES  10  /* half */
#define SP_HRESP 11  /* half + 1 */
#define SP_MEM  660  /* SP_RES * SP_HRESP * 3 */

#define RADIUS  500.


#define FSET_COLOR(ROW1,COL1,ROW2,COL2)			 		    \
{								            \
	cpack(visual[Shading? ROW2 + COL2: ROW1 + COL1]);      \
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


typedef struct view_node{
    float from[3], to[3], fov, twist; 
} Viewnode;

typedef struct key_node{
    float from[3], to[3], fov, twist, pos; 
    struct key_node *next;
} Keylist;


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
GLOBAL int *visual;
#ifdef USE_SHORT
GLOBAL short *elev_buf;
#else
#ifdef USE_CHAR
GLOBAL unsigned char *elev_buf;
#else
GLOBAL int *elev_buf;
#endif
#endif
GLOBAL unsigned int *norm_buf;
GLOBAL float vector[3];
GLOBAL float llx, lly;
GLOBAL float fvect[3];
GLOBAL int persp;
GLOBAL float farclip, nearclip, aspect;
GLOBAL float Osize;                     /* size for ortho view */
GLOBAL float *normp, *vertp;

GLOBAL int X_Mod, Y_Mod;  /* number of real cells per view cell */
GLOBAL int X_Modr, Y_Modr;  /* number of real cells per view cell,polygon */
GLOBAL Screencoord top, bottom, left, right, xcenter, ycenter;

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
GLOBAL long BGcolor;
GLOBAL long STcolor;
GLOBAL long V_Color;
GLOBAL long Label_Color;
GLOBAL long Scale_Color;
GLOBAL float Scale_Size;


/* Panel defs */

/* Control Panel */
GLOBAL Actuator *Aslow1, *Aslow2, *Afast1, *Afast2, *Altheight, *Alightxy; 
GLOBAL Actuator *Afull, *Aquit, *Ashading, *Adraw, *Areset, *Alight, *Asurface;
GLOBAL Actuator *Apoly, *Agrid, *Agridc, *Agpoly, *Ashowmod, *Aambient, *Awhat;
GLOBAL Actuator *Atransp1, *Atransp2, *Atransp3;
GLOBAL Actuator *Abright, *Atriangle, *Ashine, *Afollow, *Ared, *Agrn, *Ablu;
GLOBAL Actuator *Avect, *Afringe, *Arange1, *Arange2, *Ascript, *Anewcell;
GLOBAL Actuator *Aoptions, *Asave, *Aread, *Acancel, *Amorvect, *Amorsites;
GLOBAL Actuator *Aanimate, *Asitesiz, *Amapsite, *Asitelevel, *Asitez;
GLOBAL Actuator *Ascale, *Asites, *Anewsite, *Aspheresite,*Axsite;
GLOBAL Actuator *Aglyph1site, *Aglyph2site; 
GLOBAL Actuator *Aoctosite, *Aconsite, *Aconetree, *Arndtree;
GLOBAL Actuator *Ashownorth, *Aputscale, *Ascalesiz, *Ascaleco;
GLOBAL Actuator *Asolidscale, *Awirescale, *A3dscale, *Aflatscale;
GLOBAL Actuator *Aruler, *Anwticks, *Aseticks, *Aneticks, *Aswticks;
GLOBAL Actuator *Ascalez, *Aautoz;
GLOBAL Actuator *Alabel, *Alabelco, *Atext, *Aputlabel, *Aundolabel;
GLOBAL Actuator *Aroman, *Ahelvetica, *Abold, *Aitalic, *Afontsiz;
GLOBAL Actuator *Avwidth1, *Avwidth2, *Adrape, *Aflat, *Avcolor, *Anewvect;
GLOBAL Actuator *Adim, *Afocus, *Abgcolor, *Amorlights, *Anozero, *Anofocus;
GLOBAL Actuator *Adotype, *Acenter, *Astcolor, *Adump, *Anewelev, *Aglobe;
GLOBAL Actuator *Anoclear, *Ashowdspf, *Adrawdspf; 
GLOBAL Actuator *Adspflights, *Asetdspf, *Anewdspf;

GLOBAL Actuator *Amake, *Aframes, *Alevel, *Aterrain, *Apath, *Apathht;
GLOBAL Actuator *Astepback, *Astepfor, *Arunpath, *Arunsave, *Alookforward;
GLOBAL Actuator *Apathtilt, *Aframecnt, *Apathel, *Ashowpath, *Asmoothing;
GLOBAL Actuator *Alinpath, *Asplpath;
GLOBAL Actuator *Afwdcnt, *Ashowvect, *Arunview, *Akey, *Akeypts;
GLOBAL Actuator *Akeyframes, *Aaddkey, *Avframecnt, *Arunsavekeys, *Aclearkey;
GLOBAL Actuator *Ashowkpath, *Ashowkvect, *Astepkback, *Astepkfor, *Atension;
GLOBAL Actuator *Alinterp, *Aspline, *Aptension, *Apath2k; 

GLOBAL int Range, Init_Range;

/* Movement Panel */
GLOBAL Actuator *Alook, *Aincl, *Axy, *Aheight, *Aexag, *Apersp, *Avectz;
GLOBAL Actuator *Atwist, *Aexagval, *Aexaglab, *Aortho, *Awhere;

GLOBAL Panel *P_Lights, *P_Mvmt, *P_Script, *P_Options, *P_Vect, *P_Menus;
GLOBAL Panel *P_Path, *P_Keyframe, *P_Scale, *P_Label, *P_Animate, *P_Sites;

/* Extern routines */
char *getenv ();
char *malloc ();

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

