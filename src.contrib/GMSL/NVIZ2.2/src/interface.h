#define TRACE_GS_FUNCS
/*----------------- this is the include file section -----------------*/
#define SGI
/*
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <tk.h>
#include "gis.h"
#ifdef SGI
#include <gl.h>
#include <GL/gl.h>
/*#include <gl/glws.h>*/
#endif
/*
#include <X11/Xirisw/GlxMDraw.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
*/
#include "gsurf.h"

/* extern int *GV_get_vect_list(int *);
extern int *GS_get_surf_list(int *);
extern int *GP_get_site_list(int *); */


/*-------------- this is the data define section ---------------*/

#define X 0
#define Y 1
#define Z 2
#define W 3

#define SURF 0
#define VECT 1
#define SITE 2
#define VOL  3

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
#define SV_ATT_MAP      -6


/*------------------------------------------------------------------------ 
-            this is the data type declaration section                   -
------------------------------------------------------------------------*/

typedef struct{
        int id;
	float brt;
	float r, g, b;
	float ar, ag, ab;  /* ambient rgb */
	float x, y, z, w; /* position */
} light_data;

typedef struct {
        float Zrange, XYrange;
	
	int NumCplanes;
	int CurCplane, Cp_on[MAX_CPLANES];
	float Cp_trans[MAX_CPLANES][3];
	float Cp_rot[MAX_CPLANES][3];

	light_data light[MAX_LIGHTS];

	int BGcolor;
} Nv_data; 

/* - The following structure is used to associate client data with surfaces.
 * We do this so that we don't have to rely on the surface ID (which is libal to change
 * between subsequent executions of nviz) when saving set-up info to files.
 */

typedef struct {
  /* We use logical names to assign textual names to map objects.
     When Nviz needs to refer to a map object it uses the logical name
     rather than the map ID.  By setting appropriate logical names, we
     can reuse names inbetween executions of Nviz.  The Nviz library
     also provides a mechanism for aliasing between logical names.
     Thus several logical names may refer to the same map object.
     Aliases are meant to support the case in which two logical names
     happen to be the same.  The Nviz library automatically assigns
     logical names uniquely if they are not specified in the creation
     of a map object.  When loading a saved file containing several map
     objects, it is expected that the map 0bjects will be aliased to
     their previous names.  This ensures that old scripts will work.
     */

    char *logical_name;
  
} Nv_clientData;













