/*  gsurf.h 
    Bill Brown, USACERL  
    January 1993
*/

#define GS_UNIT_SIZE 1000.

/* current maximums */
#define MAX_SURFS      12
#define MAX_VECTS      12	
#define MAX_SITES      12
#define MAX_ATTS        7
#define MAX_LIGHTS      3
#define MAX_CPLANES     6 

/* surface display modes */
#define DM_GOURAUD   0x00000100
#define DM_FLAT      0x00000200   /* defined for symmetry */

#define DM_FRINGE    0x00000010

#define DM_WIRE      0x00000001
#define DM_COL_WIRE  0x00000002
#define DM_POLY      0x00000004
#define DM_WIRE_POLY 0x00000008

#define WC_COLOR_ATT 0xFF000000

#define IFLAG unsigned int

/* surface attribute ***descriptors***  */
#define ATT_NORM      0     /* library use only */
#define ATT_TOPO      1
#define ATT_COLOR     2
#define ATT_MASK      3
#define ATT_TRANSP    4
#define ATT_SHINE     5
#define ATT_EMIT      6
#define LEGAL_ATT(a) (a >= 0 && a < MAX_ATTS)

/* site markers */
#define ST_X    	1
#define ST_BOX    	2
#define ST_SPHERE	3
#define ST_CUBE   	4
#define ST_DIAMOND 	5
#define ST_DEC_TREE     6
#define ST_CON_TREE     7
#define ST_ASTER	8
#define ST_GYRO	        9

/* site attribute modes (what attribute of marker category is used for) */
/* TODO: may want to make these OR'able */
#define ST_ATT_NONE     0x00000000
#define ST_ATT_COLOR    0x00000001
#define ST_ATT_SIZE     0x00000002
#define ST_ATT_MARKER   0x00000004

/* Buffer modes */
#define GSD_FRONT 1
#define GSD_BACK  2
#define GSD_BOTH  3

/* fence colormodes */
#define FC_OFF           0
#define FC_ABOVE         1
#define FC_BELOW         2
#define FC_BLEND         3
#define FC_GREY          4 

extern float GS_global_exag();
