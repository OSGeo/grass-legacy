#define GRASS_OK    0
#define GRASS_ERR (-1)

#define GV_FATAL_EXIT    0
#define GV_FATAL_PRINT   1
#define GV_FATAL_RETURN  2

#define GRASS_VECT_DIRECTORY    "vector" /* name of vector directory */
#define GRASS_VECT_FRMT_ELEMENT "frmt" /* format description, data location */ 
#define GRASS_VECT_COOR_ELEMENT "coor" /* coordinates */
#define GRASS_VECT_HEAD_ELEMENT "head" /* header information */
#define GV_TOPO_ELEMENT "topo"   /* topology */
#define GV_SIDX_ELEMENT "sidx"   /* spatial index */

#define ENDIAN_LITTLE 0
#define ENDIAN_BIG    1
#define ENDIAN_OTHER  2

/* sizes of types used in portable format (different names used in
 * Vlib/ and diglib/ for the same thing) */
#define PORT_DOUBLE 8
#define PORT_FLOAT  4
#define PORT_LONG   4
#define PORT_INT    4
#define PORT_SHORT  2
#define PORT_CHAR   1

/* replace by PORT_* in Vlib later and remove : */
#define DBL_SIZ  8
#define FLT_SIZ  4
#define LNG_SIZ  4
#define SHRT_SIZ 2

/*
** assumptions:
**    double =    8 byte IEEE
**    float  =    4 byte IEEE
**    long   =    4 byte int
**    short  =    2 byte int
**
*/

/* Limits for portable types - I am not sure if OK */
#define PORT_DOUBLE_MAX 1.7976931348623157e+308
#define PORT_DOUBLE_MIN 2.2250738585072014e-308
#define PORT_FLOAT_MAX  3.40282347e+38F
#define PORT_FLOAT_MIN  1.17549435e-38F
#define PORT_LONG_MAX   2147483647L
#define PORT_LONG_MIN   (-2147483647L)
#define PORT_INT_MAX    2147483647
#define PORT_INT_MIN    (-2147483647)
#define PORT_SHORT_MAX  32767
#define PORT_SHORT_MIN  (-32768)
#define PORT_CHAR_MAX   127
#define PORT_CHAR_MIN   (-128)

/* Geometry data formats supported by lib */
/* Don't change GV_FORMAT_* values, this order is hardcoded in lib */
#define GV_FORMAT_NATIVE   0 /* grass native format */
#define GV_FORMAT_SHAPE    1 /* shapefile format */
#define GV_FORMAT_POSTGIS  2 /* postgis format */

#define GV_MODE_READ  0
#define GV_MODE_WRITE 1
#define GV_MODE_RW    2

#define VECT_OPEN_CODE   0x5522AA22
#define VECT_CLOSED_CODE 0x22AA2255

#define LEVEL_1  1
#define LEVEL_2  2
#define LEVEL_3  3

#define VECT_OPEN(Map)   (Map->open == VECT_OPEN_CODE)

/*                      VERSION    4.0                  */
#define GRASS_V_VERSION       "5.0"
#define GRASS_V_VERSION_MAJOR  5
#define GRASS_V_VERSION_MINOR  0

/* the earliest version that can read this current format  */
#define GRASS_V_EARLIEST_MAJOR  5
#define GRASS_V_EARLIEST_MINOR	0

#define GRASS_V_DIG_HEAD_LENGTH	20

#define WITHOUT_Z	0
#define WITH_Z		1

#define DIGITIZER	0
#define MOUSE		1

#define ON	1
#define OFF	0

/* note this is changed from  3.0  which had 0.04   -dpg */
#define THRESH_FUDGE   0.03

#define MAXCOLORS	13
#define MAX_OPEN_FILES 20

#define WHITE	1
#define BLACK	2
#define YELLOW	3
#define BLUE	4
#define RED	5
#define GREEN	6
#define ORANGE	7
#define GREY	8
#define MAGENTA 9
#define AQUA    10
#define INDIGO	11
#define VIOLET	12
#define BROWN	13

#define GV_LEFT	 1
#define GV_RIGHT 2

#define GV_FORWARD 1
#define GV_BACKWORD 2

/* These are from mode.h which is no longer supported w/ digit 3.0 */
/*                               ^     for the most part...        */
/*     and definately gone by 4.0                                  */
#define POINT	0
#define STREAM	1

#define POINTS		3	/* this is thrown in for get_type_cnt() */

/* types used in memory on run time - may change */
#define GV_POINT		0x01
#define GV_LINE			0x02
#define GV_BOUNDARY		0x04
#define GV_CENTROID	        0x08
#define GV_AREA	                0x10

#define GV_POINTS (GV_POINT | GV_CENTROID )
#define GV_LINES (GV_LINE | GV_BOUNDARY )

/* types used in store like 'coor' file or postgis type column - must not change */
#define GV_STORE_POINT    1
#define GV_STORE_LINE     2
#define GV_STORE_BOUNDARY 3
#define GV_STORE_CENTROID 4

/* Overlay operators */
#define GV_ON_AND     "AND"     /* intersect */
#define GV_ON_OVERLAP "OVERLAP"

typedef enum { 
    GV_O_AND,
    GV_O_OVERLAP,
} OVERLAY_OPERATOR;


#define ESC	033

#ifdef FOO
/* these have all turned into global ints.  in digit/globals.h */
/* colors of unique items on screen */
#define CLR_LINE	BLUE
#define CLR_AREA	GREY
#define CLR_DOT		GREEN
#define CLR_SITE	GREEN

#define CLR_LLINE	MAGENTA
#define CLR_LSITE	AQUA
#define CLR_LAREA	ORANGE

#define CLR_AMARK	AQUA
#define CLR_ALABEL	ORANGE
#define CLR_LLABEL	MAGENTA

#define CLR_HIGHLIGHT	YELLOW
#define CLR_ERASE	BLACK
#define CLR_UNKNOWN	WHITE
#define CLR_OVERLAY	WHITE

#define CLR_0_NODE	GREEN
#define CLR_1_NODE	ORANGE
#define CLR_2_NODE	RED
#endif

#define GV_NCATS_MAX PORT_CHAR_MAX  /* maximum number of categories for one element */
#define GV_FIELD_MAX PORT_SHORT_MAX /* maximum field */
#define GV_CAT_MAX   PORT_INT_MAX   /* maximum category value */

#define BUILD_PROG "v.build"

