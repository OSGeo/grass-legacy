#define MAX_LINE_PNTS   101  /* maximum number of points in DGN line is 101 */

#define BO_LE   1
#define BO_BE   2

/* DGN ELEMENT TYPES */
#define DGN_CELL            2
#define DGN_LINE            3
#define DGN_LINESTRING      4
#define DGN_SHAPE           6
#define DGN_TEXT           17
#define DGN_COMPLEX_STRING 12
#define DGN_COMPLEX_SHAPE  14

/* output category as */
#define CAT_MSLINK     0x01       
#define CAT_TEXT       0x02       
#define CAT_LEVEL      0x04
#define CAT_COLOR      0x08

#define PARTS_NUMBER      1
#define PARTS_LENGTH      2
#define PARTS_WRITE       3

typedef struct {
    int		level;	       /* Element Level: 0-63 */
    int         color;         /* Color index (0-255) */
    int         weight;        /* Line Weight (0-31) */
    int         style;         /* Line Style */
    int         complex;       /* TRUE if part of complex element */
    int		dgntype;  
    int         length;        /* length of element body (without head 
                                  and attributes) in 2-byte words */ 
    int         attrlen;       /* length of element attributes in 2-byte words */ 
    struct line_pnts *points;  /* coordinates in grass units */
    double      N, S, E, W;    /* bounding box in grass units */
    int         cat;           /* grass category */
    int         hole;          /* TRUE if hole */
    int         snap;          /* snapable */
} EHEAD;

#ifdef MAIN
    int gtype;   /* grass line types which should be written */ 
    int uor;     /* UOR (units of resolution) per SU (subunit) */
    int su;      /* SU per MU (master unit) */
    int entity;  /* entitynum in mscatalog */
    int endian;  /* byte order */
    int level;   /* output level */
    int color;   /* output color */
    int catas;   /* output category as */
    int append;  /* append to existing dgn file */
    int area;    /* write areas as shapes */
    int centroid;    /* write area centroids */
    int nlines;  /* number of lines */
    int slines;  /* number of split lines */
    int cat;     /* output only selected category */
#else
    extern int gtype, uor, su, entity, endian, append, nlines, slines, area, centroid;
    extern int level, color, catas, cat;
#endif 


