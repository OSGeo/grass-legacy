#include <tcl.h>
#include <tk.h>
#include "Vect.h"

#ifdef MAIN
  #define Global
#else
  #define Global extern
#endif

/*      tool in c:         name in Tk */             
typedef enum
{ 
    TOOL_NOTHING,   
    TOOL_EXIT,
    TOOL_NEW_POINT,  
    TOOL_NEW_LINE,  
    TOOL_NEW_BOUNDARY,  
    TOOL_NEW_CENTROID,  
    TOOL_MOVE_VERTEX,  
    TOOL_MOVE_LINE,  
    TOOL_DELETE_LINE,  
    TOOL_ZOOM_WINDOW,    /* zoom by window */
    TOOL_ZOOM_OUT_CENTRE, 
    TOOL_REDRAW 
} ToolNumber;
    
/* Xdriver coordinate value considered to bu null*/
#define COOR_NULL PORT_INT_MAX 

/* SYMBOLOGY */
typedef enum
{
    SYMB_DEFAULT,    /* line color acording to real line/node type from Line/NodeSymb */
    SYMB_BACKGROUND, 
    SYMB_HIGHLIGHT,
    SYMB_POINT,
    SYMB_LINE,       
    SYMB_BOUNDARY_0, /* No areas */
    SYMB_BOUNDARY_1, /* 1 area */
    SYMB_BOUNDARY_2, /* 2 areas */
    SYMB_CENTROID_IN,    /* Centroid in area */
    SYMB_CENTROID_OUT,   /* Centroid outside area */
    SYMB_CENTROID_DUPL,  /* Duplicate centroid in area */
    SYMB_NODE_0,     /* Node without lines (points or centroids) */
    SYMB_NODE_1,     /* Node with 1 line */
    SYMB_NODE_2,     /* Node with 2 lines */ 
    SYMB_COUNT       /* MUST BE LAST, number of symbology layers */ 
} SymbNumber;

typedef struct {
    int on;      /* 1 - on, 0 - off*/
    int r, g, b; /* color 0-255 */
} SYMB;

/* Category and field set for current line to be written */
Global int CatMode; /* mode of cat imput */
#define CAT_MODE_NO   0 /* No category */ 
#define CAT_MODE_MAN  1 /* Manual imput */ 
#define CAT_MODE_NEXT 2 /* Next not yet used category of given field */
#define CAT_MODE_COUNT 3 /* Count of modes */ 
#ifdef MAIN
  char *CatModeLab[] = { "No category", "Manual enry", "Next not used" };
#else
  extern char *CatModeLab[];
#endif

Global int (*FieldCat)[2];
Global int nFieldCat, aFieldCat;

/* Maximum value for field */
Global int (*MaxFieldCat)[2];
Global int nMaxFieldCat, aMaxFieldCat;

Global SYMB Symb[SYMB_COUNT];

Global struct Map_info Map;
Global struct Cell_head Region; /* Current region (synchronized with GRASS WIND) */ 
Global Tcl_Interp *Toolbox;
Global int Tool_next;           /* Next tool to be run */
Global double Xscale, Yscale;   /* Scale factors = size_in_map / size_on_screen */

Global struct Cell_head window;

Global double Scale; /* Map / xdriver */

#define SNAP_SCREEN 0 /* Snap in screen pixels */
#define SNAP_MAP    1 /* Snap in map units */
Global int Snap;      /* If to snap to nearest node */
Global int Snap_mode; /* Snapping mode (screen pixels / map units) */
Global int Snap_screen; /* Snapping threshold in screen pixels */
Global double Snap_map;    /* Snapping threshold in map units */

/* Display symbology for lines and nodes */
Global int *LineSymb; /* array of line symbology codes, starts from index 1 */
Global int aLineSymb; /* number of lines / allocated space (array size + 1) */
Global int *NodeSymb; /* array of nodes' symbology codes, start from index 1 */
Global int aNodeSymb; /* number of nodes / allocated space (array size + 1) */

/* Background commands */
#ifdef MAIN
char **Bgcmd = NULL;
int nbgcmd = 0;
int abgcmd = 0; 
#else
extern char **Bgcmd;
extern int nbgcmd;
extern int abgcmd; 
#endif

