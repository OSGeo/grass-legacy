/* definition des structures de donnees du widget View */

#include <tk.h>
#include <malloc.h>
/* #include "dig_structs.h" */
#include "Vect.h"

#define MAXAGG		128
#define MINSCALE	"0.0"
#define MAXSCALE	"100000000.0"
#define MINWIDTH	350
#define MINHEIGHT	350
#define LEGENDINDENT	20

#define NBTOKEN		11

#define METALAYER	0 		
#define POLYLINE	1		
#define POLYGONE	2		
#define DIGPOINT	3		
#define SITE		4 		
#define RASTER		5 		
#define ORASTER		6 		
#define LABELS		7
#define GRID		8
#define LEGEND		9
#define SCALE		10

#define FLOATCOLOR	1
#define FIXEDCOLOR	2

#define MAXRASTCOLOR 64

extern char *grassdir[];
extern char *keywords[];
	
typedef struct AGEOCOORD {
	double x;
	double y;
	} GEOCOORD;
	

typedef struct AGEOREG {
	GEOCOORD northEastCoordinate;
	GEOCOORD southWestCoordinate;
	double northSouthResolution;
	double eastWestResolution;
	double U_west;   /*  western edge  (UTM/meters)  */
	double U_east;   /*  eastern edge  (UTM/meters)  */
	double U_south;  /*  southern edge (UTM/meters)  */
	double U_north;  /*  northern edge (UTM/meters)  */
	double A_west;   /*  western edge  (array)       */
	double A_east;   /*  eastern edge  (array)       */
	double A_south;  /*  southern edge (array)       */
	double A_north;  /*  northern edge (array)       */
	double D_west;   /*  western edge  (screen dots) */
	double D_east;   /*  eastern edge  (screen dots) */
	double D_south;  /*  southern edge (screen dots) */
	double D_north;  /*  northern edge (screen dots) */
	double U_to_A_xconv, U_to_A_yconv;     /* UTM to Array */
	double A_to_D_xconv, A_to_D_yconv;     /* Array to Dot */
	double U_to_D_xconv, U_to_D_yconv;     /* UTM to Dot   */
	double ew_resolution ;
	double ns_resolution ;
	int ARRAY_ROWS ;
	int ARRAY_COLS ;
	} GEOREG;
	
#define D_u_to_d_row(region,U_row) (region.U_north-U_row)*region.U_to_D_yconv+region.D_north
#define D_u_to_d_col(region,U_col) (U_col-region.U_west)*region.U_to_D_xconv+region.D_west

typedef struct ALAYER {
	int				type;				
	char			*name;
	char			*mapset;
	double			scaleUpThreshold;
	double			scaleDownThreshold;
	int				lineWidth;			/* largeur de lignes pour les layers vectoriels */
	unsigned char	*dash;				/* type de lignes pour les polylines */
	int				nbdash;				/* nombre de dash pour les polylines */
	Tk_Uid			color;				/* couleur pour les layers vectoriels */	
	unsigned long	*rcolor;			/* table des couleurs pour les matrices */
	int				nrcolor;			/* dimension de la table des couleurs pour les matrices */
	int				colormode;			/* mode de construction des couleurs pour les matrices */
	Pixmap			symbol; 			/* symbole ou hachurage pour les layers vectoriels */	
	void			*sublayer[MAXAGG];	/* aggregation de layer */
	int				nb_sublayer;		/* nombre de sublayer definis */
	struct Map_info Map;				/* structure pour les fichiers vectoriels */
	int				cat;				/* specifie un filtre pour n'afficher qu'une seule 
									   	   categorie dans un fichier vectoriel */
	XFontStruct		*font;				/* police de caractere pour les labels */
	double			grid;				/* distance pour les grid */
	Tk_Window		window;				/* fenetre pour l'habillage */
	int				incx;
	int				incy;
	GC				legendGc;
	} LAYER;
	

typedef struct AVIEW {
	char		name[80];
	double		scale;
	double		res;
	int             zoommode;
	Tk_Window	tkwin;				/* utile pour tcl/tk */
	GC			gc;
	GC			drawGc;
	Tcl_Interp*	interp;
	int			updatePending;
	GEOREG		region;				/* association d'une region */
	LAYER		*layer[MAXAGG];		/* aggregation de layer */
	int			nb_layer;

	} VIEW;
	
int viewCmd();
int viewWidgetCmd();

