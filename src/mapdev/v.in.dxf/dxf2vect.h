#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"


#define DBL_MAX		9999999999999999999999.9
#define DBL_MIN		-99999999999999999999.9


#define DXF_ASCII 0
#define DXF_LABEL 1
#define DXF_LABEL_LINE 2

#define MAX_FILES 15 

#define POLYFLAG1 1

#define RSTEP  5.0


#define DXF_DIG		struct dxf_dig
DXF_DIG
{
	char	*name;
	int 	type;
	FILE	*fd;
	struct Map_info *Map;
	int	status;
};

#define ARR_INCR	256

#ifdef MAIN
	char  	*dxf_file = NULL;
	int	num_open_layers = 0;
	int	num_closed_layers = 0;
	int     from_table = 0;

#define GLOBAL_DXF
#else
	extern char  	*dxf_file;
	extern int	num_open_layers;
	extern int	num_closed_layers;
	int     from_table;
#define GLOBAL_DXF extern
#endif

GLOBAL_DXF  struct line_pnts *Points;
GLOBAL_DXF	unsigned long	file_size;
GLOBAL_DXF	int     percent;
GLOBAL_DXF	char	dig_path[240];
GLOBAL_DXF	char	basename[100];   /* dpg */
GLOBAL_DXF	int	ARR_MAX;
GLOBAL_DXF	double	n, s, e, w;
GLOBAL_DXF	long	n_off, s_off, e_off, w_off;
GLOBAL_DXF	char	zzero[8];
GLOBAL_DXF	char	eeight[8];
GLOBAL_DXF	char	tten[8];
GLOBAL_DXF	char	ttwenty[8];
GLOBAL_DXF	char	eelev[8];
GLOBAL_DXF	char	ttwentyone[8];
GLOBAL_DXF	char	entitie[12];
GLOBAL_DXF	char	header[12];
GLOBAL_DXF	char	extmin[12];
GLOBAL_DXF	char 	extmax[12];
GLOBAL_DXF	char	polyline[12];
GLOBAL_DXF	char	circle[12];
GLOBAL_DXF	char	text[12]; 	/* dpg */
GLOBAL_DXF	char	line[8];
GLOBAL_DXF	char	point[8];
GLOBAL_DXF	char	arc[8];
GLOBAL_DXF	char	vertex[8];
GLOBAL_DXF	char	seqend[8];
GLOBAL_DXF	char	dxf_line[80];
GLOBAL_DXF	DXF_DIG	layers[MAX_FILES];
GLOBAL_DXF	DXF_DIG *closed_layers;
GLOBAL_DXF	double	XMAX,XMIN,YMAX,YMIN;
GLOBAL_DXF	int	BOUNDARIES;
GLOBAL_DXF    struct Flag *ascii_flag;
GLOBAL_DXF	struct dig_head dxf_head;
GLOBAL_DXF	double *xinfo,*yinfo;


char *remap ();
char *dxf_fgets ();

DXF_DIG * dxf_which_layer ();
