/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"

#ifndef DBL_MAX
#define DBL_MAX		9999999999999999999999.9
#define DBL_MIN		-99999999999999999999.9
#endif

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

GLOBAL_DXF      struct line_pnts	*Points;
GLOBAL_DXF	unsigned long	file_size;
GLOBAL_DXF	int     percent;
GLOBAL_DXF	char	dig_path[240];
GLOBAL_DXF	char	base_name[100];   /* dpg */
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
GLOBAL_DXF	char	dxf_line[256];
GLOBAL_DXF	DXF_DIG	layers[MAX_FILES];
GLOBAL_DXF	DXF_DIG *closed_layers;
GLOBAL_DXF	double	XMAX,XMIN,YMAX,YMIN;
GLOBAL_DXF	int	BOUNDARIES;
GLOBAL_DXF      struct Flag	*ascii_flag, *txtbox_flag;
GLOBAL_DXF	struct dig_head	dxf_head;
GLOBAL_DXF	double	*xinfo,*yinfo;


/* add_arc.c */
int dxf_add_arc(FILE *);
/* add_bounds.c */
int dxf_add_boundaries(void);
/* add_circle.c */
int dxf_add_circle(FILE *);
int make_arc(int, double, double, double, double, double, int);
/* add_extents.c */
int dxf_add_extents(void);
/* add_line.c */
int dxf_add_line(FILE *);
/* add_point.c */
int dxf_add_point(FILE *);
/* add_polyline.c */
int dxf_add_polyline(FILE *);
int write_polylines(DXF_DIG *, int);
/* check_ext.c */
int dxf_check_ext(double, double);
/* close_layer.c */
int dxf_close_layer(int);
/* debug.c */
int debuginit(void);
int debugf(char *, ...);
/* entities.c */
int dxf_entities(FILE *);
/* find_lines.c */
int dxf_find_lines(FILE *);
/* header.c */
int dxf_header(FILE *);
/* init_chars.c */
int dxf_init_chars(void);
/* label_box.c */
int dxf_add_labelbox(FILE *);
int dxf_readcode(FILE *);
/* main.c */
int add_line_layer(char *);
int add_att_layer(char *);
int add_layer(char *, char *[][2], int *);
int _add_layer(char *[][2], int *, char *, char *);
char *remap(char *, int);
char *dxf_fgets(char *, int, FILE *);
int extra_help(void);
int big_percent(unsigned long, unsigned long, int);
/* make_header.c */
int dxf_make_header(DXF_DIG *);
/* open_layer.c */
int dxf_open_layer(int, int);
/* reopen_layer.c */
int dxf_reopen_layer(int, int, int);
/* which_layer.c */
DXF_DIG *dxf_which_layer(char *, int);
int set_status(int);
int find_highest_status(void);
