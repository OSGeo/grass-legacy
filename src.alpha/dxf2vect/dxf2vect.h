#include <stdio.h>
#include <string.h>
/* #include <stdlib.h> */
/* #include <float.h> */

#define DBL_MAX		9999999999999999999999.9
#define DBL_MIN		-99999999999999999999.9

#define TST(a)		fprintf(stderr,"(a)\n");
#define TSTINT(a)	fprintf(stderr,"(a):%d\n", (int) (a));
#define TSTSTR(a)	fprintf(stderr,"(a):%s\n", (a));

#define POINT	struct _point_
POINT
{
	double x, y;
};

#define DXF_DIG		struct dxf_dig
DXF_DIG
{
	long	n_off, s_off, e_off, w_off;
	char	*name;
	FILE	*fd;
};

#define ARRAY_INCR	256

#ifdef MAIN
	char	dig_path[240];
	POINT	*pt_array;
	int	arr_size;
	int	arr_max;
	double	n, s, e, w;
	char	zzero[8];
	char	eeight[8];
	char	tten[8];
	char	ttwenty[8];
	char	eelev[8];
	char	ttwentyone[8];
	char	entitie[12];
	char	polyline[12];
	char	line[8];
	char	point[8];
	char	vertex[8];
	char	seqend[8];
	char	dxf_line[80];
	int	num_layers;
	DXF_DIG	*layers;
#else
	extern char	dig_path[240];
	extern POINT	*pt_array;
	extern int	arr_size;
	extern int	arr_max;
	extern double	n, s, e, w;
	extern char	zzero[8];
	extern char	eeight[8];
	extern char	tten[8];
	extern char	ttwenty[8];
	extern char	eelev[8];
	extern char	ttwentyone[8];
	extern char	entitie[12];
	extern char	polyline[12];
	extern char	line[8];
	extern char	point[8];
	extern char	vertex[8];
	extern char	seqend[8];
	extern char	dxf_line[80];
	extern int	num_layers;
	extern DXF_DIG	*layers;
#endif
