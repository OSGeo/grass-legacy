#include <stdio.h>
#include "gis.h"


#ifdef	OLD_CPVALUE
#define	KEEPTYPE	0
#define	OVERTYPE	1
#endif


/* data type string */
#define G_TYPE_NAME(t)		(t ==  CELL_TYPE ?  "CELL" : \
				(t == FCELL_TYPE ? "FCELL" : \
				(t == DCELL_TYPE ? "DCELL" : NULL)))

/* data type format:
 * In fact this can not be used directly in *printf().
 * 
 * 	printf("This value is "G_TYPE_FORMAT(buf.type)"\n",
 * 						(buf.type == CELL_TYPE ?
 * 							buf.data.c[col] :
 * 						(buf.type == FCELL_TYPE ?
 * 							buf.data.f[col] :
 * 							buf.data.d[col])));
 *
 * 	printf(G_TYPE_FORMAT(buf.type),
 * 						(buf.type == CELL_TYPE ?
 * 							buf.data.c[col] :
 * 						(buf.type == FCELL_TYPE ?
 * 							buf.data.f[col] :
 * 							buf.data.d[col])));
 *
 * Unfortunately for two cases two arguments to printf() do not work at all.
 *
 *	for(i=0; i<3; i++)
 * 		printf((i==0 ? "%d" : (i==1 ? "%5.2f" : "%10.2lf")),
 * 				(i==0 ? 123 : (i==1 ? 99.23 : 100.32)));
 *
 * when i == 0, printf() won't work. I don't know why.
 *
 * 
 * Then?
 *
 * 	sprintf(buf, "This value is %s\n", G_TYPE_FORMAT(buf.type));
 * 	printf(buf, buf.data.f[col]);
 *
 * Hmm, how can we know that we should use buf.data.f[col] for G_TYPE_FORMAT()?
 *
 * So it's useless at all.
 */
#define G_TYPE_FORMAT(t)	(t ==  CELL_TYPE ?  "%%d" : \
				(t == FCELL_TYPE ?  "%%f" : \
				(t == DCELL_TYPE ? "%%lf" : "")))


union	RASTER_PTR
{
	void	*v;
	CELL	*c;
	FCELL	*f;
	DCELL	*d;
};

struct	RASTER_MAP_PTR
{
	RASTER_MAP_TYPE		type;
	union RASTER_PTR	data;
};


#ifdef	RASTER_VALUE_FUNC
double	raster_value(struct RASTER_MAP_PTR buf, int col);
#else
#define	raster_value(buf, col)	((double)(buf.type == CELL_TYPE ?	\
						buf.data.c[col] :	\
					(buf.type == FCELL_TYPE ?	\
					 	buf.data.f[col] :	\
						buf.data.d[col])))
#endif

int	is_null_value(struct RASTER_MAP_PTR buf, int col);
int	value2str(char *str, int width, int prec,
		struct RASTER_MAP_PTR buf, int col);
#ifndef	OLD_CPVALUE
int	cpvalue(struct RASTER_MAP_PTR dest, int dcol,
		struct RASTER_MAP_PTR src, int scol);
#else
int	cpvalue(struct RASTER_MAP_PTR dest, int dcol,
		struct RASTER_MAP_PTR src, int scol, int mode);
#endif


int
main(argc, argv)
	int	argc;
	char	**argv;
{
	struct	Option		*opt;
	struct	Cell_head	cellhd;
	char	*name, *mapset;
	int	fd, row, rows, col, cols;

	struct	RASTER_MAP_PTR	buf;
	char	str[20];

	opt = G_define_option() ;
	opt->key        = "map" ;
	opt->description= "Map to manipulate" ;
	opt->type       = TYPE_STRING ;
	opt->required   = YES ;
	opt->gisprompt  = "old,cell,raster";

	G_gisinit(argv[0]);

	if(G_parser(argc, argv))
	        exit(-1);

	name   = opt->answer;
	mapset = G_mapset();
	if(!G_find_file("cell", name, mapset)){
		fprintf(stderr, "\n** %s - not found **\n", name);
		exit(1);
	}

	G_get_cellhd(name, mapset, &cellhd);

	rows = cellhd.rows;
	cols = cellhd.cols;

	buf.type   = G_raster_map_type(name, mapset);
	buf.data.v = G_allocate_raster_buf(buf.type);

	fprintf(stderr, "%s\n", G_TYPE_NAME(buf.type));

	if((fd = G_open_cell_old(name, mapset)) < 0){
		fprintf(stderr, "\n** %s - could not read **\n", name);
		exit(1);
	}

	for(row=0; row<rows; row++){
		G_percent(row, rows, 2);
		if(G_get_raster_row(fd, buf.data.v, row, buf.type) < 0){
			G_close_cell(fd);
			exit(1);
		}

		for(col=0; col<cols; col++){
			if(is_null_value(buf, col)){
				printf("NULL ");
			}else{
				value2str(str, 15, 5, buf, col);
				printf("%s ", str);
			}
		}
		printf("\n");
	}
	G_close_cell(fd);

	fprintf(stderr, "\n%d rows, %d cols\n", rows, cols);

	{
		struct RASTER_MAP_PTR tmp;

		tmp.type   = buf.type;
		tmp.data.v = G_allocate_raster_buf(buf.type);

		value2str(str, 15, 5, tmp, 2);
		printf("\n >> %s ", str);

#ifndef	OLD_CPVALUE
		cpvalue(tmp, 2, buf, 10);
#else
		cpvalue(tmp, 2, buf, 10, KEEPTYPE);
#endif

		value2str(str, 15, 5, tmp, 2);
		printf("<< %s ", str);
	}

	exit(0);
}


#ifdef	RASTER_VALUE_FUNC
double
raster_value(buf, col)
	struct	RASTER_MAP_PTR buf;
	int	col;
{
	double	retval;

	switch(buf.type)
	{
		case CELL_TYPE:
			retval = (double) buf.data.c[col];
			break;
		case FCELL_TYPE:
			retval = (double) buf.data.f[col];
			break;
		case DCELL_TYPE:
			retval = (double) buf.data.d[col];
			break;
	}

	return retval;
}
#endif


int
is_null_value(buf, col)
	struct	RASTER_MAP_PTR buf;
	int	col;
{
	switch(buf.type)
	{
		case CELL_TYPE:
			return G_is_c_null_value(&buf.data.c[col]);
			break;
		case FCELL_TYPE:
			return G_is_f_null_value(&buf.data.f[col]);
			break;
		case DCELL_TYPE:
			return G_is_d_null_value(&buf.data.d[col]);
			break;
	}

	return -1;
}


int
value2str(str, width, prec, buf, col)
	char	*str;
	int	width, prec, col;
	struct	RASTER_MAP_PTR buf;
{
	switch(buf.type)
	{
		case CELL_TYPE:
			sprintf(str, "%*d", width, buf.data.c[col]);
			break;
		case FCELL_TYPE:
			sprintf(str, "%*.*f", width, prec, buf.data.f[col]);
			break;
		case DCELL_TYPE:
			sprintf(str, "%*.*lf", width, prec, buf.data.d[col]);
			break;
		default:
			return 0;
			break;
	}

	return strlen(str);
}


#ifndef	OLD_CPVALUE

int
cpvalue(dest, dcol, src, scol)
	struct	RASTER_MAP_PTR dest, src;
	int	dcol, scol;
{
	switch(dest.type)
	{
		case CELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dest.data.c[dcol] =
						(CELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dest.data.c[dcol] =
						(CELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dest.data.c[dcol] =
						(CELL) src.data.d[scol];
					break;
			}
			break;
		case FCELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dest.data.f[dcol] =
						(FCELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dest.data.f[dcol] =
						(FCELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dest.data.f[dcol] =
						(FCELL) src.data.d[scol];
					break;
			}
			break;
		case DCELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dest.data.d[dcol] =
						(DCELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dest.data.d[dcol] =
						(DCELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dest.data.d[dcol] =
						(DCELL) src.data.d[scol];
					break;
			}
			break;
	}

	return 1;
}

#else

int
cpvalue(dest, dcol, src, scol, mode)
	struct	RASTER_MAP_PTR dest, src;
	int	dcol, scol, mode;
{
	if(mode == KEEPTYPE && src.type != dest.type)
		return 0;

	dest.type = src.type;

	switch(src.type)
	{
		case CELL_TYPE:
			dest.data.c[dcol] = src.data.c[scol];
			break;
		case FCELL_TYPE:
			dest.data.f[dcol] = src.data.f[scol];
			break;
		case DCELL_TYPE:
			dest.data.d[dcol] = src.data.d[scol];
			break;
	}

	return 1;
}

#endif

