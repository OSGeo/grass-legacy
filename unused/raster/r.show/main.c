#include <stdio.h>
#include "gis.h"



#define G_TYPE_NAME(t)		(t ==  CELL_TYPE ?  "CELL" : \
				(t == FCELL_TYPE ? "FCELL" : \
				(t == DCELL_TYPE ? "DCELL" : NULL)))

#define G_TYPE_FORMAT(t)	(t ==  CELL_TYPE ?  "%d" : \
				(t == FCELL_TYPE ?  "%f" : \
				(t == DCELL_TYPE ? "%lf" : "")))


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


double	raster_value(struct RASTER_MAP_PTR buf, int col);
int	is_null_value(struct RASTER_MAP_PTR buf, int col);
int	value2str(char *str, int width, int prec,
		struct RASTER_MAP_PTR buf, int col);
int	cpvalue(struct RASTER_MAP_PTR dst, int dcol,
		struct RASTER_MAP_PTR src, int scol);


int
main(int argc, char *argv[])
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

		cpvalue(tmp, 2, buf, 10);

		value2str(str, 15, 5, tmp, 2);
		printf("<< %s ", str);
	}

	exit(0);
}


double
raster_value(struct RASTER_MAP_PTR buf, int col)
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


int
is_null_value(struct RASTER_MAP_PTR buf, int col)
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
value2str(char *str, int width, int prec, struct RASTER_MAP_PTR buf, int col)
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


int
cpvalue(struct RASTER_MAP_PTR dst, int dcol,
		struct RASTER_MAP_PTR src, int scol)
{
	switch(dst.type)
	{
		case CELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dst.data.c[dcol] =
						(CELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dst.data.c[dcol] =
						(CELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dst.data.c[dcol] =
						(CELL) src.data.d[scol];
					break;
			}
			break;
		case FCELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dst.data.f[dcol] =
						(FCELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dst.data.f[dcol] =
						(FCELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dst.data.f[dcol] =
						(FCELL) src.data.d[scol];
					break;
			}
			break;
		case DCELL_TYPE:
			switch(src.type)
			{
				case CELL_TYPE:
					dst.data.d[dcol] =
						(DCELL) src.data.c[scol];
					break;
				case FCELL_TYPE:
					dst.data.d[dcol] =
						(DCELL) src.data.f[scol];
					break;
				case DCELL_TYPE:
					dst.data.d[dcol] =
						(DCELL) src.data.d[scol];
					break;
			}
			break;
	}

	return 1;
}

