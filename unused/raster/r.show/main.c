/*
 * char	*
 * r_type_name[] = { "CELL", "FCELL", "DCELL" };
 *
 *		r_type_name[CELL_TYPE]  or r_type_name[0]:	"CELL"
 *		r_type_name[FCELL_TYPE] or r_type_name[1]:	"FCELL"
 *		r_type_name[DCELL_TYPE] or r_type_name[2]:	"DCELL"
 *
 * char	*
 * r_type_spec[] = { "%%d",  "%%f",   "%%lf"  };
 *
 * 		r_type_spec[CELL_TYPE]  or r_type_spec[0]:	"%%d"
 * 		r_type_spec[FCELL_TYPE] or r_type_spec[1]:	"%%f"
 * 		r_type_spec[DCELL_TYPE] or r_type_spec[2]:	"%%lf"
 *
 *		it's not flexible for precision control.
 *		use r_str_value() instead. see main_old.c for more info.
 *
 * double
 * r_get_value(RASTER_MAP buf, int col);
 *
 *		returns double value from any types
 *
 * void
 * r_set_value(RASTER_MAP buf, int col, double val)
 *
 * 		sets buf[col] to val
 *
 * int
 * r_is_null_value(RASTER_MAP buf, int col);
 *
 * 		returns 1		if buf[col] is NULL
 * 			0		if buf[col] is not NULL or unknown type
 *
 * int
 * r_str_value(char *str, int width, int prec, RASTER_MAP buf, int col);
 *
 * 		fill str buffer with given value.
 *		for CELL type, prec is meaningless.
 *
 *		returns str length	if successful
 *			0		if given type is unknown
 *
 * void
 * r_copy_value(RASTER_MAP src, int scol, RASTER_MAP dst, int dcol);
 *
 *		copies src[scol] value to dst[dcol]
 */


#include <stdio.h>
#include "gis.h"

typedef	union	_RASTER_MAP_DATA
{
	void	*v;
	CELL	*c;
	FCELL	*f;
	DCELL	*d;
} RASTER_MAP_DATA;

typedef	struct	_RASTER_MAP
{
	RASTER_MAP_TYPE	type;
	RASTER_MAP_DATA	data;
} RASTER_MAP;


char	*r_type_name[] = { "CELL", "FCELL", "DCELL" };
char	*r_type_spec[] = { "%%d",  "%%f",   "%%lf"  };


double	r_get_value(RASTER_MAP buf, int col);
void	r_set_value(RASTER_MAP buf, int col, double val);
int	r_is_null_value(RASTER_MAP buf, int col);
int	r_str_value(char *str, int width, int prec, RASTER_MAP buf, int col);
void	r_copy_value(RASTER_MAP src, int scol, RASTER_MAP dst, int dcol);


int
main(int argc, char **argv)
{
	struct	Option *opt;
	struct	Cell_head cellhd;
	char	*name, *mapset;
	int	fd, row, rows, col, cols;
	char	str[20];
	RASTER_MAP	buf;

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

	fprintf(stderr, "%s\n", r_type_name[buf.type]);

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
			if(r_is_null_value(buf, col)){
				printf("NULL ");
			}else{
				r_str_value(str, 15, 5, buf, col);
				printf("%s ", str);
			}
		}
		printf("\n");
	}
	G_close_cell(fd);

	fprintf(stderr, "\n%d rows, %d cols\n", rows, cols);

	{
		RASTER_MAP tmp;
		double	dval;

		printf("\n ** Testing functions **\n");

		tmp.type   = buf.type;
		tmp.data.v = G_allocate_raster_buf(buf.type);

		r_str_value(str, 15, 5, tmp, 2);
		printf(" tmp[2] = %s,", str);

		r_str_value(str, 15, 5, buf, 10);
		printf(" buf[10] = %s\n", str);

		printf("copy buf[10] to tmp[2]\n");
		r_copy_value(buf, 10, tmp, 2);
		r_str_value(str, 15, 5, tmp, 2);
		printf(" tmp[2] = %s\n", str);

		r_set_value(tmp, 2, 10*r_get_value(tmp, 2));
		r_str_value(str, 15, 5, tmp, 2);
		printf("tmp[2]*10 = %s\n", str);
	}

	exit(0);
}


double
r_get_value(RASTER_MAP buf, int col)
{
	double	ret;

	switch(buf.type)
	{
		case CELL_TYPE:
			ret = (double) buf.data.c[col];
			break;
		case FCELL_TYPE:
			ret = (double) buf.data.f[col];
			break;
		case DCELL_TYPE:
			ret = (double) buf.data.d[col];
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0.0;
			break;
	}

	return ret;
}


void
r_set_value(RASTER_MAP buf, int col, double val)
{
	switch(buf.type)
	{
		case CELL_TYPE:
			buf.data.c[col] = (CELL) val;
			break;
		case FCELL_TYPE:
			buf.data.f[col] = (FCELL) val;
			break;
		case DCELL_TYPE:
			buf.data.d[col] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


int
r_is_null_value(RASTER_MAP buf, int col)
{
	int	ret;

	switch(buf.type)
	{
		case CELL_TYPE:
			ret = G_is_c_null_value(&buf.data.c[col]);
			break;
		case FCELL_TYPE:
			ret = G_is_f_null_value(&buf.data.f[col]);
			break;
		case DCELL_TYPE:
			ret = G_is_d_null_value(&buf.data.d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return ret;
}


int
r_str_value(char *str, int width, int prec, RASTER_MAP buf, int col)
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
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return strlen(str);
}


void
r_copy_value(RASTER_MAP src, int scol, RASTER_MAP dst, int dcol)
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
				default:
					G_warning("Illegal raster type\n");
					return;
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
				default:
					G_warning("Illegal raster type\n");
					return;
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
				default:
					G_warning("Illegal raster type\n");
					return;
					break;
			}
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}

