#ifndef	_LIBR_H_
#define	_LIBR_H_

typedef	union	_RASTER_MAP_ROW
{
	void	*v;
	CELL	*c;
	FCELL	*f;
	DCELL	*d;
} RASTER_MAP_ROW;

typedef	struct	_RASTER_ROW
{
	RASTER_MAP_TYPE	type;
	RASTER_MAP_ROW	row;
} RASTER_ROW;

typedef	struct	_RASTER_MAP
{
	RASTER_MAP_TYPE	type;
	RASTER_MAP_ROW	*row;
} RASTER_MAP;

static	char	*r_type_name[] = { "CELL", "FCELL", "DCELL" };
static	char	*r_type_spec[] = { "%%d",  "%%f",   "%%lf"  };


double	r_get_value(RASTER_ROW buf, int col);
void	r_set_value(RASTER_ROW buf, int col, double val);
int	r_is_null_value(RASTER_ROW buf, int col);
int	r_str_value(char *str, int width, int prec, RASTER_ROW buf, int col);
void	r_copy_value(RASTER_ROW sbuf, int scol, RASTER_ROW dbuf, int dcol);

double	r_get_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col);
void	r_set_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col,
		double val);
int	r_is_null_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col);
int	r_str_value2(char *str, int width, int prec,
		RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col);
void	r_copy_value2(RASTER_MAP_TYPE stype, RASTER_MAP_ROW sdata, int scol,
		RASTER_MAP_TYPE dtype, RASTER_MAP_ROW ddata, int dcol);

#endif

