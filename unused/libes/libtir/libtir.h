/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:	GRASS Type-Independent Raster library (libtir library)
 * FILENAME:	libtir.h
 * AUTHOR(S):	Huidae Cho - Korea - hdcho@geni.cemtlo.com
 * PURPOSE:	This library functions help you program a type-independent
 * 		raster modules.
 * DATE CREATED: Apr 30 2001
 * COPYRIGHT:	(C) 2001 by the GRASS Development Team
 *
 *		This program is free software under the GNU General Public
 *		License (>=v2). Read the file COPYING that comes with GRASS
 *		for details.
 *
 *****************************************************************************/

#ifndef	_LIBTIR_H_
#define	_LIBTIR_H_

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

typedef	struct	_RASTER_ROW2
{
	RASTER_MAP_TYPE	type;
	RASTER_MAP_ROW	*row;
} RASTER_ROW2;

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

/*****************************************************************************
 * New approach
 *****************************************************************************/
double	r_get_c(RASTER_MAP_ROW data, int col);
double	r_get_f(RASTER_MAP_ROW data, int col);
double	r_get_d(RASTER_MAP_ROW data, int col);

double	*r_get_cs(RASTER_MAP_ROW data, int col, int num, double *val, int idx);
double	*r_get_fs(RASTER_MAP_ROW data, int col, int num, double *val, int idx);
double	*r_get_ds(RASTER_MAP_ROW data, int col, int num, double *val, int idx);

void	r_set_c(RASTER_MAP_ROW data, int col, double val, int num);
void	r_set_f(RASTER_MAP_ROW data, int col, double val, int num);
void	r_set_d(RASTER_MAP_ROW data, int col, double val, int num);

void	r_set_null_c(RASTER_MAP_ROW data, int col, int num);
void	r_set_null_f(RASTER_MAP_ROW data, int col, int num);
void	r_set_null_d(RASTER_MAP_ROW data, int col, int num);

int	r_is_null_c(RASTER_MAP_ROW data, int col);
int	r_is_null_f(RASTER_MAP_ROW data, int col);
int	r_is_null_d(RASTER_MAP_ROW data, int col);

int	r_str_c(char *str, int width, int prec, RASTER_MAP_ROW data, int col);
int	r_str_f(char *str, int width, int prec, RASTER_MAP_ROW data, int col);
int	r_str_d(char *str, int width, int prec, RASTER_MAP_ROW data, int col);


static	double	(*rp_get[])(RASTER_MAP_ROW data, int col) =
{
	r_get_c, r_get_f, r_get_d
};

static	double	*(*rp_gets[])(RASTER_MAP_ROW data, int col, int num,
		double *val, int idx) =
{
	r_get_cs, r_get_fs, r_get_ds
};

static	void	(*rp_set[])(RASTER_MAP_ROW data, int col, double val, int num) =
{
	r_set_c, r_set_f, r_set_d
};

static	void	(*rp_set_null[])(RASTER_MAP_ROW data, int col, int num) =
{
	r_set_null_c, r_set_null_f, r_set_null_d
};

static	int	(*rp_is_null[])(RASTER_MAP_ROW data, int col) =
{
	r_is_null_c, r_is_null_f, r_is_null_d
};

static	int	(*rp_str[])(char *str, int width, int prec,
			RASTER_MAP_ROW data, int col) =
{
	r_str_c, r_str_f, r_str_d
};

#define	rm_get(buf, c)		(rp_get[(buf).type])((buf).row, c)
#define	rm_set(buf, c, v)	(rp_set[(buf).type])((buf).row, c, v, 1)
#define	rm_set_null(buf, c)	(rp_set_null[(buf).type])((buf).row, c, 1)
#define	rm_is_null(buf, c)	(rp_is_null[(buf).type])((buf).row, c)
#define	rm_str(str, width, prec, buf, c)				\
				(rp_str[(buf).type])(str, width, prec,	\
					(buf).row, c)

#define	rm_get2(buf, r, c)	(rp_get[(buf).type])((buf).row[r], c)
#define	rm_set2(buf, r, c, v)	(rp_set[(buf).type])((buf).row[r], c, v, 1)
#define	rm_set_null2(buf, r, c)	(rp_set_null[(buf).type])((buf).row[r], c, 1)
#define	rm_is_null2(buf, r, c)	(rp_is_null[(buf).type])((buf).row[r], c)
#define	rm_str2(str, width, prec, buf, r, c)				\
				(rp_str[(buf).type])(str, width, prec,	\
					(buf).row[r], c)

/* Use these macros cafully not to exceed the upper limit of buffer with n */
#define	rm_gets(buf, c, n, v, i)(rp_gets[(buf).type])((buf).row, c, n, v, i)
#define	rm_sets(buf, c, v, n)	(rp_set[(buf).type])((buf).row, c, v, n)
#define	rm_set_nulls(buf, c, n)	(rp_set_null[(buf).type])((buf).row, c, n)

#define	rm_gets2(buf, r, c, n)	(rp_get[(buf).type])((buf).row[r], c, n)
#define	rm_sets2(buf, r, c, v, n)					\
				(rp_set[(buf).type])((buf).row[r], c, v, n)
#define	rm_set_nulls2(buf, r, c, n)					\
				(rp_set_null[(buf).type])((buf).row[r], c, n)

#endif

