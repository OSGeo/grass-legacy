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


#define	METHOD	1


/* Type definitions */

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

static	char	*G_type_name[] = { "CELL", "FCELL", "DCELL" };
static	char	*G_type_spec[] = { "%%d",  "%%f",   "%%lf"  };

/* Base function definitions */

double	G_get_c(RASTER_MAP_ROW buf, int col);
double	G_get_f(RASTER_MAP_ROW buf, int col);
double	G_get_d(RASTER_MAP_ROW buf, int col);

double	*G_get_cs(RASTER_MAP_ROW buf, int col, int num, double *val, int idx);
double	*G_get_fs(RASTER_MAP_ROW buf, int col, int num, double *val, int idx);
double	*G_get_ds(RASTER_MAP_ROW buf, int col, int num, double *val, int idx);

void	G_set_c(RASTER_MAP_ROW buf, int col, double val, int num);
void	G_set_f(RASTER_MAP_ROW buf, int col, double val, int num);
void	G_set_d(RASTER_MAP_ROW buf, int col, double val, int num);

void	G_set_null_c(RASTER_MAP_ROW buf, int col, int num);
void	G_set_null_f(RASTER_MAP_ROW buf, int col, int num);
void	G_set_null_d(RASTER_MAP_ROW buf, int col, int num);

int	G_is_null_c(RASTER_MAP_ROW buf, int col);
int	G_is_null_f(RASTER_MAP_ROW buf, int col);
int	G_is_null_d(RASTER_MAP_ROW buf, int col);

int	G_str_c(RASTER_MAP_ROW buf, int col, char *str, int width, int prec);
int	G_str_f(RASTER_MAP_ROW buf, int col, char *str, int width, int prec);
int	G_str_d(RASTER_MAP_ROW buf, int col, char *str, int width, int prec);


/* Base function array definitions */

static	double	(*G__get_r[])(RASTER_MAP_ROW buf, int col) =
{
	G_get_c, G_get_f, G_get_d
};

static	double	*(*G__gets_r[])(RASTER_MAP_ROW buf, int col, int num,
		double *val, int idx) =
{
	G_get_cs, G_get_fs, G_get_ds
};

static	void	(*G__set_r[])(RASTER_MAP_ROW buf, int col,
		double val, int num) =
{
	G_set_c, G_set_f, G_set_d
};

static	void	(*G__set_null_r[])(RASTER_MAP_ROW buf, int col, int num) =
{
	G_set_null_c, G_set_null_f, G_set_null_d
};

static	int	(*G__is_null_r[])(RASTER_MAP_ROW buf, int col) =
{
	G_is_null_c, G_is_null_f, G_is_null_d
};

static	int	(*G__str_r[])(RASTER_MAP_ROW buf, int col,
		char *str, int width, int prec) =
{
	G_str_c, G_str_f, G_str_d
};


#if METHOD == 1

/*****************************************************************************
 * METHOD 1
 *****************************************************************************/

double	G_get_r(RASTER_ROW row1, int col);
void	G_set_r(RASTER_ROW row1, int col, double val);
void	G_set_null_r(RASTER_ROW row1, int col);
int	G_is_null_r(RASTER_ROW row1, int col);
int	G_str_r(RASTER_ROW row1, int col, char *str, int width, int prec);

/**/

double	G_get_r2(RASTER_ROW2 row2, int row, int col);
void	G_set_r2(RASTER_ROW2 row2, int row, int col, double val);
void	G_set_null_r2(RASTER_ROW2 row2, int row, int col);
int	G_is_null_r2(RASTER_ROW2 row2, int row, int col);
int	G_str_r2(RASTER_ROW2 row2, int row, int col,
		char *str, int width, int prec);

/**/

double	*G_get_rs(RASTER_ROW row1, int col, int num, double *val, int idx);
void	G_set_rs(RASTER_ROW row1, int col, double val, int num);
void	G_set_null_rs(RASTER_ROW row1, int col, int num);

/**/

double	*G_get_rs2(RASTER_ROW2 row2, int row, int col, int num,
		double *val, int idx);
void	G_set_rs2(RASTER_ROW2 row2, int row, int col, double val, int num);
void	G_set_null_rs2(RASTER_ROW2 row2, int row, int col, int num);


#elif METHOD == 2	/* End of method 1 */

/*****************************************************************************
 * METHOD 2
 *****************************************************************************/

/* Macro definitions */

#define	G_get_r(r1, c)		(G__get_r[(r1).type])((r1).row, c)
#define	G_set_r(r1, c, v)	(G__set_r[(r1).type])((r1).row, c, v, 1)
#define	G_set_null_r(r1, c)	(G__set_null_r[(r1).type])((r1).row, c, 1)
#define	G_is_null_r(r1, c)	(G__is_null_r[(r1).type])((r1).row, c)
#define	G_str_r(r1, c, s, w, p)	(G__str_r[(r1).type])((r1).row, c, s, w, p)

/**/

#define	G_get_r2(r2, r, c)	(G__get_r[(r2).type])((r2).row[r], c)
#define	G_set_r2(r2, r, c, v)	(G__set_r[(r2).type])((r2).row[r], c, v, 1)
#define	G_set_null_r2(r2, r, c)	(G__set_null_r[(r2).type])((r2).row[r], c, 1)
#define	G_is_null_r2(r2, r, c)	(G__is_null_r[(r2).type])((r2).row[r], c)
#define	G_str_r2(r2, r, c, s, w, p)					\
		(G__str_r[(r2).type])((r2).row[r], c, s, w, p)

/* Use these macros cafully not to exceed the upper limit of buffer with n */
#define	G_get_rs(r1, c, n, v, i)(G__get_rs[(r1).type])((r1).row, c, n, v, i)
#define	G_set_rs(r1, c, v, n)	(G__set_r[(r1).type])((r1).row, c, v, n)
#define	G_set_null_rs(r1, c, n)	(G__set_null_r[(r1).type])((r1).row, c, n)

/**/

#define	G_get_rs2(r2, r, c, n, v, i)					\
		(G__get_rs[(r2).type])((r2).row[r], c, n, i, i)
#define	G_set_rs2(r2, r, c, v, n)					\
		(G__set_r[(r2).type])((r2).row[r], c, v, n)
#define	G_set_null_rs2(r2, r, c, n)					\
		(G__set_null_r[(r2).type])((r2).row[r], c, n)

#endif			/* End of method 2 */

#endif

