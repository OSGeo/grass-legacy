/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:	GRASS Type-Independent Raster library (libtir library)
 * FILENAME:	libtir.c
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

#include <stdio.h>
#include "gis.h"
#include "libtir.h"


double
G_get_c(RASTER_MAP_ROW buf, int col)
{
	return (double) buf.c[col];
}


double
G_get_f(RASTER_MAP_ROW buf, int col)
{
	return (double) buf.f[col];
}


double
G_get_d(RASTER_MAP_ROW buf, int col)
{
	return (double) buf.d[col];
}


double *
G_get_cs(RASTER_MAP_ROW buf, int col, int num, double *val, int idx)
{
	int	i;

	if(num <= 0 || val == NULL){
		G_warning("G_get_cs(): num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) buf.c[col+i];

	return val;
}


double *
G_get_fs(RASTER_MAP_ROW buf, int col, int num, double *val, int idx)
{
	int	i;

	if(num <= 0 || val == NULL){
		G_warning("G_get_fs(): num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) buf.f[col+i];

	return val;
}


double *
G_get_ds(RASTER_MAP_ROW buf, int col, int num, double *val, int idx)
{
	int	i;

	if(num <= 0 || val == NULL){
		G_warning("G_get_ds(): num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) buf.d[col+i];

	return val;
}


void
G_set_c(RASTER_MAP_ROW buf, int col, double val, int num)
{
	int	i;

	if(num <= 0){
		G_warning("G_set_c(): num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		buf.c[col+i] = (CELL) val;

	return;
}


void
G_set_f(RASTER_MAP_ROW buf, int col, double val, int num)
{
	int	i;

	if(num <= 0){
		G_warning("G_set_f(): num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		buf.f[col+i] = (FCELL) val;

	return;
}


void
G_set_d(RASTER_MAP_ROW buf, int col, double val, int num)
{
	int	i;

	if(num <= 0){
		G_warning("G_set_d(): num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		buf.d[col+i] = (DCELL) val;

	return;
}


void
G_set_null_c(RASTER_MAP_ROW buf, int col, int num)
{
	if(num <= 0){
		G_warning("G_set_null_c(): num <= 0");
		return;
	}

	G_set_c_null_value(&buf.c[col], num);

	return;
}


void
G_set_null_f(RASTER_MAP_ROW buf, int col, int num)
{
	if(num <= 0){
		G_warning("G_set_null_f(): num <= 0");
		return;
	}

	G_set_f_null_value(&buf.f[col], num);

	return;
}


void
G_set_null_d(RASTER_MAP_ROW buf, int col, int num)
{
	if(num <= 0){
		G_warning("G_set_null_d(): num <= 0");
		return;
	}

	G_set_d_null_value(&buf.d[col], num);

	return;
}


int
G_is_null_c(RASTER_MAP_ROW buf, int col)
{
	return G_is_c_null_value(&buf.c[col]);
}


int
G_is_null_f(RASTER_MAP_ROW buf, int col)
{
	return G_is_f_null_value(&buf.f[col]);
}


int
G_is_null_d(RASTER_MAP_ROW buf, int col)
{
	return G_is_d_null_value(&buf.d[col]);
}


int
G_str_c(RASTER_MAP_ROW buf, int col, char *str, int width, int prec)
{
	if(str == NULL){
		G_warning("G_str_c(): str == NULL");
		return 0;
	}

	sprintf(str, "%*d", width, buf.c[col]);

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}


int
G_str_f(RASTER_MAP_ROW buf, int col, char *str, int width, int prec)
{
	if(str == NULL){
		G_warning("G_str_f(): str == NULL");
		return 0;
	}

	sprintf(str, "%*.*f", width, prec, buf.f[col]);

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}


int
G_str_d(RASTER_MAP_ROW buf, int col, char *str, int width, int prec)
{
	if(str == NULL){
		G_warning("G_str_d(): str == NULL");
		return 0;
	}

	sprintf(str, "%*.*lf", width, prec, buf.d[col]);

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}


#if METHOD == 1
/*****************************************************************************
 * METHOD 1
 *****************************************************************************/

double
G_get_r(RASTER_ROW row1, int col)
{
	double	ret;

	switch(row1.type)
	{
		case CELL_TYPE:
			ret = (double) row1.row.c[col];
			break;
		case FCELL_TYPE:
			ret = (double) row1.row.f[col];
			break;
		case DCELL_TYPE:
			ret = (double) row1.row.d[col];
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0.0;
			break;
	}

	return ret;
}


void
G_set_r(RASTER_ROW row1, int col, double val)
{
	switch(row1.type)
	{
		case CELL_TYPE:
			row1.row.c[col] = (CELL) val;
			break;
		case FCELL_TYPE:
			row1.row.f[col] = (FCELL) val;
			break;
		case DCELL_TYPE:
			row1.row.d[col] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


void
G_set_null_r(RASTER_ROW row1, int col)
{
	switch(row1.type)
	{
		case CELL_TYPE:
			G_set_c_null_value(&row1.row.c[col], 1);
			break;
		case FCELL_TYPE:
			G_set_f_null_value(&row1.row.f[col], 1);
			break;
		case DCELL_TYPE:
			G_set_d_null_value(&row1.row.d[col], 1);
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


int
G_is_null_r(RASTER_ROW row1, int col)
{
	int	ret;

	switch(row1.type)
	{
		case CELL_TYPE:
			ret = G_is_c_null_value(&row1.row.c[col]);
			break;
		case FCELL_TYPE:
			ret = G_is_f_null_value(&row1.row.f[col]);
			break;
		case DCELL_TYPE:
			ret = G_is_d_null_value(&row1.row.d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return ret;
}


int
G_str_r(RASTER_ROW row1, int col, char *str, int width, int prec)
{
	if(str == NULL){
		G_warning("G_str_r(): str == NULL");
		return 0;
	}

	switch(row1.type)
	{
		case CELL_TYPE:
			sprintf(str, "%*d", width, row1.row.c[col]);
			break;
		case FCELL_TYPE:
			sprintf(str, "%*.*f", width, prec, row1.row.f[col]);
			break;
		case DCELL_TYPE:
			sprintf(str, "%*.*lf", width, prec, row1.row.d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}

/**/

double
G_get_r2(RASTER_ROW2 row2, int row, int col)
{
	double	ret;

	switch(row2.type)
	{
		case CELL_TYPE:
			ret = (double) row2.row[row].c[col];
			break;
		case FCELL_TYPE:
			ret = (double) row2.row[row].f[col];
			break;
		case DCELL_TYPE:
			ret = (double) row2.row[row].d[col];
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0.0;
			break;
	}

	return ret;
}


void
G_set_r2(RASTER_ROW2 row2, int row, int col, double val)
{
	switch(row2.type)
	{
		case CELL_TYPE:
			row2.row[row].c[col] = (CELL) val;
			break;
		case FCELL_TYPE:
			row2.row[row].f[col] = (FCELL) val;
			break;
		case DCELL_TYPE:
			row2.row[row].d[col] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


void
G_set_null_r2(RASTER_ROW2 row2, int row, int col)
{
	switch(row2.type)
	{
		case CELL_TYPE:
			G_set_c_null_value(&row2.row[row].c[col], 1);
			break;
		case FCELL_TYPE:
			G_set_f_null_value(&row2.row[row].f[col], 1);
			break;
		case DCELL_TYPE:
			G_set_d_null_value(&row2.row[row].d[col], 1);
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


int
G_is_null_r2(RASTER_ROW2 row2, int row, int col)
{
	int	ret;

	switch(row2.type)
	{
		case CELL_TYPE:
			ret = G_is_c_null_value(&row2.row[row].c[col]);
			break;
		case FCELL_TYPE:
			ret = G_is_f_null_value(&row2.row[row].f[col]);
			break;
		case DCELL_TYPE:
			ret = G_is_d_null_value(&row2.row[row].d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return ret;
}


int
G_str_r2(RASTER_ROW2 row2, int row, int col, char *str, int width, int prec)
{
	if(str == NULL){
		G_warning("G_str_r2(): str == NULL");
		return 0;
	}

	switch(row2.type)
	{
		case CELL_TYPE:
			sprintf(str, "%*d", width,
					row2.row[row].c[col]);
			break;
		case FCELL_TYPE:
			sprintf(str, "%*.*f", width, prec,
					row2.row[row].f[col]);
			break;
		case DCELL_TYPE:
			sprintf(str, "%*.*lf", width, prec,
					row2.row[row].d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}

/**/

double *
G_get_rs(RASTER_ROW row1, int col, int num, double *val, int idx)
{
	int	i;

	if(num <= 0 || val == NULL){
		G_warning("G_get_rs(): num <= 0 || val == NULL");
		return NULL;
	}

	switch(row1.type)
	{
		case CELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row1.row.c[col+i];
			break;
		case FCELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row1.row.f[col+i];
			break;
		case DCELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row1.row.d[col+i];
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return val;
}


void
G_set_rs(RASTER_ROW row1, int col, double val, int num)
{
	int	i;

	if(num <= 0){
		G_warning("G_set_rs(): num <= 0");
		return;
	}

	switch(row1.type)
	{
		case CELL_TYPE:
			for(i=0; i<num; i++)
				row1.row.c[col+i] = (CELL) val;
			break;
		case FCELL_TYPE:
			for(i=0; i<num; i++)
				row1.row.f[col+i] = (FCELL) val;
			break;
		case DCELL_TYPE:
			for(i=0; i<num; i++)
				row1.row.d[col+i] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


void
G_set_null_rs(RASTER_ROW row1, int col, int num)
{
	if(num <= 0){
		G_warning("G_set_null_rs(): num <= 0");
		return;
	}

	switch(row1.type)
	{
		case CELL_TYPE:
			G_set_c_null_value(&row1.row.c[col], num);
			break;
		case FCELL_TYPE:
			G_set_f_null_value(&row1.row.f[col], num);
			break;
		case DCELL_TYPE:
			G_set_d_null_value(&row1.row.d[col], num);
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}

/**/

double *
G_get_rs2(RASTER_ROW2 row2, int row, int col, int num, double *val, int idx)
{
	int	i;

	if(num <= 0 || val == NULL){
		G_warning("G_get_rs2(): num <= 0 || val == NULL");
		return NULL;
	}

	switch(row2.type)
	{
		case CELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row2.row[row].c[col+i];
			break;
		case FCELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row2.row[row].f[col+i];
			break;
		case DCELL_TYPE:
			for(i=0; i<num; i++)
				val[idx+i] = (double) row2.row[row].d[col+i];
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return val;
}


void
G_set_rs2(RASTER_ROW2 row2, int row, int col, double val, int num)
{
	int	i;

	if(num <= 0){
		G_warning("G_set_rs2(): num <= 0");
		return;
	}

	switch(row2.type)
	{
		case CELL_TYPE:
			for(i=0; i<num; i++)
				row2.row[row].c[col+i] = (CELL) val;
			break;
		case FCELL_TYPE:
			for(i=0; i<num; i++)
				row2.row[row].f[col+i] = (FCELL) val;
			break;
		case DCELL_TYPE:
			for(i=0; i<num; i++)
				row2.row[row].d[col+i] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


void
G_set_null_rs2(RASTER_ROW2 row2, int row, int col, int num)
{
	if(num <= 0){
		G_warning("G_set_null_rs2(): num <= 0");
		return;
	}

	switch(row2.type)
	{
		case CELL_TYPE:
			G_set_c_null_value(&row2.row[row].c[col], num);
			break;
		case FCELL_TYPE:
			G_set_f_null_value(&row2.row[row].f[col], num);
			break;
		case DCELL_TYPE:
			G_set_d_null_value(&row2.row[row].d[col], num);
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


#elif METHOD == 2	/* End of method 1 */

/*****************************************************************************
 * METHOD 2
 *****************************************************************************/

/* All macro functions are defined in libtir.h. */

#endif			/* End of method 2 */

