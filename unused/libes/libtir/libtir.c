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
r_get_c(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_get_c(): col < 0");
		return 0.0;
	}

	return (double) data.c[col];
}


double
r_get_f(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_get_f(): col < 0");
		return 0.0;
	}

	return (double) data.f[col];
}


double
r_get_d(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_get_d(): col < 0");
		return 0.0;
	}

	return (double) data.d[col];
}


double *
r_get_cs(RASTER_MAP_ROW data, int col, int num, double *val, int idx)
{
	int	i;

	if(col < 0 || num <= 0 || val == NULL){
		G_warning("r_get_cs(): col < 0 || num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) data.c[col+i];

	return val;
}


double *
r_get_fs(RASTER_MAP_ROW data, int col, int num, double *val, int idx)
{
	int	i;

	if(col < 0 || num <= 0 || val == NULL){
		G_warning("r_get_fs(): col < 0 || num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) data.f[col+i];

	return val;
}


double *
r_get_ds(RASTER_MAP_ROW data, int col, int num, double *val, int idx)
{
	int	i;

	if(col < 0 || num <= 0 || val == NULL){
		G_warning("r_get_ds(): col < 0 || num <= 0 || val == NULL");
		return NULL;
	}

	for(i=0; i<num; i++)
		val[idx+i] = (double) data.d[col+i];

	return val;
}


void
r_set_c(RASTER_MAP_ROW data, int col, double val, int num)
{
	int	i;

	if(col < 0 || num <= 0){
		G_warning("r_set_c(): col < 0 || num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		data.c[col+i] = (CELL) val;

	return;
}


void
r_set_f(RASTER_MAP_ROW data, int col, double val, int num)
{
	int	i;

	if(col < 0 || num <= 0){
		G_warning("r_set_f(): col < 0 || num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		data.f[col+i] = (FCELL) val;

	return;
}


void
r_set_d(RASTER_MAP_ROW data, int col, double val, int num)
{
	int	i;

	if(col < 0 || num <= 0){
		G_warning("r_set_d(): col < 0 || num <= 0");
		return;
	}

	for(i=0; i<num; i++)
		data.d[col+i] = (DCELL) val;

	return;
}


void
r_set_null_c(RASTER_MAP_ROW data, int col, int num)
{
	if(col < 0 || num <= 0){
		G_warning("r_set_null_c(): col < 0 || num <= 0");
		return;
	}

	return G_set_c_null_value(&data.c[col], num);
}


void
r_set_null_f(RASTER_MAP_ROW data, int col, int num)
{
	if(col < 0 || num <= 0){
		G_warning("r_set_null_f(): col < 0 || num <= 0");
		return;
	}

	return G_set_f_null_value(&data.f[col], num);
}


void
r_set_null_d(RASTER_MAP_ROW data, int col, int num)
{
	if(col < 0 || num <= 0){
		G_warning("r_set_null_d(): col < 0 || num <= 0");
		return;
	}

	return G_set_d_null_value(&data.d[col], num);
}


int
r_is_null_c(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_is_null_c(): col < 0");
		return 0;
	}

	return G_is_c_null_value(&data.c[col]);
}


int
r_is_null_f(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_is_null_f(): col < 0");
		return 0;
	}

	return G_is_f_null_value(&data.f[col]);
}


int
r_is_null_d(RASTER_MAP_ROW data, int col)
{
	if(col < 0){
		G_warning("r_is_null_d(): col < 0");
		return 0;
	}

	return G_is_d_null_value(&data.d[col]);
}


int
r_str_c(char *str, int width, int prec, RASTER_MAP_ROW data, int col)
{
	if(str == NULL || col < 0){
		G_warning("r_str_c(): str == NULL || col < 0");
		return 0;
	}

	sprintf(str, "%*d", width, data.c[col]);

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
r_str_f(char *str, int width, int prec, RASTER_MAP_ROW data, int col)
{
	if(str == NULL || col < 0){
		G_warning("r_str_f(): str == NULL || col < 0");
		return 0;
	}

	sprintf(str, "%*.*f", width, prec, data.f[col]);

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
r_str_d(char *str, int width, int prec, RASTER_MAP_ROW data, int col)
{
	if(str == NULL || col < 0){
		G_warning("r_str_d(): str == NULL || col < 0");
		return 0;
	}

	sprintf(str, "%*.*lf", width, prec, data.d[col]);

	if(width <= 0){
		int	i, j, l;

		l = strlen(str);
		for(i=0; i<l && str[i]==' '; i++);
		for(j=i; j<=l; j++)
			str[j-i] = str[j];
	}

	return strlen(str);
}



/*****************************************************************************
 * Old method
 *****************************************************************************/
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
 *		use r_str_value() instead.
 *
 * double
 * r_get_value(RASTER_ROW buf, int col);
 *
 *		returns double value from any types
 *
 * void
 * r_set_value(RASTER_ROW buf, int col, double val)
 *
 * 		sets buf[col] to val
 *
 * int
 * r_is_null_value(RASTER_ROW buf, int col);
 *
 * 		returns 1		if buf[col] is NULL
 * 			0		if buf[col] is not NULL or unknown type
 *
 * int
 * r_str_value(char *str, int width, int prec, RASTER_ROW buf, int col);
 *
 * 		fill str buffer with given value.
 *		for CELL type, prec is meaningless.
 *		if width <= 0, no space is included in str.
 *
 *		returns str length	if successful
 *			0		if given type is unknown
 *
 * void
 * r_copy_value(RASTER_ROW sbuf, int scol, RASTER_ROW dbuf, int dcol);
 *
 *		copies sbuf[scol] value to dbuf[dcol]
 */


double
r_get_value(RASTER_ROW buf, int col)
{
	double	ret;

	switch(buf.type)
	{
		case CELL_TYPE:
			ret = (double) buf.row.c[col];
			break;
		case FCELL_TYPE:
			ret = (double) buf.row.f[col];
			break;
		case DCELL_TYPE:
			ret = (double) buf.row.d[col];
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0.0;
			break;
	}

	return ret;
}


void
r_set_value(RASTER_ROW buf, int col, double val)
{
	switch(buf.type)
	{
		case CELL_TYPE:
			buf.row.c[col] = (CELL) val;
			break;
		case FCELL_TYPE:
			buf.row.f[col] = (FCELL) val;
			break;
		case DCELL_TYPE:
			buf.row.d[col] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


int
r_is_null_value(RASTER_ROW buf, int col)
{
	int	ret;

	switch(buf.type)
	{
		case CELL_TYPE:
			ret = G_is_c_null_value(&buf.row.c[col]);
			break;
		case FCELL_TYPE:
			ret = G_is_f_null_value(&buf.row.f[col]);
			break;
		case DCELL_TYPE:
			ret = G_is_d_null_value(&buf.row.d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return ret;
}


int
r_str_value(char *str, int width, int prec, RASTER_ROW buf, int col)
{
	switch(buf.type)
	{
		case CELL_TYPE:
			sprintf(str, "%*d", width, buf.row.c[col]);
			break;
		case FCELL_TYPE:
			sprintf(str, "%*.*f", width, prec, buf.row.f[col]);
			break;
		case DCELL_TYPE:
			sprintf(str, "%*.*lf", width, prec, buf.row.d[col]);
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


void
r_copy_value(RASTER_ROW sbuf, int scol, RASTER_ROW dbuf, int dcol)
{
	switch(dbuf.type)
	{
		case CELL_TYPE:
			switch(sbuf.type)
			{
				case CELL_TYPE:
					dbuf.row.c[dcol] =
						(CELL) sbuf.row.c[scol];
					break;
				case FCELL_TYPE:
					dbuf.row.c[dcol] =
						(CELL) sbuf.row.f[scol];
					break;
				case DCELL_TYPE:
					dbuf.row.c[dcol] =
						(CELL) sbuf.row.d[scol];
					break;
				default:
					G_warning("Illegal raster type\n");
					return;
					break;
			}
			break;
		case FCELL_TYPE:
			switch(sbuf.type)
			{
				case CELL_TYPE:
					dbuf.row.f[dcol] =
						(FCELL) sbuf.row.c[scol];
					break;
				case FCELL_TYPE:
					dbuf.row.f[dcol] =
						(FCELL) sbuf.row.f[scol];
					break;
				case DCELL_TYPE:
					dbuf.row.f[dcol] =
						(FCELL) sbuf.row.d[scol];
					break;
				default:
					G_warning("Illegal raster type\n");
					return;
					break;
			}
			break;
		case DCELL_TYPE:
			switch(sbuf.type)
			{
				case CELL_TYPE:
					dbuf.row.d[dcol] =
						(DCELL) sbuf.row.c[scol];
					break;
				case FCELL_TYPE:
					dbuf.row.d[dcol] =
						(DCELL) sbuf.row.f[scol];
					break;
				case DCELL_TYPE:
					dbuf.row.d[dcol] =
						(DCELL) sbuf.row.d[scol];
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


double
r_get_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col)
{
	double	ret;

	switch(type)
	{
		case CELL_TYPE:
			ret = (double) data.c[col];
			break;
		case FCELL_TYPE:
			ret = (double) data.f[col];
			break;
		case DCELL_TYPE:
			ret = (double) data.d[col];
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0.0;
			break;
	}

	return ret;
}


void
r_set_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col, double val)
{
	switch(type)
	{
		case CELL_TYPE:
			data.c[col] = (CELL) val;
			break;
		case FCELL_TYPE:
			data.f[col] = (FCELL) val;
			break;
		case DCELL_TYPE:
			data.d[col] = (DCELL) val;
			break;
		default:
			G_warning("Illegal raster type\n");
			return;
			break;
	}

	return;
}


int
r_is_null_value2(RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col)
{
	int	ret;

	switch(type)
	{
		case CELL_TYPE:
			ret = G_is_c_null_value(&data.c[col]);
			break;
		case FCELL_TYPE:
			ret = G_is_f_null_value(&data.f[col]);
			break;
		case DCELL_TYPE:
			ret = G_is_d_null_value(&data.d[col]);
			break;
		default:
			G_warning("Illegal raster type\n");
			return 0;
			break;
	}

	return ret;
}


int
r_str_value2(char *str, int width, int prec,
		RASTER_MAP_TYPE type, RASTER_MAP_ROW data, int col)
{
	switch(type)
	{
		case CELL_TYPE:
			sprintf(str, "%*d", width, data.c[col]);
			break;
		case FCELL_TYPE:
			sprintf(str, "%*.*f", width, prec, data.f[col]);
			break;
		case DCELL_TYPE:
			sprintf(str, "%*.*lf", width, prec, data.d[col]);
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


void
r_copy_value2(RASTER_MAP_TYPE stype, RASTER_MAP_ROW sdata, int scol,
		RASTER_MAP_TYPE dtype, RASTER_MAP_ROW ddata, int dcol)
{
	switch(dtype)
	{
		case CELL_TYPE:
			switch(stype)
			{
				case CELL_TYPE:
					ddata.c[dcol] =
						(CELL) sdata.c[scol];
					break;
				case FCELL_TYPE:
					ddata.c[dcol] =
						(CELL) sdata.f[scol];
					break;
				case DCELL_TYPE:
					ddata.c[dcol] =
						(CELL) sdata.d[scol];
					break;
				default:
					G_warning("Illegal raster type\n");
					return;
					break;
			}
			break;
		case FCELL_TYPE:
			switch(stype)
			{
				case CELL_TYPE:
					ddata.f[dcol] =
						(FCELL) sdata.c[scol];
					break;
				case FCELL_TYPE:
					ddata.f[dcol] =
						(FCELL) sdata.f[scol];
					break;
				case DCELL_TYPE:
					ddata.f[dcol] =
						(FCELL) sdata.d[scol];
					break;
				default:
					G_warning("Illegal raster type\n");
					return;
					break;
			}
			break;
		case DCELL_TYPE:
			switch(stype)
			{
				case CELL_TYPE:
					ddata.d[dcol] =
						(DCELL) sdata.c[scol];
					break;
				case FCELL_TYPE:
					ddata.d[dcol] =
						(DCELL) sdata.f[scol];
					break;
				case DCELL_TYPE:
					ddata.d[dcol] =
						(DCELL) sdata.d[scol];
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

