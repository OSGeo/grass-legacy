/*
 * r.show: shows values of raster map of any raster type.
 *
 * $Id$
 *
 *	Copyright (C) 2001 by the GRASS Development Team
 *	Author: Huidae Cho <hdcho@geni.cemtlo.com>
 *		Cemtlomedia
 *		Taejon, South Korea
 *
 *	This program is free software under the GPL (>=v2)
 *	Read the file COPYING coming with GRASS for details.
 */

#include <stdio.h>
#include "gis.h"
#include "libr.h"


int
main(int argc, char **argv)
{
	struct	Option *opt;
	struct	Cell_head cellhd;
	char	*name, *mapset;
	int	fd, row, rows, col, cols;
	char	str[20];
	RASTER_ROW	buf;

	opt		 = G_define_option();
	opt->key         = "map";
	opt->description = "Map to manipulate";
	opt->type        = TYPE_STRING;
	opt->required    = YES;
	opt->gisprompt   = "old,cell,raster";

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

	buf.type  = G_raster_map_type(name, mapset);
	buf.row.v = G_allocate_raster_buf(buf.type);

	fprintf(stderr, "%s\n", r_type_name[buf.type]);

	if((fd = G_open_cell_old(name, mapset)) < 0){
		fprintf(stderr, "\n** %s - could not read **\n", name);
		exit(1);
	}

	for(row=0; row<rows; row++){
		G_percent(row, rows, 2);
		if(G_get_raster_row(fd, buf.row.v, row, buf.type) < 0){
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
		RASTER_ROW tmp;
		double	dval;

		fprintf(stderr, "\n*** Test ***\n");

		tmp.type   = buf.type;
		tmp.row.v = G_allocate_raster_buf(buf.type);

		dval = 123.322;
		fprintf(stderr, " buf[10] = (%s) %lf\n",
				r_type_name[buf.type], dval);

		r_set_value(buf, 10, dval);
		r_str_value(str, 10, 5, buf, 10);
		fprintf(stderr, " buf[10] = %s,", str);

		r_str_value(str, 10, 5, tmp, 2);
		fprintf(stderr, " tmp[2] = %s\n", str);

		fprintf(stderr, "\n copy buf[10] to tmp[2]\n");
		r_copy_value(buf, 10, tmp, 2);
		r_str_value(str, 0, 5, tmp, 2);
		fprintf(stderr, " tmp[2] = %s,", str);

		r_set_value(tmp, 2, 10*r_get_value(tmp, 2));
		r_str_value(str, 0, 5, tmp, 2);
		fprintf(stderr, " tmp[2]*10 = %s\n", str);
	}

	exit(0);
}

