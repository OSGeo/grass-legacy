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
#include "libtir.h"


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

	fprintf(stderr, "%s\n", G_type_name[buf.type]);

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
			if(G_is_null_r(buf, col)){
				printf("NULL ");
			}else{
				G_str_r(buf, col, str, 15, 5);
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
				G_type_name[buf.type], dval);

		G_set_r(buf, 10, dval);
		G_str_r(buf, 10, str, 10, 5);
		fprintf(stderr, " buf[10] = %s,", str);

		G_str_r(tmp, 2, str, 10, 5);
		fprintf(stderr, " tmp[2] = %s\n", str);

		fprintf(stderr, "\n copy buf[10] to tmp[2]\n");
		G_set_r(tmp, 2, G_get_r(buf, 10));
		G_str_r(tmp, 2, str, 0, 5);
		fprintf(stderr, " tmp[2] = %s,", str);

		G_set_r(tmp, 2, 10*G_get_r(tmp, 2));
		G_str_r(tmp, 2, str, 0, 5);
		fprintf(stderr, " tmp[2]*10 = %s\n", str);
	}

	exit(0);
}

