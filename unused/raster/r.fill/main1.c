/*
 * r.fill: creates a depressionless dem.
 * 	       Based on SINK.FOR.
 *
 * SINK.FOR Author: Keith Beven <k.beven@lancaster.ac.uk>
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
#include <stdlib.h>
#include "gis.h"
#include "libtir.h"

#define	DEFAULT_DH	"0.5"


int
main(int argc, char **argv)
{
	char	buf[1024];
	char	*imap, *omap, *imapset, overwr, verbose;
	int	fd, i, j, k, row, col, rows, cols, rbytes, nsinks;
	double	min, max, lmin, val, val2, dh, maxdh;
	RASTER_ROW2	ibuf, obuf;

	struct	GModule	*module;
	struct
	{
		struct	Option	*input;
		struct	Option	*output;
		struct	Option	*dh;
		struct	Option	*maxdh;
	} params;
	struct
	{
		struct	Flag	*overwr;
		struct	Flag	*verbose;
	} flags;

	module = G_define_module();
	module->description = "Creates a depressionless dem.";

	params.input			= G_define_option();
	params.input->key		= "input";
	params.input->description	= "Elevation map";
	params.input->type		= TYPE_STRING;
	params.input->required		= YES;
	params.input->gisprompt		= "old,cell,raster";

	params.output			= G_define_option();
	params.output->key		= "output";
	params.output->description	= "Depressionless map";
	params.output->type		= TYPE_STRING;
	params.output->required		= YES;
	params.output->gisprompt	= "new,cell,raster";

	params.dh			= G_define_option();
	params.dh->key			= "dh";
	params.dh->description		= "Increment of h";
	params.dh->type			= TYPE_DOUBLE;
	params.dh->required		= NO;
	params.dh->answer		= DEFAULT_DH;

	params.maxdh			= G_define_option();
	params.maxdh->key		= "maxdh";
	params.maxdh->description	= "Maximum change of h";
	params.maxdh->type		= TYPE_DOUBLE;
	params.maxdh->required		= NO;
	params.maxdh->answer		= "up to the lowest neighbor";

	flags.overwr			= G_define_flag();
	flags.overwr->key		= 'o';
	flags.overwr->description	= "Overwrite output map";

	flags.verbose			= G_define_flag();
	flags.verbose->key		= 'v';
	flags.verbose->description	= "Output verbosely";


	G_gisinit(argv[0]);

	if (G_parser(argc, argv))
		exit(-1);

	imap = params.input->answer;
	omap = params.output->answer;
	dh = atof(params.dh->answer);
	if(dh <= 0.0)
		dh = atof(DEFAULT_DH);
	maxdh = atof(params.maxdh->answer);
	if(maxdh <= 0.0)
		maxdh = 0.0;
	else
	if(maxdh < dh)
		maxdh = dh;
	overwr  = flags.overwr->answer;
	verbose = flags.verbose->answer;

	imapset = G_find_cell(imap, "");
	if(!imapset){
		sprintf(buf, "\n** %s - not found **\n", imap);
		G_fatal_error (buf);
		exit(1);
	}

	if((fd = G_open_cell_old(imap, imapset)) < 0){
		sprintf(buf, "\n** %s - could not read **\n", imap);
		G_fatal_error (buf);
		exit(1);
	}

	if(G_find_file("cell", omap, G_mapset())){
		if(overwr)
			overwr = 2;
		else{
			sprintf(buf, "\n** %s - already exists **\n", omap);
			G_fatal_error (buf);
			exit(1);
		}
	}

	rows = G_window_rows();
	cols = G_window_cols();

	ibuf.type = G_raster_map_type(imap, imapset);
	obuf.type = ibuf.type;
	ibuf.row  = (RASTER_MAP_ROW *)G_malloc(rows*sizeof(RASTER_MAP_ROW));
	obuf.row  = (RASTER_MAP_ROW *)G_malloc(rows*sizeof(RASTER_MAP_ROW));

	rbytes = cols * G_raster_size(ibuf.type);

	if(verbose)
		fprintf(stderr, "Reading map...\n");

	i = 0;
	for(row=0; row<rows; row++){
		ibuf.row[row].v = G_allocate_raster_buf(ibuf.type);
		if(G_get_raster_row(fd, ibuf.row[row].v, row, ibuf.type) < 0){
			G_close_cell(fd);
			sprintf(buf, "\n** %s - read failed **\n", imap);
			G_fatal_error (buf);
			exit(1);
		}
		obuf.row[row].v = G_allocate_raster_buf(ibuf.type);
		G_copy(obuf.row[row].v, ibuf.row[row].v, rbytes);
		for(col=0; col<cols; col++){
			if(r_is_null_value2(ibuf.type, ibuf.row[row], col))
				continue;
			val = r_get_value2(ibuf.type, ibuf.row[row], col);
			if(!i || min > val)
				min = val;
			if(!i || max < val)
				max = val;
			if(!i)
				i = 1;
		}
	}
	G_close_cell(fd);

	if(verbose)
		fprintf(stderr, "Filling sinks...\n");
	do{
		nsinks = 0;
		for(row=0; row<rows; row++){
			if(verbose)
				G_percent(row, rows, 2);
			for(col=0; col<cols; col++){
				lmin = max;
				k = 0;
				for(i=-1; i<=1; i++){
					for(j=-1; j<=1; j++){
						if((!i && !j) ||
						    row+i < 0 ||
						    row+i > rows-1 ||
						    col+j < 0 ||
						    col+j > cols-1 ||
						    r_is_null_value2(obuf.type,
							obuf.row[row+i], col+j))
							continue;
						k++;
						val = r_get_value2(obuf.type,
							obuf.row[row+i], col+j);
						if(lmin > val)
							lmin = val;
					}
				}
				if(r_is_null_value2(obuf.type,
						obuf.row[row], col)){
					if(k == 8)
						r_set_value2(obuf.type,
							obuf.row[row], col,
							lmin);
					continue;
				}
				val = r_get_value2(obuf.type,
						obuf.row[row], col);
				if(lmin > val){
					val2 = r_get_value2(ibuf.type,
							ibuf.row[row], col);
					if(maxdh == 0.0)
						val = lmin;
					else{
						val += dh;
						j = 0;
						if(val > lmin){
							val = lmin;
							j = 1;
						}
						if(val-val2 > maxdh){
							val = val2 + maxdh;
							j = 1;
						}
						if(!j)
							nsinks++;
					}
					r_set_value2(obuf.type, obuf.row[row],
							col, val);
				}
			}
		}
		if(verbose){
			G_percent(row, rows, 2);
			fprintf(stderr, "Number of sinks: %d\n", nsinks);
		}
	}while(nsinks);

	if(overwr == 2)
		G_remove("cell", omap);

	if((fd = G_open_raster_new(omap, obuf.type)) < 0){
		sprintf(buf, "\n** %s - could not write **\n", omap);
		G_fatal_error (buf);
		exit(1);
	}

	if(verbose)
		fprintf(stderr, "Writing output map...\n");
	for(row=0; row<rows; row++){
		if(verbose)
			G_percent(row, rows, 2);
		G_put_raster_row(fd, obuf.row[row].v, obuf.type);
	}
	if(verbose)
		G_percent(row, rows, 2);
	G_close_cell(fd);

	exit(0);
}

