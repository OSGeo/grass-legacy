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
#include "libr.h"

#define	DEFAULT_DH	"0.5"


int
main(int argc, char **argv)
{
	char	buf[1024];
	char	*imap, *omap, *imapset;
	int	ifd, i, j, row, col, rows, cols, rbytes, nsinks;
	double	min, max, lmin, val, val2, dh, maxdh;
	RASTER_MAP	ibuf, obuf;

	struct	GModule	*module;
	struct
	{
		struct	Option	*input;
		struct	Option	*output;
		struct	Option	*dh;
		struct	Option	*maxdh;
	} params;

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

	imapset = G_find_cell(imap, "");
	if(!imapset){
		sprintf(buf, "\n** %s - not found **\n", imap);
		G_fatal_error (buf);
		exit(1);
	}

	if((ifd = G_open_cell_old(imap, imapset)) < 0){
		sprintf(buf, "\n** %s - could not read **\n", imap);
		G_fatal_error (buf);
		exit(1);
	}

	rows = G_window_rows();
	cols = G_window_cols();

	ibuf.type = G_raster_map_type(imap, imapset);
	ibuf.row  = (RASTER_MAP_ROW *)G_malloc(rows*sizeof(RASTER_MAP_ROW));
	obuf.type = ibuf.type;
	obuf.row  = (RASTER_MAP_ROW *)G_malloc(rows*sizeof(RASTER_MAP_ROW));

	rbytes = cols * G_raster_size(ibuf.type);

	for(row=0; row<rows; row++){
		ibuf.row[row].v = G_allocate_raster_buf(ibuf.type);
		if(G_get_raster_row(ifd, ibuf.row[row].v, row, ibuf.type) < 0){
			G_close_cell(ifd);
			sprintf(buf, "\n** %s - read failed **\n", imap);
			G_fatal_error (buf);
			exit(1);
		}
		obuf.row[row].v = G_allocate_raster_buf(ibuf.type);
		G_copy(obuf.row[row].v, ibuf.row[row].v, rbytes);
		for(col=0; col<cols; col++){
			val = r_get_value2(ibuf.type, ibuf.row[row], col);
			if((!row && !col) || min > val)
				min = val;
			if((!row && !col) || max < val)
				max = val;
		}
	}
	G_close_cell(ifd);

	do{
		nsinks = 0;
		for(row=0; row<rows; row++){
			for(col=0; col<cols; col++){
				lmin = max;
				for(i=-1; i<=1; i++){
					for(j=-1; j<=1; j++){
						if((!i && !j) ||
						    row+i < 0 ||
						    row+i > rows-1 ||
						    col+j < 0 ||
						    col+j > cols-1)
							continue;
						val = r_get_value2(obuf.type,
							obuf.row[row+i], col+j);
						if(lmin > val)
							lmin = val;
					}
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
	}while(nsinks);

	exit(0);
}

