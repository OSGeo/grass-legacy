#include "local_proto.h"


void
rdwr_gridatb()
{
	FILE	*fp;
	int	fd,row,col;
	float	idx;
	CELL	*cell;
	DCELL	*dcell;
	FCELL	*fcell;
	RASTER_MAP_TYPE		data_type;

	G_get_cellhd(iname, mapset, &cellhd);
	if(adjcellhd(&cellhd)){
		exit(1);
	}

	fp = fopen(file, "w");

	fprintf(fp, "%s\n", G_get_cell_title(iname, mapset));
	fprintf(fp, "%d %d %lf\n", cellhd.cols, cellhd.rows, cellhd.ns_res);

	data_type = G_raster_map_type(iname,mapset);
	switch(data_type){
		case CELL_TYPE:
			cell = G_allocate_c_raster_buf();
			fprintf(stderr,"CELL\n");
			break;
		case FCELL_TYPE:
			fcell = G_allocate_f_raster_buf();
			fprintf(stderr,"FCELL\n");
			break;
		case DCELL_TYPE:
			dcell = G_allocate_d_raster_buf();
			fprintf(stderr,"DCELL\n");
			break;
	}

	if((fd = G_open_cell_old(iname,mapset)) < 0){
		fprintf(stderr,"\n** %s - could not read **\n",iname);
		exit(1);
	}

	for(row=0;row<cellhd.rows;row++){
		G_percent(row,cellhd.rows,2);
		switch(data_type){
			case CELL_TYPE:
				if(G_get_c_raster_row(fd,cell,row) < 0){
					G_close_cell(fd);
					exit(1);
				}

				for(col=0;col<cellhd.cols;col++){
					if(G_is_c_null_value(&cell[col]))
						fprintf(fp, "  9999.00 ");
					else
						fprintf(fp, "%9.2f ",
							(float) cell[col]);
					if(!((col+1)%8) || col==cellhd.cols-1)
						fprintf(fp, "\n");
				}
				break;
			case FCELL_TYPE:
				if(G_get_f_raster_row(fd,fcell,row) < 0){
					G_close_cell(fd);
					exit(1);
				}

				for(col=0;col<cellhd.cols;col++){
					if(G_is_f_null_value(&fcell[col]))
						fprintf(fp, "  9999.00 ");
					else
						fprintf(fp, "%9.2f ",
							(float) fcell[col]);
					if(!((col+1)%8) || col==cellhd.cols-1)
						fprintf(fp, "\n");
				}
				break;
			case DCELL_TYPE:
				if(G_get_d_raster_row(fd,dcell,row) < 0){
					G_close_cell(fd);
					exit(1);
				}

				for(col=0;col<cellhd.cols;col++){
					if(G_is_d_null_value(&dcell[col]))
						fprintf(fp, "  9999.00 ");
					else
						fprintf(fp, "%9.2lf ",
							(double) dcell[col]);
					if(!((col+1)%8) || col==cellhd.cols-1)
						fprintf(fp, "\n");
				}
				break;
		}
	}
	G_close_cell(fd);

	fprintf(stderr,"\n%d rows, %d cols\n",cellhd.rows,cellhd.cols);

	return;
}

