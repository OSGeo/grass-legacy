#include "local_proto.h"


void
getcells(void)
{
	int	fd,i,j;
	RASTER_MAP_TYPE	data_type;


	data_type = G_raster_map_type(iname,mapset);

	if((fd = G_open_cell_old(iname,mapset)) < 0){
		fprintf(stderr,"\n* %s - could not read *\n",iname);
		exit(1);
	}

	if(data_type == CELL_TYPE)
		ccell = (CELL *)malloc(sizeof(CELL)*cellhd.cols);

	cell = (FCELL **)malloc(sizeof(FCELL *)*cellhd.rows);
	atb  = (FCELL **)malloc(sizeof(FCELL *)*cellhd.rows);
	a    = (FCELL **)malloc(sizeof(FCELL *)*cellhd.rows);

	fprintf(stderr,"Reading elevation map:");
	for(i=0;i<cellhd.rows;i++){
		G_percent(i,cellhd.rows,2);

		cell[i] = (FCELL *)malloc(sizeof(FCELL)*cellhd.cols);
		atb[i]  = (FCELL *)malloc(sizeof(FCELL)*cellhd.cols);
		a[i]    = (FCELL *)malloc(sizeof(FCELL)*cellhd.cols);

		if(data_type == CELL_TYPE){
			if(G_get_c_raster_row(fd,ccell,i) < 0){
				G_close_cell(fd);
			}
			for(j=0;j<cellhd.cols;j++){
				if(G_is_c_null_value(&ccell[j]))
					G_set_f_null_value(&cell[i][j],1);
				else
					cell[i][j] = (float) ccell[j];
			}
		}else if(G_get_f_raster_row(fd,cell[i],i) < 0){
			G_close_cell(fd);
			exit(1);
		}
	}
	if(data_type == CELL_TYPE)
		free(ccell);
	G_percent(i,cellhd.rows,2);
	G_close_cell(fd);
}


void
putcells(void)
{
	int	fd,i;

	if((fd = G_open_raster_new(oname,FCELL_TYPE)) < 0){
		fprintf(stderr,"\n* %s - could not write *\n",oname);
		exit(1);
	}

	fprintf(stderr,"Writing topographic index map:");
	for(i=0;i<cellhd.rows;i++){
		G_percent(i,cellhd.rows,2);
		G_put_f_raster_row(fd,atb[i]);
	}
	G_percent(i,cellhd.rows,2);
	G_close_cell(fd);
}

