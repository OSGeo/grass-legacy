#include "local_proto.h"


void
rdwr_gridatb()
{
	FILE	*fp;
	int	fd,i,j;
	float	idx;

	fp = fopen(file, "r");

	buf[0]=0;
	fscanf(fp, "%[^\n]", buf);
	if(!buf[0])
		getc(fp);
	
	fscanf(fp, "%d %d %lf\n", &cellhd.cols, &cellhd.rows, &cellhd.ns_res);
	cellhd.ew_res = cellhd.ns_res;
	cellhd.south  = 0;
	cellhd.north  = cellhd.south + cellhd.ns_res * cellhd.rows;
	cellhd.west   = 0;
	cellhd.east   = cellhd.west + cellhd.ew_res * cellhd.cols;
	cellhd.format = -1;
	cellhd.compressed = 1;

	if(adjcellhd(&cellhd)){
		fclose(fp);
		exit(1);
	}

	fd = G_open_raster_new(oname, FCELL_TYPE);

	cell = (FCELL *) malloc(sizeof(FCELL)*cellhd.cols);

	for(i=0; i<cellhd.rows; i++){
		G_percent(i,cellhd.rows,2);

		for(j=0; j<cellhd.cols; j++){
			idx=9999.0;
			fscanf(fp, "%f", &idx);
			if(idx>=9999.0){
				G_set_f_null_value(&(cell[j]),1);
			}else{
				cell[j]=idx;
			}
		}
		G_put_f_raster_row(fd,cell);
	}
	G_percent(i,cellhd.rows,2);
	fclose(fp);
	G_close_cell(fd);

	G_put_cell_title(oname, buf);
	G_put_cellhd(oname, &cellhd);

	return;
}

