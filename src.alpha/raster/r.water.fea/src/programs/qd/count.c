#include "gis.h"
#define INCRBY 10

void
count_cells_max(mapname,cellcounts)
char *mapname;
int *cellcounts;
{
	char *mapset;
	int row, col;
	int *count;
	int mindex;
	int fd1;
	CELL *basin_cell,basin_no,max_basin = -1;
	struct Cell_head window;
	
	/* initialize cellcounts */
		*cellcounts = 0;



	count = (int *) calloc(INCRBY , sizeof(int));
	if(count == NULL){
		fprintf(stderr,"Insufficient memory\n");
		exit(2);
	}
	mindex = INCRBY;

	G_get_set_window(&window);
	mapset = G_find_cell(mapname,"");
	fd1 = G_open_cell_old(mapname,mapset);
	if(fd1 == NULL){
		fprintf(stderr, "Map %s cannot be opened\n",mapname);
		exit(2);
	}
	basin_cell = G_allocate_cell_buf();

	for(row=0; row < window.rows; row++){
		G_get_map_row(fd1,basin_cell,row);
		for(col=0;col < window.cols; col++)
			if(basin_cell[col] != 0){
				basin_no = basin_cell[col];	

				if(basin_no > max_basin)
					max_basin = basin_no;

				if(max_basin > mindex){
					mindex += INCRBY;
					count = (int *) realloc(count, mindex * sizeof(int));
					if(count == NULL){
						fprintf(stderr,"Can not resize memory\n");
						exit(2);
					}
				}

				(count[basin_no])++;
			}
	}
	for(basin_no =1; basin_no <= max_basin; basin_no++){
			if(count[basin_no] > *cellcounts)
				*cellcounts = count[basin_no];
	}
}
