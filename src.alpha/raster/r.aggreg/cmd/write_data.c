#include "gis.h"
#include "tools.h"

extern int G_set_window(struct Cell_head*);
extern int G_close_cell(int);
extern int G_open_cell_new(char*);
extern int G_put_map_row(int,CELL*);
extern int G_adjust_Cell_head(struct Cell_head*,int,int);

void write_data_ ( char *out_name, int* matrix, int* maxcol, int *maxrow)
{
	int row, col;
	int output_fd;	
    	CELL *buf;

	char command[100];

	if ( (output_fd = G_open_cell_new( out_name )) < 0)
	{
			fprintf(stderr,"Unexpected error\n");
		exit(-1);
	}

   	buf = G_allocate_cell_buf();

 	for (row = 1; row <= *maxrow; row++) 
		{
 		for (col = 1; col <= *maxcol; col++)

			buf[col-1] = matrix[col+row*(*maxcol+2)];

		if (G_put_map_row(output_fd, buf) == -1) 
			TERMINATE("Unexpected G_put_map_row error !");


		}
	G_close_cell(output_fd);
	free(buf);

	/* Set random color table for Output map */
	
	sprintf (command, "r.colors map=%s color=random", out_name);
	system (command);
	
}
