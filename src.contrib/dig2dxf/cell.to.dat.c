/********************************************************************
	cell.to.dat is a rinky-dink program that will convert
	GRASS cell files into Landcadd's (third-party AutoCad)
	DAT format.  Because the DAT format does not include
	the cell resolution, cell.to.dat will send that info
	to the screen to be recorded manually.
	This program makes no pretense that it shows good (or
	even mediocre) GRASS programing style.
	Written by Chuck Ehlschlaeger. March 1989
********************************************************************/
#include "gis.h"

CELL			   *cell_row;
FILE			   *fpdat;
int			   elev_fd;
char			   *ele_mapset, ele_name[40];
char			   dat_name[44];
static struct Cell_head    window;

main(argc,argv)
int argc;
char *argv[];
{
    int row, col, nrows, ncols, ele;
    char *G_malloc();
    double res;

    init_vars(argc,argv);
    nrows = G_window_rows();
    ncols = G_window_cols();
    res = window.ns_res * 3.281 * 12;
    fprintf(stderr,"\nresolution of grid is %lf inches.\n", res);
    cell_row = (CELL *)G_malloc(ncols * sizeof(CELL));
    fprintf(fpdat,"%03d %03d 7\n", nrows, ncols);
    for(row=0; row<nrows; row++)
    {
	G_get_map_row(elev_fd, cell_row, row);
	for(col=0; col<ncols; col++)
	{
	    ele = cell_row[col] * 12 * 3.281;
	    fprintf(fpdat,"%07d", ele);
	}
	fprintf(fpdat,"\n");
    }
    fclose(fpdat);
    G_close_cell(elev_fd);
}

init_vars(argc,argv)
int argc;
char *argv[];
{
    char buf[100];

    G_gisinit(argv[0]);
    if(argc != 2 || sscanf(argv[1],"e=%[^\n]",ele_name) != 1)
    {
	sprintf(buf,"\ncommand line format: cell.to.dat e=elevation_file\n");
	G_fatal_error(buf);
	exit(1);
    }
    ele_mapset = G_find_file2("cell", ele_name, "");
    if(!ele_mapset)
    {
	sprintf(buf,"\nelevation file [%s] not found\n", ele_name);
	G_fatal_error(buf);
	exit(1);
    }
    if((elev_fd = G_open_cell_old(ele_name,ele_mapset)) < 0)
    {
	sprintf(buf,"\nelevation file cannot be opened\n");
	G_fatal_error(buf);
	exit(1);
    }
    strcpy(dat_name,ele_name);
    strcat(dat_name,".dat");
    fpdat = fopen(dat_name,"w");
    G_get_set_window(&window);
}
