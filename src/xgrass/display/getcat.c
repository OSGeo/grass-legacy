#include "gis.h"



int getcatnum(dithindic,x,y,lx,ty,rx,by,n,m)
int x,y;
int lx,ty,rx,by;
char *n, *m;
{
CELL	*buf;
int		fd;
int 	nrows, ncols;
int		cx, ry;

	nrows = G_window_rows();
	ncols = G_window_cols();


    cx  = (int)((double)((x-lx)*ncols)/(double)(rx-lx));
	ry  = (int)((double)((y-ty)*nrows)/(double)(by-ty));

    if (dithindic) return(cx);


	fd	= G_open_cell_old(n,m);

     if (fd <0)
	{
	char    tmp[1024];
	sprintf (tmp, "%s in %s - can't open cell file", n, m);
	G_fatal_error(tmp); }

     buf=G_allocate_cell_buf();
	if (G_get_map_row(fd,buf,ry)<0) {
	char    tmp[1024];
	sprintf (tmp, "%s in %s - can't get cell row %d ", n,m,y);
	G_fatal_error(tmp); }

	G_close_cell(fd);
	return (buf[cx]);

}

