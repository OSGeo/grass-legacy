/* %W% %G% */
#include "mapmask.h"

center_fill(begin,pts,array,x_center,y_center)
    int begin,pts,x_center,y_center,*array;
{
    int h,i,j,k,l,p,row;
    int add,start,stop;
    long offset;


    if((i = begin) == 0)
	add = 1;
    else
	add = -1;

/* dealing with case of circle outside of window  */
    p = pts;
    if(i != 0)
    {
	for(l = 1; 0 > (y_center - p--); l++)
		;
	i -= l - 1;
	row = y_center - (++p);
    }
    else 
    {
	for(l = 1; window.rows <= (y_center + p--); l++)
		;
	row = y_center;
    }

    if(pts > 1000) 
    {
	fprintf(stderr,"Circle cannot be defined\n");
	exit(-1);
    }

    for(j = l; j <= pts; j++)
    {
	start = x_center - *(array + i);
	stop  = x_center + *(array + i);

	for(k = start; k < stop; k++)
		cellbuf[k] = 1;

	G_put_map_row_random(cellfd,cellbuf+start,row,start,stop-start);
	fprintf(stderr,".");
	i += add;
	row++;
    }
}

topbtm_fill(x,y,start,num,x_center,y_center,r)
    int *x,*y;
    int x_center,y_center,start,num,r;
{
    int h,i,j,k,adj,begin,end,row;
    int from,to;
    long offset;

    if(start == 0)
    {
	adj  = 1;
	row  = y_center - r;
	from = row;
	to   = y_center - num;
    }
    else
    {
	adj  = -1;
	row  = y_center + num;
	from = row;
	to   = y_center + r;
    }


    for(i = from; i < to; i++)
    {
	j = 0;
	while(*(x+start+j+adj) == *(x+start)) j += adj;
	if(j >= 0) start += j;
	begin = x_center - *(y+start);
	end   = x_center + 1 + *(y+start);

/* circle outside of window */
	if((adj == 1 && start >= r - y_center) ||
	   (adj != 1 && start >= r + y_center - window.rows))
	{
	    for(k = begin; k < end; k++)
		    cellbuf[k] = 1;

	    G_put_map_row_random(cellfd,cellbuf+begin,row,begin,end-begin);

	    fprintf(stderr,".");
	    row++;
	}

	if(j >= 0) start += adj;
	else       start += j + adj;
    }
}
