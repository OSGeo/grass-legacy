/* %W% %G% */
#include "mapmask.h"

find_area(num_verticies)
    int num_verticies ;
{
    int node ;
    int row ;
    int incr ;
    register double A, B ;
    double delta_x, delta_y ;
    int first_row, last_row ;
    FILE *fopen(),*dumpfile;
    char command[256];

    if((dumpfile = fopen(tmpname1,"w")) == NULL)
    {
	perror(tmpname1);
	exit(-1);
    }

/* adjust Y grid coordinates to Y array coordinates */
    yadjust(num_verticies) ;

    fprintf(stderr,"Defining Polygon ");
    for (node=0; node<num_verticies; node++)
    {
	fprintf(stderr," .");
#ifdef DEBUG
fprintf(stderr,"(x,y) %.2f:%.2f %.2f:%.2f  ",
	*(Ax+node), *(Ay+node), *(Ax+node+1), *(Ay+node+1)) ;
#endif DEBUG
/*  generate equation  */
	delta_y = *(Ay+node+1) - *(Ay+node);
	delta_x = *(Ax+node+1) - *(Ax+node);
	if (delta_y != 0.0)
	{
	    B = delta_x / delta_y ;
	    A = *(Ax+node) - B * *(Ay+node)  ;
	}
#ifdef DEBUG
fprintf(stderr,"A = %f  B = %f\n", A, B) ;
#endif DEBUG

/*  determine first and last row involved */
	if (*(Ay+node+1) > *(Ay+node))
	{
	    if (*(Ay+node) > 0.0)
		first_row = *(Ay+node) + 1. ;
	    else
		first_row = *(Ay+node) ;

	    if (*(Ay+node+1) > 0.0)
		last_row = *(Ay+node+1) ;
	    else
		last_row = *(Ay+node+1) - 1. ;
	}
	else if (*(Ay+node+1) < *(Ay+node))
	{
	    if (*(Ay+node+1) > 0.0)
		first_row = *(Ay+node+1) + 1. ;
	    else
		first_row = *(Ay+node+1) ;

	    if (*(Ay+node) > 0.0)
		last_row = *(Ay+node) ;
	    else
		last_row = *(Ay+node) - 1. ;
	}

#ifdef DEBUG
fprintf(stderr,"first: %6d  last: %6d\n", first_row, last_row) ;
#endif DEBUG

	if (first_row > last_row)
	    continue ;

	if (delta_y == 0.0)
	    continue ;

	for (row=first_row; row<=last_row; row++)
	{
	    fprintf(dumpfile,"%d %.2f\n",row,(A+B*row));
	}
    }
    fprintf(stderr," .");

    fclose(dumpfile);
    sprintf(command,"sort +0n +1n %s > %s",tmpname1,tmpname2);
    system(command);

#ifdef DEBUG
sprintf(command,"more %s",tmpname2);
system(command);
#endif DEBUG
}
