/* %W% %G% */
#include "mapmask.h"
#define MIN_PROPORTION	.5
#define ONE_CELL	0
#define TWO_CELLS	1

static int rec_num = 0 ;

save_area()
{
    int i,start_row, stop_row ;
    int last_col, last_row ;
    int first_cell, last_cell ;
    char got_one ;
    int start_col, stop_col ;
    double cell_proportion ;
    int row1,row2;
    float col1,col2;
    char line[20];
    FILE *fopen(),*srcfile,*outfile,*dumpfile;

#ifdef DEBUG
    dumpfile = fopen("mapmask_dump","w");
#endif DEBUG

    if((srcfile = fopen(tmpname2,"r")) == NULL)
    {
	perror(tmpname2);
	exit(-1);
    }
    if((outfile = fopen(tmpname1,"w")) == NULL)
    {
	perror(tmpname2);
	exit(-1);
    }

    last_col = 0 ;
    last_row = 0 ;

    i = 0;
    while(1)
    {
	if(!((i++)%25)) fprintf(stderr," .");
	if(fgets(line,sizeof line,srcfile) == NULL) break;
	sscanf(line,"%d%f",&row1,&col1);
	start_row = row1 ;
	start_col = (int)col1 ;
	if(fgets(line,sizeof line,srcfile) == NULL) break;
	sscanf(line,"%d%f",&row2,&col2);
	stop_row   = row2 ;
	stop_col   = (int)col2 ;

	if (start_row != stop_row)
	{
	    fprintf(stderr,"start and end row not same, bye\n") ;
	    exit(-1) ;
	}

	/* If starting on a new row or column, then we're starting a new cell
	 * Hence, the running cell_proportion is reset to 0   */

	if (start_row != last_row)
	{
	    cell_proportion = 0.0 ;
	    last_row = start_row ;
	}
	else if(start_col != last_col)
	    cell_proportion = 0.0 ;

#ifdef DEBUG
fprintf(stderr,"%s",line);
fprintf(dumpfile,"s_r %d  l_r %d  s_c %d  e_c %d  l_c %d  c_p %6.2f\n",
start_row, last_row, start_col, stop_col, last_col, cell_proportion) ;
#endif DEBUG

	got_one = 0 ;

	switch (stop_col-start_col)
	{
	case ONE_CELL:
	    cell_proportion += col2 - col1 ;
	    if(cell_proportion >= MIN_PROPORTION)
	    {
		first_cell = start_col ;
		last_cell = start_col ;
		cell_proportion = 0.0 ;
		got_one = 1 ;
	    }
	    break ;

	case TWO_CELLS:
	    cell_proportion += (double)start_col + 1.0 - col1 ;
	    if(cell_proportion >= MIN_PROPORTION)
	    {
		first_cell = start_col ;
		got_one = 1 ;
	    }
	    else
		first_cell = start_col + 1 ;

	    cell_proportion = col1 - (double)stop_col ;
	    if(cell_proportion >= MIN_PROPORTION)
	    {
		last_cell = stop_col ;
		got_one = 1 ;
	    }
	    else
		last_cell = stop_col - 1 ;

	    break ;

	default:
	    cell_proportion += (double)start_col + 1.0 - col1 ;
	    if(cell_proportion >= MIN_PROPORTION)
		first_cell = start_col ;
	    else
		first_cell = start_col + 1 ;

	    cell_proportion = col1 - (double)stop_col ;
	    if(cell_proportion >= MIN_PROPORTION)
		last_cell = stop_col ;
	    else
		last_cell = stop_col - 1 ;

	    got_one = 1 ;

	    break ;
	}

	last_col = stop_col ;

	if (got_one)
	{
	    write_record(last_row, first_cell, last_cell, outfile) ;
	}
    }
    fclose(srcfile);
    fclose(outfile);
}
