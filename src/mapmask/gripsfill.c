/* %W% %G% */
#include "mapmask.h"
#define COL_SHIFT   0
#define ROW_SHIFT  -1

char *gets() ;

#define MAXLINE  90
#define FGET     fgets(buffer,MAXLINE,infile)
#define READLINE if (FGET==NULL){\
			fclose(infile);\
			return;\
		    }\
		    sscanf (buffer,"%d%d%d", &cur_row, &col_b, &col_e) 


gripsfill(num_verticies)
    int num_verticies;
{
    int h,i,atrow,cur_row,atcol,col_b,col_e;
    char buffer[MAXLINE];
    FILE *infile,*fopen();
    register CELL *c;

    *(Ux+num_verticies) = *Ux;
    *(Uy+num_verticies) = *Uy;
    UTMtoARAY(num_verticies);
    set_limits(window.rows,window.cols);
    find_area(num_verticies);
    save_area();

    if((infile = fopen(tmpname1,"r")) == NULL)
    {
	perror(tmpname1);
	exit(-1);
    }

    READLINE ;
    atrow = cur_row;

    fprintf(stderr,"\n\nFilling Data Layer\n\n");
    h = 1;
    while(1)
    {
	fprintf(stderr,".");
	h++;

	do
	{
	    c = cellbuf;
	    for(atcol=col_b; atcol<=col_e; atcol++)
		    *c++ = 1;
	    G_put_map_row_random (cellfd, cellbuf, atrow, col_b, col_e-col_b+1);
	    READLINE ;
	} while (cur_row == atrow) ;

	atrow = cur_row;
    }
}
