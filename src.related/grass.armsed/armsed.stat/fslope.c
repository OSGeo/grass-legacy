/* %W% %G% */
#define EXTERN extern
#define fabs(x) ((x)<0?-(x):(x))

#include "stat.h"

FILE *
fslope(elev_fd)
{

    int row,col;

    float slopex,slopey,ftemp;

    CELL *elev_cell[3]; /* for holding 3 rows of elevation data */
    CELL *temp;
    CELL *e0,*e1,*e2;
    double H,V;              /* difference divisors */
    float *slope;            /* one row of slope calculations */
    float *sl;
    FILE *slope_fd;
    char *slope_name;
    double sqrt();


    H = window.ew_res * 2;  /* Horizontal (east-west) run */
    V = window.ns_res * 2;  /* Vertical (north-south) run */

    slope_name = G_tempfile();
    slope_fd = fopen(slope_name,"w");
    if (!slope_fd)
	{
		fprintf(stderr,"Error opening temporary slope file %s\n",slope_name);
		exit(2);
	}

    elev_cell[0] = G_allocate_cell_buf();
    elev_cell[1] = G_allocate_cell_buf();
    elev_cell[2] = G_allocate_cell_buf();

    if ( !(G_get_map_row(elev_fd,elev_cell[1],0)))
	{
		fprintf(stderr,"Error getting elevation data\n");
        exit(5);
	}

    if ( !(G_get_map_row(elev_fd,elev_cell[2],1)))
	{
		fprintf(stderr,"Error getting elevation data\n");
        exit(5);
	}

    slope = (float *)G_calloc(ncols,sizeof(float));

    sl = slope;

    for (col=0; col<ncols; col++)
        *sl++ = 0.;

    if (fwrite(slope,sizeof(float),ncols,slope_fd) != ncols)
    {
        fprintf(stderr,"error in writing slope file\n");
        exit(2);
    }

/**********************************************************************
 * calculate drainage direction and slope
 *********************************************************************/

    for (row = 1; row < nrows-1; row++)
    {

        temp = elev_cell[0];
        elev_cell[0] = elev_cell[1];
        elev_cell[1] = elev_cell[2];

        if (!(G_get_map_row(elev_fd,elev_cell[2]=temp,row+1)))
	    {
	    	fprintf(stderr,"Error getting elevation data\n");
            exit(5);
	    }

        e0 = elev_cell[0];
        e1 = elev_cell[1];
        e2 = elev_cell[2];

        sl = slope;
        *sl++ = 0;

        e0++; e1++; e2++;

        for(col = 1; col < ncols-1; col++) 
        {

            ftemp = (float)(*(e1+1) - *(e1-1))/H;
            slopex = fabs(ftemp);
            e1++;
            ftemp = (float)(*e2++ - *e0++)/V;
            slopey = fabs(ftemp);
            if (slopex==0. && slopey==0.)
                *sl++ = 0.;
            else
                *sl++ = sqrt((slopex * slopex) +
                   (slopey * slopey));

        }

        *sl = 0.;

        if (fwrite(slope,sizeof(float),ncols,slope_fd) != ncols)
        {
            fprintf(stderr,"error in writing slope file\n");
            exit(2);
        }

    }

    sl = slope;

    for (col=0; col<ncols; col++)
        *sl++ = 0.;

    if (fwrite(slope,sizeof(float),ncols,slope_fd) != ncols)
    {
        fprintf(stderr,"error in writing slope file\n");
        exit(2);
    }

    fclose(slope_fd);

    slope_fd = fopen(slope_name,"r");

    return slope_fd;
}
