/*
generate curve of a Zero Crossings boundary
Hong Chun Zhuang
Sep.21, 1992
at EC, CERL
based on i.zc codes
*/
#include <math.h>
#include "gis.h"
#include "globals.h"
#define POINTS 1000
#define PI 3.1415926536


int zc_curve (int inputfd, int zcfd, double Width,
	double ds[POINTS], double dalpha[POINTS],
	int *sout, double *totals)
{
        int imagfd; /* the input and output file descriptors */
        char *inmapset; /* the input mapset name */
        struct Cell_head window;
        CELL *cell_row, *cell_row2;
        int result, yks, xks;
        double max, min, scale, ltotal;
	int k, kk, yk0, xk0, pi, pj, s, kscan, scan, scani[20], scanj[20];
	double temp, x0, y0, x[POINTS], y[POINTS], ls[POINTS];
	double alpha, valpha[POINTS];

        long max_pow2(); /* Used to find the smallest power of 2 >= to a number
   */
        long min_pow2(); /* Used to find the largest power of 2 >= to a number
   */
        int i,j;         /* Loop control variables */
        int or,oc;      /* Original dimensions of image */
        int rows,cols;  /* Smallest powers of 2 >= number of rows & columns */
        int size;      /* the length of one side */
        long totsize; /* the Total number of data points */
        double *data[2]; /* Data structure containing real & complex values of FFT */
        int save_args(); /* function to stash the command line arguments */

        /* get the current window for later */
        G_get_set_window(&window);

        /* get the rows and columns in the current window */
        or = G_window_rows();
        oc = G_window_cols();
        rows = max_pow2(or);
        cols = max_pow2(oc);
        size = (rows>cols) ? rows : cols;
        totsize = size * size;

        fprintf(stderr,"Power 2 values : %d rows %d columns\n",rows,cols);

        /* Allocate appropriate memory for the structure containing
     the real and complex components of the FFT.  DATA[0] will
     contain the real, and DATA[1] the complex component.
     */
        data[0] = (double *) G_malloc(totsize*sizeof(double));
        data[1] = (double *) G_malloc(totsize*sizeof(double));
        if (data[0] == NULL || data[1] == NULL)
        G_fatal_error("Insufficent memory for allocation of data structure");

        /* Initialize real & complex components to zero */
        fprintf (stdout,"Initializing data...\n");
        for (i=0; i<(totsize); i++) 
	{
                *(data[0]+i) = 0.0;
                *(data[1]+i) = 0.0;
        }

        /* allocate the space for one row of cell map data */
        cell_row = G_allocate_cell_buf();

        /* Read in cell map values */
        fprintf (stdout,"Reading the raster map...\n");
        for (i=0; i<or; i++) 
	{
                if (G_get_map_row(inputfd, cell_row, i)<0)
                        G_fatal_error("Error while reading input raster map.");
                for (j=0; j<oc; j++)
                        *(data[0]+(i*size)+j) = (double) cell_row[j];
        }

        /* take the del**2g of image */
        del2g(data, size, Width);

        	/* find the zero crossings:  Here are several notes -
     			1) this routine only uses the real values
     			2) it places the zero crossings in the imaginary array 
		*/

        findzc(data[0], size, data[1], Thresh);

        cell_row = G_allocate_cell_buf();

        /* Write out result to a new cell map */
        for (i=0; i<or; i++) {
                for (j=0; j<oc; j++) {
                        *(cell_row+j) = (CELL) (*(data[1]+i*cols+j));
                }
                G_put_map_row(zcfd, cell_row);
        }
        G_close_cell(zcfd);

        G_free(cell_row);

        fprintf (stdout,"Transform successful\n");
				/*
-------------------------------------------------------------------
				*/



				k=0;
	fprintf (stdout,"zero crossing cell file ... \n");
			/* print cell map file for check 
	for (i=0; i<or; i++)
		{
		 fprintf (stdout,"for row %d\n",i);
		 for (j=0; j<oc; j++)
			{
		temp = *(data[1]+i*cols+j);
                 fprintf (stdout,"%f, ", temp);
			}
		}
			*/

			/* scan to find out the first point of curve 
			for boundary, s=0 */
	for (i=0; i<or; i++)
		{
		 fprintf (stdout,"for row %d\n",i);
		 for (j=0; j<oc; j++)
			{
		temp = *(data[1]+i*cols+j);
		if(temp > 0.5) 
				{
				yk0= or-i-1;
				xk0= j;
				y[0]= (double) yk0;
				x[0]= (double) xk0;
/*
		fprintf (stdout,"vector x0 = %f, y0 = %f\n", x[0], y[0]);
*/
				*(data[1]+i*cols+j) = 0.0;
				k=k++;
				ds[0]=0.0;
				}
			if(k !=0)break;
			}
		if(k !=0)break;
		}

			/*
			begin from the first point 
			to search for next neighbouring point of the boundary
			*/
pi = or - (yk0+1);
pj = xk0;
s=0;
scancycle:
/*-------------------------*/
kscan=2;
scani[0]=pi-1;
scanj[0]=pj-1;
scani[1]=pi-1;
scanj[1]=pj;
scani[2]=pi-1;
scanj[2]=pj+1;
scani[3]=pi;
scanj[3]=pj+1;
scani[4]=pi+1;
scanj[4]=pj+1;
scani[5]=pi+1;
scanj[5]=pj;
scani[6]=pi+1;
scanj[6]=pj-1;
scani[7]=pi;
scanj[7]=pj-1;
for(scan=0; scan<8; scan++)
{
if( *(data[1]+scani[scan]*cols+scanj[scan]) > 0.5 )  	
							/* 
							it means 
							fond another point
							in the boundary
							*/
	{
	s=s+1;
        yks=or - scani[scan] - 1;
        xks=scanj[scan];
	y[s]=(double) yks;
	x[s]=(double) xks;
/*
fprintf (stdout,"points=%d, x=%f, y=%f\n", s, x[s], y[s]);
*/
	*(data[1]+scani[scan]*cols+scanj[scan]) = 0;
	kscan=0;
	break;
	}
}
if(kscan == 2)
{
scani[0]=pi-1;
scanj[0]=pj-2;
scani[1]=pi-2;
scanj[1]=pj-2;
scani[2]=pi-2;
scanj[2]=pj-1;
scani[3]=pi-2;
scanj[3]=pj;
scani[4]=pi-2;
scanj[4]=pj+1;
scani[5]=pi-2;
scanj[5]=pj+2;
scani[6]=pi-1;
scanj[6]=pj+2;
scani[7]=pi;
scanj[7]=pj+2;
scani[8]=pi+1;
scanj[8]=pj+2;
scani[9]=pi+2;
scanj[9]=pj+2;
scani[10]=pi+2;
scanj[10]=pj+1;
scani[11]=pi+2;
scanj[11]=pj;
scani[12]=pi+2;
scanj[12]=pj-1;
scani[13]=pi+2;
scanj[13]=pj-2;
scani[14]=pi+1;
scanj[14]=pj-2;
scani[15]=pi;
scanj[15]=pj-2;
for(scan=0; scan<16; scan++)
{
if( *(data[1]+scani[scan]*cols+scanj[scan]) > 0.5 )
							/* 
							it means 
							fond another point
							in the boundary
							*/
        {
	s=s+1;
        yks=or - scani[scan] - 1;
        xks=scanj[scan];
        y[s]=(double) yks;
        x[s]=(double) xks;
fprintf (stdout,"for larger scan, points=%d, x=%f, y=%f\n", s, x[s], y[s]);
        *(data[1]+scani[scan]*cols+scanj[scan]) = 0;
        kscan=0;
        break;
        }
}
}
if(kscan == 0)		/* if fond one boundary point 
			   then register ls[] and valpha[] as parameters of 
			   the previous point
			*/
{
ls[s-1]=sqrt( (x[s]-x[s-1])*(x[s]-x[s-1])+(y[s]-y[s-1])*(y[s]-y[s-1]) );
alpha=asin((y[s]-y[s-1])/ls[s-1]);
if((x[s]-x[s-1]) < 0.0) alpha=PI - alpha;
if(alpha < 0.0)alpha=TWOPI + alpha;
ltotal=ls[s-1]+ltotal;
ds[s]=ltotal;		
			/*ds is as independent value of curvature function
			but ls is for derivative operation of alpha */
valpha[s-1]=alpha;
fprintf (stdout,"s=%d, ls=%f,ltotal=%f, valpha=%f\n",s-1,ls[s-1],ltotal,valpha[s-1]);
}
if(kscan == 2)
{
	s=s+1;
        yks=yk0;
        xks=xk0;
        y[s]=(double) yks;
        x[s]=(double) xks;
	ls[s-1]=sqrt( (x[s]-x[s-1])*(x[s]-x[s-1])+(y[s]-y[s-1])*(y[s]-y[s-1]) );
	alpha=asin((y[s]-y[s-1])/ls[s-1]);
	if((x[s]-x[s-1]) < 0.0) alpha=PI - alpha;
	if(alpha < 0.0)alpha=TWOPI + alpha;
	ltotal=ls[s-1]+ltotal;
	ds[s]=ltotal;		
	valpha[s-1]=alpha;
fprintf (stdout,"last point, s=%d, ls=%f,ltotal=%f, valpha=%f\n",
	s-1,ls[s-1],ltotal,valpha[s-1]);
	ls[s]=ls[0];
	valpha[s]=valpha[0];
}

/*-------------------------------*/
if((yks != yk0 || xks != xk0) && s < 1000 && kscan ==0) 
{
pi=or-(yks+1);
pj=xks;
goto scancycle;
}

for(i=0;i<s; i++)
{
dalpha[i]=(valpha[i+1]-valpha[i])/ls[i]*ltotal;
fprintf (stdout,"x[%d]=%f, y[%d]=%f\n", i, x[i], i, y[i]);
fprintf (stdout,"dalpha[%d]=%f\n", i, dalpha[i]);
}
dalpha[s]=dalpha[0];
fprintf (stdout,"x[%d]=%f, y[%d]=%f\n", s, x[s], s, y[s]);
fprintf (stdout,"dalpha[%d]=%f\n", s, dalpha[s]);
*sout=s+1;
*totals=ltotal;

        /* Release memory resources */
/*        for (i=0 ; i<2 ; i++) G_free(data[i]);*/

return 0;
}

