/*
This software package is for spacial shape identification and classification
Hong Chun Zhuang
Sep.21,1992 at EC, CERL

$Id$

*/

#define MAIN

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "globals.h"
#include <math.h>

int main (int argc, char *argv[])
{
        /* Global variable & function declarations */

        int inputfd, zcfd, imagfd; /* the input and output file descriptors */
        char *inmapset; /* the input mapset name */
        struct Cell_head window;
        CELL *cell_row, *cell_row2;
        int result, ext_s, s, smax2;
        double max, min, scale, temp, dalpha[POINTS], ext_dalpha[3*POINTS];
	float Width;

	FILE *fdout;
		/* Used to find the smallest power of 2 >= to a number */
        int i,j, nclass;         /* Loop control variables */
        int or,oc;      /* Original dimensions of image */
        int rows,cols;  /* Smallest powers of 2 >= number of rows & columns */
        int size;      /* the length of one side */
        long totsize; /* the Total number of data points */
        double *data[2], *curve[2], spect[POINTS], ampl[POINTS]; 
		/* Data structure containing real & complex values of FFT */
        int save_args(); /* function to stash the command line arguments */
        struct Option *input_map, *output_map, *width, *threshold, 
		*orientations ;
        char *me, fileout[40];
	double totals, ds[POINTS],length[1024],curvature[1024],dy;
	double ext_ds[3*POINTS];
	double dels,xlen,ycurv;
        double yp1,ypn,y2[POINTS];


        G_gisinit(argv[0]);
        me = G_program_name();

        /* define options */
        input_map = G_define_option();
        input_map->key          = "input";
        input_map->type         = TYPE_STRING;
        input_map->required             = YES;
        input_map->multiple             = NO;
        input_map->gisprompt  = "old,cell,raster" ;
        input_map->description  = "input raster map";
#define INPUT_MAP input_map->answer

        output_map = G_define_option();
        output_map->key                 = "output";
        output_map->type                = TYPE_STRING;
        output_map->required            = YES;
        output_map->multiple            = NO;
        output_map->description         = "Fourier transformation output file";
#define OUTPUT_MAP  output_map->answer

        width = G_define_option();
        width->key              = "width";
        width->type             = TYPE_DOUBLE;
        width->required         = NO;
        width->multiple         = NO;
        width->description = "x-y extent of the Gaussian filter";
        width->answer           = "9";

        threshold = G_define_option();
        threshold->key          = "threshold";
        threshold->type         = TYPE_DOUBLE;
        threshold->required     = NO;
        threshold->multiple     = NO;
        threshold->description = "sensitivity of Gaussian filter";
        threshold->answer      = "10";

        orientations = G_define_option();
        orientations->key               = "orientations";
        orientations->type              = TYPE_INTEGER;
        orientations->required  = NO;
        orientations->multiple  = NO;
        orientations->description = "number of azimuth directions categorized";
        orientations->answer    = "1";

        /* call parser */
        if(G_parser(argc, argv))
                exit(-1);
                
/*	fdout=fopen(fileout, "w");
	fprintf(fdout, "output of shape finding software\n\n");*/
	fprintf(stdout, "output of shape finding software\n\n");

        /* open input cell map */
        if ((inmapset = G_find_cell(INPUT_MAP,"")) == NULL )
        {
                fprintf (stderr, "%s: %s - raster map not found\n", me, INPUT_MAP);
                exit(1);
        }
        inputfd = G_open_cell_old(INPUT_MAP, inmapset );
        if (inputfd < 0) exit(1);
        		/* 
			open the output cell maps and 
			allocate cell row buffers 
			*/
        fprintf (stdout,"Writing transformed data to file...\n");
        if ((zcfd = G_open_cell_new(OUTPUT_MAP)) < 0)
                exit(1);

					/* obtain curve of the shape */
        sscanf(threshold -> answer, "%lf", &Thresh);
        if (Thresh<=0.0)
                G_fatal_error("Threshold less than or equal to zero not allowed.  ");
        Thresh /= 100.0;

        sscanf(width -> answer, "%1f", &Width);

        if (Width<=0.0)
                G_fatal_error("Width less than or equal to zero not allowed.");

        sscanf(orientations -> answer, "%d", &NumOrients);
        if(NumOrients<1)
                G_fatal_error("Fewer than 1 orientation classes not allowed.");


	zc_curve(inputfd, zcfd, Width, ds, dalpha, &s, &totals);
					/* where s=total number of points in
					the boundary including the end point
					which equal to the begining point
					*/
					/* 
					do Fast Fourier Transformation
					for the curvature of the shape curve 
					*/ 
/*
fprintf (stdout,"in main after call zc_curve, s=%d\n",s);
for (i=0; i<s; i++)
fprintf (stdout,"ds[%d]=%f, dalpha=%f\n",i,ds[i],dalpha[i]);
*/
					/* 
					shift vector to
					1,2,....n
					as the function splint.c required
					*/
for(i=0;i<s;i++)
{
fprintf (stdout,"before interpolation, ds[%d]=%f, dalpha=%f\n",
i,ds[i],dalpha[i]);
}

	for(i=1;i<=s;i++)
	{
	ext_ds[i]=ds[i-1];
	ext_dalpha[i]=dalpha[i-1];
fprintf (stdout,"ext_ds[%d]=%f;\n ext_dalpha[%d]=%f;\n", 
i, ext_ds[i], i, ext_dalpha[i]);
/*
*/
	}
fprintf (stdout,"and s=%d, totals=%f\n", s, totals);
					/* interpoliting into 1024 points */
	dels=totals/1023.0;
        yp1=(ext_dalpha[2]-ext_dalpha[1])/(ext_ds[2]-ext_ds[1]);
        ypn=(ext_dalpha[s]-ext_dalpha[s-1])/(ext_ds[s]-ext_ds[s-1]);

/*
fprintf (stdout,"in main, dels=%f\n",dels);
*/

	for(i=1;i<=1024;i++)
	{
	xlen=(i-1) * dels;
        spline(ext_ds,ext_dalpha,s,yp1,ypn,y2);
        splint(ext_ds,ext_dalpha,y2,s,xlen, &ycurv);
	length[i-1]=xlen;
	curvature[i-1]=ycurv;
	}

for(i=0;i<1024;i++)
{
fprintf (stdout,"after interpolation, length[%d]=%f, curvature=%f\n",
i,length[i],curvature[i]);
}

	fft_curve(curvature, 1024, 1);

					/*
					make a increasing sequence of spectrum
					*/
					/* identification or classification
for (i=0; i<1024; i++)
fprintf (stdout,"curvature[%d]=%f\n",i,curvature[i]);
					*/

	for (i=0;i<512;i++)
{	
spect[i]= curvature[i];
fprintf (stdout,"positive spectrum[%d]=%f\n",i,spect[i]);
}

for(i=0; i<512; i=i+2)
{
ampl[i/2]=sqrt(spect[i]*spect[i] + spect[i+1]*spect[i+1]);
fprintf (stdout,"amplitude[%d]=%f\n", i/2, ampl[i/2]);
}

	class(spect, nclass);

	fprintf (stdout,"the shape is class %d\n", nclass);

        /* close input cell map */
        G_close_cell(inputfd);
        exit(0);
}
