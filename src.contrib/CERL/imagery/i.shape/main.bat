/*
This software package is for spacial shape identification and classification
Hong Chun Zhuang
Sep.21,1992
at EC, CERL
*/

#define MAIN

#include "gis.h"
#include "globals.h"
#include <math.h>


main(argc,argv)
int argc;
char *argv[];
{
        /* Global variable & function declarations */

        int inputfd, zcfd, imagfd; /* the input and output file descriptors */
        char *inmapset; /* the input mapset name */
        struct Cell_head window;
        CELL *cell_row, *cell_row2;
        int result, ext_s, s, smax2;
        double max, min, scale, temp, dalpha[POINTS], ext_dalpha[3*POINTS];
	float Width;
	void polint();

	FILE *fdout, *fopen();
        long max_pow2(); 
        long min_pow2(); 
		/* Used to find the smallest power of 2 >= to a number */
        int i,j, nclass;         /* Loop control variables */
        int or,oc;      /* Original dimensions of image */
        int rows,cols;  /* Smallest powers of 2 >= number of rows & columns */
        int size;      /* the length of one side */
        long totsize; /* the Total number of data points */
        double *data[2], *curve[2], spect[POINTS], ampl[POINTS]; 
		/* Data structure containing real & complex values of FFT */
        int save_args(); /* function to stash the command line arguments */
	void class();
	void fft_curve();
        struct Option *input_map, *output_map, *width, *threshold, 
		*orientations ;
        char *me, *fileout[40];
	double totals, ds[POINTS],length[1024],curvature[1024],dy;
	double ext_ds[3*POINTS];
	double dels,xlen,ycurv;

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


	fdout=fopen(fileout, "w");
	fprintf(fdout, "output of shape finding software\n\n");
        /* call parser */
        if(G_parser(argc, argv))
                exit(-1);

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
        printf("Writing transformed data to file...\n");
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
					/* do Fast Fourier Transformation
					for the curvature of the shape curve */ 
/*
printf("in main after call zc_curve, s=%d\n",s);
for (i=0; i<s; i++)
printf("ds[%d]=%f, dalpha=%lf\n",i,ds[i],dalpha[i]);
*/
					/* extend range 
					and shift vector to
					1,2,....n
					as the function polint.c required
					*/
	for(i=1;i<s+1;i++)
	{
	ext_ds[i]=ds[i-1];
	ext_dalpha[i]=dalpha[i-1];
printf("ext_ds[%d]=%lf, ext_dalpha=%lf\n", i, ext_ds[i], ext_dalpha[i]);
/*
*/
	}

	for(i=1;i<s+1;i++)
	{
	ext_ds[i+s]=totals+ds[i-1];
	ext_dalpha[i+s]=dalpha[i-1];
/*
*/
printf("ext_ds[%d]=%lf, ext_dalpha=%lf\n", i+s, ext_ds[i+s], ext_dalpha[i+s]);
	}

	for(i=1;i<s+1; i++)
	{	
	ext_ds[i+2*s]=2.0*totals+ds[i-1];
	ext_dalpha[i+2*s]=dalpha[i-1];
/*
*/
printf("ext_ds[%d]=%lf, ext_dalpha=%lf\n", i+2*s, ext_ds[i+2*s], ext_dalpha[i+2*s]);
	}
/*
*/
	for(i=1;i<3*s+1;i++)
printf("ext_ds[%d]=%lf, ext_dalpha=%lf\n", i, ext_ds[i], ext_dalpha[i]);
					/* interpoliting into 1024 points */
	dels=totals/1023.0;
printf("in main, dels=%lf\n",dels);
	ext_s=s*3.0;

	for(i=1;i<1025;i++)
	{
	xlen=totals+(i-1) * dels;
printf("xlength[%d]=%lf\n",i,xlen);
	polint(ext_ds,ext_dalpha,ext_s,xlen, &ycurv,&dy);
	length[i-1]=xlen;
	curvature[i-1]=ycurv;
	}
for(i=0;i<1024;i++)
{
printf("length[%d]=%f, curvature=%f\n",i,length[i],curvature[i]);
}

/*
printf("in main before call fft_curve, smax2=%d\n", smax2);
for (i=0; i<smax2; i++)
printf("dalpha[%d]=%lf\n",i,dalpha[i]);
*/

	fft_curve(curvature, 1024, 1);

					/*
					make a increasing sequence of spectrum
					*/
					/* identification or classification
					*/
for (i=0; i<1024; i++)
printf("curvature[%d]=%lf\n",i,curvature[i]);

	for (i=0;i<512;i++)
{	
spect[i]= curvature[i];
printf("positive spectrum[%d]=%lf\n",i,spect[i]);
}

for(i=0; i<512; i=i+2)
{
ampl[i/2]=sqrt(spect[i]*spect[i] + spect[i+1]*spect[i+1]);
printf("amplitude[%d]=%lf\n", i/2, ampl[i/2]);
}

	class(spect, nclass);

	printf("the shape is class %d\n", nclass);

        /* close input cell map */
        G_close_cell(inputfd);
        exit(0);
}
