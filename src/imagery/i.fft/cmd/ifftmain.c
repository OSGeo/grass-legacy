/*
 Central Washington University GIS Laboratory
 Programmer: David B. Satnik
and
 Programmer : Ali R. Vali
             Center for Space Research
             WRW 402
             University of Texas
             Austin, TX 78712-1085

             (512) 471-6824

*/

#define MAIN

#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "gmath.h"
#include "globals.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
        /* Global variable & function declarations */

        FILE *realfp, *imagfp; /* the input and output file descriptors */
        int outputfd, maskfd; /* the input and output file descriptors */
        char *realmapset, *imagmapset; /* the input mapset names */
        struct Cell_head orig_wind, realhead;
        CELL *cell_row, *maskbuf;
        char buffer[100];

        int i,j;         /* Loop control variables */
        int or,oc;      /* Original dimensions of image */
        int rows,cols;  /* Smallest powers of 2 >= number of rows & columns */
        long totsize;         /* Total number of data points */
        int halfrows, halfcols ;
        double *data[2]; /* Data structure containing real & complex values of FFT */
        int inv_save_args(); /* function to stash the command line arguments */
        struct Option *op1, *op2, *op3;
        struct GModule *module;
        char *me;

        G_gisinit(argv[0]);
        me = G_program_name();
        
        /* Set description */
        module              = G_define_module();
        module->description = ""\
        "Inverse Fast Fourier Transform (ifft) for image processing.";

        /* define options */
        op1=G_define_option();
        op1->key              = "real_image";
        op1->type             = TYPE_STRING;
        op1->required         = YES;
        op1->multiple         = NO;
        op1->gisprompt        = "old,cell,raster";
        op1->description      = "input raster file (image fft, real part)";

        op2=G_define_option();
        op2->key              = "imaginary_image";
        op2->type             = TYPE_STRING;
        op2->required         = YES;
        op2->multiple         = NO;
        op2->gisprompt        = "old,cell,raster";
        op2->description      = "input raster file (image fft, imaginary part";

        op3=G_define_option();
        op3->key              = "output_image";
        op3->type             = TYPE_STRING;
        op3->required         = YES;
        op3->multiple         = NO;
        op3->gisprompt        = "new,cell,raster";
        op3->description      = "output inverse raster file after ifft";

        /*call parser*/
        if(G_parser(argc, argv))
                exit(-1);

        strcpy(Cellmap_real, op1->answer) ;
        strcpy(Cellmap_imag, op2->answer) ;
        strcpy(Cellmap_orig, op3->answer) ;

        /* open input raster map */
        if ((realmapset = G_find_cell2(Cellmap_real, "")) == NULL)
        {
                fprintf (stderr, "%s: %s - Unable to find the real-image map\n", me,
                    Cellmap_real);
                exit(1);
        }
        sprintf(buffer, "cell_misc/%s", Cellmap_real);
        if ((realfp = G_fopen_old(buffer, FFTREAL, realmapset)) == NULL)
        {
                G_fatal_error("Unable to open real-image in the cell_misc directory.\nInput map probably wasn't created by i.fft") ;
                exit(1);
        }

        if ((imagmapset = G_find_cell2(Cellmap_imag, "")) == NULL)
        {
                fprintf (stderr, "%s: %s - Unable to find the imaginary-image\n", me,
                    Cellmap_imag);
                exit(1);
        }
        sprintf(buffer, "cell_misc/%s", Cellmap_imag);
        if ((imagfp = G_fopen_old(buffer, FFTIMAG, imagmapset)) == NULL)
        {
                G_fatal_error("Unable to open imaginary-image in the cell_misc directory.\nInput map probably wasn't created by i.fft") ;
                exit(1) ;
        }


        /* check command line args for validity */
        if (G_legal_filename(Cellmap_orig) < 0)
        {
                fprintf (stderr, "%s: %s - illegal name for output raster file\n", me,
                    Cellmap_orig);
                exit(1);
        }

        /* get and compare the original window data */
        get_orig_window(&orig_wind, realmapset, imagmapset);

        or = orig_wind.rows;
        oc = orig_wind.cols;
        G_get_cellhd(Cellmap_real, realmapset, &realhead);
        G_set_window(&realhead); /* set the window to the whole cell map */

        /* get the rows and columns in the current window */
        rows = G_window_rows();
        cols = G_window_cols();
        totsize = rows * cols;
        halfrows = rows / 2 ;
        halfcols = cols / 2 ;

        fprintf(stderr,"Power 2 values : %d rows %d columns\n",rows,cols);

        /* Allocate appropriate memory for the structure containing
     the real and complex components of the FFT.  DATA[0] will
     contain the real, and DATA[1] the complex component.
     */
        data[0] = (double *) G_malloc((rows*cols)*sizeof(double));
        data[1] = (double *) G_malloc((rows*cols)*sizeof(double));
        if (data[0] == NULL || data[1] == NULL)
                G_fatal_error("Insufficent memory for allocation of data structure");

        /* Initialize real & complex components to zero */
        fprintf(stderr,"Reading the raster maps...\n");
        {
                fread((char *) data[0], sizeof(double), totsize, realfp);
                fread((char *) data[1], sizeof(double), totsize, imagfp);
        }

        /* Read in cell map values */
        fprintf(stderr,"Masking the raster maps...\n");
        maskfd = G_maskfd();
        if (maskfd >= 0) maskbuf = G_allocate_cell_buf();

        if (maskfd >= 0) {
                for (i=0; i<rows; i++) {
                        double *data0, *data1 ;
                        data0 = data[0] + i * cols ;
                        data1 = data[1] + i * cols ;
                        G_get_map_row(maskfd, maskbuf, i);
                        for (j=0; j<cols; j++, data0++, data1++) {
                                if (maskbuf[j] == (CELL) 0) {
                                        *(data0) = 0.0;
                                        *(data1) = 0.0;
                                }
                        }
                }
        }

        fprintf(stderr,"Rotating data arrays...\n");
        /* rotate the data array for standard display */
        for (i=0; i<rows; i++) {
                double temp;
                for (j=0; j<halfcols; j++) {
                        temp = *(data[0]+i*cols+j);
                        *(data[0]+i*cols+j) = *(data[0]+i*cols+j+halfcols);
                        *(data[0]+i*cols+j+halfcols) = temp;
                        temp = *(data[1]+i*cols+j);
                        *(data[1]+i*cols+j) = *(data[1]+i*cols+j+halfcols);
                        *(data[1]+i*cols+j+halfcols) = temp;
                }
        }
        for (i=0; i<halfrows; i++) {
                double temp;
                for (j=0; j<cols; j++) {
                        temp = *(data[0]+i*cols+j);
                        *(data[0]+i*cols+j) = *(data[0]+(i+halfrows)*cols+j);
                        *(data[0]+(i+halfrows)*cols+j) = temp;
                        temp = *(data[1]+i*cols+j);
                        *(data[1]+i*cols+j) = *(data[1]+(i+halfrows)*cols+j);
                        *(data[1]+(i+halfrows)*cols+j) = temp;
                }
        }


        /* close input cell maps and release the row buffers */
        fclose(realfp);
        fclose(imagfp);
        if (maskfd >=0) {
                G_close_cell(maskfd);
                free(maskbuf);
        }

        /* perform inverse FFT */
        fprintf(stderr,"Starting Inverse FFT...\n");
        fft(1,data,totsize,cols,rows);
        fprintf(stderr,"Inverse FFT completed...\n");

        /* set up a window for the transform cell map */
        G_set_window(&orig_wind);

        /* open the output cell map and allocate a cell row buffer */
        if ((outputfd = G_open_cell_new(Cellmap_orig)) < 0)
                exit(1);
        cell_row = G_allocate_cell_buf();

        /* Write out result to a new cell map */
        fprintf(stderr,"Writing data to file...\n");
        for (i=0; i<or; i++) {
                for (j=0; j<oc; j++) {
                        *(cell_row+j) = (CELL) (*(data[0]+i*cols+j) + 0.5);
                }
                G_put_raster_row(outputfd, cell_row, CELL_TYPE);
        }
        G_close_cell(outputfd);

        free(cell_row);
        {
                struct Colors colors;
                struct Range range ;
                CELL min, max ;

                /* make a real component color table */
                G_read_range(Cellmap_orig, G_mapset(), &range) ;
		G_get_range_min_max (&range, &min, &max);
                G_make_grey_scale_colors(&colors, min, max) ;
                G_write_colors(Cellmap_orig, G_mapset(), &colors) ;
        }

        /* Release memory resources */
        free(data[0]);
        free(data[1]);
        fprintf(stderr,"Transform successful\n");

exit(0);
}

