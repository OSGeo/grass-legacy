
/***** Principal Component Transformation of Satellite Data *****/
/*

             Center for Space Research
             WRW 402
             University of Texas
             Austin, TX 78712-1085

             (512) 471-6824

*/

#define MAIN

#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "globals.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
  /* Global variable & function declarations */

  int i,j,k;          /* Loop control variables */
  long rows,cols;     /* Number of rows & columns */
  long row,col;
  double sum;         /* sum of all entries in a band */ 
  long bands;         /* Number of image bands */
  long NN;            /* Total number of data points */
  double *mu;         /* Mean vector for image bands */
  double **covar;     /* Covariance Matrix */
  double *eigval;
  double **eigmat;
  char **inp_names;
  char **out_names, temp[600];
  int result, infd, outfd, PASSES, pass;
  int *inp_file_descr;
  int scale, scale_max, scale_min;
  double min, max;
  char tempbuf[500], *mapset;
  CELL *rowbuf1, *rowbuf2;
  double *d_buf; /* a cell buf only double in order not to loose precision */

  struct GModule *module;
  struct Option *opt1, *opt2, *opt3 ;

  module = G_define_module();
  module->description =
	"Principal components analysis (pca) "
	"program for image processing.";

  /* Define the different options */

  opt1 = G_define_option() ;
  opt1->key        = "input";
  opt1->type       = TYPE_STRING;
  opt1->required   = YES;
  opt1->multiple   = YES;
  opt1->gisprompt  = "old,cell,raster" ;
  opt1->description= "input layer name" ;

  opt2 = G_define_option() ;
  opt2->key        = "output";
  opt2->type       = TYPE_STRING;
  opt2->required   = YES;
  opt2->gisprompt  = "new,cell,raster" ;
  opt2->description= "output layer name";

  opt3 = G_define_option() ;
  opt3->key        = "rescale";
  opt3->type       = TYPE_INTEGER;
  opt3->key_desc   = "min,max";
  opt3->required   = NO;
  opt3->answer     = "1,255"; 
  opt3->description= "Rescaling range output (For no rescaling use 0,0)";

  /***** Start of main *****/
  G_gisinit(argv[0]);

  if (G_parser(argc, argv) < 0)
    exit(-1);

  rows = G_window_rows();
  cols = G_window_cols();
  NN = rows * cols;

  rowbuf1 = G_allocate_cell_buf();
  rowbuf2 = G_allocate_cell_buf();

  scale = 1;
  scale_min = 1;
  scale_max =255;  /* default values */
  if(opt3->answer)
  {
     sscanf(opt3->answers[0], "%d", &scale_min);
     sscanf(opt3->answers[1], "%d", &scale_max);

     if(scale_min==scale_max)
     {
	if(scale_min==0) scale = 0;
	else
	{
          fprintf(stderr, "Scale range length should be >0; Using default values: 1,255\n\n");
          scale_min = 1;
          scale_max = 255;
        }
     }
     if(scale_max<scale_min)
     {
       int temp;
       temp = scale_max;
       scale_max = scale_min;
       scale_min = temp;
     }
  }

  for(bands=0; opt1->answers[bands] != NULL; bands++);

  covar = (double **) G_calloc(bands, sizeof(double *));
  mu = (double *) G_malloc(bands * sizeof(double ));
  inp_file_descr = (int *) G_malloc(bands * sizeof(int ));
  eigmat = (double **) G_calloc(bands, sizeof(double *));
  eigval = (double *) G_calloc(bands, sizeof(double));
  inp_names = (char **) G_calloc(bands, sizeof(char *));
  out_names = (char **) G_calloc(bands, sizeof(char *));
  d_buf = (double *) G_malloc(cols * sizeof(double));
  for(i=0;i<bands;i++)
  {
      covar[i] = (double *) G_calloc((bands) , sizeof(double));
      eigmat[i] = (double *) G_calloc((bands) , sizeof(double)); 
  }

  if (bands < 2)
    G_fatal_error("Sorry, you must provide at least 2 input bands") ;

  for (i=0 ; i<bands ; i++)
    {
      if ((mapset=G_find_cell(opt1->answers[i], "")) == NULL) {
        sprintf(tempbuf, "Unable to find cell map <%s>", opt1->answers[i]);
        G_fatal_error(tempbuf);
      }
      if ((inp_file_descr[i] =G_open_cell_old(opt1->answers[i], mapset)) < 0)
        {
          sprintf(tempbuf,"Error opening %s\n",opt1->answers[i]);
          G_fatal_error(tempbuf);
        }

      inp_names[i] = G_fully_qualified_name(opt1->answers[i], mapset);

      sprintf(tempbuf, "%s.%d", opt2->answer, i+1); 
      out_names[i] = G_fully_qualified_name(tempbuf, G_mapset());
      /* G_close_cell (infd) */;
    }

  /* check output file */
  /*****************************/
  /*if (strlen(opt2->answer) >=13)
    G_fatal_error("The output cell map name can not be longer than 12 characters.");
  */
  /*
  for(i=0;i<bands;i++)
     for(j=0;j<bands;j++)
       {
	  if(strcmp(inp_names[i], out_names[j]) == 0)
	  {
	      sprintf(tempbuf, "The output file %s will overwrite the input file %s",
			out_names[i], inp_names[i]);
	      G_fatal_error(tempbuf);
	  }
       }
  */

  fprintf(stderr, "Calculating covariance matrix...\n");

  for (i=0;i<bands;i++) 
  {
     sum = 0.;
     fprintf(stdout, "Computing Means for band number %d...", (i+1));
     fflush(stdout);
     for (row=0 ; row<rows ; row++)
     {
	 G_percent(row, rows-1,2);
         G_get_map_row(inp_file_descr[i], rowbuf1, row);
         for (col=0 ; col<cols ; col++)
	    sum += (double)rowbuf1[col];
     }
     mu[i] = sum / (double) NN;
  }

   for (j=0 ; j<bands ; j++)
      for (k=0 ; k<bands ; k++)
	     covar[k][j] = 0.;
   for (j=0 ; j<bands ; j++)
   {
      fprintf(stdout, "Computing row number %d of covariance matrix...", (j+1));
      fflush(stdout);
      for (row=0 ; row<rows ; row++)
      {
	 G_percent(row , rows-1,2);
         G_get_map_row(inp_file_descr[j], rowbuf1, row);
         for (k=j ; k < bands; k++)
         {
            G_get_map_row(inp_file_descr[k], rowbuf2, row);
            for (col=0 ; col<cols ; col++)
	        covar[j][k] += ((double)rowbuf1[col] - mu[j])*((double)rowbuf2[col] - mu[k]);
            covar[k][j] = covar[j][k];
         }

      }
  }
  for (i=0 ; i<bands ; i++)
    for (j=0 ; j<bands ; j++)
    {
      covar[i][j]=covar[i][j]/((double) (NN-1));
      /* fprintf(stdout, "Covar[%d][%d] = %f\n",i,j,covar[i][j]); */

    }

  fprintf(stderr, "Calculating eigenvalues and eigenvectors...\n");
  eigen(covar, eigmat, eigval, bands);
  /*   DEBUG  
  for(i=0;i<bands;i++)
  {
     for (j=0;j<bands;j++)
	fprintf(stdout, "%f  ", eigmat[i][j]);
     fprintf(stdout, "\n");
  }
  for (i=0;i<bands;i++)
     fprintf(stdout, "%f  ", eigval[i]);
  fprintf(stdout, "\n");
  */

  fprintf(stderr, "Ordering eigenvalues in descending order...\n");
  egvorder(eigval,eigmat,bands);
  fprintf(stderr, "Transposing eigen matrix...\n");
  transpose(eigmat,bands);

  fprintf(stderr, "Transforming data...\n");
  /* transform(DATA,NN,eigmat,bands,outbandmax); */

  strcpy(temp, G_tempfile());
  mapset = G_mapset();
  if(scale) PASSES = 2;
  else PASSES = 1;
     for(i=0;i<bands;i++) 
     {
         char name[100], command[300];
	 double old_range, new_range;

         for (pass = 1; pass<=PASSES; pass++)
	 {

             sprintf(name, "%s.%d", opt2->answer, (i+1));
             if((scale)&&(pass==PASSES))
             {

                 fprintf(stdout, "%s:   Rescaling the data to %d,%d range ... ", 
    			 name, scale_min, scale_max);
       	         fflush(stdout);
	         old_range = max - min;
	         new_range = (double) (scale_max - scale_min);
             }
             if(pass==1)
	     {
	         fprintf(stdout,"%s:   ", name);
                 fflush(stdout);
                 if ((infd=G_open_cell_new(name)) < 0)
                 {
                     sprintf(tempbuf,"Error opening %s\n",G_fully_qualified_name(name, mapset));
                     G_fatal_error(tempbuf);
                 }
             }
             for(row=0 ; row<rows ; row++)
             {
	        G_percent(row,rows,2);
                for(col=0 ; col<cols ; col++)
	           d_buf[col] = 0.;
	        for(j=0;j<bands;j++)
	        {
	           G_get_map_row( inp_file_descr[j], rowbuf2, row); 
                   for(col=0 ; col<cols ; col++)
	           /* add into the output cell eigmat[i][j] * corresp cell 
	              of j-th band for current j */
                   {
                       d_buf[col]  += eigmat[i][j] * (double) rowbuf2[col];
		       			     /* corresp. cell of j-th band */
		       if(j==(bands-1))/* the cell entry is complete */
		       {
		          if(pass==1)
		          {
		             rowbuf1[col] = round(d_buf[col] + .5);
                             if((row==0)&&(col==0))min = max = d_buf[0];
                             if (rowbuf1[col]<min) min=d_buf[col];
                             if (rowbuf1[col]>max) max=d_buf[col];
		          }
		          else if(scale)
		          {
		            /* scaling the cell */
	                    if(min==max) rowbuf1[col] = 1;   
	                    else 
	                    /* first mapping data to 0,(new_range-1) 
		                  and then adding new_min */
	                       rowbuf1[col] = 
	        	          round((new_range*(d_buf[col]-min)/old_range) + scale_min);
                          }
                       } /* writing the cell */
                   }  /* for col=0 to cols */
	        } /* for j = 0 to bands */
	        if(pass==PASSES)
	           G_put_map_row( infd, rowbuf1); 
             }  /* for row = 0 to rows */
	     G_percent(row,rows,2);
	     if(pass==PASSES)
	     {
                G_close_cell(infd);
                /* make grey scale color table */
                sprintf(command, "r.colors %s color=grey", name);
                system(command);
                /* write a color table */
	     }

         } /* for pass = 1 to PASSES */
    } /* for i=0 to bands-1 */

    fprintf(stdout, "Eigen vectors:\n");
    for(i=0;i<bands;i++) 
    {
	fprintf(stdout, "( ");
        for(j=0;j<bands;j++)
	   fprintf(stdout, "%.2f ",eigmat[i][j]);
        fprintf(stdout, ")\n");
    } /* for i=0 to bands-1 */

    for(i=0;i<bands;i++)
      G_unopen_cell(inp_file_descr[i]); 
  exit(0);
}

CELL 
round (double x)
{
  CELL n;

   if (x >= 0.0)
       n = x + .5;
   else
   {
       n = -x + .5;
       n = -n;
   }
   return n;
}

