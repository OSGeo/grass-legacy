/*

		main.c
		v.to.rast2
		Marco Negretti and Ludovico Biagi
		Politecnico di Milano - Facolta' di Como

		This command allows the rasterization of GRASS vector data.
		The 'old' method is available: it is called by the G_system command;
		In this function 2 new rasterising methods are available.
		-1) dominant unit rastering;
		-2) importance method.

 http://www.ing.unitn.it/~zatelli/grass_days_2001/it/slides/grass_days_2001_negretti_biagi.pdf
 
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "segment.h"

#define LUNG05 50
#define LUNG2  200

#define TEMPRAST "tmp"

int main(argc,argv)
int	argc;
char	**argv;
{
struct Option *input,*output,*mod,*opzrast,*importance,*threshold;
char rasttmp[LUNG05];
char cmd[LUNG2];
int i,j,k,l;
int risol;
int orast;
/* region definition */
   struct Cell_head reg_orig,reg_rast;
   double colonne,righe,scolonne,srighe;
/* segmentation */
   int srows,scols,len,row;
   int fseg;
   char segname[]="tmp.seg";
   SEGMENT seg;
   int value;
   CELL *buff;
/* variables for the dominant unit rasterising */
   int s,max_s,esiste,prev,ind;
   int *num;
   int *valore;
/* variables for the importance rasterising */
   FILE *fimp;
   int t,max_t;
   int importante;
   int imp;
   double soglia;

G_gisinit (argv[0]);

input = G_define_option();
   input->key		= "input";
   input->type		= TYPE_STRING;
   input->required	= YES;
   input->multiple	= NO;
   input->gisprompt	= "old,dig,vector";
   input->description	= "vector file to convert to raster";

output = G_define_option();
   output->key		= "output";
   output->type		= TYPE_STRING;
   output->required	= YES;
   output->multiple	= NO;
   output->gisprompt	= "new,cell,raster";
   output->description	= "output raster";

mod = G_define_option();
   mod->key		= "mod";
   mod->type		= TYPE_STRING;
   mod->required	= NO;
   mod->multiple	= NO;
   mod->options		="cnt,dom,imp";
   mod->answer		="dom";
   mod->description	= "rasterising mode: central point (cnt), dominant (dom), importance (imp)";

opzrast = G_define_option();
   opzrast->key		= "optrast";
   opzrast->type	= TYPE_INTEGER;
   opzrast->required	= NO;
   opzrast->multiple	= NO;
   opzrast->answer	= "2";
   opzrast->description	= "rasterising option";

importance = G_define_option();
   importance->key		= "imp";
   importance->type		= TYPE_STRING;
   importance->required	= NO;
   importance->multiple	= NO;
   importance->description	= "importance class file";

threshold = G_define_option();
   threshold->key		= "thr";
   threshold->type		= TYPE_DOUBLE;
   threshold->required	= NO;
   threshold->multiple	= NO;
   threshold->answer	= "10";
   threshold->description	= "threshold";


if (G_parser (argc, argv))
   exit (-1);

/* defining thin rasterising step */
risol=atoi(opzrast->answer);

if ((strcmp(mod->answer,"cnt")==0)||(risol<=1))
{
   /* central point rasterising */
   sprintf(cmd,"v.to.rast input=%s output=%s",input->answer,output->answer);
   G_system(cmd);
}
else
{
   /* importance rasterising data checking */
   if ((strcmp(mod->answer,"imp"))==0)
   {
      if ((fimp=fopen(importance->answer,"r"))==NULL)
      {
         fprintf(stderr, "\n%s not found!\nEXIT\n",importance->answer);
         exit(1);
      }
      if ((fscanf(fimp,"N=%d",&max_t))==0)
      {
         fprintf(stderr, "\nFormat file %s not exact\nEXIT\n",importance->answer);
         exit(1);
      }
      soglia=atof(threshold->answer);
      if (soglia<0)
      {
         fprintf(stderr, "\nThreshold ERROR\nEXIT\n");
         exit(1);
      }
      fclose(fimp);
   }

   /* set the thin rasterising region */
   G_get_window(&reg_orig);
   reg_rast.ew_res=(reg_orig.ew_res)/(double)risol;
   reg_rast.ns_res=(reg_orig.ns_res)/(double)risol;
   colonne=((reg_orig.east)-(reg_orig.west))/(reg_rast.ew_res);
   righe=((reg_orig.north)-(reg_orig.south))/(reg_rast.ns_res);
   reg_rast.cols=(int)colonne;
   reg_rast.rows=(int)righe;
   reg_rast.format=reg_orig.format;
   reg_rast.compressed=reg_orig.compressed;
   reg_rast.proj=reg_orig.proj;
   reg_rast.zone=reg_orig.zone;
   reg_rast.north=reg_orig.north;
   reg_rast.south=reg_orig.south;
   reg_rast.east=reg_orig.east;
   reg_rast.west=reg_orig.west;
   /* set the region for v.to.rast command */
   if (G_put_window(&reg_rast)==-1)
   {
      fprintf(stderr, "ERROR region setup\nEXIT\n");
      exit(1);
   }
   G_set_window(&reg_rast);
   /* imposto la regione per le chiamate di libreria */
//   G_set_window(&reg_rast)==-1;

   /* checking the temp rast file */
   i=0;
   strcpy(rasttmp,TEMPRAST);
   while (G_find_cell(rasttmp,G_mapset())!=NULL)
   {
      strcpy(rasttmp,TEMPRAST);
      i++;
      sprintf(rasttmp,"%s.%d",rasttmp,i);
   }

   /* v.to.rast command */
   sprintf(cmd,"v.to.rast input=%s output=%s",input->answer,rasttmp);
   G_system(cmd);

   /* from thin raster to final raster */

   /* size of segmented file */
   srighe=(reg_orig.ns_res)/(reg_rast.ns_res);
   scolonne=(reg_orig.ew_res)/(reg_rast.ew_res);
   srows=(int)srighe;
   scols=(int)scolonne;
   len=sizeof(CELL);

   fseg=creat(segname,0666);
   fprintf(stderr, "Allocate disk space... ");
   if (segment_format(fseg,reg_rast.rows,reg_rast.cols,srows,scols,len)!=1)
   {
      fprintf(stderr, "\nsegment_format ERROR: verify disk space\nEXIT\n");
      exit(1);
   }
   close(fseg);
   fprintf(stderr, "OK\n");

   /* from nonsegmented matrix to segmented format */
   orast=G_open_cell_old(rasttmp,G_mapset());
   fseg=open(segname,2);
   if (segment_init(&seg,fseg,1)!=1)
   {
      fprintf(stderr, "segment_init ERROR\nEXIT\n");
      exit(1);
   }

   /* copy the raster file to segmented file */
   buff=G_allocate_c_raster_buf();
   fprintf(stderr,"Reading temporary raster map: ");
   for (row=0;row<(reg_rast.rows);row++)
   {
     G_percent(row,reg_rast.rows,5);
     G_get_c_raster_row(orast,buff,row);
     segment_put_row(&seg,buff,row);
   }
   fprintf(stderr,"done    \n");
   G_free(buff);

   G_close_cell(orast);

   /* read the cell blocks and search the dominant values */
   fprintf(stderr,"Creating raster map <%s>: ",output->answer);
   for (i=0;i<reg_orig.rows;i++)
   {
      G_percent(i,reg_orig.rows,5);
      for (j=0;j<reg_orig.cols;j++)
      {
         /* setting variables */
         valore=(int*)G_calloc(srows*scols,sizeof(int));
         num=(int*)G_calloc(srows*scols,sizeof(int));
         s=0;
         max_s=0;
         ind=0;
         prev=0;
         /* read the single block and search the dominant value */
         for (k=0;k<srows;k++)
         {
            for (l=0;l<scols;l++)
            {
               if (segment_get(&seg,&value,srows*i+k,scols*j+l)!=1)
               {
                  fprintf(stderr, "segment_get ERROR\nEXIT\n");
                  exit(1);
               }
               esiste=0;
               for (s=0;s<=max_s;s++)
               {
                  if (value==valore[s])
                  {
                  /* current value has been previously found: counter is increased */
                     num[s]=num[s]+1;
                     esiste=1;
                     if (num[s]>prev)
                     /* a larger and existing category of data is found */
                     {
                           prev=num[s];
                           ind=s;
                     }
                  }
               }
               if (esiste==0)
               {
                  /* a new category is found: saving informations */
                  max_s=max_s++;
                  valore[max_s]=value;
                  num[max_s]=1;
               }
            }
         }

         /* importance rasterising mode */
         if ((strcmp(mod->answer,"imp"))==0)
         {
            importante=0;
            /* search importance class */
            fimp=fopen(importance->answer,"r");
            fscanf(fimp,"N=%d",&max_t);
            for (t=0;t<max_t;t++)
            {
               fscanf(fimp,"%d",&imp);
               for (s=0;s<=max_s;s++)
               {
                  if (valore[s]==imp)
                  {
                     if(((double)num[s]/(scols*srows))>(soglia/100))
                     {
                        importante=1;
                        ind=s;   	/* this contains the value to assign to every cell */
                        s=max_s;	/* exit for */
                     }
                  }
               }
               if (importante==1)
                  t=max_t;	/* importance class found */
            }
            fclose(fimp);
         }	/* importance class is given as output */

         /* writing new value in cells */
         for (k=0;k<srows;k++)
         {
            for (l=0;l<scols;l++)
            {
               segment_put(&seg,&valore[ind],srows*i+k,scols*j+l);
            }
         }
         G_free(valore);
         G_free(num);
      } /* next block */
   } /* now raster in segmented format contains prevalent values */

   fprintf(stderr,"done    \n");

   /* new raster file */
   orast=G_open_raster_new(output->answer,CELL_TYPE);
   segment_flush(&seg);
   buff=G_allocate_c_raster_buf();

   /* copy the segmented format file in the raster file */
   fprintf(stderr,"Writing raster map <%s>: ",output->answer);
   for (row=0;row<(reg_rast.rows);row++)
   {
     G_percent(row,reg_rast.rows,5);
     segment_get_row(&seg,buff,row);
     G_put_c_raster_row(orast,buff);
   }
   fprintf(stderr,"done    \n");
   G_free(buff);

   G_close_cell(orast);
   segment_release(&seg);
   close(fseg);

   /* resetting the initial region */
   G_put_window(&reg_orig);

   /* r.mapcalc */
   sprintf(cmd,"r.mapcalc %s=%s",output->answer,output->answer);
   G_system(cmd);

   sprintf(cmd,"rm %s",segname);
   G_system(cmd);
   sprintf(cmd,"g.remove rast=%s > null",rasttmp);
   G_system(cmd);
   fprintf(stderr, "Raster map <%s> done\n",output->answer);
}

} /* end */
