
/* Glut - December 1988 - DBA Systems, Inc. */

#include <stdio.h>
#include "gis.h"

#define NONE        0
#define CATEGORIES  1
#define PERCENTAGES 2

int spec_type = NONE;

struct Specs 
   {
   int   index;
   int   R,
	 G,
	 B;
   };

main(argc,argv)
int argc;
char *argv[];
{
CELL   min,
       max;
char   name[50],              /* name of cell file */
       msg[50],               /* for error messsages */
       *mapset;               /* mapset of cell file */
struct Range range;           /* range specification */
struct Specs specs[50];       /* array of color specifications */ 
struct Colors colors;         /* GRASS's structure for LUT */
int    ncolors,               /* no. of colors (also no. of cats) */
       num_specs;

/* initialize GIS library */
G_gisinit(argv[0]);

/* exit if cell file is not found */
strcpy(name,argv[1]);
mapset = G_find_cell(name);
if (mapset == NULL)
   {
   sprintf(msg,"%s - cell file not found",name);
   G_fatal_error(msg);
   }

/* get range of categories in cell file */
G_read_range(name,mapset,&range);
min = range.nmin ? range.nmin : range.pmin;
max = range.pmax ? range.pmax : range.nmax;
ncolors = max - min + 1;

/* initialize the lut */
G_init_colors(&colors);
G_set_color(min,255,255,255,&colors);
G_set_color(max,255,255,255,&colors);

/* get the specifications for the lut */
read_specs(specs,&num_specs,min,max);

/* build the lut according to the specs */
build_lut(specs,num_specs,&colors);

/* write the new lut to the cellfile */
G_write_colors(name,mapset,&colors);
}

/***********************************************************************/

build_lut(specs,num_specs,colors)
struct Colors *colors;
int num_specs;
struct Specs specs[50];
{
float Rincr,Gincr,Bincr,
      Iincr,
      Rsum,Gsum,Bsum;
int   i=0,
      R,G,B;
CELL  j=0;

if (num_specs == -1)
   exit(-1);

for (i=0; i<num_specs; i++)
   {
   Iincr = (specs[i+1].index - specs[i].index);
   Rincr = (specs[i+1].R - specs[i].R) / Iincr;
   Gincr = (specs[i+1].G - specs[i].G) / Iincr;
   Bincr = (specs[i+1].B - specs[i].B) / Iincr;
   Rsum  = Gsum = Bsum = 0.0;

   for (j=specs[i].index; j<=specs[i+1].index; j++)
      {
      R = specs[i].R+Rsum;
      G = specs[i].G+Gsum;
      B = specs[i].B+Bsum;
      G_set_color(j,R,G,B,colors);
      Rsum += Rincr;
      Gsum += Gincr;
      Bsum += Bincr;
      }
   }
}

/***********************************************************************/

read_specs(specs,num_specs,min,max)
struct Specs specs[50];
int *num_specs;
CELL min,max;
{
struct Specs raw_specs[50];
char flag[5];
int i,ind=0,
    num=0,
    R,G,B,
    num_colors,
    num_scanned,
    last_one;
float incriment;

scanf("%s",flag);

while (1)
   {
   if (num_scanned = scanf("%d %d %d %d",&num,&R,&G,&B) != 4)
      break;
   raw_specs[ind].index = num;
   raw_specs[ind].R     = R;
   raw_specs[ind].G     = G;
   raw_specs[ind++].B   = B;
   }
while (num_scanned == 4);

*num_specs = ind;

if (strcmp(flag,"%")==0)
   {
   num_colors = max-min+1;
   incriment  = num_colors/100.0;
   last_one = -1;
   for (i=0; i<*num_specs; i++)
      {
      if (raw_specs[i].index < 0 || raw_specs[i].index > 100)
         G_fatal_error("specification index out of range");
      else if (raw_specs[i].index < last_one)
         G_fatal_error("specification index out of order");
      else
         {
         last_one = raw_specs[i].index;
         specs[i].index = min + (int)(raw_specs[i].index*incriment);
         specs[i].R = raw_specs[i].R;
         specs[i].G = raw_specs[i].G;
         specs[i].B = raw_specs[i].B;
         }
      }
   }
else if (strcmp(flag,"#")==0)
   {
   last_one = min-1;
   for (i=0; i<*num_specs; i++)
      {
      if (raw_specs[i].index < min || raw_specs[i].index > max)
         G_fatal_error("specification index out of range");
      else if (raw_specs[i].index < last_one)
         G_fatal_error("specification index out of order");
      else
         {
         last_one = raw_specs[i].index;
         specs[i].index = raw_specs[i].index;
         specs[i].R = raw_specs[i].R;
         specs[i].G = raw_specs[i].G;
         specs[i].B = raw_specs[i].B;
         }
      }
   }
else
   G_fatal_error("format character not found");
}
