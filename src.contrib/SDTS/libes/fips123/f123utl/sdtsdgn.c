/*
                           U.S. Geological Survey
                          National Mapping Division

                 Spatial Data Transfer Standard (SDTS)
                                 FIPS 173


                          Intergraph MicroStation Import Routine

                 program:         sdtsdgn.c
                 author:          Bob Lazar
                 date:            March 3, 1993
                 language:        C

      This program reads a file created by linedump.c from an SDTS line
      module and writes chains to an Intergraph design file as line
     string elements.

      This program runs only on Intergraph Unix workstations running
      MicroStation.  It uses Intergraph Micro CSL routines.

      to compile:

      cc sdtsdgn.c -lmcsl -lm -lbsd -o sdtsdgn

*/
#include <stdio.h>
#include <microcsl.h>
#include <opctrl.h>


FILE *fpin;
char input_file[100];
char output_file[100];
char mod_name[5];
short irc;
long  rec_numb;
long  no_attr;
short no_xy;
short unused1, opentype, format, unused6, unused8, attr;
int    i;
long   x;
long   y;
long   gg_numb;
Ggroup ggroup;
short  level;
Specs  specs;
Point2d xy[3000];

main()
{

printf ("\n    SDTS Line Dump ---> MicroStation Translator\n");
printf ("\n    Enter input line dump file name: ");

scanf ("%s%*1c",input_file);

fpin = fopen (input_file,"r");
if (fpin == NULL)
       {
       printf ("\nERROR OPENING FILE %s",input_file);
       }

printf ("\n\nEnter output design file name: ");

scanf ("%s%*1c",output_file);

opentype = 0;
format = 1;
indfpi (&unused1 ,output_file, &opentype, NULL, &format, &unused6, &irc, 
      &unused8);
if (irc)
  {
  printf ("\n\nINDFPI ERROR %d",irc);
  mcsl_error (&irc);
  exit();
  }

level = 1;
specs.class = 0;
specs.status = 0;
specs.style = 0;
specs.weight = 0;
specs.color = 10;

while (! feof(fpin))
{

/*      Read input file "line header"        */

fscanf (fpin,"%*1c%4s%11ld%11ld%*6c%5ld%*1c",mod_name,&rec_numb,&no_attr,&no_xy);

/*       Read attribute primary module record number and set to graphic group 
         number  (to be compatible with USGS RETSAM software         */

gg_numb = 0;

for (i=0; i<no_attr; i++)
       fscanf (fpin,"%*1c%4s%11ld%*1c",mod_name,&gg_numb);
if (no_attr > 0)
  {
  ggroup.code = 1;
  ggroup.number = gg_numb;
  }
else
  {
  ggroup.code = 1;
  ggroup.number = 6;/* unattribute line */
  }

/*       Read coordinates from input file         */

for (i=0; i<no_xy; i++)
       {
       fscanf (fpin,"%11ld%11ld%*1c",&x,&y);
         xy[i].x = x;
         xy[i].y = y;
       }

/*       Place line string in design file        */

lsdfpi (&ggroup, &level, &specs, xy, &no_xy, &irc, &attr);
if (irc)
  {
  printf ("\nLSDFPI ERROR %d",irc);
  mcsl_error (&irc);
  break;
  }
}
dedfpi (&unused8); 
fclose (fpin);
}
