/* %W% %G% */
#ident " @(#)main.c     1.2   02/26/91 "
/* qcalc    1.1   12/10/87
* m.qcalc    1.2   02/26/91  for 4.0
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: Productivity tool
*             calculation of acre and hectare values for cellsize
*			     cellsize and hectare for acres
*			     cellsize and acres for hectares
*	      conversion of  feet to meters, miles, and kilometers
*	                     meters to feet, miles, and kilometers
*			     miles to feet, meters, and kilometers
*			     kilometers to feet, meters, and miles
*	      desk calulator
*
*  ------Rev 4.+ arguements --------------------------------------
*    Input arguements:
*             m.qcalc [-sah] init=initial value
*                            [end=limiting value]
*                            [incr=increment value]
*                        
*                     [-c]   init=value  units=ft/mt/mi/km
*                     [-m] 
*  flags:
*       -s : size
*          creates a table : 
*          Cell Size       Sq.Ft.      Acres       Hectares
* 
*       -a : acres
*          creates a table : 
*          Acres           Sq.Ft.      Hectares    Cell Size
* 
*       -h : hectares
*          creates a table : 
*          Hectares        Sq.Ft.      Acres       Cell Size
* 
*       -c <ft> <value>
*       -c <mt> <value>
*       -c <mi> <value>
*       -c <km> <value>
*
*       -m 
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "gis.h"

int 
main (int argc, char *argv[])

{
	int ier;
	int size, incr, range;
	char units[10], buffr[100];
	double tmp_value, rincr, range_ft, sq_ft, acres, hectares;
        double size_rng, acre_rng, hect_rng;
	double feet, meters, miles, kilom;
	struct Option *initopt, *endopt, *incropt, *unitopt, *valuopt;
	struct Flag *s_flag, *a_flag, *h_flag, *c_flag, *m_flag;

	G_gisinit (argv[0]);


          /* set up the options and flags for the command line parser */

       s_flag = G_define_flag();
       s_flag->key              = 's';
       s_flag->description      = "Do a cell size table";

       a_flag = G_define_flag();
       a_flag->key              = 'a';
       a_flag->description      = "Do a acres size table";

       h_flag = G_define_flag();
       h_flag->key              = 'h';
       h_flag->description      = "Do a hectares size table";

       c_flag = G_define_flag();
       c_flag->key              = 'c';
       c_flag->description      = "Do conversion to units";

       m_flag = G_define_flag();
       m_flag->key              = 'm';
       m_flag->description      = "Do a calculation";
				    
       initopt = G_define_option();
       initopt->key              = "init";
       initopt->type             =  TYPE_DOUBLE;
       initopt->required         =  NO;
       initopt->description      =  "Initial value ";

       endopt = G_define_option();
       endopt->key              = "end";
       endopt->type             =  TYPE_DOUBLE;
       endopt->required         =  NO;
       endopt->description      =  "Ending value ";

       incropt = G_define_option();
       incropt->key              = "incr";
       incropt->type             =  TYPE_DOUBLE;
       incropt->required         =  NO;
       incropt->description      =  "Increment value ";

       unitopt = G_define_option();
       unitopt->key              = "unit";
       unitopt->type             =  TYPE_STRING;
       unitopt->required         =  NO;
       unitopt->options          =  "ft,mt,mi,km";
       unitopt->description      =  "Units";

       if (G_parser (argc, argv))
	     exit(-1);

	     /* start checking flags and options */

        if (s_flag->answer)         /* check size table flag */
	   {
	   if (initopt->answer == NULL)
              {		
	      fprintf (stderr,"\n An init value is required for this option\n");
	      exit(1);
	      }
	   sprintf(buffr,"%s",initopt->answer);
	   sscanf(buffr,"%d",&range);
	   size_rng = range+1.005; rincr=1;   /* relieve roundoff pressure */
	   if (endopt->answer != NULL)
	      {
	      sprintf(buffr,"%s",endopt->answer);
	      sscanf(buffr,"%lf",&size_rng);
	      }
	   if (incropt->answer != NULL)
	      {
	      sprintf(buffr,"%s",incropt->answer);
	      sscanf(buffr,"%lf",&rincr);
	      }
	   fprintf (stdout,"\n\t    Cell_Size           Sq.Ft.           Acres         Hectares\n\n");
	   while (range <= size_rng)
	      {
	      range_ft = range * 3.280843;
	      sq_ft = range_ft * range_ft;
	      acres = sq_ft / 43560.0;
	      hectares = acres / 2.471044;
	      fprintf (stdout,"\t%5d x %5d  %15.2f     %10.2f      %10.2f\n",(int)(range),(int)(range),sq_ft,acres,hectares);
	      range+=rincr;
	      }
	   }

        if (a_flag->answer)         /* check acre table flag */
	   {
	   if (initopt->answer == NULL)
              {		
	      fprintf (stderr,"\n An init value is required for this option\n");
	      exit(1);
	      }
	   sprintf(buffr,"%s",initopt->answer);
	   sscanf(buffr,"%lf",&acres);
	   acre_rng = acres+1.005; rincr=.1;   /* relieve roundoff pressure */
	   if (endopt->answer != NULL)
	      {
	      sprintf(buffr,"%s",endopt->answer);
	      sscanf(buffr,"%lf",&acre_rng);
	      }
	   if (incropt->answer != NULL) 
	      {
	      sprintf(buffr,"%s",incropt->answer);
	      sscanf(buffr,"%lf",&rincr);
	      }

	   fprintf (stdout,"\n\t      Acres             Sq.Ft.        Hectares         Cell Size\n\n");
           while ( acres <= acre_rng )
	      {
	      hectares = acres / 2.471044;
              sq_ft = 43560.0 * acres;
              range = sqrt (hectares * 10000.);
	      fprintf (stdout,"\t %10.2f    %15.2f      %10.2f      %5d x %5d\n",acres,sq_ft,hectares,range,range);
              acres+=rincr;
	      }
	   }

        if (h_flag->answer)          /* check hectare table flag */
	   {
	   if (initopt->answer == NULL)
              {		
	      fprintf (stderr,"\n An init value is required for this option\n");
	      exit(1);
	      }
	   sprintf(buffr,"%s",initopt->answer);
	   sscanf(buffr,"%lf",&hectares);
	   hect_rng = hectares+1.005; rincr=.1;  /* relieve roundoff pressure */
	   if (endopt->answer != NULL)
	      {
	      sprintf(buffr,"%s",endopt->answer);
	      sscanf(buffr,"%lf",&hect_rng);
	      }
	   if (incropt->answer != NULL)
	      {
	      sprintf(buffr,"%s",incropt->answer);
	      sscanf(buffr,"%lf",&rincr);
	      }

	   fprintf (stdout,"\n\t    Hectares            Sq.Ft.           Acres         Cell Size\n\n");
           while (hectares <= hect_rng)
	      {
	      acres = hectares * 2.471044;
	      sq_ft = 43560.0 * acres;
	      range = sqrt (hectares * 10000.);
	      fprintf (stdout,"\t %10.2f    %15.2f      %10.2f      %5d x %5d\n",hectares,sq_ft,acres,range,range);
	      hectares+=rincr;
	      }
           }


/* conversions ? */

        if (c_flag->answer)
	   {  
	   if (initopt->answer == NULL || unitopt->answer == NULL)
              {		
	      fprintf (stderr,"\n An init value and a unit value are required for this option\n");
	      exit(1);
	      }
           sprintf(buffr,"%s",initopt->answer);
           sprintf(units,"%2s",unitopt->answer);
           if ((strcmp(units,"ft") == 0) || (strcmp(units,"FT") == 0))
	      {
	      sscanf(buffr,"%lf",&feet);
	      meters = .3048006 * feet;
	      miles = feet / 5280.;
	      kilom = meters / 1000.;
	      }
           if ((strcmp(units,"mt") == 0) || (strcmp(units,"MT") == 0))
              {				    
	      sscanf(buffr,"%lf",&meters);
	      feet = 3.280833 * meters;
	      miles = feet / 5280.;
	      kilom = meters / 1000.;
              }
           if ((strcmp(units,"mi") == 0) || (strcmp(units,"MI") == 0))
              {		
	      sscanf(buffr,"%lf",&miles);
              feet = miles * 5280.;
	      meters = .3048006 * feet;
	      kilom = meters / 1000.;
              }
           if ((strcmp(units,"km") == 0) || (strcmp(units,"KM") == 0))
              {	
	      sscanf(buffr,"%lf",&kilom);
	      meters = kilom * 1000.;
	      miles = .62137 * kilom;
	      feet = miles * 5280.;
              }
           fprintf (stderr,"\n\t Feet      Meters       Miles    Kilometers\n");
           fprintf (stderr,"\n   %10.2f  %10.2f  %10.2f  %10.2f\n",feet,meters,miles,kilom);
	   }

	if (!s_flag->answer && !a_flag->answer &&
	    !h_flag->answer && !c_flag->answer)
	    {
            if (m_flag->answer)
	       {			
	       fprintf (stderr,"\n\tEnter a numeric values for calculation,");
	       fprintf (stderr,"   use  <cntl-d>  to quit.\n");
	       fprintf (stderr,"\t    simple math only (+,-,*,/,%%)\n");
	       system("bc");
	       exit(0);
               }
           }

	return 0;
}
