/*** Documentation Start *********
 
 ****************************************************
 *                                                  *
 *  LOOP1TR55 procedure                              *
 *                                                  *
 ****************************************************
 
SYNOPSIS:
*************************************************/
 
#ifdef _DOS
 
 #include <math.h>
 #include <string.h>
 #include <stdio.h>
 #include "input.h"
 #include "binary.h"
 #include "debugflg.h"

#else

#include <math.h>
#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif

extern INITIAL_INFOPTR initialptr;
extern COLUMN_INFOPTR *columndata;
extern int             debuginfo;
extern HYDRO_REC_PTR   hydro;
extern int	       hydro_info;

extern FLAGS_BASIC     bflags;
extern FLAGS_ROUTINE   rflags;
 
/*UNITS: VOID loop1tr55( , HOURS) */
 
#ifdef _UNIX_K_AND_R
 
 void loop1tr55(col_num, overland_duration)
 
   int col_num;
   float *overland_duration;
 
#else
 
 void loop1tr55(int col_num, float *overland_duration)
 
#endif
{
 
/* LOCAL VARIABLES */
 
float time_overland;      /* Duration of overland flow (hours) */
float cell_width;         /* Width of current cell */
float shallow_length;     /* Length of shallow flow path in cell */
float time_shallow;       /* Duration of shallow flow (hours)*/
float velocity_shallow;   /* Velocity of the shallow flow (cf/s) */
 
/*
 
     PARAMETERS
 
col_num              [INPUT]  current column number we are processing
 
***********************************************
DESCRIPTION:
  This procedure calculates the overland flow for loop 1 when the user
  has selected the TR55 method of calculation with or without geomorphic
  calculations.
 
RETURNS:
 overland_duration
 
NOTES:
  This procedure is used in loops 1 and 2 only.  Wsdtr55.c is used
  for loop 3.
 
DATA STORES:
 
 columndata[column_number]->area        	      	[V]
 columndata[column_number]->overland_mannings           [V]
 columndata[column_number]->primary_cell	      	[F]
 
 columndata[column_number]->slope->average_land_slope 	[V]
 columndata[column_number]->slope->slope_length       	[V]
 
 initialptr->geomorphic_calc                          	[F]
 initialptr->storm_rainfall                           	[V]
 
 
HISTORY:
 
date    bug  Prog Description
 
7/25/92      MAK  Original creation of CELLTR55 procedure
7/27/92 S12  MAK  Fixed TR55 prob. w/ water channel
8/20/92      MAK  Developed this version for loop1
 
 
SEE ALSO:
loop1.c
 
************************* Documentation End ****************/
 
 /* Use regular triangular calculations for width depth and length */
 /* Uses width, depth, and channel side slope (2 of the 3 are user input */
 /* the third is calulated in the length procedure. */
 /* Note that the geomorphic and non-geomorphic overland durations for TR55
    are calculated the same way. */
 
 if(bflags.tr_55)
   {
    fprintf (stderr,"****\n");
    fprintf (stderr,"1.9 Calculate Overland Duration -TR55\n");
   }
 
 
 
 /******** 1. CALCULATE TIME OF OVERLAND FLOW ***********/
 
 
	time_overland=.007*pow((columndata[col_num]->overland_mannings*
	    columndata[col_num]->slope->slope_length),.8)/
	    (pow(initialptr->storm_rainfall,.5)*pow((columndata[col_num]->
	     slope->average_land_slope/100),.4));
 
 
 
  /******** 2. CALCULATE CELL WIDTH ******/
 
	cell_width=sqrt(columndata[col_num]->area*43560.0);
 
 
 
  /******** 3. CALCULATE SHALLOW LENGTH *********/
 
	if(columndata[col_num]->primary_cell == 0)
 
	  shallow_length=0.884* cell_width -(columndata[col_num]->
		     slope->slope_length);
	else

	  shallow_length=0.375* cell_width -(columndata[col_num]->
		     slope->slope_length);



  /********* 4. CALCULATE SHALLOW VELOCITY ***********/

	velocity_shallow=16.1345*pow((columndata[col_num]->slope->average_land_slope/100),
			 .5);

	if(hydro_info)
	  hydro->velocity_shallow=velocity_shallow;

	if(velocity_shallow>2)
	  velocity_shallow=2;




  /******** 5. CALCULATE TIME OF SHALLOW FLOW ************/

	time_shallow=shallow_length/(3600*velocity_shallow);



  /********* 6. CALCULATE OVERLAND FLOW DURATION ***********/


 *overland_duration = time_overland + time_shallow;

 if(hydro_info)
   {
    hydro->overland_mannings=columndata[col_num]->overland_mannings;
    hydro->time_overland=time_overland;
    hydro->area=columndata[col_num]->area;
    hydro->cell_width=cell_width;
    hydro->primary_cell=columndata[col_num]->primary_cell;
    hydro->shallow_length=shallow_length;
    hydro->velocity_shallow_after=velocity_shallow;
    hydro->time_shallow=time_shallow;
    hydro->overland_flow_duration=*overland_duration;
   }



 if(bflags.tr_55)
   {
    fprintf (stderr,"INPUTS: overland mannings %f  slope length %d(feet)\n",
	  columndata[col_num]->overland_mannings,columndata[col_num]->
	  slope->slope_length);
    fprintf (stderr,"        storm rainfall %f(inches) land slope %f(%) area %f(acres)\n",
	   initialptr->storm_rainfall,columndata[col_num]->slope->
	   average_land_slope,columndata[col_num]->area);
    fprintf (stderr,"INTER:  shallow velocity %f(ft/sec) shallow length %d(feet) cell width %f(feet)\n",
	  velocity_shallow,shallow_length,time_shallow);
    fprintf (stderr,"OUTPUTS:overland time %f(hours) shallow time %f(hours) overland duration %f(hours)\n\n",
	  time_overland,time_shallow,*overland_duration);
   }

 if (rflags.tr_55)
   {
    fprintf (stderr,"TR-55 routine: \n");
    fprintf (stderr,"   Input:  %d...col_number\n",col_num);
    fprintf (stderr,"    Vars:  %f...cell_width   %f...shallow_length\n",cell_width,shallow_length);
    fprintf (stderr,"           %f...velocity_shallow  %f...time_shallow\n",velocity_shallow,time_shallow);
    fprintf (stderr,"   Output: %f...time_overland\n",time_overland);
   }


}
