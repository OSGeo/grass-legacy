/****************************************************************
 ****************** OVRLD - FLOW DURATION ***********************
 ****************************************************************/
 
/* This routine calculates the amount of time needed for a
   concentrated flow to occur.  The actual equation looks as
   follows...

 flow duration=slope length/flow velocity

 flow velocity=10^.5*log10(percent slope)-surface condition constant

 Note: The original equation is from the USDA-SCS (1972).
       the original equation had land slope multiplied by 100.  This
       was to convert the slope to a percent. The slope that agnps uses
       is already in percent form, so therefore there is no need to
       multiply slope by 10 as the original equation does.
 */

 /* INPUTS:
     slope        (float)=field slope as a percent.
     slope_length (float)=field slope length in feet.
     surface_condition(float)=cell characteristic that accounts for the
		effects of land use and vegetation.

    OUTPUTS:
     overland_flow_duration (float)=flow time in seconds.
 */


#ifdef _DOS

 #include "input.h"
 #include <stdio.h>
 #include <math.h>
 #include "binary.h"
 #include "debugflg.h"

#else
 
#include "input.h"
#include <stdio.h>
#include <math.h>
#include "binary.h"
#include "debugflg.h"

#endif

extern int hydro_info;
extern HYDRO_REC_PTR hydro;


extern FLAGS_BASIC         bflags;
extern FLAGS_ROUTINE       rflags;


/*UNITS: SEC ovrld( PERCENT, FEET, NOD) */

#ifdef _UNIX_K_AND_R

float ovrld(slope, slope_length, surface_condition)

	float slope;
	int   slope_length;
	float surface_condition;

#else

float ovrld(float slope,               /* percent */
	    int   slope_length,        /* feet    */
	    float surface_condition)   /* English */

#endif

{
double power;
double flow_velocity;            /* water velocity in ft/sec */
float  overland_flow_duration;   /* seconds                  */



   if (slope <= 0.0)   /* change slope so we don't get an error */
       slope =  0.01;  /* because we took the log of zero.      */

		    /* figure the exponent for flow velocity */
   power = 0.5 * log10(slope) - surface_condition;

   flow_velocity = pow(10.0, power);  /* calculate the flow velocity */

   if(hydro_info)
     hydro->flow_velocity=flow_velocity;

   if ((surface_condition != -0.18) && (flow_velocity > 2.0))
       flow_velocity = 2.0;


		    /* calculate the duration of the flow */
   if (flow_velocity > 0.0)
       overland_flow_duration = slope_length/flow_velocity;
   else
       overland_flow_duration = 0.0;


   if(bflags.flow)
    {
     fprintf (stderr,"****\n");
     fprintf (stderr,"1.10  OVERLAND FLOW CALULATION, USING AGNPS\n");
     fprintf (stderr,"INPUT: slope= %f  Slope Length= %d Surface Condition= %f\n",
	    slope,slope_length,surface_condition);
     fprintf (stderr,"OUTPUT: Overland Flow Duration= %f\n\n",overland_flow_duration);
    }


   if(rflags.overland)
    {
     fprintf (stderr,"OVRLD routine: \n");
     fprintf (stderr,"   Input:   %f...slope   %d...slope_length\n",slope,slope_length);
     fprintf (stderr,"            %f...surface_condition\n",surface_condition);
     fprintf (stderr,"   Output:  %f...overland_flow_duration (seconds)\n",overland_flow_duration);
    }


   if(hydro_info)
     {
      hydro->flow_velocity_after=flow_velocity;
      hydro->slope=slope;
      hydro->surface_condition=surface_condition;
      hydro->power=power;
      hydro->slope_length=slope_length;
      hydro->overland_flow_duration=overland_flow_duration;
     }


   return(overland_flow_duration);   /* seconds */
}


/*
  Note:  The special case for surface_condition == -0.18 above is
	 the result of the feedlot routines.  That is the scc specifically
	 reserved for a grass waterway.  The only oddity is that we don't
	 limit the velocity to no more than 2.0 ft/sec
*/
