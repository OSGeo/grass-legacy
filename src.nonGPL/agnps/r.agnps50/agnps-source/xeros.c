/************* Documentation Start **********
 
$NAME$
NAME: XEROS.C
 
$PATHS$
  hydrology\runoff
 
$1$
SYNOPSIS:
 
  This procedure calculates the soil erosion from an area, based
  on the USLE equation.
 
HEADER FILES:
*/
 
#ifdef _DOS

 #include "input.h"
 #include <stdio.h>
 #include <math.h>
 #include "debugflg.h"

#else

#include "input.h"
#include <stdio.h>
#include <math.h>
#include "debugflg.h"

#endif


/*
EXTERNAL VARIABLES:
*/

extern int sedimentinfo;

extern FLAGS_BASIC     bflags;
extern FLAGS_ROUTINE   rflags;


#ifdef _UNIX_K_AND_R

xeros(slope_shape_code, average_land_slope, slope_length,
       storm_energy_intensity, soil_erodibility_factor, cropping_factor,
       practice_factor, upland_erosion)

      int slope_shape_code;
      float average_land_slope;
      int   slope_length;
      float storm_energy_intensity;
      float soil_erodibility_factor;
      float cropping_factor;
      float practice_factor;
      float *upland_erosion;
 
#else
 
xeros(int   slope_shape_code,
      float average_land_slope,
      int   slope_length,
      float storm_energy_intensity,
      float soil_erodibility_factor,
      float cropping_factor,
      float practice_factor,
      float *upland_erosion)
 
#endif
{
 
 /*
 INPUT PARAMETERS:
 
  slope_shape_code        (int)   unitless
  average_land_slope      (float) percent
  slope_length            (int)   feet
  storm_energy_intensity  (float) unitless
  soil_erodibility_factor (float) unitless
  cropping_factor         (float) unitless
  practice_factor         (float) unitless
 
 OUTPUT PARAMETERS:
 
  upland_erosion          (float) tons
 
 LOCAL VARIABLES:
 */
 
 float  slope_length_exponent;   /* unitless */
 double slope_length_factor;     /* unitless */
 double slope_steepness_factor;  /* unitless */
 float  slope_shape_factor;      /* unitless */
 int    error_code;              /* unitless */
 
 /*
 RETURNS:
  No return value
 
 NOTES:
  This equation is based on the Universal Soil Loss Equation.  See the
  AGNPS flow documents and the data dictionary, along with source
  documentation.
 
 DATA STORES:
 
 
 
 HISTORY:
       Date	Bug#	Prog	Desc
 
       10/22/92 none	MAK	finished coding
 
 SEE ALSO:
 
 *********** Documentation End *************/
 
 
 /*** 2.1.1 LOOKUP SLOPE SHAPE FACTOR ****
 
  This procedure looks up the proper factor given the users input for a
  slope shape factor.  The factors given below were calculated using
  complex slope factors from Wischmeier and Smith, 1978.  Predicting
  rainfall erosion losses. U.S. Dep. Agric. Agriculture Handbook. 537
  page 58.  Also see the AGNPS Model Documentation for more info.
 
  1= uniform (flat) slope shape
  2= convex slope shape
  3= concave slope shape
 
  */
 
 error_code=0;
 if((slope_shape_code<1) || (slope_shape_code>3)) error_code=1;
 
 switch (slope_shape_code)
  {
   case 1 :  slope_shape_factor = 1.0;
		break;
   case 2 :  slope_shape_factor = 1.3;
		break;
   case 3 :  slope_shape_factor = 0.88;
		break;
   default:  slope_shape_factor = 1.0;
  }
 
 /*** 2.1.2 CALCULATE SLOPE LENGTH EXPONENT ***
 
  This procedure calculates a slope length exponent based on the average
  land slope.  This variable goes into calcualting the slope length factor
  below.
 
 */
 
 slope_length_exponent = 0.4;

 if(average_land_slope < 4)
	 slope_length_exponent = 0.3;
 
 if(average_land_slope >= 5)
	 slope_length_exponent = 0.5;
 
 
 /*** 2.1.3 CALCULATE SLOPE LENGTH FACTOR ***
 
  The slope length factor is one of the variables that is used in the
  USLE equation.  This is from the same article as described above.
 */
 
 slope_length_factor = pow((slope_length/72.6),
	      slope_length_exponent);
 
 /*** 2.1.4 CALCULATE SLOPE STEEPNESS FACTOR ***
 
  The slope steepness factor is another variable in the USLE equation.
 */

 
 slope_steepness_factor = (430.0 *
	      pow((average_land_slope/100.0),2.0) +
	      30.0 * (average_land_slope/100.0) + 0.43)/6.574;
 
 /*** 2.1 CALCULATE UPLAND EROSION ***
 
 This equation is the actual calcualtion of the USLE equation. */
 
 
 *upland_erosion = storm_energy_intensity *
		   soil_erodibility_factor *
		   slope_length_factor * slope_steepness_factor *
		   cropping_factor * practice_factor *
		   slope_shape_factor;
 
 if((sedimentinfo) || (bflags.sediment))
  {
   fprintf (stderr,"****\n");
   fprintf (stderr,"1.7 CALCUALTE UPLAND EROSION\n");
   fprintf (stderr,"INPUTS: Shape Code= %d  Land Slope= %f (per) Slope Length= %d (feet)\n",
	    slope_shape_code,average_land_slope,slope_length);
   fprintf (stderr,"        Storm EI= %f Soil Erodibility= %f  Cropping Factor= %f\n",
      storm_energy_intensity,soil_erodibility_factor,cropping_factor);
   fprintf (stderr,"         Practice Factor= %f\n",practice_factor);
   fprintf (stderr,"OUTPUTS:  Upland Erosion= %f (tons/acre)  Error Code= %d\n\n",
	  *upland_erosion,error_code);
  }


  if (rflags.xeros)
   {
    fprintf (stderr,"XEROS routine: \n");
    fprintf (stderr,"   Input:  %d...slope_shp_code     %f...ave_land_slope\n",slope_shape_code,average_land_slope);
    fprintf (stderr,"           %d...storm_ei           %f...soils_erod_fact\n",storm_energy_intensity,soil_erodibility_factor);
    fprintf (stderr,"           %f...c_factor           %f...p_factor\n",cropping_factor,practice_factor);

    fprintf (stderr,"   Output: %f...upland_erosion\n",*upland_erosion);
   }



 return;
}
