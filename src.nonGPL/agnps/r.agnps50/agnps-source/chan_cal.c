/*************************** Documentation Start ******************
 
NAME: CHAN_CAL.C
 
SYNOPSIS:  This procedure calculates the channel width and depth for each
	   increment in the hydrograph.  The width and depth are calculated
	   at the top and the bottom of the cell.  The shear stress and
	   velocity are also calculated in this routine.

** NOTE: **  THESE CHANNEL CALCULATIONS ARE FOR TR-55 METHOD !!!

HEADER FILES:
*/
#ifdef _DOS
 
 #include <math.h>
 #include <stdio.h>
 #include "input.h"
 #include "debugflg.h"

#else

#include <math.h>
#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif
 
 
/* FUNCTION PROTOTYPES:  No procedures called from this procedure */
 
/* GLOBAL VARIABLES: */
 
extern COLUMN_INFOPTR *columndata;
extern INITIAL_INFO   *initialptr;
extern int debuginfo;
 
/*************************************************************/
 
/* INPUT PARAMETERS: (Also See Data Dictionary )*/
 
#ifdef _UNIX_K_AND_R
 
void calculate_channel(col, upstream_flow_rate, downstream_flow_rate,
	    velocity_in, velocity_out, channel_width_in,
	    channel_width_out, shear_stress_in, shear_stress_out,
	    channel_depth_out)
 
   int col;
   float upstream_flow_rate;
   float downstream_flow_rate;
   float *velocity_in;
   float *velocity_out;
   float *channel_width_in;
   float *channel_width_out;
   float *shear_stress_in;
   float *shear_stress_out;
   float *channel_depth_out;
 
#else
 
void calculate_channel(
   int col,                   /* column number that we are processing */
   float upstream_flow_rate,  /* flow rate at top of cell */
   float downstream_flow_rate,/* flow rate at the bottom of the cell */
   float *velocity_in,        /* velocity at the top of the cell */
   float *velocity_out,       /* velocity at the bottom of the cell */
   float *channel_width_in,   /* channel width at the top of the cell */
   float *channel_width_out,  /* channel width at the bottom of the cell */
   float *shear_stress_in,    /* shear stress at the top of the cell */
   float *shear_stress_out,   /* shear stress at the bottom of the cell */
   float *channel_depth_out)  /* channel depth at the bottom of the cell */
 
#endif
 
{
 
 /* LOCAL VARIABLES: */
 
 CHANNEL_INFO *pCHANNELPTR=columndata[col]->channel;
			       /* pointer to the channel structure */
 float nDECIMAL_CHANNEL_SLOPE; /* channel slope as a decimal number      */
 float nDECIMAL_SIDE_SLOPE;    /* channel side slope as a decimal number */
 float nCHANNEL_DEPTH_IN;      /* channel depth at top of cell */
 
 
 /**********************************************************************
 DESCRIPTION:  This procedures calcualtes the channel depth and width
	      for each increment of the hydrograph.  There are two
	      different ways of calculating these values.  The first is
	      use the TR55 method of calculation which assumes the use
	      of a rectangular channel.  This means that as the flow
	      rate varies through the hydrograph, the depth of the
	      channel varies, but the width stays the same.  In order
	      to implement this, we set the width constant regardless
	      of the flow rate throught the channel.  The width is set
	      to the width the user has input or the width calculated
	      by the geomorphic relationships, depending on which option
	      the user has selected (Geomorphic/ Non-geomorphic ).  When
	      AGNPS is selected as the method for calculating the peak
	      flow, the channel is assumed to be triangular.  This means
	      that both the width and the depth will vary when the flow
	      rate varies.  The width is calculated according to the flow
	      rate and the channel depth is calculated by using the
	      triangular relationships between the side slope and the
	      channel depth.  This allows both the depth and the width to
	      change as the flow rate in the channel changes.
 
 
 RETURNS: velocity_in
	 velocity_out
	 channel_width_in
	 channel_width_out
	 shear_stress_in
	 shear_stress_out
	 channel_depth_out
 
 NOTES:
 
 DATA STORES:
 
 HISTORY:
	  Date		Bug#	Prog	Desc
	  9/25/93       none    MAK	Finished coding
	  6/14/93	1	MAK	Fixed problem with water cells
 
 
 SEE ALSO:
	 cellcalc.c, newsoil.c
 
 */
 /*** CALCULATE DECIMAL EQUIVALENTS FOR CHANNEL SIDE SLOPE AND SLOPE ***/
 
 nDECIMAL_SIDE_SLOPE = ((float) pCHANNELPTR->channel_side_slope)
		     /100.0;
 
 nDECIMAL_CHANNEL_SLOPE    = ((float) pCHANNELPTR->channel_slope)
		     /100.0;
 
 
 
/* Calculate the channel depth and width */
 
 if(initialptr->calc_method == TR55) /* TR55 method is being used */
  {
 
    if(upstream_flow_rate == 0)
      {
	/* If there is no flow rate at the top of the cell, there can't be
	any depth in the channel */
	nCHANNEL_DEPTH_IN=0.0;
      }
 
    else /* there is a flow rate in the channel */
      {
       nCHANNEL_DEPTH_IN = pow((upstream_flow_rate*pCHANNELPTR->channel_mannings)
			 /(pCHANNELPTR->width*
			 pow((pCHANNELPTR->channel_slope/100),0.5)*1.49),0.6);
      }

    *channel_depth_out =pow((downstream_flow_rate*pCHANNELPTR->channel_mannings)
			  /(pCHANNELPTR->width*
			  pow((pCHANNELPTR->channel_slope/100),0.5)*1.49),0.6);
    /* Set width equal to user input width.  This is the same for all
       increments */

    *channel_width_in  = pCHANNELPTR->width;

    *channel_width_out = pCHANNELPTR->width;
  }
 else /* AGNPS method is being used */
  {

   *channel_width_in  = 2.05 * pow(nDECIMAL_SIDE_SLOPE,-0.625) * pow(1.0
			  + nDECIMAL_SIDE_SLOPE * nDECIMAL_SIDE_SLOPE, 0.125) *
			  pow(upstream_flow_rate * pCHANNELPTR->channel_mannings
			  /pow(nDECIMAL_CHANNEL_SLOPE, 0.5), 0.375);

   *channel_width_out = 2.05 * pow(nDECIMAL_SIDE_SLOPE,-0.625) * pow(1.0
			  + nDECIMAL_SIDE_SLOPE * nDECIMAL_SIDE_SLOPE, 0.125) *
			  pow(downstream_flow_rate * pCHANNELPTR->channel_mannings
			  /pow(nDECIMAL_CHANNEL_SLOPE, 0.5), 0.375);


   nCHANNEL_DEPTH_IN = nDECIMAL_SIDE_SLOPE * *channel_width_in / 2.0;

   *channel_depth_out = nDECIMAL_SIDE_SLOPE * *channel_width_out / 2.0;

  }

  if((pCHANNELPTR->channel_indicator == 0) ||
      (columndata[col]->soil->soil_type == 5))
    {
      nCHANNEL_DEPTH_IN = pCHANNELPTR->max_depth;

      /* This change was added due to the bug that was found by Al, Jim, and
	 Gary.  This bug number is A-1.  */

      if(pCHANNELPTR->max_depth != 0.0)
	   *channel_depth_out = pCHANNELPTR->max_depth;

      /* Finished change with bug A-1 */

      *channel_width_in = pCHANNELPTR->width;
      *channel_width_out = pCHANNELPTR->width;

    }



 /*** CALCULATE SHEAR STRESS AT TOP AND BOTTOM OF CELL ***/

 if(initialptr->calc_method == TR55)
  {
   *shear_stress_in = 62.4 * nCHANNEL_DEPTH_IN  *
		       (pCHANNELPTR->channel_slope/100);

   *shear_stress_out = 62.4 * *channel_depth_out *
		       (pCHANNELPTR->channel_slope/100);

  }
 else
  {
   *shear_stress_in  = 62.4 * pow(nDECIMAL_SIDE_SLOPE, 0.375)/pow(2.0 * pow(1.0
		       + nDECIMAL_SIDE_SLOPE * nDECIMAL_SIDE_SLOPE, 0.5), 0.75) *
		       pow(nDECIMAL_CHANNEL_SLOPE, 0.8125) * pow(pCHANNELPTR->channel_mannings
		       * upstream_flow_rate/1.49, 0.375);

   *shear_stress_out = 62.4 * pow(nDECIMAL_SIDE_SLOPE, 0.375)/pow(2.0 * pow(1.0
		       + nDECIMAL_SIDE_SLOPE * nDECIMAL_SIDE_SLOPE, 0.5), 0.75) *
		       pow(nDECIMAL_CHANNEL_SLOPE, 0.8125) * pow(pCHANNELPTR->channel_mannings
		       * downstream_flow_rate/1.49, 0.375);
  }




  if (pCHANNELPTR->channel_mannings == 0.0)
   {
    *velocity_in  = 0.0;
    *velocity_out = 0.0;
   }

  else if(pow(2.0 * pow(1.0 +nDECIMAL_SIDE_SLOPE * nDECIMAL_SIDE_SLOPE
		   , 0.5),0.5) == 0.0)
   {
    *velocity_in  = 0.0;
    *velocity_out = 0.0;
   }


  else

   {
    if(initialptr->calc_method == TR55)
     {
      if(upstream_flow_rate == 0)
       {
	*velocity_in = 0.0;
       }
      else
       {
	*velocity_in = upstream_flow_rate / (*channel_width_in *
	      nCHANNEL_DEPTH_IN);
       }

      *velocity_out = downstream_flow_rate / (*channel_width_out *
	   *channel_depth_out);
     }


    else /* use agnps methods of calculation */
     {

      if((pCHANNELPTR->channel_indicator == 0) ||
	 (columndata[col]->soil->soil_type == 5))
       {
         if (*channel_width_out * *channel_depth_out > 0.0)
		*velocity_out = downstream_flow_rate / (*channel_width_out *
		   *channel_depth_out);

         else
           *velocity_out = 0.0;

       }
     else
       {
	 *velocity_in = pow(1.49/pCHANNELPTR->channel_mannings, 0.75) *
	    pow(nDECIMAL_SIDE_SLOPE, 0.25)/pow(2.0 * pow(1.0 + nDECIMAL_SIDE_SLOPE
	    * nDECIMAL_SIDE_SLOPE, 0.5), 0.5) * pow(nDECIMAL_CHANNEL_SLOPE, 0.375)
	    * pow(upstream_flow_rate, 0.25);

	 *velocity_out = pow(1.49/pCHANNELPTR->channel_mannings, 0.75) *
	    pow(nDECIMAL_SIDE_SLOPE, 0.25)/pow(2.0 * pow(1.0 + nDECIMAL_SIDE_SLOPE
	    * nDECIMAL_SIDE_SLOPE, 0.5), 0.5) * pow(nDECIMAL_CHANNEL_SLOPE, 0.375)
	    * pow(downstream_flow_rate, 0.25);
       }
     }
   }

} /* End of procedure */
