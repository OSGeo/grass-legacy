 
/*************** Documentation Start ****************************
 
NAME:
 
 ************************************************************
 *                                                          *
 *	length.c                                            *
 *                                                          *
 ************************************************************
 
 This proc reads in values for the channel, then checks them to
 see if they fall in the correct range.
 
SYNOPSIS:
***********************************************************/
 
/* header files included in this procedure */
 
#ifdef _DOS
 
 #include <mem.h>    /* include memory routines */
 #include <math.h>   /* include math routines */
 #include <stdio.h>
 #include "input.h"  /* include initial & column structures */
 #include "binary.h"
 #include "debugflg.h"

#else

#include <math.h>   /* include math routines */
#include <stdio.h>
#include "input.h"  /* include initial & column structures */
#include "binary.h"
#include "debugflg.h"

#endif
 
/* global structures used in this procedure */
 
extern COLUMN_INFOPTR *columndata; /* pointer to column data */
extern INITIAL_INFOPTR initialptr; /* pointer to the initial structure */
extern int debuginfo;              /* indicator for debug statements */
extern HYDRO_REC_PTR   hydro;
extern int hydro_info;

extern FLAGS_BASIC    bflags;
extern FLAGS_ROUTINE  rflags;

#ifdef _UNIX_K_AND_R

void length(column_number)

   int column_number;

#else

void length(int column_number)

#endif
{

/*

INPUT PARAMETERS:

  column_number (int)     current column number that we are processing

OUTPUT PARAMETERS:




LOCAL PARAMETERS:*/

CHANNEL_INFO *chanpath;      /* pointer to channel info in current column */
SLOPE_INFO   *slopepath;     /* pointer to slope info in current column */
SOIL_INFO    *soilpath;      /* pointer to soil info in current column */

/****************************************************

DESCRIPTION:

RETURNS:
 
NOTES:
 
   channel indicators
   0 = water
   1 = no discernable channel
   2 = drainage ditch
   3 = road ditch
   4 = grass waterway
   5 = perennial stream
   6 = other
 
DATA STORES:
 
R=Read in
W=Output
U=Updated
 
 
[R,U]  columndata[column_number]->channel->channel_slope
[R,U]  columndata[column_number]->channel->channel_side_slope
[R,U]  columndata[column_number]->channel->channel_length
[R]    columndata[column_number]->channel->length_coef
[R]    columndata[column_number]->channel->length_exp
[R]    columndata[column_number]->channel->depth
 
[R]    columndata[column_number]->channel->depth_coef
[R]    columndata[column_number]->channel->depth_exp
[R]    columndata[column_number]->channel->width
[R]    columndata[column_number]->channel->width_coef
[R]    columndata[column_number]->channel->width_exp
[R,U]  columndata[column_number]->channel->channel_mannings
[R,U]  columndata[column_number]->cell_mannings
 
 
HISTORY:
 
Date    Bug#   Prog    Desc
 
7/28/92  S12   MAK    Fix bug with depth problem in TR55.
 

SEE ALSO:

loop1.c
 
***************** Documentation End *********************/
 
    chanpath  = columndata[column_number]->channel;
    slopepath = columndata[column_number]->slope;
    soilpath  = columndata[column_number]->soil;
 
 
   /* set cell and channel mannings to .990 if the soil type is water */
 
 
    if ((chanpath->channel_indicator <= 0) || (soilpath->soil_type == 5))
	{
	chanpath->channel_mannings = 0.09;
	columndata[column_number]->overland_mannings = 0.09;
	}
 
 
   /* This section calculates the channel characteristics, if they have not
      been input by use of the normal triangular methods(not geomorphic).
      This is selected in the spreadsheet.  If the side slope, width, &
      depth are all input by the user, the program recalculates the side
      slope to fit the triangle.  This is only done for the TR55 way of
      calculating peak flow, because agnps does its own calulations for
      width and depth. */
 
      if(bflags.channel)
       {
	fprintf (stderr,"****\n");
	fprintf (stderr,"1.6 CALCULATE CHANNEL INFORMATION\n");
       }
 
 
    if (initialptr->calc_method == TR55)
	{
	if (initialptr->geomorphic_calc == NONGEOMORPHIC)
	    {
	     if (bflags.channel)
		fprintf (stderr,"  TR55   NON-GEOMORPHIC CALCULATIONS\n");
 
	if((chanpath->channel_indicator != 0 ) &&
	   (soilpath->soil_type != 5))
 
	     chanpath->channel_side_slope =
				(chanpath->depth * 2 /chanpath->width)*100;
 
 
	    }  /* end the if for the type of calculations */
	}      /* end the if for the type of peak flow calculations */

 
 
  /* I believe that it is used to get a value for side slope if the
     user has selected TR55 and geomorphic.  Note that if the user
     has selected nongeomorphic and TR55, then the value would have
     been calculated above.  If the user put in a value, then this
     will not be calculated */
 
	   if(initialptr->calc_method == TR55)
	    {
 
	     if(initialptr->geomorphic_calc == GEOMORPHIC)
	      {
		if(bflags.channel)
		  fprintf (stderr,"    TR55   GEOMORPHIC CALCULATION\n");
 
 
		chanpath->depth = chanpath->depth_coef *
		   pow(columndata[column_number]->area,chanpath->depth_exp);
 
 
		chanpath->width = chanpath->width_coef *
		   pow(columndata[column_number]->area,chanpath->width_exp);
 
 
		/* BUG A-7 */
 
		if((chanpath->channel_indicator == 0)
		   || (soilpath->soil_type ==5))
		  {
		    if(chanpath->width == 0.0)
		     chanpath->width = (sqrt(columndata[column_number]->area
			    * ACRES_TO_SQ_FEET))/2;
 
		     if(chanpath->depth == 0.0 )
		       {
			  chanpath->depth = 0.5;
			  chanpath->bank_depth = 1.0;
		       }
 
 
		    chanpath->channel_side_slope =
				(chanpath->depth * 2 /chanpath->width)*100;
		  }
		/* Finish BUG A-7 */
 
	      } /* end if geomorphic calc */
	    } /* end if calc method is TR55 */
 
 
   /* set minimum value of channel side slope to .1 */
 
    if ((chanpath->channel_side_slope<0.1) &&
				(chanpath->channel_side_slope>0))
	 {
 
	   if(bflags.channel)
	      fprintf (stderr,"Minimum channel side slope has been set to 0.1\n");
 
 
	   chanpath->channel_side_slope = 0.1;
	 }
 
  /* user has not input channel slope, calculate channel slope */
 
    if (chanpath->channel_slope == 0)
      {
 
       if(bflags.channel)
	     fprintf (stderr,"User has not input channel slope, calculate\n");
 
       chanpath->channel_slope = slopepath->average_land_slope/2;
      }
 
  /* user has not input side slope, set value to 10 */
 
    if (chanpath->channel_side_slope == 0)
      {
 
	if(bflags.channel)
	   fprintf (stderr,"User has not input side slope, set to 10 percent\n");
 
	chanpath->channel_side_slope = 10;
      }
 
  /* set maximum value of side slope to 100 */
 
    if (chanpath->channel_side_slope > 100)
       {

       if(bflags.channel)
	fprintf (stderr,"Maximum channel side slope has been set to 100\n");
 
	chanpath->channel_side_slope = 100;
       }
 
  /* set minimum value of channel slope to .1. */
 
    if (chanpath->channel_slope < 0.1)
      {
 
	if(bflags.channel)
	   fprintf (stderr,"Minimum Channel Slope has been set to 0.1\n");
 
	chanpath->channel_slope = 0.1;
      }
 
 

    if(hydro_info)
     {
      hydro->channel_side_slope_after=chanpath->channel_side_slope;
      hydro->channel_slope=chanpath->channel_slope;
      hydro->depth=chanpath->depth;
      hydro->width=chanpath->width;
      hydro->bank_depth=chanpath->bank_depth;
      hydro->width_coef=chanpath->width_coef;
      hydro->width_exp=chanpath->width_exp;
      hydro->depth_coef=chanpath->depth_coef;
      hydro->depth_exp=chanpath->depth_exp;
     }


  /* set values if the channel is water */
 
     if (chanpath->channel_indicator == 0)
	{
	 if(bflags.channel)
	  fprintf (stderr,"WATER CELL, VALUES ADJUSTED\n");
 
	 if(chanpath->channel_slope == 0.0);
		 chanpath->channel_slope      = 0.001;/* This slope is in % */
 
	 if(initialptr->calc_method == CREAMS)
	    chanpath->channel_slope = 0.001;
 
 
	 chanpath->channel_side_slope = 0.1;
 
 
	 if((initialptr->calc_method!=TR55) &&
	    (initialptr->geomorphic_calc !=GEOMORPHIC))
	      chanpath->channel_length     = 0.0;

/*
  BUG A-7
 
	 if(initialptr->calc_method == CREAMS)
	     chanpath->bank_depth = 0.5;

 
 
	 if((initialptr->calc_method == TR55) &&
	    (initialptr->geomorphic_calc == GEOMORPHIC))
	     chanpath->depth= 0.5;
 
 
	BUG A-7
	 if(initialptr->calc_method == CREAMS)
	     chanpath->width = (sqrt(columndata[column_number]->area
			       * ACRES_TO_SQ_FEET))/2;
	 end bug A-7
       */
 
       /* BUG A-9 */
 
	if(initialptr->calc_method == CREAMS)
	  {
	   chanpath->depth = 0.0;
	   chanpath->width = 0.0;
	  }
	/* Finish Bug A-9 */
 
 
	if( columndata[column_number]->sourcelist == NULL)
	{
	 if(chanpath->depth == 0.0)
	    {
	      chanpath->bank_depth = 1.0;
	      chanpath->depth = 0.5;
	    }
	}
 
 
	 if(chanpath->width == 0.0)
		 chanpath->width = (sqrt(columndata[column_number]->area
			    * ACRES_TO_SQ_FEET))/2;
 
	}
 
 
  /* set values if there is no discernable channel */
 
    if(chanpath->channel_indicator == 1)
	{
 
	 /* BUG A-8 */
 
	 if(initialptr->calc_method == CREAMS)
	   {
	    chanpath->width = 0.0;
	    chanpath->depth = 0.0;
	   }
	 /* Finish BUG A-8 */

	 if(bflags.channel)
	   fprintf (stderr,"NO DISCERNABLE CHANNEL, VALUES ADJUSTED\n");

	if (chanpath->channel_slope == 0.0)
	  chanpath->channel_slope    = slopepath->average_land_slope/2;

	if(initialptr->calc_method == CREAMS)
	  chanpath->channel_slope = slopepath->average_land_slope/2;

	chanpath->channel_mannings = columndata[column_number]->
							overland_mannings;

	if (chanpath->channel_slope < 0.1)
	    chanpath->channel_slope = 0.1;


	chanpath->channel_side_slope = 10.0;

	if((initialptr->calc_method != TR55) &&
	   (initialptr->geomorphic_calc != NONGEOMORPHIC))
	    chanpath->channel_length     = 0.0;



	}

    if(hydro_info)
      {
	hydro->calc_method=initialptr->calc_method;
	hydro->geomorphic_calc=initialptr->geomorphic_calc;
	hydro->channel_indicator=chanpath->channel_indicator;
	hydro->soil_type=soilpath->soil_type;
	hydro->water_depth=chanpath->depth;
	hydro->water_slope=chanpath->channel_slope;
	hydro->water_bank_depth=chanpath->bank_depth;
	hydro->water_slope=chanpath->channel_slope;
	hydro->no_depth=chanpath->depth;
	hydro->no_width=chanpath->width;
	hydro->no_slope=chanpath->channel_slope;
      }


    if (rflags.length)
       {
        fprintf (stderr,"LENGTH routine: \n");
        fprintf (stderr,"   Input:  %d...column_number\n");
        fprintf (stderr,"   Output: %f...chan_length    %f...chan_width \n",chanpath->channel_length,chanpath->width);
        fprintf (stderr,"           %f...chan_depth     %f...chan_slope \n",chanpath->depth,chanpath->channel_slope);
        fprintf (stderr,"           %f...chan_side_slope \n",chanpath->channel_side_slope);
       }


    if (bflags.channel)
       {
	fprintf (stderr,"OUTPUTS: slope= %f  side slope= %f \n",chanpath->channel_slope,chanpath->channel_side_slope);
	fprintf (stderr,"         width= %f  length= %f depth= %f \n\n",
		chanpath->width,chanpath->channel_length,chanpath->depth);
       }
 
    return;
}
