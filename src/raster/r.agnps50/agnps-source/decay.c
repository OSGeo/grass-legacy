/*************** Documentation Start ****************************
 
NAME:
 
 ************************************************************
 *                                                          *
 *	decay.c                                             *
 *                                                          *
 ************************************************************
 
  This is the place to put a routine that will decay the
  concentrations of the nutrients during the time they spend
  flowing down the channel.
 
 
 
SYNOPSIS:
***********************************************************/
 
/* header files included in this procedure */
 
#ifdef _DOS
 
 #include "input.h"  /* include initial & column structures */
 #include <stdlib.h>
 #include <math.h>
 #include "debugflg.h"

#else

#include "input.h"
#include <stdio.h>
#include <math.h>
#include "debugflg.h"

#endif

/* global structures used in this procedure */

extern COLUMN_INFOPTR *columndata; /* pointer to column data */

extern int sourceinfo;

extern SOURCEACCT2PTR	sourceact2ptr;
extern SOURCEACCT2	sourceact2;

extern FLAGS_TABLE    tflags;
extern CHEM_TABLE     ctable;

 
 
#ifdef _UNIX_K_AND_R
 
 void decay_nutrients(column_number, max_duration)
 
   int column_number;
   float max_duration;
 
#else
 
 void decay_nutrients(int column_number,
		     float max_duration)
 
#endif
 
{
 
/*
 
INPUT PARAMETERS:
 
  column_number (int)     current column number that we are processing
  max_duration      (float)   maximum cell max_duration for current cell
 
OUTPUT PARAMETERS:
 
 
 
 
LOCAL PARAMETERS:*/
 
RUNOFF_INFO *runoffpath;      /* pointer to channel info in current column */
CHANNEL_INFO *channel;
NONFEEDLOT_INFO *point_source=columndata[column_number]->nonfeedlot;
 
 
float bottom_nit=0.0;
float bottom_phos=0.0;
float bottom_cod=0.0;
float temp;
float temp_cod;
float temp_n;
float temp_p;

 
 
/****************************************************
 
DESCRIPTION:
 
RETURNS:
 
NOTES:
 
DATA STORES:
 
 
HISTORY:
 
Date    Bug#   Prog    Desc
 
 
 
SEE ALSO:
 
 
***************** Documentation End *********************/
 
  runoffpath  = columndata[column_number]->runoff;
  channel     = columndata[column_number]->channel;
 
  temp = max_duration * CU_FT_H2O_IN_LBS * 1.0e-6;
 
 
  /* Sum up all the point sources that enter at bottom */
 
 
   while(point_source != NULL)
     {
      if(point_source->enter_at_top == 0) /* pt src enters at bottom */
	{
	 bottom_nit += temp * point_source->water_discharge *
			  point_source->nitrogen_concentration;
	 bottom_phos += temp * point_source->water_discharge *
			  point_source->phosphorus_concentration;
	 bottom_cod += temp * point_source->water_discharge *
			  point_source->cod_concentration;
	 point_source=point_source->next;
	}
      }
 
 
 
 
  if(channel->agnps_decay == 1)  /* use agnps decay functions */
    {
     if(channel->channel_indicator == 0) /* Water cell */
      {
       channel->per_n_decay =   0.005 * max_duration;
       channel->per_p_decay =   0.012 * max_duration;
       channel->per_cod_decay = 0.003 * max_duration;
 
       if(channel->per_n_decay>10)
	  channel->per_n_decay=10;
       if(channel->per_p_decay>35)
	  channel->per_p_decay=35;
       if(channel->per_cod_decay>10)
	  channel->per_cod_decay=10;
      }
 
     else if(channel->channel_indicator == 1)
      {
       channel->per_n_decay = -103.2768 + (72.1406 * log(max_duration));
       channel->per_p_decay = -124.2822 + (72.1406 * log(max_duration));
       channel->per_cod_decay = -141.8737 + (72.1406 * log(max_duration));
      }
 
    else if(channel->channel_indicator == 4)
      {
       channel->per_n_decay =   25.53 + (0.047 * max_duration);
       channel->per_p_decay =   21.20 + (0.036 * max_duration);
       channel->per_cod_decay = 15.95 + (0.033 * max_duration);
      }
 
    else
      {
       channel->per_n_decay =   16.69 + (0.028 * max_duration);
       channel->per_p_decay =   13.86 + (0.020 * max_duration);
       channel->per_cod_decay = 10.43 + (0.018 * max_duration);
      }
 
    if (channel->per_n_decay >75)
       channel->per_n_decay=75;
 
    if(channel->per_p_decay > 75)
       channel->per_p_decay=75;
 
    if(channel->per_cod_decay >75)
       channel->per_cod_decay=75;
 
    }
 
 
/* Do we even need to decay the soluble_**_runoff, or is this not used.
   The output screen has the nutrients within, the variable that is used
   is the solubl_**_runoff.  In a sinkhole or regular cell, does the decay
   affect the nutrients within the cell, or just at the outlet of the cell.
   What is the true definition of the nutrients within the cell in the
   output screen.  The yield nutrients are the only ones that are summed
   in the routing. 

ANS:  The soluble nutrients are those that are generated within the cell
      and these are the ones that should be printed out.  There should be
      no decay involved, nor should there be any summation on this 
      variable. */
 

  if(sourceinfo)
    {
      temp_cod=runoffpath->soluble_cod_yield - bottom_cod;
      temp_n  =runoffpath->soluble_nitrogen_yield - bottom_nit;
      temp_p  =runoffpath->soluble_phosphorus_yield - bottom_phos;
      sourceact2ptr->sol_cod_decay += temp_cod * (channel->per_cod_decay/100);
      sourceact2ptr->sol_n_decay += temp_n * (channel->per_n_decay/100);
      sourceact2ptr->sol_p_decay += temp_p * (channel->per_p_decay/100);
    }



  if(columndata[column_number]->receiving_cell_position != 0)
   {
    runoffpath->soluble_nitrogen_yield = ((runoffpath->soluble_nitrogen_yield -
	 bottom_nit) * (1-(channel->per_n_decay/100))) + bottom_nit;  /* lbs      */

    runoffpath->soluble_phosphorus_yield = ((runoffpath->soluble_phosphorus_yield -
	 bottom_phos) * (1-(channel->per_p_decay/100))) + bottom_phos;   /* lbs      */

    runoffpath->soluble_cod_yield = ((runoffpath->soluble_cod_yield -
	 bottom_cod) * (1-(channel->per_cod_decay/100))) + bottom_cod;   /* lbs      */


    /* There is no decay on the soluble runoff.  This is because this
       amount is calculated for each cell seperately and is only for
       the overland portion, before channelized flow.  There is no decay
       that takes place before channelized flow. */

    runoffpath->soluble_nitrogen_runoff = ((runoffpath->soluble_nitrogen_runoff -
	 (bottom_nit/columndata[column_number]->area)))
	 + (bottom_nit/columndata[column_number]->area);   /* lbs/acre     */

    runoffpath->soluble_phosphorus_runoff = ((runoffpath->soluble_phosphorus_runoff -
	 (bottom_phos/columndata[column_number]->area))) +
	 (bottom_phos/columndata[column_number]->area);   /* lbs/acre      */

    runoffpath->cod_runoff = ((runoffpath->cod_runoff -
	 (bottom_cod/columndata[column_number]->area))) +
	 (bottom_cod/columndata[column_number]->area);   /* lbs/acre      */

   }

  else /* Cell is a sinkhole, no nutrients will flow out */

  {
   runoffpath->soluble_nitrogen_yield = 0.0;
   runoffpath->soluble_phosphorus_yield = 0.0;
   runoffpath->soluble_cod_yield = 0.0;
  }


  if (tflags.chem_table)
   {
    ctable.ct[column_number].N_dis_decay_factor = channel->per_n_decay;
    ctable.ct[column_number].P_dis_decay_factor = channel->per_p_decay;
    ctable.ct[column_number].COD_decay_fct =  channel->per_cod_decay;
   }
 
}

