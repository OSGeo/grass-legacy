/******************** Documentation Start ************************

NAME: RECROUTE.C

SYNOPSIS: This procedure does the actual routing and the accumulation of
	  cells flowing into the current cell.

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

/* GLOBAL VARIABLES */

extern COLUMN_INFOPTR  *columndata;        /* main data structure */
extern INITIAL_INFOPTR initialptr;
extern SINKHOLE first_sinkhole;

extern FLAGS_BASIC     bflags;
extern FLAGS_ROUTINE   rflags;
extern FLAGS_TABLE     tflags;
extern HYDRO_TABLE     htable;
extern CHEM_TABLE      ctable;

int sinkhole;

/*
FUNCTION PROTOTYPES:
*/

#ifdef _UNIX_K_AND_R
  float path();

#else
 void cellcalc( int column );
 static void  recursive_route( int column );
 static void  prepare_to_route( int column );
 static float path(int aspect, int rec_aspect);

#endif


/* INPUT PARAMETERS:
*/

#ifdef _UNIX_K_AND_R

void routing_loop(outlet_column)

  int outlet_column;

#else

void routing_loop( int outlet_column)

#endif

{

/* LOCAL VARIABLES:
*/

SINKHOLEPTR cur_sinkhole;

/**************************************************************
DESCRIPTION:


RETURNS:

NOTES:

DATA STORES:

HISTORY:

	Date		Bug#	Prog	Desc.
	9/25/92         none	MAK	Finished COding
	9/15/93		C0005	MAK	Fixed geo. depth and width calc.

SEE ALSO:

*/


    if (rflags.routing)
     {
      fprintf (stderr,"ROUTING routine: \n");
      fprintf (stderr,"   Input:  %d...outlet_column\n",outlet_column);
      fprintf (stderr,"   Output: none\n");
     }

    sinkhole = FALSE;

    if(rflags.recursive)
      {
       fprintf (stderr,"RECURSIVE routine: column...%d",outlet_column);
      }

    recursive_route( outlet_column );

    cur_sinkhole = first_sinkhole.next;

    while (cur_sinkhole != NULL)
	{
	sinkhole = TRUE;
 	recursive_route(cur_sinkhole->col_number);
	cur_sinkhole = cur_sinkhole->next;
	}
}

#ifdef _UNIX_K_AND_R
recursive_route(column)
     int column;
 
#else
 
void recursive_route(int column)
 
#endif
 
{
SOURCEPTR temp;
extern int debuginfo;
 
    temp = columndata[column]->sourcelist;

    while (temp != NULL)
	{
        if (rflags.recursive)
         {
          fprintf (stderr,"-->%d",temp->source_column);
         }
	recursive_route(temp->source_column);
	temp = temp->next;
	}

    if (rflags.recursive)
     {
      fprintf (stderr,"...routing path                 \n");
     }

    if(bflags.routing)
     {
       fprintf (stderr,"*****************************************************\n");
       fprintf (stderr,"   Routing Cell # %d %d\n",columndata[column]->cell_number,
	   columndata[column]->cell_division);
       fprintf (stderr,"*****************************************************\n");
     }
 
 
#ifdef _DOS
    fprintf (stderr,"  %d  ... Executing Routing Loop            \r",column);
#endif

    prepare_to_route( column );
    cellcalc( column );

}
 
 
 
 
/* ======================================================================= */
/*                                                                         */
/*   In this routine, determine everything that requires knowledge of the  */
/*   longest flow path down to the bottom of the current cell.             */
/*                                                                         */
/* ======================================================================= */
 
#ifdef _UNIX_K_AND_R
 
prepare_to_route(column)
 
   int column;
 
#else
 
void prepare_to_route( int column )
 
#endif
 
{
COLUMN_INFOPTR longest_path;
COLUMN_INFOPTR curcolumn   = columndata[column];
SOURCEPTR      temp        = curcolumn->sourcelist;
SOURCEPTR      cells_in    = curcolumn->sourcelist;
CHANNEL_INFO   *curchannel = curcolumn->channel;
ACCUM_VALUES   *curaccum   = curcolumn->accumulated;
RUNOFF_INFO    *currunoff  = curcolumn->runoff;
 
float          temp_interior_length    = 0.0;
float          greatest_time;
float          greatest_length;
float          greatest_depth;
float          longest_duration;
float          longest_interior_length = 0.0;
float          long_drainage_area;
int            j;
 
 
/* For a NONPRIMARY cell, we step through the linked list of source cells  */
/*  to accumulated some values and to determine which gives the longest    */
/*  flow path out to the bottom to the current cell.  Variables named temp */
/*  depend on the current source cell and varibles named longest record,   */
/*  as we go along, which has given thet longest channel.  Much of the     */
/*  complication comes from generating a default if the user gives 0.0 for */
/*  the channel length of the current cell.                                */



    if (temp != NULL)                                     /* NONPRIMARY */
	{
	longest_path = columndata[temp->source_column];

        if (tflags.hydro_table)
         {
          htable.ht[column].next_routing_cell = longest_path->cell_number;
         }

	while (temp != NULL)
	    {
	    if (curchannel->channel_length == 0.0)    /* we supply default */
		{
		temp_interior_length = 1.25 *
			  sqrt( curcolumn->area * ACRES_TO_SQ_FEET ) *
			  path( columndata[temp->source_column]->
				flow_direction,	curcolumn->flow_direction );
		}
 
	    if (columndata[temp->source_column]->accumulated->
		      length_to_bottom + temp_interior_length
					>
		      longest_path->accumulated->length_to_bottom +
		      longest_interior_length)
		{
		longest_path = columndata[temp->source_column];
		longest_interior_length = temp_interior_length;
 
		}
 
 
	    curaccum->drainage_area +=                           /* acres */
			columndata[temp->source_column]->
			accumulated->drainage_area;
 
 
	    curaccum->cn_area +=
			columndata[temp->source_column]->
			accumulated->cn_area;

	    curaccum->runoff_vol_above  +=                       /* ft^3 */
			columndata[temp->source_column]->
			accumulated->runoff_vol_below;
 
 

	    curaccum->runoff_flow_above +=                       /* cfs */
			columndata[temp->source_column]->
			accumulated->runoff_flow_below;

	    curaccum->sum_psource_flows +=                       /* cfs */
			columndata[temp->source_column]->
			accumulated->sum_psource_flows;

	    curaccum->sum_psource_volumes +=                     /* ft^3 */
			columndata[temp->source_column]->
			accumulated->sum_psource_volumes;

            if (tflags.chem_table)
             {
              ctable.ct[column].N_dis_into +=                    /* lbs */
                			columndata[temp->source_column]->
			                   runoff->soluble_nitrogen_yield;

              ctable.ct[column].P_dis_into +=
			                columndata[temp->source_column]->
			                   runoff->soluble_phosphorus_yield;

              ctable.ct[column].COD_into_lbs +=
			                columndata[temp->source_column]->
			                   runoff->soluble_cod_yield;
             }

	    currunoff->soluble_nitrogen_yield +=                 /* lbs */
			columndata[temp->source_column]->
			runoff->soluble_nitrogen_yield;

	    currunoff->soluble_phosphorus_yield  +=              /* lbs */
		        columndata[temp->source_column]->
		        runoff->soluble_phosphorus_yield;

	    currunoff->soluble_cod_yield +=
			columndata[temp->source_column]->        /* lbs */
			runoff->soluble_cod_yield;


	    for (j=1; j<=5; j++)
		currunoff->available_sediment[j] +=              /* lbs */
			columndata[temp->source_column]->
			runoff->sediment_yield[j] * 2000.0;

	    /* Added to sum the amount of n and p in sediment from
	       each cell */


	    currunoff->total_n_cell_outlet +=
		       columndata[temp->source_column]->runoff->
		       total_n_cell_outlet;

	    currunoff->total_p_cell_outlet +=
		       columndata[temp->source_column]->runoff->
		       total_p_cell_outlet;

            if (tflags.chem_table)
             {
              ctable.ct[column].N_att_into = currunoff->total_n_cell_outlet;
              ctable.ct[column].P_att_into = currunoff->total_p_cell_outlet;
             }

	    curcolumn->pesticide->soluble_pest_exit +=
			columndata[temp->source_column]->
			pesticide->soluble_pest_exit;

	    curcolumn->pesticide->sediment_pest_exit +=
			columndata[temp->source_column]->
			pesticide->sediment_pest_exit;

	    temp = temp->next;
	    }

	if (curchannel->channel_length == 0.0)
	    curchannel->channel_length = longest_interior_length;

	if(initialptr->geomorphic_calc == GEOMORPHIC)
	 {
	   curaccum->length_to_bottom = pow((curaccum->drainage_area + curcolumn->area),
			      curchannel->length_exp)*curchannel->length_coef;

	 }
	else

	  curaccum->length_to_bottom =                            /* feet */
		longest_path->accumulated->length_to_bottom +
		curchannel->channel_length;




	  curaccum->sum_of_lengths = curaccum->length_to_bottom;

	/*  curaccum->runoff_vol_above = curaccum->runoff_vol_above /
		  curaccum->drainage_area; */



	/* Used to calulate the geomorphic length on a cell basis.  This part
	   sums the accumulated lengths of any path leading into the cell.
	   Used in geomorphic to calculate the length_weighted slope */

/*	if(initialptr->geomorphic_calc==GEOMORPHIC)

	 { */
	  long_drainage_area = 0.0;

	  greatest_time = 0.0;

	  longest_duration = 0.0;

	  greatest_length = 0.0;

	  greatest_depth = 0.0;

	  while (cells_in != NULL)
	   {

	    if(columndata[cells_in->source_column]->runoff->time_previous >
		    greatest_time)

	       greatest_time =
		   columndata[cells_in->source_column]->runoff->time_previous;



	    if(columndata[cells_in->source_column]->channel->total_length >
		   greatest_length)

	       greatest_length =
		   columndata[cells_in->source_column]->channel->total_length;


	    if(columndata[cells_in->source_column]->channel->duration >
	       longest_duration)

	       longest_duration = columndata[cells_in->source_column]->channel->duration;



	    if(columndata[cells_in->source_column]->channel->max_depth >
	      greatest_depth)

	      greatest_depth = columndata[cells_in->source_column]->channel->max_depth;



	    if(columndata[cells_in->source_column]->accumulated->drainage_area > long_drainage_area)

		long_drainage_area = columndata[cells_in->source_column]->accumulated->drainage_area;


	     cells_in = cells_in->next;
	   }


	  /* Change C0005  */
	  /* Change so that we calculate a geomorphic width and depth for every cell */

	 if(initialptr->geomorphic_calc==GEOMORPHIC)

	 {
	   curchannel->depth=curchannel->depth_coef * (pow(
		     (curaccum->drainage_area+curcolumn->area),curchannel->depth_exp));

	   curchannel->width=curchannel->width_coef * (pow(
		     (curaccum->drainage_area+curcolumn->area),curchannel->width_exp));
	 }


	  curcolumn->runoff->time_previous = greatest_time;
	  curcolumn->channel->long_dur = longest_duration;


	  if(curcolumn->channel->bank_depth == 1.0)
	     curcolumn->channel->max_depth = greatest_depth;
	   else
	     curcolumn->channel->max_depth = curchannel->depth;

       if(initialptr->geomorphic_calc == GEOMORPHIC)
	{

	  curchannel->channel_length = curaccum->length_to_bottom -
		   (pow(long_drainage_area,curchannel->length_exp)*
		       curchannel->length_coef);


	  curchannel->tr55_length = (pow((curaccum->drainage_area+curcolumn->area),
	      curchannel->length_exp)* curchannel->length_coef) -
	      greatest_length;

	  curchannel->total_length = greatest_length + curchannel->tr55_length;

	}

	if(initialptr->geomorphic_calc == NONGEOMORPHIC)
	  curchannel->tr55_length = curchannel->channel_length;



	curaccum->length_slope =                              /*  ft * %  */
		longest_path->accumulated->length_slope +
		curchannel->channel_length *
		curchannel->channel_slope;

	curaccum->length_side_slope =                         /*  ft * %  */
		longest_path->accumulated->length_side_slope +
		curchannel->channel_length *
		curchannel->channel_side_slope;

	curaccum->length_mannings =
		longest_path->accumulated->length_mannings +
		curchannel->channel_length *
		curchannel->channel_mannings;

	curcolumn->runoff->time_overland =                    /* sec */
		longest_path->runoff->time_overland;

	curcolumn->runoff->time_shallow =                     /* sec */
		longest_path->runoff->time_shallow;



	}
    else                                            /* PRIMARY cell */
	{

	  if(initialptr->geomorphic_calc == GEOMORPHIC)

	     curchannel->channel_length = 0.0;

	if (curchannel->channel_length == 0.0)
	 {
	   if(initialptr->geomorphic_calc == GEOMORPHIC)
	     curchannel->channel_length = (pow(curcolumn->area,
		  curchannel->length_exp)*curchannel->length_coef);
	   else
	     curchannel->channel_length =
			0.625 * sqrt( curcolumn->area * ACRES_TO_SQ_FEET );

	 }


	curchannel->total_length = curchannel->channel_length;

	curchannel->long_dur = 0.0;

	/* BUG A-7 */
	curchannel->max_depth = curchannel->depth;
	/* finish bug A-7 */

	curchannel->tr55_length = curchannel->channel_length;
 
	curaccum->length_to_bottom =
			curchannel->channel_length;
 
	curaccum->sum_of_lengths = curchannel->channel_length;
 
	curaccum->length_slope =
			curchannel->channel_length *
			curchannel->channel_slope;
 
	curaccum->length_side_slope =
			curchannel->channel_length *
			curchannel->channel_side_slope;
 
	curaccum->length_mannings =
			curchannel->channel_length *
			curchannel->channel_mannings;
 
/*	curaccum->runoff_vol_above = 0.0; */
	}
 
    curaccum->drainage_area += curcolumn->area;
 
    curaccum->cn_area += curcolumn->management->curve_number *
				curcolumn->area;
 
    if (tflags.chem_table)
     {
      ctable.ct[column].Pest_dis_into =
                                    curcolumn->pesticide->soluble_pest_exit;
      ctable.ct[column].Pest_att_into =
                                   curcolumn->pesticide->sediment_pest_exit;
     }

    curcolumn->pesticide->soluble_pest_exit +=
		curcolumn->pesticide->soluble_pest;
 
    curcolumn->pesticide->sediment_pest_exit +=
		curcolumn->pesticide->sediment_pest;

    if (rflags.prep_route)
     {
      fprintf (stderr,"PREP_ROUTE routine:                \n");
      fprintf (stderr,"   Input:  %d...col_number\n",column);
      fprintf (stderr,"   Output: %f...drainage_area   %f...cn_area\n",curaccum->drainage_area,curaccum->cn_area);
      fprintf (stderr,"           %f...soluble_pest    %f...sediment_pest\n",curcolumn->pesticide->soluble_pest_exit,curcolumn->pesticide->sediment_pest_exit);
     }



}
 
 
 
 
/*************************  PATH  *******************************/
/*                                                              */
/*        calculates the path length factor across a cell       */
/*        based on the difference between the flow directions   */
/*                                                              */
/****************************************************************/
 
 
#ifdef _UNIX_K_AND_R
 
 static float path(aspect, rec_aspect)
 
       int aspect;
       int rec_aspect;
 
#else
 
static float path(int aspect, int rec_aspect)
 
#endif
 
{
int asp_difference;        /* absolute value of the difference */
			   /* between aspect and rec_aspect    */
float pth_length_factor = 1.0;
 
float cospi4 = cos(3.1415927 / 4.0);
 
 
   asp_difference = abs(aspect - rec_aspect);
 
   if (asp_difference == 4)
	     pth_length_factor = 0.0;
 
   else if ((asp_difference == 1) || (asp_difference == 7))  /* differ by 45 or 315 degrees */
	     pth_length_factor = 1.0 / cos(atan(0.5));
 
   else if ((asp_difference == 3) || (asp_difference == 5))  /* differ by 135 or 225 degrees */
	     pth_length_factor = cospi4;
 
   else if (asp_difference == 0)                         /* if flowpath is straight across cell */
      {
       pth_length_factor = (1 / cospi4);
 
       if ((rec_aspect % 2) == 1)                        /* if aspect is 1,3,5 or 7 */
	    pth_length_factor = 1.0;
      }
 
   if ((asp_difference == 2) || (asp_difference == 6))   /* differ by 90 or 270 degrees */
      {
       pth_length_factor = cospi4;
 
       if ((rec_aspect % 2) != 1)                        /* if aspect is 1,3,5 or 7 */
	    pth_length_factor = 1 / pth_length_factor;
      }
 
   return(pth_length_factor);
}
