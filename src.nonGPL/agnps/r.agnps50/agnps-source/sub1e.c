
#ifdef _DOS
 
 #include <stdio.h>
 #include <alloc.h>
 #include <math.h>
 #include "input.h"
 #include "binary.h"
 #include "debugflg.h"

#else

#include <stdio.h>
#include <math.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif

/**********************  EXTERNAL VARIABLES  **********************/

extern COLUMN_INFOPTR *columndata;   /* main data structure */
extern int debuginfo;

extern PARTICLE_DATA partinfo [6];
extern int hydro_info;
extern HYDRO_ROUTE_REC_PTR hydro_route;

extern SOURCEACCT2PTR	     sourceact2ptr;
extern SOURCEACCT2	     sourceact2;

extern int sourceinfo;

extern FLAGS_BASIC           bflags;
extern FLAGS_ROUTINE         rflags;
 
/**************************** hydrology *****************************/
/*                                                                  */
/* HYDROLOGY calculates the Peak flow rate (cfs) and duration (sec) */
/*           using an equation taken from the CREAMS model          */
/*                                                                  */
/********************************************************************/
 
 
#ifdef _UNIX_K_AND_R

void hydrology(area, chan_slope, runoff, chan_length, peak_flow)
 
     float area;
     float chan_slope;
     float runoff;
     float chan_length;
     float *peak_flow;
 
#else
 
void hydrology(float area,            /* acres               */
	       float chan_slope,      /* percent             */
	       float runoff,          /* inches              */
	       float chan_length,     /* flow length in feet */
	       float *peak_flow)      /* cfs                 */
 
#endif
 
{
float term1, term2;
 
 if((bflags.flow) || (bflags.flow))
   fprintf (stderr,"*** HYDRO PEAK FLOW CALCULATION");
 
    if (area > 0.0)
	{
	term1 = pow( runoff, (0.824 * pow(area, 0.0166)) );
 
	term2 = pow( (pow( chan_length, 2.0)/(area * 43560.0)) , -0.187);
 
	*peak_flow = 8.484 * pow(area, 0.7) * pow(chan_slope
			/ 100.0, 0.159) * term1 * term2;
 
	}
    else
	{
	*peak_flow     = 0.0;
	}

    if(hydro_info)
      {
       hydro_route->length_slope=chan_slope;
       hydro_route->sum_of_lengths=chan_length;
       hydro_route->term1=term1;
       hydro_route->term2=term2;
       hydro_route->peak_flow=*peak_flow;
      }
 
    if ((bflags.channel) || (bflags.flow))
       {
       fprintf (stderr,"INPUTS: Runoff = %f Area= %f Chan. Length= %f Chan. Slope= %f\n",
	       runoff,area,chan_length,chan_slope);
       fprintf (stderr,"OUTPUT: Peak flow %f cfs \n", *peak_flow);
       }
}
 
 
 
/*********************** channel calculations ***********************/
/*                                                                  */
/* CHANNEL CALCULATIONS calculates the channel width going out of   */
/*         of the cell (ft), the shear stress(lbs/sqr ft), and the  */
/*         velocity (ft/sec) using equations based on the           */
/*         triangular channel and mannings equation.                */
/*                                                                  */
/********************************************************************/
 
#ifdef _UNIX_K_AND_R
 
void channel_calculations(col, flow_rate, channel_width, shear_stress,
	   velocity)
 
	 int col;
	 float flow_rate;
	 float *channel_width;
	 float *shear_stress;
	 float *velocity;
 
#else
 
void channel_calculations(int col, float flow_rate, float *channel_width,
			  float *shear_stress, float *velocity)
 
#endif
 
{
CHANNEL_INFO *channelptr;
 
float decimal_channel_slope, /* channel slope as a decimal number      */
      decimal_side_slope;    /* channel side slope as a decimal number */
 
    channelptr = columndata[col]->channel;
 
    decimal_side_slope = ((float) channelptr->channel_side_slope)
			     /100.0;
    decimal_channel_slope    = ((float) channelptr->channel_slope)
			     /100.0;
 
    *channel_width = 2.05 * pow(decimal_side_slope,-0.625) * pow(1.0
			 + decimal_side_slope * decimal_side_slope, 0.125) *
			 pow(flow_rate * channelptr->channel_mannings
			 /pow(decimal_channel_slope, 0.5), 0.375);
 
 
    *shear_stress  = 62.4 * pow(decimal_side_slope, 0.375)/pow(2.0 * pow(1.0
		   + decimal_side_slope * decimal_side_slope, 0.5), 0.75) *
		   pow(decimal_channel_slope, 0.8125) * pow(channelptr->channel_mannings
		   * flow_rate/1.49, 0.375);
 
    if (channelptr->channel_mannings == 0.0) *velocity = 0.0;
 
    else if(pow(2.0 * pow(1.0 +decimal_side_slope * decimal_side_slope
		   , 0.5),0.5) == 0.0) *velocity = 0.0;
 
    else (*velocity = pow(1.49/channelptr->channel_mannings, 0.75) *
	 pow(decimal_side_slope, 0.25)/pow(2.0 * pow(1.0 + decimal_side_slope
	 * decimal_side_slope, 0.5), 0.5) * pow(decimal_channel_slope, 0.375)
	 * pow(flow_rate, 0.25));
 
    if (bflags.channel)
    {
       fprintf (stderr,"Non-Geomorphic Calculations\n");
       fprintf (stderr,"shear stress %f l/f2 channel width %f ft velocity %f f/s\n",*shear_stress,*channel_width,*velocity);
       fprintf (stderr,"Flow Rate %f \n",flow_rate);
    }

 
}
 
 
/************************** sediment flow ***************************/
/*                                                                  */
/* SEDIMENT FLOW calculates the sediment flow and transport         */
/*     capacity for each cell, it is used in the sediment routing   */
/*     and is based on transport capacity                           */
/*                                                                  */
/********************************************************************/
 
#ifdef _UNIX_K_AND_R
 
void sediment_flow(p_type, channel_width_out, flow_length, channel_width_in,
		   sed_flow_rate_in, shear_stress_in, velocity_in,
		   shear_stress_out, velocity_out, sed_flow_rate,
		   downstream_qp, upstream_qp, sed_flow_rate_out,
		   trans_capacity_out)
 
 
	int p_type;
	float channel_width_out;
	float flow_length;
	float channel_width_in;
	float sed_flow_rate_in;
	float shear_stress_in;
	float velocity_in;
	float shear_stress_out;
	float velocity_out;
	float sed_flow_rate;
	float downstream_qp;
	float upstream_qp;
	float *sed_flow_rate_out;
	float *trans_capacity_out;
 
#else
 
void sediment_flow(int p_type,                /* particle type number     */
		   float channel_width_out,   /* feet                chw2 */
		   float flow_length,         /* feet                xl   */
		   float channel_width_in,    /* feet                chw1 */
		   float sed_flow_rate_in,    /* lbs/sec             qsed1*/
		   float shear_stress_in,     /* lbs/ft^2            tw1  */
		   float velocity_in,         /* ft/sec              v1   */
		   float shear_stress_out,    /* lbs/ft^2            tw2  */
		   float velocity_out,        /* ft/sec              v2   */
		   float sed_flow_rate,       /* lbs/sec (overland)  cqsed*/
		   float downstream_qp,       /* cfs                 qp2  */
		   float upstream_qp,         /* cfs                 qp1  */
		   float *sed_flow_rate_out,  /* lbs/sec             qsed2*/
		   float *trans_capacity_out) /* lbs/sec/ft          tc2  */

#endif
 
{
register int   j     = p_type;
 
float efficiency_in  = 0.0;
float efficiency_out = 0.0;
float trans_capacity_within;
float factor;
float temp1 = 0.0;
float temp2 = 0.0;
float temp3 = 0.0;
 
     if (shear_stress_in > 0.0)
	   efficiency_in  = 0.74 * pow( shear_stress_in /
				     ((partinfo[j].weight - 62.4) *
				     partinfo[j].diameter / 304.8), -1.98);
 
     if (shear_stress_out > 0.0)
	   {
	   efficiency_out = 0.74 * pow( (shear_stress_out /
				     ((partinfo[j].weight - 62.4) *
				     partinfo[j].diameter / 304.8)), -1.98);
	   }
 
     if (partinfo[j].set_velocity * efficiency_in == 0.0)
	   trans_capacity_within = 0.0;
     else
	   trans_capacity_within = shear_stress_in * velocity_in *
				velocity_in * partinfo[j].trans_cap /
				partinfo[j].set_velocity * efficiency_in;
 
     if (partinfo[j].set_velocity * efficiency_out == 0)
	   *trans_capacity_out = 0.0;
     else
	   *trans_capacity_out = (shear_stress_out) * (velocity_out) *
				(velocity_out) * (partinfo[j].trans_cap) /
				partinfo[j].set_velocity * efficiency_out;
 
     if (channel_width_out == 0.0)
	   factor = 0.0;
     else
	   factor = 2.0 * downstream_qp / channel_width_out;
 
 
     if ((factor + flow_length * partinfo[j].set_velocity) == 0.0)
	   temp1 = 0.0;
 
     else
	   temp1 = factor/(factor + flow_length * partinfo[j].set_velocity);
 
     if (channel_width_in != 0.0)
	   temp2 = partinfo[j].set_velocity * channel_width_in / upstream_qp *
			(sed_flow_rate_in / channel_width_in -
			trans_capacity_within);
 
     if (downstream_qp == 0.0)
	   temp3 = 0.0;
 
     else
	   temp3 = partinfo[j].set_velocity * (*trans_capacity_out) *
			  channel_width_out / downstream_qp;
 
     *sed_flow_rate_out = temp1 * (sed_flow_rate_in + sed_flow_rate -
	      (channel_width_out + channel_width_in) * flow_length /
	       4.0 * (temp2 - temp3));
 
     if (bflags.sediment)
     {
	fprintf (stderr,"INITIAL SED FLOW: sed flow rate out %f l/s trans cap out %f l/s/f\n",*sed_flow_rate_out,*trans_capacity_out);
     }
 
}

 
 
/************************ sum point sources *************************/
/*                                                                  */
/*  SUM_POINT_SOURCES sums the soluable nitrogen, phosphorous and   */
/*      cod, also calculates the concentrations of each             */
/*  was pusntr in sub1.for                                          */
/*                                                                  */
/*    sec    lbs    ft^3   ppm                                      */
/*    ---- x ---- x ---- x --- = lbs of nutrient                    */
/*    10^6   ft^3   sec     1                                       */
/*                                                                  */
/********************************************************************/
 
#ifdef _UNIX_K_AND_R
 
void sum_point_sources(col, flow_duration)
 
    int col;
    float flow_duration;
 
#else
 
void sum_point_sources( int   col,
			float flow_duration) /* sec */
 
#endif
 
{
float temp;
float sum_nit  = 0.0,   /* lbs of N   */
      sum_phos = 0.0,   /* lbs of P   */
      sum_cod  = 0.0;   /* lbs of COD */
 
NONFEEDLOT_INFO *point_source = columndata[col]->nonfeedlot;
RUNOFF_INFO     *runoff       = columndata[col]->runoff;
 
  temp = flow_duration * CU_FT_H2O_IN_LBS * 1.0e-6;
 
  while (point_source != NULL)
      {
      sum_nit  += temp * point_source->water_discharge *
				point_source->nitrogen_concentration;
      sum_phos += temp * point_source->water_discharge *
				point_source->phosphorus_concentration;
      sum_cod  += temp * point_source->water_discharge *
				point_source->cod_concentration;
      point_source = point_source->next;
      }


 if(sourceinfo)
 {
  sourceact2ptr->sol_n_nonfeedlots=sum_nit;
  sourceact2ptr->sol_p_nonfeedlots=sum_phos;
  sourceact2ptr->sol_cod_nonfeedlots=sum_cod;
 }

  runoff->soluble_nitrogen_yield    += sum_nit;    /* lbs */
  runoff->soluble_phosphorus_yield  += sum_phos;
  runoff->soluble_cod_yield	    += sum_cod;
 
						    /* lbs/acre */
  runoff->soluble_nitrogen_runoff   += sum_nit  / columndata[col]->area;
  runoff->soluble_phosphorus_runoff += sum_phos / columndata[col]->area;
  runoff->cod_runoff                += sum_cod  / columndata[col]->area;
}
 
 
 
 
/**********************  sum nonfeedlot sources  ********************/
/*                                                                  */
/*  SUM NONFEEDLOT SOURCES:  sums all non feedlot point sources for */
/*       the cell.                                                  */
/*                                                                  */
/********************************************************************/
 
/*
float sum_nonfeedlot_sources(int column)
{
float sum_sources = 0.0;
NONFEEDLOT_INFO  *point;
 
   point = columndata[column]->nonfeedlot;
 
   while (point != NULL)
      {
      sum_sources += point->water_discharge;
      point = point->next;
      }
   return(sum_sources);
}
 
*/
 
 
#ifdef _UNIX_K_AND_R
 
  int locate_source_cells(columns, first_sink)
 
	int columns;
	SINKHOLEPTR first_sink;
 
#else
 
  int locate_source_cells(int columns, SINKHOLEPTR first_sink)
 
#endif
 
{
int         i, j;
int         outletcell = 1;
SOURCEPTR   tempsource;
SINKHOLEPTR tempsinkhole = first_sink;
 
    for (i=1; i<=columns; i++)
	{
	if (columndata[i]->receiving_cell_position == 0)
	    {
	    tempsinkhole->next = (SINKHOLEPTR) calloc(1, sizeof(SINKHOLE));
	    tempsinkhole       = tempsinkhole->next;
	    tempsinkhole->col_number = i;
	    }
	else if (columndata[i]->receiving_cell_position > columns)
	    {
	    outletcell = i;
	    }
	else
	    {
	    j = columndata[i]->receiving_cell_position;
 
	    if (columndata[j]->sourcelist == NULL)
		{
		columndata[j]->sourcelist = (SOURCEPTR)
					calloc( 1, sizeof(SOURCE));
		tempsource = columndata[j]->sourcelist;
		}
	    else
		{
		tempsource = columndata[j]->sourcelist;
 
		while (tempsource->next != NULL)
		    {
		    tempsource = tempsource->next;
		    }
 
		tempsource->next = (SOURCEPTR) calloc(1, sizeof(SOURCE));
		tempsource = tempsource->next;
		}
	    tempsource->source_column = i;
	    }
	}

    if (rflags.src_cells)
     {
      fprintf (stderr,"SRC_CELLS routine: \n");
      fprintf (stderr,"   Input:  %d...col_number\n",columns);
      fprintf (stderr,"   Output: %d...outlet_cell\n",outletcell);
     }


    return ( outletcell );
}
