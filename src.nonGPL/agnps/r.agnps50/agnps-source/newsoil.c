/************************ Documentation Start **********************
 
NEWSOIL.C
 
SYNOPSIS:
*********************************************************************/
 
#ifdef _DOS
 
 #include <stdio.h>
 #include <stdlib.h>
 #include <math.h>
 #include "input.h"
 #include "debugflg.h"

#else
 
#include <stdio.h>
#include <math.h>
#include "input.h"
#include "debugflg.h"

#endif
 
/*************************  EXTERNAL VARIABLES  **************************/
 
extern COLUMN_INFOPTR *columndata;         /* main cell structure */
extern SEDIMENT_DATA   outlet_sediment[];
extern SEDIMENT_INFO   sediment[];
extern int sedimentinfo;
extern int sinkhole;
extern float  sed_yield_for_j[6];

extern FLAGS_TABLE  tflags;
extern SED_TABLE    stable;
 
PARTICLE_DATA partinfo[] = {{   0.0,  0.0,   0.0,       0.0      },
			    { 162.37, 0.002, 0.0000102, 0.006242 },
			    { 165.49, 0.010, 0.000263,  0.006053 },
			    { 112.41, 0.035, 0.00125,   0.012478 },
			    {  99.92, 0.500, 0.0542,    0.016631 },
			    { 165.49, 0.200, 0.0759,    0.006053 }};
 
 
/*************************************************************************/
 
#ifdef _UNIX_K_AND_R
 
 void newsoil(velocity_out, channel_width_out, shear_stress_out,
	     downstream_qp, flow_length, column, increment,
	     increment_duration, last_overland_incr, channel_depth_out,
	     number_of_increments, duration)
 
      float velocity_out;
      float channel_width_out;
      float shear_stress_out;
      float downstream_qp;
      float flow_length;
      int   column;
      int   increment;
      float increment_duration;
      int   last_overland_incr;
      float channel_depth_out;
      int number_of_increments;
      float duration;
 
#else
 
 void newsoil(float velocity_out,
	     float channel_width_out,
	     float shear_stress_out,
	     float downstream_qp,
	     float flow_length,
	     int   column,
	     int   increment,
	     float increment_duration,
	     int   last_overland_incr,
	     float channel_depth_out,
	     int   number_of_increments,
	     float duration)
 
#endif
 
{
 
 
IMPOUND_INFO *impoundptr;
 int   j;                             /* j = particle type */
 int   calculate;
 int impoundment;
 float weight;
 float Nd;
 float froude_value_out;
 float efficiency_out;
 static float trans_capacity_within[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
 static float trans_capacity_out[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
 float transport_capacity_out = 0.0;
 float trans_capacity;
 float sum_trans_capacity_out = 0.0;
 float old_sed;
 float sed_flow_rate_out;
 static float sed_flow_out[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
 static float froude_value_ina[5] ={0.0, 0.0, 0.0, 0.0, 0.0};
 static float froude_value_outa[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 static float efficiency_ina[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 static float efficiency_outa[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 static float old_seda[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 static float old_sedb[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 static float transport_capacity_outa[5]={0.0, 0.0, 0.0, 0.0, 0.0};
 float sed_flow_rate_in;
 float shear_velocity;
 float constant_prop;


 /****************************************************************

 DESCRIPTION:
   This procedure calculates the amount of sediment that is deposited
   in the cell and the amount that flows through the cell and into the
   next cell.

 RETURNS:

 NOTES:

 DATA STORES:

 HISTORY:
 Date		Bug#	Prog	Desc
 3/30/93        B1	MAK	Sinkhole Flow Rate Problem


 SEE ALSO:
  agrun,

 **************** Documentation End ********************************/
 
 
 if(sedimentinfo)
  {
   if(increment<=last_overland_incr)
    {
     fprintf (stderr,"Sediment Rates including Overland sediment flows.\n");
     fprintf (stderr,"   \n");
     fprintf (stderr,"                                                      Sed    Total\n");
     fprintf (stderr,"          Part                                        Flow   Overland\n");
     fprintf (stderr,"Column	Size	Factor    Temp1    Temp2    Temp3   Rate   Sediment\n");
     fprintf (stderr,"===================================================================\n");
    }
  }
 
 
 
 for (j=1; j<=5; j++)    /* j represents particle type */
  {
   if(columndata[column]->receiving_cell_position != 0)
    {

   /* CALCULATE FROUDE VALUES ***/


   froude_value_out =(shear_stress_out)/
		     ((partinfo[j].weight - 62.4)*partinfo[j].diameter/
			304.8);

   /*** CALCULATE EFFICIENCIES ***/
   /* This part of the model calcualtes the efficiency value for
      each paricle size.  If there is a sinkhole, then the froude
      value would be a zero, because the flow rate out of a sinkhole
      is zero.  This would cause an error if we try to take the value
      to the power of another value.  If the froude value is a zero,
      then we set efficiency out to a zero.
   */

     if(froude_value_out >0)
	  efficiency_out = 0.322 * pow(froude_value_out,-1.6262);
     else
	  efficiency_out =0.0;

     if(efficiency_out >1.0)  /* max value for efficiency out is 1.0 */
	efficiency_out = 1.0;


   /*** CALCULATE TRANSPORT CAPACITIES ***/

   if (partinfo[j].set_velocity * efficiency_out > 0.0)
	   trans_capacity_out[j] = (shear_stress_out) * (velocity_out) *
				(velocity_out) * (partinfo[j].trans_cap) /
				partinfo[j].set_velocity * efficiency_out;
   else
	   trans_capacity_out[j] = 0.0;


   /*** SUM TRANSPORT CAPACITIES ***/

   sum_trans_capacity_out += trans_capacity_out[j];


   /* This is where the new equation should start */

   /*** CALCULATE SED FLOW RATE ***/


     /* Used to prevent scouring in water cells */

    if((increment) <= number_of_increments)
      {
	if( channel_width_out <= 0.0)
	  sed_flow_rate_in = 0.0;
	else
	  sed_flow_rate_in = sediment[j].sed_flow_rate_into / channel_width_out;
      }
     else
	sed_flow_rate_in =0.0;

    impoundptr=columndata[column]->impound;

    for(impoundment=1;impoundment<=columndata[column]->num_impoundments;
       impoundment++)
      {

	if(impoundptr->duration>(increment*increment_duration))
	  sed_flow_rate_in += (((impoundptr->sediment_out[j]*2000)
		       /impoundptr->duration)/channel_width_out);

	if((impoundptr->duration<(increment * increment_duration)) &&
	   (impoundptr->duration>((increment-1)*increment_duration)))
	{

	   weight = (impoundptr->duration-((increment-1)*increment_duration))/
		    increment_duration;

	   sed_flow_rate_in += (((impoundptr->sediment_out[j]*2000)
		 /impoundptr->duration) * weight)/channel_width_out;
	}

      }



       calculate=FALSE;

       if(columndata[column]->channel->clay_indicator==TRUE && j==1)
	      calculate=TRUE;
       if(columndata[column]->channel->silt_indicator==TRUE && j==2)
	      calculate=TRUE;
       if(columndata[column]->channel->small_agg_indicator==TRUE && j==3)
	      calculate=TRUE;
       if(columndata[column]->channel->large_agg_indicator==TRUE && j==4)
	      calculate=TRUE;
       if(columndata[column]->channel->sand_indicator==TRUE && j==5)
	      calculate=TRUE;
       Nd=0.00;
       constant_prop = 0.00;
       shear_velocity=0.00;

      if(columndata[column]->receiving_cell_position != 0 )
       {

	if((columndata[column]->channel->channel_indicator == 0) ||
	   (columndata[column]->soil->soil_type == 5))

	/* Force water cells to go through the deposition routine */

	{

	  sed_flow_rate_out = trans_capacity_out[j];

	  if(sed_flow_rate_out > sed_flow_rate_in)
	    sed_flow_rate_out = sed_flow_rate_in;

	}
       else
	{

	 if(sed_flow_rate_in <= trans_capacity_out[j])
	  {

	   if (calculate == TRUE)
	      sed_flow_rate_out = trans_capacity_out[j];
	   else
	      sed_flow_rate_out = sed_flow_rate_in;

	  }
	 else
	  {

	   shear_velocity = pow((32.2 * channel_depth_out *
			   (columndata[column]->channel->channel_slope/100))
			   ,0.5);


	   if( j <= 3 )
	     constant_prop = 1.0;
	   else
	     constant_prop = ((6 * partinfo[j].set_velocity) /
	       (0.4 * shear_velocity)) / (1-exp(-(6.0 * partinfo[j].set_velocity)/
	       (0.4 * shear_velocity)));

	   sed_flow_rate_out = trans_capacity_out[j] + ((sed_flow_rate_in -
		trans_capacity_out[j])* exp(-((constant_prop *
		partinfo[j].set_velocity * flow_length)/(downstream_qp/
		channel_width_out))));


	   Nd=(constant_prop * partinfo[j].set_velocity * flow_length)/
		(downstream_qp/channel_width_out);


	  }
	 }
       }
    else
     {
       sed_flow_rate_out =0.0;
     }


    if (tflags.sed_table)
     {
      stable.st[column].constant_prop[j] = constant_prop;
      stable.st[column].part_fall_vel[j] = partinfo[j].set_velocity;
      stable.st[column].str_seg_length[j] = flow_length;
      stable.st[column].trans_cap_factor[j] = partinfo[j].trans_cap;

      if (channel_width_out > 0.0)
       stable.st[column].water_discharge[increment][j] = downstream_qp /
                                                          channel_width_out;
      else
       stable.st[column].water_discharge[increment][j] = 0.0;

      stable.st[column].sed_trans_cap[increment][j] = trans_capacity_out[j];
      stable.st[column].up_sed_discharge[increment][j] = sed_flow_rate_in;
      stable.st[column].down_sed_discharge[increment][j] = sed_flow_rate_out;
      stable.st[column].deposition_number[increment][j] = Nd;
     }

    if(sedimentinfo)
     {
      fprintf (stderr,"Constant Prop.= %f  Shear Velocity= %f \n",constant_prop,
	  shear_velocity);
      fprintf (stderr,"Transport Capacity= %f sed. flow in= %f sed flow out= %f\n",
	   trans_capacity_out[j],sed_flow_rate_in,sed_flow_rate_out);
      fprintf (stderr,"flow length %f flow rate %f channel width %f \n",
	   flow_length,downstream_qp,channel_width_out);
      fprintf (stderr,"Nd = %f \n",Nd);

      }



       sed_flow_out[j] = sed_flow_rate_out * channel_width_out * increment_duration;

    if(sedimentinfo)
    fprintf (stderr,"sed. flow out %f\n",sed_flow_out[j]);


      if(sedimentinfo)
       {
	froude_value_outa[j]=froude_value_out;
	efficiency_outa[j]=efficiency_out;
	old_seda[j]=sed_flow_rate_in;
	old_sedb[j]=sed_flow_rate_out;
	transport_capacity_outa[j]=transport_capacity_out;
       }






 /*** SUM SEDIMENT ***/


       if(increment== 1)
	 sed_yield_for_j[j]=sed_flow_out[j];
       else
	 sed_yield_for_j[j]+=sed_flow_out[j];
     if(sedimentinfo)
      {
       fprintf (stderr,"SED YIELD BY TOTAL %f\n",sed_yield_for_j[j]);
       fprintf (stderr,"****\n");
      }

     }
     else
     {
      sed_yield_for_j[j]=0.0;
     }
 
     } /* end looping on particle sizes */
 
 
   if(sedimentinfo)
    {
     for (j=1;j<=5;j++)
      {
       if(j==1)
	{
	 fprintf (stderr,"Sediment Flow Rates for channel flow only\n\n");
	 fprintf (stderr,"\n                            |------sediment------| \n");
	 fprintf (stderr,"Part  Froude Eff.  Trans Cap    flow       flow  tot \n");
	 fprintf (stderr,"size   out   out      out      rate in  rate out sed \n");
	 fprintf (stderr,"---------------------------------------------------------------------------\n");
	 fprintf (stderr," \n");
	}
       fprintf (stderr,"%d %8.2f %f %.4f %.4f %.4f %.2f\n",
	  j,froude_value_outa[j],efficiency_outa[j],trans_capacity_out[j],
	  old_seda[j],old_sedb[j],sed_flow_out[j]);
      }
    }
 
 
 
}
 
 
/* TOTAL SEDIMENT INFO FOR THE CELL */
 
#ifdef _UNIX_K_AND_R
 
void calc_sediment(column)
 
  int column;
 
#else
 
void calc_sediment(int column)

#endif
 
{
 
 float potential_gully_erosion;
 float total_sed_avail = 0.0;
 float total_sed_out   = 0.0;
 float sediment_from_above = 0.0;
 int   j;
 RUNOFF_INFO *runoff;
 
 
 runoff = columndata[column]->runoff;
 
 for (j=1; j<=5; j++)
  {
 
 
 
   if(columndata[column]->sourcelist == NULL) /* Primary cell */
 
       potential_gully_erosion = (sed_yield_for_j[j] -
				 sediment[j].sed_available) / 2000.0;
   else
       potential_gully_erosion = (sed_yield_for_j[j] -
				 (sediment[j].sed_available +
				  runoff->available_sediment[j]))
				  / 2000;
   if (potential_gully_erosion < 0.0)
	potential_gully_erosion = 0.0;
 
 
 
 
      runoff->sediment_yield[j]         = sed_yield_for_j[j] / 2000.0;
 
   if(sinkhole == FALSE)
      outlet_sediment[j].gully_erosion += potential_gully_erosion;
 
   total_sed_avail                  += sediment[j].sed_available;
   total_sed_out                    += sed_yield_for_j[j];
 
   if(columndata[column]->sourcelist != NULL)
       sediment_from_above              += runoff->available_sediment[j];
 
  }
 
  if(columndata[column]->sourcelist == NULL)
      potential_gully_erosion = (total_sed_out - total_sed_avail) / 2000.0;
  else
      potential_gully_erosion = (total_sed_out - (sediment_from_above +
	    total_sed_avail)) / 2000;
 
  if (potential_gully_erosion < 0.0)
	  potential_gully_erosion = 0.0;
 
  if(sinkhole == FALSE)
   {
     outlet_sediment[6].gully_erosion += potential_gully_erosion;
 
     outlet_sediment[6].area_weighted_erosion +=
			      runoff->total_eroded_sediment / 2000.0;
   }
 
}
