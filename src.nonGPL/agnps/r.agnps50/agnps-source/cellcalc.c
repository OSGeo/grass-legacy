  /* cellcalc.c */
#ifdef _DOS

 #include <math.h>
 #include <stdlib.h>
 #include <stdio.h>
 #include <assert.h>
 #include "input.h"
 #include <alloc.h>
 #include "binary.h"
 #include "debugflg.h"

#else

#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif


#define MIN_RUNOFF 0.01


extern COLUMN_INFOPTR *columndata;         /* main cell structure */
extern INITIAL_INFOPTR initialptr;
extern GENERAL_PESTICIDE_DATA *general_pest_info;
extern SEDIMENT_INFO sediment[];
extern int debuginfo;
extern HYDRO_ROUTE_REC_PTR   hydro_route;
extern POINT_SOURCE_REC_PTR  point_source_rec;

extern IMPOUND_ROUTE_REC     impound_data_rec;
extern IMPOUND_REC_PTR       impound_data_ptr;

extern SOURCEACCT2PTR        sourceact2ptr;
extern SOURCEACCT2           sourceact2;

extern int hydro_info;
extern int hydroinfo;
extern char hydro_out_file2[128];
extern char source_acct2[128];
extern int hydro2_file_open;
extern FILE *hydrofile2;
extern FILE *sourcefile2;
extern FILE *hyd;
extern FILE *imp;
extern int source_acct_open2;
extern int sourceinfo;
extern int columns;
extern float sed_yield_for_j[6];

extern FLAGS_BASIC    bflags;
extern FLAGS_ROUTINE  rflags;
extern FLAGS_TABLE    tflags;
extern HYDRO_TABLE    htable;
extern SED_TABLE      stable;
extern CHEM_TABLE     ctable;

#ifdef _UNIX_K_AND_R
 
void  hydrology();
void  chantr55();
void  calc_initial_sed_info();
void  calculate_channel();
void  calc_channel_width();
void  newsoil();
void  calc_sediment();
void decay_nutrients();
void  curve_number_runoff();
void  sum_point_sources();
void  partition_pesticide();
 
#else
 
void  hydrology( float, float, float, float, float* );
void  chantr55( int, float, float, float, float, float* );
void  calc_initial_sed_info( int, int, float, float, int* );
void  calculate_channel(int, float, float, float*, float*, float*, float*,
	       float*, float*, float*);
void  calc_channel_width(int, float, float, float*, float*);
 
void  newsoil(float vout, float wout, float ssout, float qpout,
			float length, int col, int incr,
			float incr_dur, int last_overland_incr,
			float channel_depth_out,
			int number_of_increments,
			float duration);
 
void  calc_sediment(int);
 
void decay_nutrients(int, float);

void  curve_number_runoff( float, float, int, char[20], float*);
void  sum_point_sources( int column, float duration );
 
void  partition_pesticide(float sediment_lbs,
			float water_ft3, float sed_yield_lbsPacre,
			float Kd, int soil_type,
			int column,
			float *percent_surface,
			float *sed_portion, float *water_portion);
 
#endif
 
#ifdef _UNIX_K_AND_R
 
void cellcalc(column)
 
  int column;

#else
 
void cellcalc( int column )

#endif
 
 
 
{
 
NONFEEDLOT_INFO *psourceptr;
IMPOUND_INFO    *impoundptr;
ACCUM_VALUES    *accumpath;
SOIL_INFO       *soilpath;
PESTICIDE_INFO  *pest;
RUNOFF_INFO     *runoffpath;

float Kd;
 
float drainage_area_above;              /* acres    */
float runoff_volume_below;              /* inches   */
float runoff_volume_above;              /* inches   */
float time_to_peak_in;                  /* sec      */
float time_to_peak_out;                 /* sec      */
float time_to_peak_ave;                 /* sec      */
float duration;                         /* sec      */
float max_duration;                     /* sec      */
float peak_flow_out;                    /* cfs      */
float flow_rate_below;                  /* cfs      */
float base_increment_duration;          /* sec      */
float impoundment_duration;             /* sec      */
float sum_n_impound;
float sum_p_impound;
float sum_cod_impound;
float percent_runoff;
float impoundment_nitrogen;
float impoundment_phosphorus;
float impoundment_cod;
float per_n_decay;
float per_p_decay;
float per_cod_decay;
float n_after_decay;
float p_after_decay;
float cod_after_decay;
float total_n;
float total_p;
float total_cod;
float total_drainage_area;

int   particle_size;
float sediment_change_in_channel;

float *ave_incr_downflowrate;           /* cfs      */
float *ave_incr_upflowrate;             /* cfs      */
float channel_depth_out;
 
float psource_flow_top;                 /* cfs      */
float psource_flow_bottom;              /* cfs      */
float psource_volume_top;               /* ft^3     */
float psource_volume_bottom;            /* ft^3     */
float channel_side_slope;
float channel_slope;

int particle;
int j;
float particle_surface[6];
static float surface_constant[6]= {0, 1.5708, 0.3142, 0.9429, 0.6330, 0.0157};
float sum_of_particle_surfaces;
float total_nitrogen;
float total_phosphorus;
float percent_surface[6];
float sed_avail;
float percent_flowing_out;
float part_n_yield;
float part_p_yield;
 
 
float drainage_area_below;              /* acres (omits terraced area) */

int   number_point_sources;
int   max_increments;
int   increment;
int   number_of_increments;
int   num_increments;
int   impoundments;
int   last_overland_incr;
int   error_code;
int   psource;
float sum_impound_volume;
float sediment_lbs;
 
float velocity_in;
float velocity_out;
float channel_width_in;
float channel_width_out;
float shear_stress_in;
float shear_stress_out;
float impoundment_flows;
float peak_flow_bottom;
 
CHANNEL_INFO   *channel = columndata[column]->channel;
 
 
    accumpath  = columndata[column]->accumulated;
    psourceptr = columndata[column]->nonfeedlot;
    impoundptr = columndata[column]->impound;
    soilpath   = columndata[column]->soil;
    runoffpath = columndata[column]->runoff;
 

 
 
/*** 1.0 CALCULATE TOP OF CELL VALUES *****/
    drainage_area_above = columndata[column]->accumulated->drainage_area -
			  columndata[column]->area;

	   /* runoff_vol_above  (cubic feet) runoff from all cells above */
	   /* convert vol from cubic feet to inches */

    if (drainage_area_above > 0.0)
	runoff_volume_above = (accumpath->runoff_vol_above /
				IN_ACRE_TO_CU_FT) / drainage_area_above;
    else
	runoff_volume_above = 0.0;


    if(bflags.drain_area)                 /* JW 03/29/95 SUPP!*/
       {
	fprintf (stderr,"****\n");
	fprintf (stderr,"1.0 CALCULATE TOP OF CELL VALUES...Col %d:\n",column);
	fprintf (stderr,"Drainage Area Above= %f (acres)  Runoff Volume Above = %f (cu. ft.)\n\n",
	 drainage_area_above,runoff_volume_above);
       }


/**** 2.0  CALCULATE TOP OF CELL DURATION ****/

    error_code=0;
 
    if (accumpath->runoff_flow_above > MIN_RUNOFF)
	time_to_peak_in  = 3600.0 * initialptr->K_triangle *
		       runoff_volume_above *
		       drainage_area_above /
		       (640.0 * accumpath->runoff_flow_above);
    else
	 time_to_peak_in = 0.0;
 
    if(bflags.flow)
      {
	fprintf (stderr,"****\n");
	fprintf (stderr,"2.0 CALCUATE TIME TO PEAK AT TOP OF CELL...Col %d:\n",column);
	fprintf (stderr,"INPUTS: K Triangle=%f  Runoff Volume Top=%f (cu. ft.)\n",
	     initialptr->K_triangle,runoff_volume_above);
	fprintf (stderr,"        Runoff Flow Top=%f (cfs) Drainage Area Above=%f (acres)\n",
	     accumpath->runoff_flow_above,drainage_area_above);
	fprintf (stderr,"OUTPUTS: Time to Peak Top=%f (sec) Error Code=%d \n\n",
	     time_to_peak_in,error_code);
      }
 
/**** 3.0  CALCULATE NON-TERRACED DRAINAGE AREA ****/

    drainage_area_below = drainage_area_above +
			    columndata[column]->area_not_terraced;

    if(tflags.hydro_table)
     {
      htable.ht[column].drainage_area = drainage_area_below;
     }

    if(bflags.drain_area)
     {
      fprintf (stderr,"****\n");
      fprintf (stderr,"3.0 CALCULATE DRAINAGE AREA\n");
      fprintf (stderr,"OUTPUTS: Drainage Area Below %f(acres)\n\n",
	     drainage_area_below);
     }
 

/**** 4.0 CALCULATE VOLUME OF RUNOFF W/O POINT SOURCES OR IMPOUNDMENTS ****/

/*    curve_number_runoff((accumpath->cn_area / accumpath->drainage_area),
		     initialptr->storm_rainfall,
		     column,
		     "Cell Calc Routine",
		     &runoff_volume_below);  */

  runoff_volume_below = ((runoff_volume_above*drainage_area_above) +
		 (columndata[column]->runoff->cell_run_off*
		   columndata[column]->area_not_terraced))/drainage_area_below;

  if(hydro_info)
    hydro_route->runoff_volume_below=runoff_volume_below;

/*** 5.0  CALCULATE PEAK FLOW EXITING CURRENT CELL USING TR55 OR AGNPS ****/

/* This part of the program calculates peak flow from each cell based
   on the TR55 or AGNPS calculations, whichever the user choose.  Note
   that the duration is ignored, because this is obtained from the
   triangular hydrograph.  */
 
    if(bflags.flow)
     {
      fprintf (stderr,"LENGTH CALCULATION FOR FLOW PATH\n");
      fprintf (stderr,"Flow Length= %f\n",accumpath->length_to_bottom);
     }


    if (initialptr->calc_method == CREAMS)

	  /* correct way */

	hydrology( drainage_area_below,
		   accumpath->length_slope / accumpath->sum_of_lengths,
		   runoff_volume_below,
		   accumpath->length_to_bottom,
		   &peak_flow_out);



    else
	{
	chantr55(column,
		 columndata[column]->channel->channel_length,
		 accumpath->drainage_area,    /* includes terraced area */
		 accumpath->cn_area,
		 drainage_area_below,         /* omits terraced area */
		 &peak_flow_out);
	}

/* BUG */

      if( runoff_volume_below <= 0.0)
	peak_flow_out=0.0;

 
/*    BUG A-8 */

    if((initialptr->calc_method == CREAMS) &&
       (columndata[column]->channel->channel_indicator))
      {
	columndata[column]->channel->max_depth =
	 1.025*pow((columndata[column]->channel->channel_side_slope/100),0.375)*
	 pow((1+pow((columndata[column]->channel->channel_side_slope/100),2.0)),
	 0.125)*pow((peak_flow_out*columndata[column]->channel->channel_mannings/
	 pow((columndata[column]->channel->channel_slope/100),0.5)),0.375);
      }

 
/* Finish BUG A-8 */



/**** 6.0 ADD POINT SOURCES ABOVE THE CELL TO VOLUME OF RUNOFF ****/

  /* convert pt source volume from cubic feet to inches */


    if(drainage_area_below >0)
	runoff_volume_below += (accumpath->sum_psource_volumes /
			IN_ACRE_TO_CU_FT) / drainage_area_below;



    if(bflags.flow)
	fprintf (stderr,"runoff Below %f\n",runoff_volume_below);

/**** 7.0 ADD POINT SOURCES ABOVE THE CELL TO FLOW RATE ****/


    flow_rate_below = peak_flow_out + accumpath->sum_psource_flows;
 
 
    if(columndata[column]->receiving_cell_position == 0)
      accumpath->runoff_flow_below =0.0;
    else
      accumpath->runoff_flow_below = flow_rate_below;
 
    if(bflags.flow)
	fprintf (stderr,"7. flow rate below = %f\n",flow_rate_below);
 
/**** 8.0  CALCULATE BOTTOM OF CELL DURATION ***/

    if (flow_rate_below > 0.0)
	time_to_peak_out = 3600.0 * initialptr->K_triangle *
		       runoff_volume_below *
		       drainage_area_below /
		       (640.0 * flow_rate_below);
    else
	 time_to_peak_out = 0.0;   /* perhaps change this ??????????? */

 
    if(bflags.flow)
	fprintf (stderr,"8. time to peak out %f\n",time_to_peak_out);
 
/**** 9.0 CALCULATE THE AVERAGE DURATION FOR THE CELL ****/

    time_to_peak_ave = (time_to_peak_in + time_to_peak_out) / 2;

    duration = time_to_peak_ave / initialptr->prepeak_RO_fraction;

    if((columndata[column]->channel->channel_indicator == 0) ||
       (columndata[column]->soil->soil_type == 5))
 
       {
	if(columndata[column]->sourcelist != NULL) /* not a primary */
	  {
	   duration = columndata[column]->channel->long_dur;
	   time_to_peak_ave = (duration * initialptr->prepeak_RO_fraction);
	  }
 
       }
 

    columndata[column]->channel->duration = duration;
 
    if((columndata[column]->channel->channel_indicator == 0) ||
       (columndata[column]->soil->soil_type == 5))
      {
       flow_rate_below = (runoff_volume_below / 12.00 * 43560.00 *
			 drainage_area_below) / duration;
 
       flow_rate_below+= accumpath->sum_psource_flows;
       accumpath->runoff_flow_below=flow_rate_below;
      }
 
   if(initialptr->calc_method == CREAMS)
       {
	channel_side_slope = columndata[column]->channel->channel_side_slope/100;
	channel_slope = columndata[column]->channel->channel_slope/100;

	columndata[column]->channel->bank_depth =
	  1.025*pow(channel_side_slope,0.375)*
	  pow((1+pow(channel_side_slope,2.0)),0.125)*
	  pow((flow_rate_below/(pow(channel_slope,0.5))),0.375);
       }
 

      else
	columndata[column]->channel->bank_depth =
	     columndata[column]->channel->depth;
 
/*
 BUG A-7
 
    if((columndata[column]->channel->channel_indicator == 0) ||
       (columndata[column]->soil->soil_type == 5))
	{
 
 
	 if(columndata[column]->channel->depth == 0)
	    columndata[column]->channel->depth =
		columndata[column]->channel->max_depth;

	 if(columndata[column]->sourcelist == NULL)
	   {
	    if(columndata[column]->channel->depth == 0)
	       columndata[column]->channel->depth =
		  columndata[column]->channel->bank_depth;
	   }

	}
 
 
 
      if(columndata[column]->sourcelist == NULL)
	columndata[column]->channel->max_depth =
	   columndata[column]->channel->bank_depth;

*/



    if(bflags.flow)
	fprintf (stderr,"9. Duration = %f\n",duration);

/**** 10.0 CALCULATE THE DURATION OF EACH INCREMENT IN HYDROGRAPH ****/

    base_increment_duration = time_to_peak_ave /3.0;

    if(bflags.flow)
	fprintf (stderr,"10. Increment Duration = %f\n",base_increment_duration);

/**** 11.0 SUM POINT SOURCE FLOW RATES AND VOLUMES IN THE CURRENT CELL ****/

   if(hydro_info)
    {
     if(!hydro2_file_open)
	{
	 hydrofile2=fopen(hydro_out_file2,"wb");
	 fwrite(&columns,sizeof(int),1,hydrofile2);
	 hydro2_file_open=1;
	}
     fwrite(&columndata[column]->num_impoundments,sizeof(int),1,hydrofile2);
     fwrite(&columndata[column]->num_nonfeedlots,sizeof(int),1,hydrofile2);
    }


    psource_flow_top      = 0.0;
    psource_flow_bottom   = 0.0;
    psource_volume_top    = 0.0;
    psource_volume_bottom = 0.0;
    psource               =0;

  while(psourceptr != NULL)
	{
	if (psourceptr->enter_at_top)
	    {
	    psource_flow_top   += psourceptr->water_discharge;
	    psource_volume_top += psourceptr->water_discharge * duration;
	    }

	psource_flow_bottom   += psourceptr->water_discharge;
	psource_volume_bottom += psourceptr->water_discharge * duration;

	psource++;



	if(hydro_info)
	 {
	  point_source_rec->enter_at_top=psourceptr->enter_at_top;
	  point_source_rec->water_discharge=psourceptr->water_discharge;
	  point_source_rec->volume=psourceptr->water_discharge*duration;
	  point_source_rec->total_cell_flow_rate_top=psource_flow_top;
	  point_source_rec->total_cell_flow_rate_bottom=psource_flow_bottom;
	  point_source_rec->total_cell_volume_top=psource_volume_top;
	  point_source_rec->total_cell_volume_bottom=psource_volume_bottom;
	  point_source_rec->next=NULL;
	  fwrite(point_source_rec,sizeof(POINT_SOURCE_REC),1,hydrofile2);
	 }
	 psourceptr = psourceptr->next;
	}

    accumpath->sum_psource_volumes += psource_volume_bottom;
    accumpath->sum_psource_flows   += psource_flow_bottom;

    if(hydro_info)
     {
      hydro_route->total_psource_flow=accumpath->sum_psource_flows;
      hydro_route->total_psource_volume=accumpath->sum_psource_volumes;
      hydro_route->num_psources=psource;
     }

 
/**** 12.0 CALCULATE THE MAX NUMBER OF INCREMENTS WE WILL NEED ****/


    max_duration = duration; /* set max duration to cell duration */
 
    max_increments = 0;
 
    for (impoundments=1; impoundments<=columndata[column]->num_impoundments;
						    impoundments++)
	{
 
	if(impoundptr->volume_runoff != 0.0)
	    impoundment_duration = impoundptr->volume_runoff /
			       impoundptr->peak_flow;
	else
	  impoundment_duration=0.0;

 
	impoundptr->duration = impoundment_duration;
 
	/* set duration to the impoundment duration if it is longer
	   This is used for the time to pass to the decay function  */
 
	if(impoundment_duration>max_duration)
	   max_duration=impoundment_duration;
 
	if(impoundment_duration != 0.0)
		num_increments = ceil(impoundment_duration /
				base_increment_duration);

	else
	 num_increments=0;

	if (max_increments < num_increments)
	    max_increments = num_increments;


	if(columndata[column]->receiving_cell_position != 0)
	   accumpath->runoff_flow_below += impoundptr->peak_flow;

	impoundptr = impoundptr->next;
	}



/**** 13.0 ALLOCATE ARRAY NOW THAT WE KNOW THE NUMBER OF INCREMENTS ****/

    number_of_increments = ceil(3.0 / initialptr->prepeak_RO_fraction);

    if (tflags.sed_table)
      stable.st[column].num_partitions = number_of_increments;

    if (max_increments < number_of_increments)
	max_increments = number_of_increments;
    ave_incr_upflowrate   = (float*) calloc(max_increments+1, sizeof(float));

    if (ave_incr_upflowrate == NULL)
     {
      fprintf (stderr,"Error in Calloc [CELLCALC.C #617]\n");
      exit(1);
     }

    ave_incr_downflowrate = (float*) calloc(max_increments+1, sizeof(float));
    if (ave_incr_downflowrate == NULL)
     {
      fprintf (stderr,"Error in Calloc [CELLCALC.C #623] col %d...\n",column);
      exit(1);
     }

/**** 14.0 Calculate the channel width at peak flow ***/

    if(bflags.flow)
      {
    /*  fprintf (stderr,"14. peak flow bottom = %f  chan width= %f\n",peak_flow_bottom,
	    channel_width_out); */

	fprintf (stderr,"BEFORE ADDING IN IMPOUNDMENTS\n");

       fprintf (stderr," Increment      Top Flow     Bottom Flow \n");
      }


/**** 14.0 BUILD THE TOP OF CELL and BOTTOM OF CELL HYDROGRAPHS ***/

  if(hydro_info)
   {
    hydro_route->point_source_flow_top=psource_flow_top;
    hydro_route->point_source_flow_bottom=psource_flow_bottom;
   }



  for (increment=1; increment<=number_of_increments; increment++)
   {

    if(time_to_peak_ave >0)
     {
	if (increment <= 3)     /* 3 prepeak increments */
	    {

	      ave_incr_upflowrate[increment] =
			  accumpath->runoff_flow_above *
			  (2 * increment - 1) *
			  base_increment_duration /
			  (2.0 * time_to_peak_ave);
	       if(hydro_info)
		 hydro_route->ave_incr_upflow_rate[increment]=
		      ave_incr_upflowrate[increment];

		ave_incr_upflowrate[increment] += psource_flow_top;

		 if(hydro_info)
		   hydro_route->total_incremental_upflow_rate[increment]=
		     ave_incr_upflowrate[increment];


		ave_incr_downflowrate[increment] =
			  flow_rate_below *
			  (2 * increment - 1) *
			  base_increment_duration /
			  (2.0 * time_to_peak_ave);

		if(hydro_info)
		  hydro_route->ave_incr_downflow_rate[increment]=
		       ave_incr_downflowrate[increment];

	       ave_incr_downflowrate[increment] += psource_flow_bottom;

	       if(hydro_info)
		 hydro_route->total_incremental_downflow_rate[increment]=
		    ave_incr_downflowrate[increment];
	    }

	else if (increment < number_of_increments)
	    {

	     ave_incr_upflowrate[increment] =
				accumpath->runoff_flow_above *
				(2.0 * duration - (2 * increment - 1) *
				base_increment_duration) /
				(2.0 * (duration - time_to_peak_ave));

	      if(hydro_info)
		 hydro_route->ave_incr_upflow_rate[increment]=
		      ave_incr_upflowrate[increment];

		ave_incr_upflowrate[increment] += psource_flow_top;

		 if(hydro_info)
		   hydro_route->total_incremental_upflow_rate[increment]=
		     ave_incr_upflowrate[increment];

		ave_incr_downflowrate[increment] =
				flow_rate_below *
				(2.0 * duration - (2 * increment - 1) *
				base_increment_duration) /
				(2.0 * (duration - time_to_peak_ave));
		if(hydro_info)
		  hydro_route->ave_incr_downflow_rate[increment]=
		       ave_incr_downflowrate[increment];

	       ave_incr_downflowrate[increment] += psource_flow_bottom;

		if(hydro_info)
		 hydro_route->total_incremental_downflow_rate[increment]=
		    ave_incr_downflowrate[increment];
	    }

	else      /* last increment */
	    {
		ave_incr_upflowrate[increment] =
			accumpath->runoff_flow_above *
			(2.0 * duration - (2* increment - 1) *
			base_increment_duration ) /
			(2.0 * (duration - time_to_peak_ave));

	    if (ave_incr_upflowrate[increment] < 0.0)
		ave_incr_upflowrate[increment] = 0.0;


	     if(hydro_info)
		 hydro_route->ave_incr_upflow_rate[increment]=
		      ave_incr_upflowrate[increment];

		ave_incr_upflowrate[increment] += psource_flow_top;

		 if(hydro_info)
		   hydro_route->total_incremental_upflow_rate[increment]=
		     ave_incr_upflowrate[increment];

		ave_incr_downflowrate[increment] =
			flow_rate_below *
			(2.0 * duration - (2 * increment - 1) *
			base_increment_duration ) /
			(2.0 * (duration - time_to_peak_ave));


	    if (ave_incr_downflowrate[increment] < 0.0)
		ave_incr_downflowrate[increment] = 0.0;

		if(hydro_info)
		  hydro_route->ave_incr_downflow_rate[increment]=
		       ave_incr_downflowrate[increment];

	    ave_incr_downflowrate[increment] += psource_flow_bottom;

	     if(hydro_info)
		 hydro_route->total_incremental_downflow_rate[increment]=
		    ave_incr_downflowrate[increment];

	    } /* end interior else */

	if(columndata[column]->receiving_cell_position == 0)
	  ave_incr_downflowrate[increment] = 0.0;

	if(hydro_info)
	  hydro_route->new_downflow_rate[increment]=
	     ave_incr_downflowrate[increment];

	if(hydroinfo)
	  fprintf(hyd,"%d      %d             %f          %f         %f\n",column,
	    increment, ave_incr_upflowrate[increment],
	    ave_incr_downflowrate[increment],base_increment_duration);

     }
      else
     {
      ave_incr_downflowrate[increment] = 0.0;
      ave_incr_upflowrate[increment] = 0.0;

      if(hydro_info)
      {
	hydro_route->ave_incr_upflow_rate[increment]=0.0;
	hydro_route->ave_incr_downflow_rate[increment]=0.0;
	hydro_route->total_incremental_upflow_rate[increment]=0.0;
	hydro_route->total_incremental_downflow_rate[increment]=0.0;
      }

     }




   }

/**** 15.0  CALCULATE IMPOUNDMENT HYDROGRAPH FOR EACH IMPOUNDMENT ****/

    impoundptr = columndata[column]->impound;


    for (impoundments=1;
		   impoundments<=columndata[column]->num_impoundments;
		   impoundments++)
	{
	sum_impound_volume = 0.0;

	if (impoundptr->volume_runoff != 0.0)
		impoundment_duration = impoundptr->volume_runoff /
			       impoundptr->peak_flow;
	else
	   impoundment_duration=0.0;



	if (impoundment_duration != 0.00)
		num_increments = ceil(impoundment_duration /
				base_increment_duration);
	else
	      num_increments=0;


	assert( num_increments <= max_increments );


	if(hydroinfo)
	  fprintf(imp,"%d         %d           %d        %f          %f     %f\n",
	  column,
	    impoundments, num_increments, impoundptr->peak_flow,
	    base_increment_duration,impoundment_duration);


	for (increment=1; increment<num_increments; increment++)
	    {
	     ave_incr_downflowrate[increment] += impoundptr->peak_flow;


	     sum_impound_volume += impoundptr->peak_flow *
					base_increment_duration;
	    }

      /*        assert( impoundptr->volume_runoff - sum_impound_volume <
			impoundptr->peak_flow * base_increment_duration);*/

	/* Not sure what this is doing-MAK */

      if (impoundptr->volume_runoff > 0.0)
	{

	  if(columndata[column]->receiving_cell_position == 0)
	      ave_incr_downflowrate[num_increments] = 0.0;
	  else
	      ave_incr_downflowrate[num_increments] +=
			  (impoundptr->volume_runoff - sum_impound_volume);

	}

      else
	ave_incr_downflowrate[num_increments] = 0.0;



	if(hydro_info)
	  {
	   impound_data_ptr->volume_runoff=impoundptr->volume_runoff;
	   impound_data_ptr->peak_flow=impoundptr->peak_flow;
	   impound_data_ptr->duration=impoundment_duration;
	   impound_data_ptr->number_increments=num_increments;
	   impound_data_ptr->receiving_cell_position=
	       columndata[column]->receiving_cell_position;
	   impound_data_ptr->left_over_volume=
	      (impoundptr->volume_runoff-sum_impound_volume);
	   impound_data_ptr->next=NULL;
	   fwrite(impound_data_ptr,sizeof(IMPOUND_ROUTE_REC),1,hydrofile2);
	  }




	impoundptr = impoundptr->next;
	}




/** DECAY THE NUTRIENTS WITHIN THE IMPOUNDMENT ***/

  sum_n_impound = 0.0;
  sum_p_impound = 0.0;
  sum_cod_impound = 0.0;
  total_n=0.0;
  total_p=0.0;
  total_cod=0.0;
  total_drainage_area = 0.0;

  impoundptr= columndata[column]->impound;

  for (impoundments=1;impoundments<=columndata[column]->num_impoundments;
       impoundments++)
	{
	 total_drainage_area += impoundptr->drainage_area;
	 impoundptr = impoundptr->next;
	}

  impoundptr= columndata[column]->impound;

  for(impoundments=1;impoundments<=columndata[column]->num_impoundments;
     impoundments++)
      {
      if (impoundptr->volume_runoff != 0.0)
	impoundment_duration = impoundptr->volume_runoff /
			       impoundptr->peak_flow;
      else
	impoundment_duration = 0.0;


	if(impoundptr->volume_runoff != 0.0)
		percent_runoff = impoundptr->volume_runoff/
	   (runoffpath->cell_run_off*IN_ACRE_TO_CU_FT*columndata[column]->area);
	else
	  percent_runoff= 0.0;


	/* Had to use nutrient runoff, because the nutrient yield already
	   has the nutrients from above added to it */

	/* Decay the nutrients that are within the impoundment */

	impoundment_nitrogen = runoffpath->sol_impound_nit *
		  (impoundptr->drainage_area
		   /total_drainage_area);

	impoundment_phosphorus = runoffpath->sol_impound_phos *
		   (impoundptr->drainage_area
		   /total_drainage_area);

	impoundment_cod=runoffpath->sol_impound_cod *
		   (impoundptr->drainage_area
		   /total_drainage_area);


/*      impoundment_nitrogen =
		 runoffpath->soluble_nitrogen_runoff*
		  columndata[column]->area*percent_runoff;

	impoundment_phosphorus =
		 runoffpath->soluble_phosphorus_runoff*
		  columndata[column]->area*percent_runoff;

	impoundment_cod =
		 runoffpath->cod_runoff*
		  columndata[column]->area*percent_runoff; */

	total_n+=impoundment_nitrogen;
	total_p+=impoundment_phosphorus;
	total_cod+=impoundment_cod;


	per_n_decay =   0.005 * impoundment_duration;
	per_p_decay =   0.012 * impoundment_duration;
	per_cod_decay = 0.003 * impoundment_duration;

       if(per_n_decay>10)
	  per_n_decay=10;
       if(per_p_decay>35)
	  per_p_decay=35;
       if(per_cod_decay>10)
	  per_cod_decay=10;

	n_after_decay = impoundment_nitrogen * (1-(per_n_decay/100));
	p_after_decay = impoundment_phosphorus * (1-(per_p_decay/100));
	cod_after_decay = impoundment_cod * (1-(per_n_decay/100));

	runoffpath->soluble_nitrogen_yield -=impoundment_nitrogen;
	runoffpath->soluble_phosphorus_yield -=impoundment_phosphorus;
	runoffpath->soluble_cod_yield -= impoundment_cod;


	runoffpath->soluble_nitrogen_yield+=n_after_decay;
	runoffpath->soluble_phosphorus_yield+=p_after_decay;
	runoffpath->soluble_cod_yield+=cod_after_decay;

	sum_n_impound += n_after_decay;
	sum_p_impound += p_after_decay;
	sum_cod_impound += cod_after_decay;

     if(sourceinfo)
       {
	sourceact2ptr->sol_n_impoundments+=impoundment_nitrogen;
	sourceact2ptr->sol_p_impoundments+=impoundment_phosphorus;
       }

	impoundptr = impoundptr->next;

       }

    if(sourceinfo)
     {
      sourceact2ptr->sol_n_decay=total_n-sum_n_impound;
      sourceact2ptr->sol_p_decay=total_p-sum_p_impound;
      sourceact2ptr->sol_cod_decay=total_cod-sum_cod_impound;
     }


    if (tflags.chem_table)
     {                  /* NOTE: The first two have been moved to recroute */
/*    ctable.ct[column].N_dis_into = runoffpath->soluble_nitrogen_yield;   */
/*    ctable.ct[column].N_att_into = runoffpath->total_n_cell_outlet;      */
      ctable.ct[column].N_tot_into = ctable.ct[column].N_dis_into +
				      ctable.ct[column].N_att_into;

/*    ctable.ct[column].P_dis_into = runoffpath->soluble_phosphorus_yield; */
/*    ctable.ct[column].P_att_into = runoffpath->total_p_cell_outlet;      */
      ctable.ct[column].P_tot_into = ctable.ct[column].P_dis_into +
				      ctable.ct[column].P_att_into;

/*    ctable.ct[column].Pest_dis_into = pest->soluble_pest_exit;           */
/*    ctable.ct[column].Pest_att_into = pest->sediment_pest_exit;          */
      ctable.ct[column].Pest_tot_into = ctable.ct[column].Pest_dis_into +
					 ctable.ct[column].Pest_att_into;

/*    ctable.ct[column].COD_into_lbs = runoffpath->soluble_cod_yield;      */

      if ((ctable.ct[column].COD_into_lbs > 0.0) && (runoff_volume_above > 0.0))
	ctable.ct[column].COD_into_ppm = 1000000     *
      				         ctable.ct[column].COD_into_lbs  /
					 (drainage_area_above * runoff_volume_above *
                                         IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS);

/*    ctable.ct[column].Pest_att_into = pest->sediment_pest_exit;          */
     }


/**** 16.0 CALCULATE TOTAL VOLUME BELOW in cubic feet ****/

    accumpath->runoff_vol_below =
			runoff_volume_below * IN_ACRE_TO_CU_FT *
			columndata[column]->accumulated->drainage_area  +
			psource_volume_bottom;




/**** 17.0 ADD TO NUTRIENTS FROM THE NONFEEDLOT POINT SOURCES *****/

    sum_point_sources( column, duration );


/*** 18.0 CALCULATE THE DECAY RATES FOR THE NUTRIENTS ***/

    if(runoff_volume_below > 0.0)
       decay_nutrients(column, duration);


/*** 19.0 PREPARE TO ROUTE SEDIMENTS *******************************/

    /* BUG */

    if(runoff_volume_below > 0.0)
	calc_initial_sed_info(soilpath->soil_type, column,
			  base_increment_duration,
			  duration, &last_overland_incr);


/*** 20.0 ROUTE SEDIMENTS ******************************************/


/*     if(bflags.channel)
	   {
	     fprintf (stderr,"          channel      channel     Shear      Shear    Velocity    Velocity\n");
	     fprintf (stderr,"Increment width in    width out  Stress in  Stress in     in          out\n");
	   } */

    for (increment=1; increment<=max_increments; increment++)
	{

	/* Skip the channel calculations if the cell is a sinkhole */

	if(columndata[column]->receiving_cell_position != 0)
	 {

	 if (ave_incr_downflowrate[increment] > 0)
	   calculate_channel(column, ave_incr_upflowrate[increment],
		   ave_incr_downflowrate[increment],
		   &velocity_in, &velocity_out, &channel_width_in,
		   &channel_width_out, &shear_stress_in, &shear_stress_out,
		   &channel_depth_out);
	 else
	   {
	    velocity_in =0.0;
	    velocity_out=0.0;
	    channel_width_in = 0.0;
	    channel_width_out = 0.0;
	    shear_stress_in = 0.0;
	    shear_stress_out = 0.0;
	    channel_depth_out = 0.0;
	   }

	 if(bflags.channel)
	   {
	    fprintf (stderr,"\n***********  Column #%d  Increment #%d ************\n",column,increment);
	    fprintf (stderr,"Width: In=%f  Out=%f \n",channel_width_in,channel_width_out);
	    fprintf (stderr,"Shear Stress: In=%f  Out=%f\n",shear_stress_in,shear_stress_out);
	    fprintf (stderr,"Velocity: In=%f  Out=%f\n",velocity_in,velocity_out);
	    fprintf (stderr,"Channel Depth Out = %f \n",channel_depth_out);
	   }
	 }


	newsoil(velocity_out,
		channel_width_out,
		shear_stress_out,
		ave_incr_downflowrate[increment],
		channel->tr55_length,
		column,
		increment,
		base_increment_duration,
		last_overland_incr,
		channel_depth_out,
		number_of_increments,
		duration);
    }

  for (particle_size=1; particle_size<=5; particle_size++)
       {
	     /* calc. amount of sediment for each particle size */


	     sediment_change_in_channel =
		     sed_yield_for_j[particle_size]-
		      (runoffpath->available_sediment[particle_size] +
		      sediment[particle_size].sed_available);

	 if (sourceinfo)
	  {
	   if(particle_size==1)
	      if(sediment_change_in_channel >0.0)
	       sourceact2ptr->clay_bed=sediment_change_in_channel;
	      else
	       sourceact2ptr->clay_deposition=sediment_change_in_channel*-1.0;

	   if(particle_size==2)
	      if(sediment_change_in_channel >0.0)
	       sourceact2ptr->silt_bed=sediment_change_in_channel;
	      else
	       sourceact2ptr->silt_deposition=sediment_change_in_channel*-1.0;

	   if(particle_size==3)
	      if(sediment_change_in_channel >0.0)
	       sourceact2ptr->sagg_bed=sediment_change_in_channel;
	      else
	       sourceact2ptr->sagg_deposition=sediment_change_in_channel*-1.0;

	   if(particle_size==4)
	      if(sediment_change_in_channel >0.0)
	       sourceact2ptr->lagg_bed=sediment_change_in_channel;
	      else
	       sourceact2ptr->lagg_deposition=sediment_change_in_channel*-1.0;

	   if(particle_size==5)
	      if(sediment_change_in_channel >0.0)
	       sourceact2ptr->sand_bed=sediment_change_in_channel;
	      else
	       sourceact2ptr->sand_deposition=sediment_change_in_channel*-1.0;

	 }
     }





/*** 20.0 CALC SEDIMENT ON A CELL BASIS ****************************/

    calc_sediment(column);

/*** 21.0 CALCULATE NUTRIENTS AT CELL OUTLET ***/

  if(columndata[column]->receiving_cell_position != 0)
   {

    sum_of_particle_surfaces = 0.0;
    part_n_yield = 0.0;
    part_p_yield = 0.0;

 /* Calculate the surface area for each particle in the sediment in the
    current cell.  Total these amounts for the soil as a whole. */


    for (particle=1; particle<=5; particle++)
     {
       /* Calcualte the amount of sediment that flows into the current cell,
	 plus the amount from gullies in the current cell, plus the amount
	 generated within the current cell.  Note that sediment[particle]
	 .sed_available includes sediment from above and that from gullies
	 and impoundments.  The runoffpath->available sediment is the amount
	 of sediment from overland flow (USLE). */
 
      sed_avail = (runoffpath->available_sediment[particle] +
		   sediment[particle].sed_available)/2000;
 
      /* sediment_yield is in tons */

      particle_surface[particle] = sed_avail *
				   surface_constant[particle];
 
      sum_of_particle_surfaces += particle_surface[particle];

     }
 
    /* Calculate total of nitrogen and phosphorus flowing from cells
       above and the amount generated within the current cell */
 
    total_nitrogen = runoffpath->total_n_cell_outlet +
		      runoffpath->total_n_within_cell;

    total_phosphorus = runoffpath->total_p_cell_outlet +
		       runoffpath->total_p_within_cell;

    for ( particle=1; particle <=5; particle++)
     {

      /* Calculate the percent of the total surface area that each particle
	 has in the sediment for the current cell */
 
      if (sum_of_particle_surfaces <= 0.0)
	percent_surface[particle] = 0.0;
      else
	percent_surface[particle] = particle_surface[particle] /
				   sum_of_particle_surfaces;

      /* sed avail is in tons */

       /* Calcualte the amount of sediment that flows into the current cell,
	 plus the amount from gullies in the current cell, plus the amount
	 generated within the current cell.  Note that sediment[particle]
	 .sed_available includes sediment from above and that from gullies
	 and impoundments.  The runoffpath->available sediment is the amount
	 of sediment from overland flow (USLE). */
 
      sed_avail = (runoffpath->available_sediment[particle] +
		   sediment[particle].sed_available)/2000;

      /* Calculate the percentage of sediment that is flowing out of the
      cell, compared to the amount that is flowing in and generated in the
      current cell.  Since we know what percent of the nutrients each
      particle size is carrying, and we know the percent of each particle
      size that is depositied, we know the amount of nutrients that are
      depositied along with the sediment. */
 
      /* In the case of peat soils, the percentages for many of the
	 particle sizes are zero */

      if(sed_avail != 0.0)
      percent_flowing_out = runoffpath->sediment_yield[particle] /
			    sed_avail;
      else
      percent_flowing_out = 0.0;
 
 
      /* N and P is in lbs */
 
      part_n_yield += percent_surface[particle] * percent_flowing_out *
		     total_nitrogen;
 
      part_p_yield += percent_surface[particle] * percent_flowing_out *
		   total_phosphorus;
     }
 
    runoffpath->total_n_cell_outlet = part_n_yield;
    runoffpath->total_p_cell_outlet = part_p_yield;
   }
  else
   {
    runoffpath->total_n_cell_outlet = 0.0;
    runoffpath->total_p_cell_outlet = 0.0;
   }

  if(sourceinfo)
   {
    sourceact2ptr->sed_n_deposition=total_nitrogen-part_n_yield;
   /* if(sourceact2ptr->sed_n_deposition<0.0)
      sourceact2ptr->sed_n_deposition=0.0;*/

    sourceact2ptr->sed_p_deposition=total_phosphorus-part_p_yield;
/*    if(sourceact2ptr->sed_p_deposition<0.0)
      sourceact2ptr->sed_p_deposition=0.0; */
   }


/*** 21.0 ReALLOCATE PESTICIDE BETWEEN SEDIMENT AND WATER **********/


 pest = columndata[column]->pesticide;

 if((pest->soluble_pest_exit > 0.0) || (pest->sediment_pest_exit > 0.0))
  {

  if(columndata[column]->receiving_cell_position != 0)
   {
      Kd   = 0.0058 * general_pest_info->organic_carbon_sorption *
				soilpath->soil_organic_matter;

     sediment_lbs = 0.0;

     for (j=1; j<6; j++)
	 sediment_lbs += 2000.0 * columndata[column]->runoff->
			      sediment_yield[j];

     if (accumpath->drainage_area > 0.0)
       partition_pesticide(sediment_lbs,
			 accumpath->runoff_vol_below,
			 sediment_lbs / accumpath->drainage_area,
			 Kd,
			 soilpath->soil_type,
			 column,
			 percent_surface,
			 &pest->sediment_pest_exit,
			 &pest->soluble_pest_exit);
     else
       partition_pesticide(sediment_lbs,
			 accumpath->runoff_vol_below,
			 0.0,
			 Kd,
			 soilpath->soil_type,
			 column,
			 percent_surface,
			 &pest->sediment_pest_exit,
			 &pest->soluble_pest_exit);

   }
  else
   {
    pest->sediment_pest = 0.0;
    pest->soluble_pest = 0.0;
   }
  }



    if (tflags.hydro_table)
     {
      htable.ht[column].pk_dis_outlet_cfs = flow_rate_below;
      htable.ht[column].tot_ro_outlet_in  = runoff_volume_below;
      htable.ht[column].time_to_pk        = time_to_peak_ave;
      htable.ht[column].time_to_base      = duration;

      htable.ht[column].avg_pk_discharge  = (accumpath->runoff_flow_above +
					     flow_rate_below)    /  2;

      htable.ht[column].avg_ro_volume = (runoff_volume_above *
					 drainage_area_above +
					  runoff_volume_below *
					   drainage_area_below ) / 24;

      htable.ht[column].ro_pk_volume = initialptr->prepeak_RO_fraction * 100;
     }




/*** 22.0 CLEAN UP AFTERWARDS **************************************/

    if(hydro_info)
     {
      hydro_route->column=column;
      hydro_route->drainage_area_above=drainage_area_above;
      hydro_route->accumulated_drainage_area=
	      columndata[column]->accumulated->drainage_area;
      hydro_route->area=columndata[column]->area;
      hydro_route->runoff_volume_above=runoff_volume_above;
      hydro_route->accumulated_runoff_volume_above=
	      accumpath->runoff_vol_above;
      hydro_route->time_to_peak_in=time_to_peak_in;
      hydro_route->K_triangle=initialptr->K_triangle;
      hydro_route->runoff_flow_above=accumpath->runoff_flow_above;
      hydro_route->drainage_area_below=drainage_area_below;
      hydro_route->area_not_terraced=columndata[column]->area_not_terraced;
      hydro_route->cell_run_off=columndata[column]->runoff->cell_run_off;
      hydro_route->calc_method=initialptr->calc_method;
      hydro_route->max_depth_new=columndata[column]->channel->max_depth;
      hydro_route->sum_psource_volumes=accumpath->sum_psource_volumes;
      hydro_route->new_runoff_volume_below=runoff_volume_below;
      hydro_route->sum_psource_flows=accumpath->sum_psource_flows;
      hydro_route->adjusted_flow_rate_below=flow_rate_below;
      hydro_route->time_to_peak_out=time_to_peak_out;
      hydro_route->prepeak_RO_fraction=initialptr->prepeak_RO_fraction;
      hydro_route->duration=duration;
      hydro_route->longest_duration=columndata[column]->channel->long_dur;
      hydro_route->new_time_to_peak=time_to_peak_ave;
      hydro_route->water_flow_rate_below_after=flow_rate_below;
      hydro_route->new_channel_side_slope=channel_side_slope;
      hydro_route->new_channel_slope=channel_slope;
      hydro_route->new_bank_depth=columndata[column]->channel->bank_depth;
      hydro_route->base_increment_duration=base_increment_duration;
      hydro_route->total_vol_below=accumpath->runoff_vol_below;
      hydro_route->max_increments=max_increments;


      fwrite(hydro_route,sizeof(HYDRO_ROUTE_REC),1,hydrofile2);
     }

    if(sourceinfo)
    {
     sourceact2ptr->cell_number=columndata[column]->cell_number;
     sourceact2ptr->cell_division=columndata[column]->cell_division;
     if(!source_acct_open2)
	{
	 sourcefile2=fopen(source_acct2,"wb");
	 source_acct_open2=1;
	}



     fwrite(sourceact2ptr,sizeof(SOURCEACCT2),1,sourcefile2);

sourceact2ptr->cell_number=0;
sourceact2ptr->cell_division=0;
sourceact2ptr->clay_bed=0.0;
sourceact2ptr->clay_deposition=0.0;
sourceact2ptr->silt_bed=0.0;
sourceact2ptr->silt_deposition=0.0;
sourceact2ptr->sagg_bed=0.0;
sourceact2ptr->sagg_deposition=0.0;
sourceact2ptr->lagg_bed=0.0;
sourceact2ptr->lagg_deposition=0.0;
sourceact2ptr->sand_bed=0.0;
sourceact2ptr->sand_deposition=0.0;
sourceact2ptr->sed_n_deposition=0.0;
sourceact2ptr->sed_p_deposition=0.0;
sourceact2ptr->sol_n_decay=0.0;
sourceact2ptr->sol_p_decay=0.0;
sourceact2ptr->sol_cod_decay=0.0;
sourceact2ptr->sol_n_nonfeedlots=0.0;
sourceact2ptr->sol_p_nonfeedlots=0.0;
sourceact2ptr->sol_cod_nonfeedlots=0.0;
sourceact2ptr->sol_n_impoundments=0.0;
sourceact2ptr->sol_p_impoundments=0.0;

    }

    free(ave_incr_downflowrate);
    free(ave_incr_upflowrate);

}
