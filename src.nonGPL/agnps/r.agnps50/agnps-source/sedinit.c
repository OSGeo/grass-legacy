/************************ Documentation Start **********************
 
 
NAME: sedinit.c
 
 
SYNOPSIS:
 
HEADER FILES:
*/
 
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

/*
FUNCTION PROTOTYPES:
*/
 
extern float soil_break_down[6][7];    /* [soil_type][particle_type] */
extern COLUMN_INFOPTR *columndata;
extern SEDIMENT_DATA   outlet_sediment[];
extern SEDIMENT_INFO   sediment[];
extern int sedimentinfo;
 
/*
GLOBAL VARIABLES:
*/
 
extern sinkhole;
 
 
/*************************************************************************/
 
#ifdef _UNIX_K_AND_R
 
void calc_initial_sed_info(soil_type_num, column, increment_duration,
		       duration, last_overland_incr)
 
	int   soil_type_num;
	int   column;
	float increment_duration;
	float duration;
	int   *last_overland_incr;
 
#else
 
void calc_initial_sed_info(int   soil_type_num,
			   int   column,
			   float increment_duration,
			   float duration,
			   int   *last_overland_incr)
 
#endif
 
{
 
 
/*
INPUT PARAMETERS:
 
      soil_type_num      (int) user input for the type of soil in current cell
      column             (int) current column number
      increment_duration (float) duration in secs of each increment
      duration           (float)
*/
 
int   j			      = 0;  /* j = particle type */
float particle_fraction       = 0.0;
 
RUNOFF_INFO *runoff;                 /* temp ptr to runoff data */
 
 
 
/****************************************************************
 
DESCRIPTION:
 
RETURNS:
 
NOTES:
 
DATA STORES:
 
HISTORY:
 
SEE ALSO:
 
**************** Documentation End ********************************/
 
 /*** CALCULATE SEDIMENT FLOW FOR EACH PARTICLE SIZE ***/
 
      runoff = columndata[column]->runoff;
 
 
      *last_overland_incr = ceil(runoff->overland_flow_duration /
			     increment_duration);
 
      if(sedimentinfo)
	{
	   fprintf (stderr,"INITIAL SEDIMENT INFORMATION \n");
	   fprintf (stderr,"                       sediment   sediment   overland  Cumul.\n");
	   fprintf (stderr,"  particle             flow rate  flow rate  available  area\n");
	   fprintf (stderr,"    size    fraction      into     within    sediment  erosion\n");
	   fprintf (stderr,"--------------------------------------------------------------\n");
	}
 
 
      for (j=1; j<=5; j++)    /* j represents particle type */
	 {
 
	   /*** 1.0 CALCULATE PARTICLE SIZE FRACTIONS OF SOIL ***/
 
/**	   particle_fraction = soil_break_down[soil_type_num][j]; */
 
/**
 
	   sediment[j].sed_flow_rate_into = runoff->available_sediment[j] /
					    duration;
 
 
	   sediment[j].sed_available = runoff->total_eroded_sediment *
		      columndata[column]->area_not_terraced *
		      particle_fraction / columndata[column]->area +
		      runoff->sediment_yield[j] * 2000.0;
 
 
 
	   sediment[j].sed_flow_rate_within = sediment[j].sed_available /
					runoff->overland_flow_duration;
*/
 
	 /* New section added to account for SCS sediment change */
 
	  particle_fraction = soil_break_down[soil_type_num][j];
 
	  sediment[j].sed_available = runoff->total_eroded_sediment *
		      columndata[column]->area_not_terraced *
		      particle_fraction / columndata[column]->area +
		      runoff->sediment_yield[j] * 2000.0;
 
	/* This sed fleo rate into is only for the duration of the cell */
 
	 sediment[j].sed_flow_rate_into =
		(runoff->available_sediment[j] + sediment[j].sed_available)
		/duration;
 
 
 
 
 
	    if(sinkhole == FALSE)
	    outlet_sediment[j].area_weighted_erosion +=
	       runoff->total_eroded_sediment * particle_fraction / 2000.0;
 
	 if(sedimentinfo)
	    fprintf (stderr,"%d      %f    %f    %f     %f     %f\n",j,particle_fraction,
		  sediment[j].sed_flow_rate_into,sediment[j].sed_flow_rate_within,
		  sediment[j].sed_available,outlet_sediment[j].area_weighted_erosion);
 
 
	  }
 
 
 
   return;
} 