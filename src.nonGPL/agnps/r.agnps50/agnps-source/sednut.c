/*********** Documentation Start **************

$NAME$
NAME
sednut.c -- Calculates the amount of sediment attached Nitrogen and
	    Phosphorus from Overland and Gully Sources.

$PATHS$
    hydrology\runoff
$1$

SYNOPSIS

***********************************************/

#ifdef _DOS

 #include <math.h>
 #include <stdio.h>
 #include "input.h"
 #include <stdlib.h>
 #include "debugflg.h"
/* #include <memcheck.h> */

#else

#include <math.h>
#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif

/*
columndata
errorfp

column
overland_nit
overland_phos
total_gully_sed
*/
/*
EXTERNAL VARIABLES:
*/

extern COLUMN_INFOPTR *columndata;
extern FILE *errorfp;

extern FLAGS_ROUTINE   rflags;
extern FLAGS_TABLE     tflags;
extern CHEM_TABLE      ctable;

/*UNITS: INCHES runoff( NOD, INCHES ) */

#ifdef _UNIX_K_AND_R

 void calc_sed_nut(column, overland_nit, overland_phos, total_gully_sed)

       int column;
       float *overland_nit;
       float *overland_phos;
       float total_gully_sed;

#else

 void calc_sed_nut(int column,float *overland_nit, float *overland_phos, float total_gully_sed)

#endif


{
/********************************************************************
DESCRIPTION

LOCAL VARIABLES:
*/

int    error_code        = 0;
double gully_sediment    = 0.0;
double nit_enrichment_ratio;
double overland_sediment;
double phos_enrichment_ratio;

RUNOFF_INFO *runoffdata;
SOIL_INFO   *soildata;
GULLY_INFO  *gullydata;

double nit_coef  =  7.4;
double nit_exp   = -0.2;
double phos_coef =  7.4;
double phos_exp  = -0.2;

static float transport_factor[5] ={0.0, 0.85, 1.00, 1.15, 1.5};



/*

RETURNS:

EXAMPLE:

NOTES:

DATA STORES:
None

HISTORY:
Date		Bug#	Prog	Desc
09/06/93  	none    MAK	Finished Coding
06/15/93 	A-2	MAK	Problem with the correct type for
				arguments to the procedure.
09/22/93 	C0007	MAK	Finished putting in code to allow for
				multiple gullies.

SEE ALSO:
  loop1 (calling procedure)

********** Documentation end **********/
 
  runoffdata = columndata[column]->runoff;
  soildata   = columndata[column]->soil;
  gullydata  = columndata[column]->gully;
 
 
 
/* Calculate overland sediment in kg/ha */
 
  overland_sediment = (runoffdata->total_eroded_sediment /
		       columndata[column]->area) * LBSpACRE_TO_KGpHA;

/* Calculate the Nitrogen for the current cell from overland sediment */
/* This is calculated using the amount of sediment from the USLE equation */

  if(overland_sediment != 0.0)
   {
     nit_enrichment_ratio = nit_coef * pow(overland_sediment,nit_exp) *
			 transport_factor[soildata->soil_type];

     runoffdata->total_n_within_cell = soildata->base_soil_nitrogen *
				    overland_sediment * nit_enrichment_ratio;
   }


/* Calculate the Phosphorus for the current cell from overland sediment */
 if(overland_sediment != 0.0)
  {
    phos_enrichment_ratio = phos_coef * pow(overland_sediment,phos_exp) *
			  transport_factor[soildata->soil_type];

    runoffdata->total_p_within_cell = soildata->base_soil_phosphorus *
				   overland_sediment * phos_enrichment_ratio;
  }


 *overland_nit=runoffdata->total_n_within_cell/LBSpACRE_TO_KGpHA*
	    columndata[column]->area;
 *overland_phos=runoffdata->total_p_within_cell/LBSpACRE_TO_KGpHA*
	    columndata[column]->area;;


  while(gullydata != NULL)
  {

  /* C0007 Added ability to calculate gullies with different nitrogen levels.
     Allows the ability to use multiple gullies */

  /* Calculate gully sediment in kg/ha */

      gully_sediment = ((gullydata->gully_source * 2000.0) / columndata[column]->area) *
		    LBSpACRE_TO_KGpHA;

  /* Calculate the Nitrogen from Gully */

   if(gully_sediment != 0.0)
    {
     nit_enrichment_ratio = nit_coef * pow(gully_sediment,nit_exp) *
			 transport_factor[gullydata->gully_soil];

     runoffdata->total_n_within_cell += gullydata->gully_nitrogen *
				    gully_sediment * nit_enrichment_ratio;
    }


/* Calcualte the Phosphorus from Gully */

   if(gully_sediment != 0.0)
    {
     phos_enrichment_ratio = phos_coef * pow(gully_sediment,phos_exp) *
			  transport_factor[gullydata->gully_soil];

     runoffdata->total_p_within_cell += gullydata->gully_phos *
				    gully_sediment * phos_enrichment_ratio;
    }

  gullydata=gullydata->next;

 } /* end while loop1 */

 /* End c0007 */




 runoffdata->total_n_within_cell = runoffdata->total_n_within_cell/LBSpACRE_TO_KGpHA
				   * columndata[column]->area;
 runoffdata->total_p_within_cell = runoffdata->total_p_within_cell/LBSpACRE_TO_KGpHA
				   * columndata[column]->area;

 if (rflags.calc_sed)
  {
   fprintf (stderr,"CALC_SED routine: \n");
   fprintf (stderr,"   Input:  %d...col_number\n",column);
   fprintf (stderr,"   Output: %f...ovrlnd_N    %f...ovrlnd_P\n",*overland_nit,*overland_phos);
   fprintf (stderr,"           %f...total_gully_sediment \n",total_gully_sed);
   fprintf (stderr,"           %f...tot_N_within_cell   %f...tot_P_within_cell\n",runoffdata->total_n_within_cell,runoffdata->total_p_within_cell);
  }

  if (tflags.chem_table)
   {
    ctable.ct[column].N_tot_enrichment = nit_enrichment_ratio;
    ctable.ct[column].P_tot_enrichment = phos_enrichment_ratio;
   }



return;
}