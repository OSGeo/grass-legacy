
#ifdef _DOS
 
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dos.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#else

#include <math.h>
#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif






extern COLUMN_INFOPTR *columndata;
extern int columns;
extern GENERAL_PESTICIDE_DATA *general_pest_info;
extern int pesticideinfo;
extern FILE *errorfp;
extern FILE *pestfile;
extern FILE *pestfile2;
extern pest_info;
extern int pest_file_open;
extern int pest_file_open2;
extern char pest_out_file[128];
extern char pest_out_file2[128];
extern SEDIMENT_INFO sediment[];


extern PEST_REC_PTR		pest_ptr;
extern PEST_ROUTE_REC_PTR       pestroute;


extern FLAGS_ROUTINE            rflags;
extern FLAGS_TABLE              tflags;
extern CHEM_TABLE               ctable;


#ifdef _UNIX_K_AND_R

float bulk_den_lookup();

#else

float bulk_den_lookup(int type_of_soil);

#endif
static float tf[] = {0.00, 0.85, 1.00, 1.15, 1.5, 1.15};


/* CHANGES
Date		Bug	Prog.	Description
10/07/93	C0009	MAK	Need to include math.h for UNIX version
10/07/93	C0010	MAK	Change the file open option to WB for routing
10/07/93	C0011	MAK     Change file pointer to global, not both local and global
11/03/93	C0018	MAK	Added long filenames

*/



/**************************************************************************/
/*                                                                        */
/* Agnps pesticide extraction routine.  Taken largely from CREAMS but     */
/*   modified by Dan Pantone.  Coded into C by Eric Hjelmfelt.            */
/*                                                                        */
/*                                                                        */
/* Key Units calculation:                                                 */
/*                                                                        */
/*   kg      ha         m^2      10^3 g   10^6 micro g   10 micro g       */
/*   -- x -------- x --------- x ------ x ------------ = ----------       */
/*   ha   10^4 m^2   10^4 cm^2     kg         g             cm^2          */
/*                                                                        */
/* With that in mind:                                                     */
/*                                                                        */
/*   10 micro g     cm^3       1      10 micro g                          */
/*   ---------- x   ----  x   ---   = ---------- =  10 ppm (soil)         */
/*      cm^2         g        cm          g                               */
/*                                                                        */
/*   App_rate   / Blk_den /  depth  =  ppm soil                           */
/*                                                                        */
/*   The depth is 1 cm (CREAMS model only considers interaction w/ top cm)*/
/*                                                                        */
/* Therefore, to convert kg/ha to ppm soil we need only to multiply       */
/*   by 10 and divide by the bulk density                                 */
/*                                                                        */
/**************************************************************************/




#ifdef _UNIX_K_AND_R


void pesticide(soil_type, rainfall, runoff, sediment, cell_area,
	       pest, genpest, column_number)

	       int soil_type;
	       float rainfall;
	       float runoff;
	       float sediment;
	       float cell_area;
	       PESTICIDE_INFO *pest;
	       GENERAL_PESTICIDE_DATA *genpest;
	       int column_number;


#else

void pesticide( int    soil_type,
		float  rainfall,        /* in inches                    */
		float  runoff,          /* in inches                    */
		float  sediment,        /* eroded sediment    lbs/acre  */
		float  cell_area,       /* cell area in acres           */
		PESTICIDE_INFO *pest,
		GENERAL_PESTICIDE_DATA *genpest,
		int column_number)


#endif

{
float effpest           = 0.0; /* effective amount of pesticide available lbs/acre */
float pestplant         = 0.0; /* new pesticide on plant canopy           lbs/acre */
float initpestplant     = 0.0; /* initial pesticide on the plant          lbs/acre */
float pestsoil          = 0.0; /* new pesticide in soil layer             lbs/acre */
float initpestsoil      = 0.0; /* initial pesticide in the soil           lbs/acre */
float init_pestsoil_ppm = 0.0; /* initial parts pest per million parts soil     */
float bulk_density      = 0.0; /* looked up from table based on soil_type  g/cm^3  */
float totpestplant      = 0.0; /* pesticide on plant at start of event    lbs/acre */
float totpestsoil       = 0.0; /* pesticide in soil at start of event     ppm soil */
float washoff           = 0.0; /* amount foliar pesticide moved to soil   ppm soil */
float comb_pestsoil     = 0.0; /* actual pesticide avail                  ppm soil */
float porosity          = 0.0; /* top layer soil porosity                 nod      */
float surf_storage      = 0.0; /* water stored in top cm of soil          mm       */
float Kd                = 0.0; /* soil/water partitioning coefficient     nod      */
float B_value           = 0.0; /* extraction coefficient                  nod      */
float efi               = 0.0; /* effective infiltration                  mm       */
float Cav               = 0.0; /* runoff avail pesticide in soil          ppm soil */
float enrich_ratio      = 0.0; /* enrichment ratio                        nod      */
int   error_code        = 0;
char problem[80];
char problem2[80];
PEST_REC pest_data;
 
  if( soil_type!=5)
   {
   /* Use the soil type to look up the appropriate bulk density */
 
    bulk_density  = bulk_den_lookup(soil_type);
 
   /* 4.2.2.1 Calculate Soil Porosity */
 
    porosity      = 1.0 - bulk_density/ROCK_DENSITY;
 
   /* End 4.2.2.1 */

   /* 4.2.2.2 Calculate the surface storage (mm)*/

    surf_storage  = 10.0 * porosity;

   /* End 4.2.2.2 */


   /* 4.2.3.3 Calc. enrichment ratio */

    if (sediment > 0.0)
     {
      enrich_ratio  = 7.4 * pow((sediment/cell_area) * LBSpACRE_TO_KGpHA, -0.2) *
                       tf[soil_type];
     }
    else               /* SUPP! Added 04/28/95 by JW for KB verification. */
      enrich_ratio = 0.0;

    if (tflags.chem_table)
     {
      ctable.ct[column_number].Pest_tot_enrich = enrich_ratio;
     }

   /* End 4.2.3.3 */


   /* 4.2.2.4 Calculate Soil/Water Partitioning Coef. */

    Kd = 0.0058 * genpest->organic_carbon_sorption *
		columndata[column_number]->soil->soil_organic_matter;

   /* End 4.2.2.4 */

   /* 4.2.3.1 Addendum Lookup Extractional Coefficient */

    if (Kd <= 1.0)
	B_value = 0.5;
    else if (Kd <= 3.0)
	B_value = 0.7 - 0.2 * Kd;
    else
	B_value = 0.1;

   /* End 4.2.3.1 Addendum */


  } /* End if soil type is water */

   /* 4.1.1.1 Determine Pesticide on field (lbs/A)*/

    effpest       = pest->application_rate *
		      (pest->application_efficiency/100);

   /* End 4.1.1.1 */

  if(soil_type !=5)
   {

   /* 4.1.1.2 Determine Pesticide on plant (lbs/A)*/

    pestplant     = effpest * pest->canopy_cover/100.0;
    initpestplant = pestplant + pest->foliar_residue;

   /* End 4.1.1.2 */

   /* 4.1.1.3 Determine Pesticide on soil (ppm)

      This calculation is a bit different than in the paper of equations.
      Instead of using that equation, we just subtract the amount of
      pesticide on the plant from the total amount of pesticide on the
      field.  The equation for calculating the concentration of pesticide
      in the soil, refer to equations I-190 and I-192 in the Creams manual
      on page 91. */


    pestsoil      = effpest - pestplant;
    initpestsoil  = pestsoil + pest->initial_soil_residue;

    if(pest_info)
    {
      pest_ptr->pestsoil=pestsoil;
      pest_ptr->total_sed_pest=initpestsoil;
      pest_ptr->remaining_pesticide_on_plant=effpest;
    }

   /* Convert to concentration */

    if(bulk_density > 0)
       init_pestsoil_ppm = initpestsoil * LBSpACRE_TO_KGpHA *
				(10.0 / bulk_density);
    else
      error_code = 1;

   if(pest_info)
    pest_ptr->init_pestsoil_ppm =init_pestsoil_ppm;


   /* End 4.1.1.3 */




   /* 4.1.3 Determine Tillage Effect (cm)*/


    if (pest->incorporation_depth > 0.0)
	init_pestsoil_ppm = init_pestsoil_ppm *
		((pest->incorporation_efficiency/100) /
		(pest->incorporation_depth/CM_TO_INCHES));

   /* End 4.1.3 */


/* ********** Equation I-196 of CREAMS on page 92 *********************** */

    /* 4.1.2 Calculate Pesticide on Foliage (lbs/A) */

    if(pest->time_of_application > 2)
      {

       if(genpest->foliar_residue_halflife > 0.0)
	 totpestplant = initpestplant * exp( -0.693 *
	       pest->time_since_application /
	       genpest->foliar_residue_halflife);
       else
	 error_code = 2;
      }


   /* End 4.1.2 */


   /* 4.1.4 Calculate Pesticide on Soil (ppm) */

    if( genpest->soil_residue_halflife > 0.0)
      totpestsoil = init_pestsoil_ppm * exp( -0.693 *
	       pest->time_since_application /
	       genpest->soil_residue_halflife);

    else
      error_code = 3;


   /* End 4.1.4 */


    /* 4.2.1 Calculate amount of pesticide on ground
       This amount of pesticide that is on the ground is due
       to amount that was originally on the ground and the amount
       that has washed off. */

    if (rainfall > genpest->foliar_washoff_threshold)
     {
      if(bulk_density > 0.0)
	washoff = totpestplant * LBSpACRE_TO_KGpHA *
			(genpest->foliar_washoff_fraction/100) *
					10.0 / bulk_density;
     }
    else
	washoff = 0.0;


    comb_pestsoil = totpestsoil + washoff;

   /* End 4.2.1 */


    /* 4.2.2.3 Calculate Infiltration Rate */

    efi = 25.4 * (rainfall - runoff) - surf_storage;    /* mm */

    if (efi < 0.0)
      efi = 0.0;       /* SUPP! added 04/28/95 by JW for KB verification. */

   /* End 4.2.2.3 */

/* ************* Equation I-204 in CREAMS on page 94 ******************** */
/* ************* except we use concentration instead of amount ********** */


    /* 4.2.2.5 Calc. Pesticide Runoff */

    if (genpest->solubility > 1)       /* ppm */
       {
	if((porosity + Kd * bulk_density) > 0.0)
	   Cav = comb_pestsoil * exp(-efi * 0.1/(porosity + Kd * bulk_density));
	else
	   error_code = 4;

	}
    else
	Cav = comb_pestsoil;

    /* End 4.2.2.5 */




/* *********** calculate percolated pesticide amount here  */
/* changes made to add soil percolation */

    pest->percolated = comb_pestsoil - Cav;

    pest->percolated = pest->percolated * (bulk_density/10.0)
		       / LBSpACRE_TO_KGpHA;

    if(pest->application_rate > 0.0)
       pest->percolation_per = 100 * pest->percolated / pest->application_rate;
    else
       error_code = 5;


/* ************ Equations I-209  CREAMS on page 95 ********************* */

 /* 4.2.3.1 Calculate amount of pesticide in runoff water (ppm) */

    if ((1.0 + B_value * Kd) > 0.0)
     pest->soluble_con   = Cav * B_value / (1.0 + B_value * Kd);
    else
     error_code = 6;

   if(pest_info)
    pest_ptr->soluble_con=pest->soluble_con;

							/* ppm water */
  /* Check to see that the runoff concentration is not greater than
     the solubility of the pesticide  */

    if (pest->soluble_con > genpest->solubility)
	pest->soluble_con = genpest->solubility;

  /* End 4.2.3.1 */


/* **** H2O_con is in ppm water, runoff is in inches,   ***************** */
/* **** soluble_pest is in lbs/acre                     ***************** */
/*                                                                        */
/*  mg   mm   lbs   ha            lbs                                     */
/*  -- x -- x --- x --  x 100  =  ----                                    */
/*  L    1    ac    kg            acre                                    */
/*                                                                        */
/*  mg   1000 L   mm      m      lbs   ha   10^4 m^2    kg         g      */
/*  -- x ------ x -- x ------- x --- x -- x -------- x ------ x -------   */
/*  L      m^3    1    1000 mm   ac    kg      ha      10^3 g   10^3 mg   */
/*                                                                        */
/*                  * 100   = lbs/acre                                    */
/*                                                                        */
/**************************************************************************/


    pest->soluble_pest = pest->soluble_con * (runoff * .254)  /
					  LBSpACRE_TO_KGpHA;

    if ( pest->application_rate > 0.0)
       pest->soluble_pest_per = 100.0 * pest->soluble_pest /
						pest->application_rate;

/* ************ Equations I-210  CREAMS on page 95 ********************* */

    /* 4.2.3.2 Calc. amount of pesticide in soil */

      pest->soil_con      = pest->soluble_con * Kd;        /* ppm soil  */

    /* End 4.2.3.2 */


  /* 4.2.3.4 Calc concentration of pesticide in sediment */

    pest->sediment_con  = pest->soil_con * enrich_ratio;     /* ppm soil  */

  /* End 4.2.3.4 */

    /** change made by dan pantone **/
/*  pest->sediment_pest = pest->soil_pest * enrich_ratio; */

    pest->sediment_pest = (pest->sediment_con * (sediment/1000000.0)/cell_area);/* lbs/acre  */
   if(pest_info)                       /* SUPP!  used to divide by 40 rather than cell_area! */
    {
      pest_ptr->soluble_pest=pest->soluble_pest;
      pest_ptr->sediment_pest=pest->sediment_pest;
    }


    if( pest->application_rate > 0.0)
	pest->sediment_pest_per = 100.0 * pest->sediment_pest /
					pest->application_rate;

  } /* end if soil type is water */


/* ******* convert the two to be routed into lbs from lbs/acre ********** */

    pest->sediment_pest *= cell_area;
    pest->soluble_pest  *= cell_area;


 if (rflags.pesticide)
  {
   fprintf (stderr,"PESTICIDE routine: \n");
   fprintf (stderr,"   Input:  %d...col_number   %d...soil_id\n",column_number,soil_type);
   fprintf (stderr,"           %f...rainfall     %f...runoff\n",rainfall,runoff);
   fprintf (stderr,"           %f...sediment     %f...cell_area\n",sediment,cell_area);
   fprintf (stderr,"   Output: %f...pesticide_in_sediment \n",pest->sediment_pest);
   fprintf (stderr,"           %f...soluble_pesticide \n",pest->soluble_pest);
  }



 if(pest_info == TRUE)
  {
   if(pest_file_open==0)
     {
      pestfile = fopen(pest_out_file,"wb");
      fwrite(&columns,sizeof(int),1,pestfile);
     }

   pest_file_open++;

   pest_ptr->column_number=column_number;
   pest_ptr->soil_type=soil_type;
   pest_ptr->bulk_density=bulk_density;
   pest_ptr->porosity=porosity;
   pest_ptr->surf_storage=surf_storage;
   pest_ptr->enrich_ratio=enrich_ratio;
   pest_ptr->tf=tf[soil_type];
   pest_ptr->sediment=sediment;
   pest_ptr->Kd=Kd;
   pest_ptr->B_value=B_value;
   pest_ptr->effpest=effpest;
   pest_ptr->pestplant=pestplant;
   pest_ptr->initpestplant=initpestplant;
   pest_ptr->total_after_app=pest->foliar_residue;
   pest_ptr->initpestsoil=pest->initial_soil_residue;
   pest_ptr->init_pestsoil_after=init_pestsoil_ppm;
   pest_ptr->totpestplant=totpestplant;
   pest_ptr->totpestsoil=totpestsoil;
   pest_ptr->rainfall=rainfall;
   pest_ptr->runoff=runoff;
   pest_ptr->washoff=washoff;
   pest_ptr->comb_pestsoil=comb_pestsoil;
   pest_ptr->efi=efi;
   pest_ptr->Cav=Cav;
   pest_ptr->percolated=pest->percolated;
   pest_ptr->percolated_per=pest->percolation_per;
   pest_ptr->soluble_after=pest->soluble_con;
   pest_ptr->soluble_pest=pest->soluble_pest;
   pest_ptr->soluble_pest_per=pest->soluble_pest_per;
   pest_ptr->solubility=genpest->solubility;
   pest_ptr->soil_con=pest->soil_con;
   pest_ptr->sediment_con=pest->sediment_con;
   pest_ptr->sediment_pest_per=pest->sediment_pest_per;
   pest_ptr->sediment_lbs=pest->sediment_pest;
   pest_ptr->soluble_lbs=pest->soluble_pest;
   pest_ptr->cell_area=cell_area;
   pest_ptr->application_rate=pest->application_rate;
   pest_ptr->application_efficiency=pest->application_efficiency;
   pest_ptr->canopy_cover=pest->canopy_cover;
   pest_ptr->incorporation_depth=pest->incorporation_depth;
   pest_ptr->incorporation_efficiency=pest->incorporation_efficiency;
   pest_ptr->time_since_application=pest->time_since_application;
   pest_ptr->foliar_residue_half_life=genpest->foliar_residue_halflife;
   pest_ptr->soil_residue_half_life=genpest->soil_residue_halflife;
   pest_ptr->foliar_washoff_threshold=genpest->foliar_washoff_threshold;
   pest_ptr->foliar_washoff_fraction=genpest->foliar_washoff_fraction;
   pest_ptr->organic_carbon_sorption=genpest->organic_carbon_sorption;
   pest_ptr->organic_matter=
	   columndata[column_number]->soil->soil_organic_matter;

   fwrite(pest_ptr,sizeof(PEST_REC),1,pestfile);
  }



     if(error_code > 0)
      {
       fprintf(errorfp,"Error Occured in the Pesticide cell routine!!\n");
       fprintf(errorfp,"Error occured while processing cell..%d \n",column_number);
       fprintf(errorfp,"Routine is located in loop 1 (pesticid.c) \n");
       fprintf(errorfp,"------------------------------------------------------------\n\n");


       if(error_code == 1)
	 {
	   strcpy(problem,"Bulk Density is zero. ");
	   strcpy(problem2,"Check to make sure there is a valid soil type.");
	 }
       if(error_code == 2)
	 {
	  strcpy(problem,"Foliar Residue half life <= 0.0-Check input values");
	  strcpy(problem2,"Check that there is a value input for foliar residue half life");
	 }
       if(error_code == 3)
	 {
	  strcpy(problem,"Soil Residue half life <= 0.0-Check input values");
	  strcpy(problem2,"Check to see that there is a value input for soil residue half life");
	 }
       if(error_code == 4)
	 {
	  strcpy(problem,"Porosity+Kd*bulk density = 0 -Check the input values");
	  strcpy(problem2,"for soil type, organic_carbon_sorption, and soil organc matter");
	 }
       if(error_code == 5)
	{
	 strcpy(problem,"Application Rate <= 0.0-Check input values");
	 strcpy(problem2,"Check to see if there is a value input for application rate");
	}
       if(error_code == 6)
	{
	 strcpy(problem,"1+B_value*Kd = 0 - Check the input values for");
	 strcpy(problem2,"organic carbon sorption and soil organic matter");
	}


       fprintf(errorfp,"ERROR #%d    %s \n",error_code,problem);
       fprintf(errorfp,"%s \n\n",problem2);
       fprintf(errorfp,"========================================================= \n");

       fprintf(errorfp,"Data Dump........\n");
       fprintf(errorfp,"INPUT:  Bulk Density            :     %f \n",
		bulk_density);
       fprintf(errorfp,"        Soil Type	        :     %d \n",
	       soil_type);
       fprintf(errorfp,"        Foliar Residue Half Life:     %f \n",
		genpest->foliar_residue_halflife);
       fprintf(errorfp,"        Soil Residue Half Life  :     %f \n",
		genpest->soil_residue_halflife);
       fprintf(errorfp,"        Application Rate        :     %f \n",
	 pest->application_rate);
       fprintf(errorfp,"        Porosity                :     %f \n",
	      porosity);
       fprintf(errorfp,"        Kd Value                :     %f \n",
	      Kd);
       fprintf(errorfp,"        B value                 :     %f \n",
	      B_value);
       fprintf(errorfp,"        Organic Carbon Sorption :     %f \n",
	     genpest->organic_carbon_sorption);
       fprintf(errorfp,"        Soil Organic Matter     :     %d \n",
	     columndata[column_number]->soil->soil_organic_matter);
       fprintf (stderr,"Error encountered-Program stopped-Check error.log");
       fclose(errorfp);
       sleep(5);
       exit(1);
    }

}




/* ********************************************************************** */
/*                                                                        */
/* partition_pesticide() solves the two simultaneos equations:            */
/*                                                                        */
/*  sed_portion      water                                                */
/*  ----------- x -------------  =  Kd_adjust  =  Kd * enrichment_ratio   */
/*   sediment     water_portion                                           */
/*                                                                        */
/*  sed_portion + water_portion = Total_pesticide_available               */
/*                                                                        */
/* ********************************************************************** */
 
 
 
#ifdef _UNIX_K_AND_R
 
  void partition_pesticide(sediment_lbs, water,
	      sediment_yield,Kd, soil_type, column, percent_surface,
	      sediment_portion, water_portion)
 
		 float sediment_lbs;      /* lbs      */
		 float water;             /* ft^3     */
		 float sediment_yield;    /* lbs/acre */
		 float Kd;                /* Cs / Cw  */
		 int   soil_type;
		 int   column;
		 float *percent_surface;
		 float *sediment_portion; /* lbs      */
		 float *water_portion;    /* lbs      */
 
#else
 
  void partition_pesticide(float sediment_lbs,      /* lbs      */
			   float water,             /* ft^3     */
			   float sediment_yield,    /* lbs/acre */
			   float Kd,                /* Cs / Cw  */
			   int   soil_type,
			   int   column,
			   float *percent_surface,
			   float *sediment_portion, /* lbs      */
			   float *water_portion)    /* lbs      */

#endif

{
float enrich_ratio;
int   error_code=0;
float water_lbs;
float sed_avail;
float percent_flowing_out;
float total_pesticide;
float Kd_adjust;
float pesticide_yield=0.0;
int   particle;
float water_con; /* concentration of pesticide in water   ppm */
char problem[20];
char problem2[20];
PEST_ROUTE_REC pest_data_route;
 
RUNOFF_INFO *runoffpath;
 
 
 
  runoffpath=columndata[column]->runoff;
 
/* Now that the amount of pesticide in the sediment has been repartitioned,
   we calculate the amount of pesticide lost, due to deposition of sediment
*/


   if(pest_info)
    {
     pestroute->initial_sediment_portion=*sediment_portion;
     pestroute->water_before=*water_portion;
    }
 
 
   for (particle=1; particle<=5; particle++)
    {
 
      sed_avail = (runoffpath->available_sediment[particle] +
			   sediment[particle].sed_available)/2000;
 
      if(sed_avail != 0.0)
	  percent_flowing_out = runoffpath->sediment_yield[particle] /
			    sed_avail;
      else
	  percent_flowing_out = 0.0;
 
      if(percent_flowing_out > 1.0)
       percent_flowing_out=1.0;
 
 
   /* Pesticide is in lbs */
 
      pesticide_yield += percent_surface[particle] * percent_flowing_out *
		     *sediment_portion;
 
      if(pest_info)
       {
	pestroute->sed_avail[particle]=sed_avail;
	pestroute->avail_sed[particle]=runoffpath->
		   available_sediment[particle];
	pestroute->sediment_avail[particle]=sediment[particle].sed_available;
	pestroute->percent_flowing_out[particle]=percent_flowing_out;
	pestroute->sed_yield[particle]=runoffpath->sediment_yield[particle];
	pestroute->percent_surface[particle]=percent_surface[particle];
	pestroute->pesticide_yield[particle]=pesticide_yield;
       }
 
 
 
   }
 
 
 
  *sediment_portion=pesticide_yield;
 
 
  /* If we want to decay soluble pesticides, we would add that routine
     at this point. MAK */
 

  total_pesticide=*sediment_portion + *water_portion;
 
 
    /* Calculate enrichment ratio */
 
     if((columndata[column]->receiving_cell_position == 0) ||
         !( sediment_yield > 0.0))   /* SUPP! Added 04/28 jw & kb...  */
       enrich_ratio = 0.0;
     else
       enrich_ratio = 7.4 * pow(sediment_yield * LBSpACRE_TO_KGpHA, -0.2)
		     * tf[soil_type];
 
    /* End enrichment ratio calc */
 
 
    /* Calculate adjusted partitioning coefficient */
 
    Kd_adjust = Kd * enrich_ratio;
 
    /* end kd adjustment */
 
    /* Calc amount of water in lbs */
 
    water_lbs = water * CU_FT_H2O_IN_LBS; /* convert water to lbs */
 
 
 
    /* Calc. amount of pesticide in sediment and water */
 
    if(columndata[column]->receiving_cell_position == 0)
     {
      *sediment_portion = 0.0;
      *water_portion = 0.0;
     }
    else
     {
      if((water_lbs + Kd_adjust * sediment_lbs)>0.0)
	 *sediment_portion = Kd_adjust * sediment_lbs * total_pesticide /
				     (water_lbs + Kd_adjust * sediment_lbs);
      else
	error_code = 1;
 
 
      *water_portion = total_pesticide - *sediment_portion;
 

 
     }


     /* added to check if the pesticide concentration in the water is
       greater than the water solubility value of the pesticide */

     /* Check to see that solubility rule is met */


     if((water_lbs/1000000.0)>0.0)
	water_con = *water_portion/(water_lbs/1000000.0);
     else
	error_code = 2;

     if(pest_info)
	 pestroute->water_con_before=water_con;

     if(water_con > general_pest_info->solubility)
      {
       water_con = general_pest_info->solubility;
       *water_portion = water_con * (water_lbs/1000000.0);
      }



 if(pest_info == TRUE)
  {
   if(pest_file_open2==0)
     {
      pestfile2 = fopen(pest_out_file2,"wb");
      fwrite(&columns,sizeof(int),1,pestfile2);
     }

   pest_file_open2++;

   pestroute->column=column;
   pestroute->enrich_ratio=enrich_ratio;
   pestroute->sediment_yield=sediment_yield;
   pestroute->soil_type=soil_type;
   pestroute->tf=tf[soil_type];
   pestroute->Kd_adjust=Kd_adjust;
   pestroute->Kd=Kd;
   pestroute->water_lbs=water_lbs;
   pestroute->water=water;
   pestroute->receiving_cell_num=columndata[column]->
			   receiving_cell_number;
   pestroute->receiving_cell_div=columndata[column]->
			   receiving_cell_division;
   pestroute->sediment_portion=*sediment_portion;
   pestroute->sediment=sediment_lbs;
   pestroute->total_pesticide=total_pesticide;
   pestroute->solubility=general_pest_info->solubility;
   pestroute->water_con_after=water_con;
   pestroute->water_portion_after=*water_portion;


   fwrite(pestroute,sizeof(PEST_ROUTE_REC),1,pestfile2);
  }



    /* Error check routines */

   if(error_code > 0)
      {
       fprintf(errorfp,"Error Occured in the Pesticide Routing routine!!\n");
       fprintf(errorfp,"Error occured while processing cell..%d \n",column);
       fprintf(errorfp,"Routine is located in routing loop  (pesticid.c) \n");
       fprintf(errorfp,"========================================================= \n\n");


       if(error_code == 1)
	 {
	   strcpy(problem,"water_lbs + Kd_adjust * sediment <= 0.0");
	   strcpy(problem2,"Check to see if there is any sediment leaving cell");
	 }
       if(error_code == 2)
	 {
	  strcpy(problem,"water_lbs/1000000 <= 0.0 ");
	  strcpy(problem2,"Check that there actually is some runoff.");
	 }


       fprintf(errorfp,"ERROR #%d    %s \n",error_code,problem);
       fprintf(errorfp,"%s \n\n",problem2);
       fprintf(errorfp,"========================================================= \n");

       fprintf(errorfp,"Data Dump........\n");
       fprintf(errorfp,"INPUT:  Water (lbs)             :     %d \n",
		water_lbs);
       fprintf(errorfp,"        Adjusted Kd value       :     %d \n",
	       Kd_adjust);
       fprintf(errorfp,"        Sediment amount         :     %d \n",
		sediment);
       fprintf (stderr,"Error encountered-Program stopped-Check error.log");
       fclose(errorfp);
       sleep(5);
       exit(1);
    }

}
