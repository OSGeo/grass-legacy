 
/* include files */
 
#ifdef _DOS
 
 #include <stdlib.h>
 #include <math.h>
 #include <stdio.h>
 #include "input.h"
 #include "debugflg.h"
/* #include <memcheck.h> */
 
#else
 
#include <math.h>
#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif
 
 
/* GLOBAL VARIABLES: */
extern COLUMN_INFOPTR *columndata;
extern int sourceinfo;

extern SOURCEACCT    sourceact;
extern SOURCEACCTPTR sourceactptr;

extern INITIAL_INFOPTR initialptr;
extern int debuginfo;

extern FLAGS_BASIC   bflags;
extern FLAGS_ROUTINE rflags;

/* PROTOTYPES */
 
#ifdef _UNIX_K_AND_R
 
float bulk_den_lookup();
void  slnut();
 
#else
 
float bulk_den_lookup(int);
void  slnut(int, float, float*, float*);
 
#endif
 
/********************************************************************/
/*                                                                  */
/* slnut      = soil nutrition  	                            */
/* Used in loop1.c only                                             */
/* calculates: nitrogen_runoff                                      */
/*             phosphorus_runoff                                    */
/*                                                                  */
/********************************************************************/
 
/*UNITS: VOID slnut( , INCHES, LBS/ACRE, LBS/ACRE ) */
 
#ifdef _UNIX_K_AND_R
 
void slnut(column_number, run_off, nitrogen_runoff, phosphorus_runoff)
 
     int   column_number;
     float run_off;
     float *nitrogen_runoff;
     float *phosphorus_runoff;
 
#else
 
void slnut(int   column_number,
	   float run_off,                /* inches             */
	   float *nitrogen_runoff,       /* output as lbs/acre */
	   float *phosphorus_runoff)     /* output as lbs/acre */
 
#endif
 
{
MANAGEMENT_INFO *mgmtpath;
SOIL_INFO       *soilpath;
 
int    error;
double rainfall;                     /* rainfall converted to mm      */
double runoff;                       /* runoff   converted to mm      */
float  porosity;                     /* fraction of pore space        */
double nitrogen_fert;                /* fertilizer N            kg/ha */
double phosphorus_fert;              /* fertilizer P            kg/ha */
double surface_soluble_phos;         /*                         kg/ha */
double soluble_nitrogen;             /* in soil surface & fert. kg/ha */
double soluble_phosphorus;           /* in soil surface & fert. kg/ha */
double total_porosity;               /* pore volume in top cm (in mm) */
double effective_rainfall;           /* rain - pore storage      mm   */
double effective_infiltration;       /* rain - runoff - storage  mm   */
double porosity_factor              = 0.0; /*                      cc/cc ?  */
double available_soluble_nitrogen   = 0.0; /* in soil surface & fert. kg/ha */
double available_soluble_phosphorus = 0.0; /* in soil surface & fert. kg/ha */
double available_rainfall_nitrogen  = 0.0; /* N from rainfall         kg/ha */
double available_initial_phosphorus = 0.0; /* P from soil surface     kg/ha */
 
double  downward_movement_n         = 0.0;          /* XKFN1 */
double  runoff_movement_n           = 0.0;          /* XKFN2 */
double  downward_movement_p         = 0.0;          /* XKFP1 */
double  runoff_movement_p           = 0.0;          /* XKFP2 */
double exponent_one;
double exponent_two;

double	soil_n;
double	soil_p;
double	fert_n;
double	fert_p;
double	total_n;
double	total_p;
double	per_n_overland;
double	per_p_overland;
double	per_n_fert;
double	per_p_fert;


 if(bflags.nutrient)
  {
   fprintf (stderr,"****\n");
   fprintf (stderr,"1.11 CALCULATE NUTRIENT RUNOFF VALUES\n");
  }
 
 mgmtpath  = columndata[column_number]->management;
 soilpath  = columndata[column_number]->soil;
 
 
 /* Convert rainfall and runoff from inches to mm */
 rainfall  = initialptr->storm_rainfall * 25.4;
 runoff    = run_off * 25.4;
 
 
 /* Calculate the porosity of soil
    See Eq. II-21 in CREAMS Manual page 295 */
 
 porosity  = 1.0 - (bulk_den_lookup(soilpath->soil_type) / ROCK_DENSITY);
 
 /* error checking */
 
 if(porosity > 1.0 ) error = 1;
 if(porosity <= 0.0) error = 2;
 
 /*UNITS: LBS/ACRE management.nitrogen_application_rate */



 /* Convert lbs/acre to kg/ha */

 nitrogen_fert   = mgmtpath->nitrogen_application_rate   * LBSpACRE_TO_KGpHA;
 phosphorus_fert = mgmtpath->phosphorus_application_rate * LBSpACRE_TO_KGpHA;



 /* Calculate Initial Soluble Nitrogen and Phosphorus in top 1 cm of soil
    See. Eq. II-22 in CREAMS Manual page 296 This equation is based on
    percent of soil pore space */

 soluble_nitrogen     = 0.10 * soilpath->soil_pore_nitrogen   * porosity;
 soluble_phosphorus   = 0.10 * soilpath->soil_pore_phosphorus * porosity;



 surface_soluble_phos = soluble_phosphorus; /* Does not include fertilizer
					       nitrogen */


 /* Calculate nitrogen from rainfall */

 *nitrogen_runoff     = initialptr->rainfall_nitrogen * rainfall * 0.01;

  if(sourceinfo)
  {
    soil_n=soluble_nitrogen + *nitrogen_runoff;
    soil_p=soluble_phosphorus;

    fert_n=nitrogen_fert * (mgmtpath->nitrogen_availability/100.0);
    fert_p=phosphorus_fert * (mgmtpath->phosphorus_availability/100.0);

    total_n=soil_n + fert_n;
    total_p=soil_p + fert_p;


    if(total_n > 0.0)
     {
	 per_n_overland=soil_n/total_n;
	 per_n_fert=fert_n/total_n;
     }
    else
     {
	per_n_overland=0.0;
	per_p_overland=0.0;
     }

    if(total_p > 0.0)
     {
	  per_p_overland=soil_p/total_p;
	  per_p_fert=fert_p/total_p;
     }
    else
     {
	per_p_overland=0.0;
	per_n_fert=0.0;

     }

  }

 /* Add fertilizer to the amount of nitrogen and phosphorus */


 soluble_nitrogen   += nitrogen_fert *
				(mgmtpath->nitrogen_availability / 100.0);

 soluble_phosphorus += phosphorus_fert *
				(mgmtpath->phosphorus_availability / 100.0);



 /* Calculate the amount of effective infiltration  */
 /* Total_porosity is multiplied by 10 to account for the 10mm depth that
    is considered the depth of interaction */


 total_porosity      = 10.0 * porosity;

 effective_rainfall  = rainfall - total_porosity;

 if (effective_rainfall < 0.0)
     effective_rainfall = 0.0;

 effective_infiltration = effective_rainfall - runoff;



 if (soilpath->soil_type <= 4) /* If cell is not water */
   {

     /* Calc Porosity Factor */

     porosity_factor             = 0.00001/porosity;

     /* Calculate the initial nutrient values */

     available_soluble_nitrogen   = soluble_nitrogen     * porosity_factor;

     available_soluble_phosphorus = soluble_phosphorus   * porosity_factor;

     available_initial_phosphorus = surface_soluble_phos * porosity_factor;

     available_rainfall_nitrogen  = initialptr->rainfall_nitrogen * 0.000001;



     /* Calculate the coefficients */
 
     downward_movement_n = soilpath->nitrogen_leaching_extraction
			   / total_porosity;
     runoff_movement_n   = soilpath->nitrogen_runoff_extraction
			   / total_porosity;
     downward_movement_p = soilpath->phosphorus_leaching_extraction
			   /total_porosity;
     runoff_movement_p   = soilpath->phosphorus_runoff_extraction
			   /total_porosity;
 
 
 
 
     exponent_one = exp(-downward_movement_n*effective_infiltration);
     exponent_two = exp(-downward_movement_n*effective_infiltration -
			 runoff_movement_n*runoff);

     if (effective_rainfall > 0.0)
      {
       *nitrogen_runoff = ((available_soluble_nitrogen -
	    available_rainfall_nitrogen) * exponent_one -
	    (available_soluble_nitrogen - available_rainfall_nitrogen) *
	    exponent_two)/porosity_factor +
	    (*nitrogen_runoff) * runoff / effective_rainfall;
      }
     else
      {
       *nitrogen_runoff = ((available_soluble_nitrogen -
	    available_rainfall_nitrogen) * exponent_one -
	    (available_soluble_nitrogen - available_rainfall_nitrogen) *
	    exponent_two)/porosity_factor;
      }


     *phosphorus_runoff = ((available_soluble_phosphorus -
	    available_initial_phosphorus) *
	    exp( -downward_movement_p * effective_infiltration) -
	    (available_soluble_phosphorus-available_initial_phosphorus) *
	    exp( -downward_movement_p * effective_infiltration -
		  runoff_movement_p*runoff) ) / porosity_factor +
	    available_initial_phosphorus * runoff_movement_p *
	    runoff/porosity_factor;
 
     *nitrogen_runoff   /= LBSpACRE_TO_KGpHA;
     *phosphorus_runoff /= LBSpACRE_TO_KGpHA;
     }
 
 
 else
     {
 
 
     *nitrogen_runoff   /= LBSpACRE_TO_KGpHA;
     *phosphorus_runoff  = 0.0;             /* No phosphorus in lakes */
 
 
     }
 
 
 if (effective_rainfall <= 0.0)
     {
     *nitrogen_runoff   = 0.0;
     *phosphorus_runoff = 0.0;
     }


 if (sourceinfo)
    {
	sourceactptr->sol_n_overland =*nitrogen_runoff *  per_n_overland;
	sourceactptr->sol_p_overland =*phosphorus_runoff * per_p_overland;
	sourceactptr->sol_n_fertilizer =*nitrogen_runoff * per_n_fert;
	sourceactptr->sol_p_fertilizer =*phosphorus_runoff * per_p_fert;
    }

 
 if(bflags.nutrient)
  {
   fprintf (stderr,"INPUTS: rainfall %f(mm) runoff %f(mm) porosity %f\n",
	   rainfall,run_off,porosity);
   fprintf (stderr,"        Nit fert. rate %f(kg/ha) p fert. rate %f(kg/ha)\n",
	  nitrogen_fert,phosphorus_fert);
   fprintf (stderr,"        Nit avail. %d(per)  p avail. %d(per)  soil pore Nit %f (ppm)\n",
	  mgmtpath->nitrogen_availability,mgmtpath->phosphorus_availability,
	  soilpath->soil_pore_nitrogen);
   fprintf (stderr,"        soil pore p %f(ppm) n leaching extract. %f\n",
	  soilpath->soil_pore_phosphorus,soilpath->nitrogen_leaching_extraction);
   fprintf (stderr,"        p leaching extract. %f n runoff extrct. %f\n",
	  soilpath->phosphorus_leaching_extraction,soilpath->
	  nitrogen_runoff_extraction);
   fprintf (stderr,"        p runoff extract. %f\n",soilpath->phosphorus_runoff_extraction);
   fprintf (stderr,"INTER:  soluble nitrogen %f(kg/ha) soluble phosphorus %f (kg/ha)\n",
	  soluble_nitrogen,soluble_phosphorus);
   fprintf (stderr,"        effective infiltration %f(mm) total porosity %f(mm)\n",
	  effective_infiltration,total_porosity);
   fprintf (stderr,"        effective rainfall %f(mm) porosity factor %f\n",
	  effective_rainfall,porosity_factor);
   fprintf (stderr,"        avail soil n %f(kg/ha) avail soil p %f(kg/ha) init phos %f(kg/ha)\n",
	  available_soluble_nitrogen,available_soluble_phosphorus,
	  available_initial_phosphorus);
   fprintf (stderr,"        avail rainfall n %f(kg/ha)\n",available_rainfall_nitrogen);
   fprintf (stderr,"        down. mov. n %f  down. mov. p %f  runoff mov. n %f  runoff mov. p %f\n",
	  downward_movement_n,downward_movement_p,runoff_movement_n,
	  runoff_movement_p);

 }

 if (rflags.soil_nutr)
  {
   fprintf (stderr,"SOIL_NUTR routine: \n");
   fprintf (stderr,"   Input:  %d...col_number   %f...runoff\n",column_number,run_off);
   fprintf (stderr,"   Output: %f...N_runoff     %f...P_runoff\n",*nitrogen_runoff,*phosphorus_runoff);
  }


 return;
}
 
/**************************************************************************/
/*                                                                        */
/* A partial units analysis:                                              */
/*    "soluble_nitrogen = 0.10 * soilpath->soil_pore_nitrogen * porosity" */
/*                                                                        */
/*  10 mm   mg   cc     10 mm    g      10 mm           kg                */
/*  ----- x -- x --  =  ----- x ---  =  ----- x ----------------------    */
/*    1     L    cc       1     m^3       1     m^2 x mm * 1000 * 1000    */
/*                                                                        */
/*        10          kg           10000 m^2          kg                  */
/*      = -- x ----------------- x ---------  = 0.1 x --                  */
/*        1    m^2 x 10000 * 100      ha              ha                  */
/*                                                                        */
/**************************************************************************/



/******************************************************
 **	      bulk_den_lookup   		     **
 **           used by slnut and pesticide            **
 ******************************************************/

#ifdef _UNIX_K_AND_R

float bulk_den_lookup(type_of_soil)
 
  int type_of_soil;
 
#else
 
float bulk_den_lookup(int type_of_soil)
 
#endif
 
{
 
 /* This procedure simply uses the user input for soil type to look up
    the correct bulk density for the soil */
 
 if (rflags.bulk_dens)
  {
   fprintf (stderr,"BULK_DENS routine: \n");
   if (type_of_soil == 1)
    {
     fprintf (stderr,"   Input:    1 = Sand soil type\n");
     fprintf (stderr,"   Output: 1.6...Bulk_Density\n");
    }
   else if (type_of_soil == 2)
    {
     fprintf (stderr,"   Input:    2 = Silt soil type\n");
     fprintf (stderr,"   Output: 1.3...Bulk_Density\n");
    }
   else if (type_of_soil == 3)
    {
     fprintf (stderr,"   Input:    3 = Clay soil type\n");
     fprintf (stderr,"   Output: 1.4...Bulk_Density\n");
    }
   else if (type_of_soil == 4)
    {
     fprintf (stderr,"   Input:    4 = Peat soil type\n");
     fprintf (stderr,"   Output: 1.2...Bulk_Density\n");
    }
   else if (type_of_soil == 5)
    {
     fprintf (stderr,"   Input:    5 = Water cell\n");
     fprintf (stderr,"   Output: 0.0...Bulk_Density\n");
    }
   else
    {
     fprintf (stderr,"   Input:    %d = Unknown soil type\n",type_of_soil);
     fprintf (stderr,"   Output: 0.0...Bulk_Density\n");
    }
  }


  switch (type_of_soil)
	{
	case 1 : return( 1.6 );      /* 1.6 if soil type is sand */
	case 2 : return( 1.3 );      /* 1.3 if soil type is silt */
	case 3 : return( 1.4 );      /* 1.4 if soil type is clay */
	case 4 : return( 1.2 );      /* 1.2 if soil type is peat */
	default: return( 0.0 );      /* 0.0 if water */
	}


}
