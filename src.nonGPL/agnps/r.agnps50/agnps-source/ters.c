/**************************************************************/
/*    ters.c                                                  */
/*    was ters() in LOOP1.FOR                                 */
/* Subroutine to calculate sediment yield from an impoundment */
/* Relations taken from CREAMS pond element                   */
/* Relations calculate the yield in lbs from the fraction     */
/* representing the portion of the particular particle size   */
/* passing through the terrace                                */
/**************************************************************/
 
#ifdef _DOS
 
 #include <math.h>
 #include "input.h"
 #include <stdio.h>
 #include "debugflg.h"

#else
 
#include <math.h>
#include "input.h"
#include <stdio.h>
#include "debugflg.h"

#endif
 
extern COLUMN_INFOPTR *columndata;
extern float	      soil_break_down[6][7];  /* a data array */
extern int debuginfo;

extern FLAGS_BASIC    bflags;
extern FLAGS_ROUTINE  rflags;
 
#ifdef _UNIX_K_AND_R
 
 float ters(currentptr, particle_size, upland_erosion, column_number)
 
	 IMPOUND_INFO *currentptr;
	 int   particle_size;
	 float upland_erosion;
	 int   column_number;
 
#else
 
 float ters(IMPOUND_INFO *currentptr,
	   int   particle_size,
	   float upland_erosion,        /* tons/acre */
	   int   column_number)
 
#endif
 
{
       /* For the following two arrays, the first 0
	  is nothing but a placeholder. */
 
 static float infiltration_rate[5]        =
			{0.0, 0.7, 0.4, 0.05, 1.5};
 static int   equivalent_sand_diameter[7] =
			{0, 0, 2, 10, 20, 158, 201}; /* microns */
 
 float sed_leaving_pond = 0.0;           /* tons         */
 float total_runoff_volume;              /* ft^3         */
 float conversion;                       /* Cor  in docs */
 float temp_var;
 float temp_var_ys;
 float temp_var_a1;
 float temp_var_b1;
 float difference;                       /* microns */
 float fraction_particle_size;           /* 0.0-1.0 */
 float particle_fraction;                /* 0.0-1.0 */
 float sand_diameter;                    /* microns */
 float sand_diameter_l;                  /* microns */
 float infiltration;                     /* in/hr   */
 
 SOIL_INFO   *soilpath;
 RUNOFF_INFO *runoffpath;
 
 soilpath   = columndata[column_number]->soil;
 runoffpath = columndata[column_number]->runoff;
 
 if (soilpath->soil_type <= 4)
  {
   if (currentptr->infiltration > 0.0)
     infiltration = currentptr->infiltration;
   else
     infiltration = infiltration_rate[ soilpath->soil_type ];
 
 
     total_runoff_volume = runoffpath->cell_run_off * IN_ACRE_TO_CU_FT *
				currentptr->drainage_area;
     conversion = 13968.0 * pow((currentptr->pipe_diameter/12), 2.0);
 
     temp_var = -0.00000668 * 7500.0 - 0.0903 * 1.5 + 0.000119 * conversion -
		 0.00000342 * total_runoff_volume -
		 20400.0 * infiltration / 43200.0;
 
     temp_var_ys = 0.0000328 * 7500.0 + 0.123 * 1.5 - 0.00024 * conversion +
		    0.00000810 * total_runoff_volume - 11880.0 *
		    infiltration / 43200.0;
 
     temp_var_a1 =  1.136 * exp(temp_var);
     temp_var_b1 = -0.152 * exp(temp_var_ys);
 
     sand_diameter   = equivalent_sand_diameter[ particle_size+1 ];
     sand_diameter_l = equivalent_sand_diameter[ particle_size ];
 
     difference = sand_diameter - sand_diameter_l;
 
     fraction_particle_size = temp_var_a1 *
	      (exp(temp_var_b1 * sand_diameter) -
	       exp(temp_var_b1 * sand_diameter_l))/(temp_var_b1 * difference);
 
     if(fraction_particle_size > 1)
	 fraction_particle_size = 1;
 
     particle_fraction = soil_break_down[soilpath->soil_type][particle_size];
     sed_leaving_pond = upland_erosion * currentptr->drainage_area *
		particle_fraction * fraction_particle_size;
 
     if(bflags.impound)
      if(particle_size==1)
	fprintf (stderr,"Soil Type %d  Infiltration %f  Runoff Volume %f\n",
	     soilpath->soil_type,infiltration,total_runoff_volume);
 

     if (rflags.ters)
      {
       fprintf (stderr,"TERS routine: \n");
       fprintf (stderr,"   Input:  %d...col_number    %d...particle_size\n",column_number,particle_size);
       fprintf (stderr,"           %f...upland_erosion\n",upland_erosion);
       fprintf (stderr,"   Output: %f...sediment_leaving_pond\n",sed_leaving_pond);
      }


  }
 return(sed_leaving_pond);
}