
#ifdef _DOS

 #include <stdio.h>
 #include <stdlib.h>
 #include <math.h>
 #include <string.h>
 #include "input.h"
 #include "debugflg.h"

#else

#include <stdio.h>
#include <math.h>
#include "input.h"
#include "debugflg.h"

#endif

extern  FLAGS_ROUTINE      rflags;


#ifdef _UNIX_K_AND_R

 void curve_number_runoff();
 float ovrld();

#else
 void curve_number_runoff(float, float, int, char[20], float*);
 float ovrld(float, int, float);

#endif

/*UNITS: VOID feedlot( , INCHES, , LBS/ACRE, LBS/ACRE, LBS/ACRE ) */

#ifdef _UNIX_K_AND_R

 void feedlot(feedlot_number, rainfall, current_feed_ptr, point_source_N,
		point_source_P, point_source_COD, column)

   int          feedlot_number;
   float        rainfall;
   FEEDLOT_INFO *current_feed_ptr;
   float        *point_source_N;
   float	*point_source_P;
   float	*point_source_COD;
   int          column;

#else

 void feedlot(int         feedlot_number,       /* 1,2,...  */
	     float        rainfall,             /* inches   */
	     FEEDLOT_INFO *current_feed_ptr,
	     float        *point_source_N,      /* lbs/acre */
	     float	  *point_source_P,      /* lbs/acre */
	     float	  *point_source_COD,
	     int          column )

#endif

{
int   counter;
int   done=FALSE;

float runoff;                   /* inches      */
float volume_runoff_1;          /* in-acres    */
float volume_runoff_2;          /* in-acres    */
float volume_runoff_3;          /* in-acres    */
float time_of_contact;
float overland_time_contact;
float grass_time_contact;
float equivalent_animal_cod;
float equivalent_animal_p;
float equivalent_animal_n;
float manure_pack_cod;
float manure_pack_phosphorus;
float manure_pack_nitrogen;
float cod_runoff_lbs;
float p_runoff_lbs;
float n_runoff_lbs;
float cod_conc_discharge;
float p_conc_discharge;
float n_conc_discharge;
float cod_runoff_conc;
float p_runoff_conc;
float n_runoff_conc;
float cod_discharge_lbs;
float p_discharge_lbs;
float n_discharge_lbs;
float remain_cod_strength;
float remain_p_strength;
float remain_n_strength;
float volume_one_transformed;
float volume_two_transformed;
float total_volume;
float cod_mas;
int   feedlot_rating_number;
float first_feedlot_rating_factor;
float weighted_feedlot_rating_factor;
 
 
  *point_source_N       = 0.0;
  *point_source_P       = 0.0;
  *point_source_COD     = 0.0;
  volume_runoff_2       = 0;
  volume_runoff_3       = 0;
  overland_time_contact = 0;
  grass_time_contact    = 0;
  equivalent_animal_cod = 0;
  equivalent_animal_p   = 0;
  equivalent_animal_n   = 0;
 
  /*UNITS: INCHES ro( , INCHES ) */
  /*UNITS: ACRES  feedlot.area, feedlot.roofed_area */
 
  curve_number_runoff((float) current_feed_ptr->curve_number,
			      rainfall,
			      column,
			      "Feedlot Area 1",
			      &runoff);

  volume_runoff_1 = runoff * current_feed_ptr->area;
 
 
 
  for (counter=1; counter<=6; counter++)
      {
      if (current_feed_ptr->area2_curve_number[counter] != 0)
	  {
	   curve_number_runoff((float) current_feed_ptr->
			   area2_curve_number[counter],
			   rainfall,
			   column,
			   "Feedlot Area 2",
			   &runoff);
 
	  volume_runoff_2 += runoff * current_feed_ptr->area2_area[counter];
	  }
      }

  volume_runoff_2 +=  rainfall * current_feed_ptr->roofed_area;
 
 
 
  for (counter=1; counter<=6; counter++)
      {
      if (current_feed_ptr->area3_curve_number[counter]!=0)
	  {
	  curve_number_runoff((float)current_feed_ptr->area3_curve_number[counter],
				 rainfall,
				 column,
				 "Feedlot Area 3",
				 &runoff);
 
	  volume_runoff_3 += runoff * current_feed_ptr->area3_area[counter];
	  }
      }
 

  done    = FALSE;
  counter = 1;
 
  while (!done)
     {
     if (current_feed_ptr->buffer_surface_constant[counter] != 0)
	{
	if (current_feed_ptr->buffer_surface_constant[counter] == 1.0)
	      current_feed_ptr->buffer_surface_constant[counter] = -0.18;
 
 
	/*UNITS: SEC ovrld( PERCENT, FEET, ) */
 
	time_of_contact = ovrld(current_feed_ptr->buffer_slope[counter],
			current_feed_ptr->buffer_flow_length[counter],
			current_feed_ptr->buffer_surface_constant[counter]);
 

	if (current_feed_ptr->buffer_surface_constant[counter] != -0.18)
	      overland_time_contact += time_of_contact;
	else
	      grass_time_contact    += time_of_contact;
 
	if (counter == 3)
	       done=TRUE;
 
	counter++;
	}
 
     else
	done = TRUE;
     }
 
 
 
  done    = FALSE;
  counter = 1;

  while (!done)
     {
     if (current_feed_ptr->number_of_animals[counter] > 0)
	{
	equivalent_animal_cod +=
		  current_feed_ptr->number_of_animals[counter] *
		  current_feed_ptr->cod_factor[counter];
 
	equivalent_animal_p +=
		current_feed_ptr->number_of_animals[counter] *
		current_feed_ptr->animal_phosphorus[counter];
 
	equivalent_animal_n +=
		current_feed_ptr->number_of_animals[counter] *
		current_feed_ptr->animal_nitrogen[counter];
 
	if (counter == 3)
	    done = TRUE;

	counter++;
	}
 
     else
	done = TRUE;
     }
 
 
 
  manure_pack_cod        = equivalent_animal_cod/current_feed_ptr->area;
  manure_pack_phosphorus = equivalent_animal_p  /current_feed_ptr->area;
  manure_pack_nitrogen   = equivalent_animal_n  /current_feed_ptr->area;
 
  if (manure_pack_cod > 100.0)
      manure_pack_cod = 100.0;
 
  if (manure_pack_phosphorus > 100.0)
      manure_pack_phosphorus = 100.0;

  if (manure_pack_nitrogen   > 100.0)
      manure_pack_nitrogen   = 100.0;
 
 
/* If the user enters a negative or zero for any of these numbers, that  */
/*   means that we are to calculate the decrease in the standard method  */
 
  if(overland_time_contact == 0.0)
    {
     current_feed_ptr->decrease_cod_overland = 0.0;
     current_feed_ptr->decrease_p_overland = 0.0;
     current_feed_ptr->decrease_n_overland = 0.0;
    }
  else
    {
     if (current_feed_ptr->decrease_cod_overland <= 0)
      current_feed_ptr->decrease_cod_overland = -27.9 + 42.8 *
					      log10(overland_time_contact);


     if (current_feed_ptr->decrease_p_overland <= 0)
      current_feed_ptr->decrease_p_overland = -49.3 + 50.5 *
					      log10(overland_time_contact);
 
     if (current_feed_ptr->decrease_n_overland <= 0)
      current_feed_ptr->decrease_n_overland = -16.8 + 42.3 *
				      log10(overland_time_contact);
    }
 
 
  if (grass_time_contact > 0)
      {
      if (current_feed_ptr->decrease_cod_grass <= 0)
	  current_feed_ptr->decrease_cod_grass =  15.95 + 0.33  *
						grass_time_contact;
 
      if (current_feed_ptr->decrease_p_grass <= 0)
	  current_feed_ptr->decrease_p_grass = -21.2  + 0.36  *
						grass_time_contact;
 
      if (current_feed_ptr->decrease_n_grass <= 0)
	  current_feed_ptr->decrease_n_grass =  25.5  + 0.047 *
						grass_time_contact;
      }
 
 
  if (current_feed_ptr->decrease_cod_overland < 0.0)
      current_feed_ptr->decrease_cod_overland = 0.0;
 
  if (current_feed_ptr->decrease_cod_overland > 100)
      current_feed_ptr->decrease_cod_overland = 100;
 
  if (current_feed_ptr->decrease_p_overland < 0.0)
      current_feed_ptr->decrease_p_overland = 0.0;
 
  if (current_feed_ptr->decrease_p_overland > 100.0)
      current_feed_ptr->decrease_p_overland = 100.0;
 
  if (current_feed_ptr->decrease_n_overland < 0.0)
      current_feed_ptr->decrease_n_overland = 0.0;
 
  if (current_feed_ptr->decrease_n_overland > 100.0)
      current_feed_ptr->decrease_n_overland = 100.0;
 
  if (current_feed_ptr->decrease_cod_grass < 0.0)
      current_feed_ptr->decrease_cod_grass = 0.0;
 
  if (current_feed_ptr->decrease_cod_grass > 100.0)
      current_feed_ptr->decrease_cod_grass = 100.0;
 
  if (current_feed_ptr->decrease_p_grass < 0.0)
      current_feed_ptr->decrease_p_grass = 0.0;
 
  if (current_feed_ptr->decrease_p_grass > 100.0)
      current_feed_ptr->decrease_p_grass = 100.0;

  if (current_feed_ptr->decrease_n_grass < 0.0)
      current_feed_ptr->decrease_n_grass = 0.0;
 
  if (current_feed_ptr->decrease_n_grass > 100.0)
      current_feed_ptr->decrease_n_grass = 100.0;
 
 
 
 
 
 
  remain_cod_strength = (1 - current_feed_ptr->decrease_cod_grass/100.0) *
			(1 - current_feed_ptr->decrease_cod_overland/100.0);
  remain_p_strength   = (1 - current_feed_ptr->decrease_p_grass/100.0) *
			(1 - current_feed_ptr->decrease_p_overland/100.0);
  remain_n_strength   = (1 - current_feed_ptr->decrease_n_grass/100.0) *
			(1 - current_feed_ptr->decrease_n_overland/100.0);
 
  if(volume_runoff_1 > 30)
     {
     volume_one_transformed = volume_runoff_1 + 30;
     volume_two_transformed = volume_runoff_2 - 30;
     }
  else
     {
     volume_one_transformed = volume_runoff_1 + volume_runoff_2;
     volume_two_transformed = 0.0;
     }
 

  total_volume = volume_one_transformed + volume_two_transformed +
				volume_runoff_3;
 
 
  current_feed_ptr->feedlot_cod = current_feed_ptr->feedlot_cod/100;
  current_feed_ptr->feedlot_nitrogen = current_feed_ptr->feedlot_nitrogen/100;
  current_feed_ptr->feedlot_phosphorus = current_feed_ptr->feedlot_phosphorus/100;

 
  cod_runoff_conc = (volume_one_transformed * manure_pack_cod *
		     current_feed_ptr->feedlot_cod) +
		     (volume_two_transformed * 60);
 
  p_runoff_conc   = (volume_one_transformed * manure_pack_phosphorus *
		     current_feed_ptr->feedlot_phosphorus)+
				   (volume_two_transformed * 2);
 
  n_runoff_conc   = (volume_one_transformed * manure_pack_nitrogen *
		     current_feed_ptr->feedlot_nitrogen) +
				   (volume_two_transformed * 12);
 
 
 
  cod_runoff_lbs = cod_runoff_conc * remain_cod_strength
						+ volume_runoff_3 * 60;
  p_runoff_lbs   = p_runoff_conc   * remain_p_strength
						+ volume_runoff_3 * 2;
  n_runoff_lbs   = n_runoff_conc   * remain_n_strength
						+ volume_runoff_3 * 12;
 
 
 
  cod_conc_discharge = cod_runoff_lbs/total_volume;
  p_conc_discharge   = p_runoff_lbs  /total_volume;
  n_conc_discharge   = n_runoff_lbs  /total_volume;
 
 
					     /* check units !!! */
 
  cod_discharge_lbs = cod_runoff_lbs * (IN_ACRE_TO_CU_FT *
					  CU_FT_H2O_IN_LBS)/1.0e6;
  p_discharge_lbs   = p_runoff_lbs   * (IN_ACRE_TO_CU_FT *
					  CU_FT_H2O_IN_LBS)/1.0e6;
  n_discharge_lbs   = n_runoff_lbs   * (IN_ACRE_TO_CU_FT *
					  CU_FT_H2O_IN_LBS)/1.0e6;


  current_feed_ptr->feedlot_cod = current_feed_ptr->feedlot_cod/100;
 
  cod_mas = manure_pack_cod * current_feed_ptr->feedlot_cod *
		remain_cod_strength * volume_one_transformed * 0.2265;
 
  if (cod_mas <=0 )
     feedlot_rating_number = 0;
  else
     {
     first_feedlot_rating_factor    = (log10(cod_mas) - 2.0)/3.0;
     weighted_feedlot_rating_factor = 0.8 + (0.1 * log10(total_volume));
     feedlot_rating_number          = (int) (first_feedlot_rating_factor *
			weighted_feedlot_rating_factor * 100.0);
     }
 
  if(feedlot_rating_number < 0)
     feedlot_rating_number = 0;

  *point_source_N   = n_discharge_lbs;        /* check units!!! */
  *point_source_P   = p_discharge_lbs;
  *point_source_COD = cod_discharge_lbs;
 
  current_feed_ptr->feedlot_number        = feedlot_number;
  current_feed_ptr->n_conc_discharge      = n_conc_discharge;
  current_feed_ptr->p_conc_discharge      = p_conc_discharge;
  current_feed_ptr->cod_conc_discharge    = cod_conc_discharge;
  current_feed_ptr->n_discharge_lbs       = n_discharge_lbs;
  current_feed_ptr->p_discharge_lbs       = p_discharge_lbs;
  current_feed_ptr->cod_discharge_lbs     = cod_discharge_lbs;
  current_feed_ptr->feedlot_rating_number = feedlot_rating_number;


  if (rflags.pntsrc)
   {
    fprintf (stderr,"FEEDLOT routine: \n");
    fprintf (stderr,"   Input:  %d...col_number    %d...feedlot_number \n",column,feedlot_number);
    fprintf (stderr,"           %f...rainfall \n",rainfall);
    fprintf (stderr,"   Output: %f...feedlot_N     %f...feedlot_P\n",*point_source_N,*point_source_P);
    fprintf (stderr,"           %f...feedlot_COD\n",*point_source_COD);
   }



  return;
}