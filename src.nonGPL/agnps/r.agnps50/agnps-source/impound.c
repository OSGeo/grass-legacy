/*******************************************************************/
/*   impound()                                                     */
/*   was "do_impound" in LOOP1.FOR                                 */
/*******************************************************************/
 
#ifdef _DOS
 
 #include <stdlib.h>
 #include <mem.h>
 #include <stdio.h>
 #include "input.h"
 #include "binary.h"
 #include <math.h>
 #include "debugflg.h"

#else

#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif

extern COLUMN_INFOPTR   	*columndata;
extern FILE			*hydrofile;
extern HYDRO_IMP_REC_PTR        hydroimp;
extern float		        soil_break_down[6][7];
extern int			hydro_info;
extern int			sourceinfo;

extern FLAGS_ROUTINE            rflags;
 
#ifdef _UNIX_K_AND_R
 
 void  terh();
 float ters();
 
#else
 
 void  terh(int col_number, IMPOUND_INFO *currentptr);
 float ters(IMPOUND_INFO *currentptr, int, float, int);
 
#endif

/*UNITS VOID impound( , , TONS/ACRE ) */

extern SOURCEACCT		sourceact;
extern SOURCEACCTPTR		sourceactptr;

#ifdef _UNIX_K_AND_R
 
 void  impound(column_number, temp_receiving_cell, upland_erosion, impounded_area)

      int   column_number;
      int   temp_receiving_cell;
      float upland_erosion;
      float impounded_area;
#else

 void  impound(int   column_number,
	      int   temp_receiving_cell,
	      float upland_erosion,
	      float impounded_area)         /* tons/acre  */

#endif
 
{
 IMPOUND_INFO *currentptr;
 RUNOFF_INFO  *runoffpath;
 int    particle_size;
 float  sed_leaving_pond;                    /* tons       */
 float sed_flowing_in;
 float nit_enrichment_ratio;
 float phos_enrichment_ratio;
 float total_nit_impound;
 float total_phos_impound;
 float sum_of_particle_sizes;
 float sum_of_particle_surfaces;
 float particle_fraction;
 float particle_surface[6];
 static float surface_constant[6]= {0, 1.5708,0.3142, 0.9429, 0.6330, 0.0157};
 float part_n_yield;
 float part_p_yield;
 float percent_surface[6];
 float percent_flowing_out;
 float impound_n_leaving=0.0;
 float impound_p_leaving=0.0;
 
static float transport_factor[5]={0.0,0.85,1.00,1.15,1.5};


 runoffpath = columndata[column_number]->runoff;
 

 currentptr = columndata[column_number]->impound;
 

		     /* i.e. is there an impoundment? */
 while ( currentptr != NULL )
  {
   if (temp_receiving_cell != 0)   /**** i.e. its not a sinkhole ****/
    {
     if(hydro_info)
      {
       hydroimp->drainage_area=currentptr->drainage_area;
       hydroimp->pipe_diameter=currentptr->pipe_diameter;
      }

     if (currentptr->drainage_area < 0.5)  /* old AgNPS uses 2 acres */
	 currentptr->drainage_area = 0.5;

     if (currentptr->pipe_diameter < 3.0)
	 currentptr->pipe_diameter = 3.0;

     /*UNITS: ACRES  impound.drainage_area */
     /*UNITS: INCHES impound.pipe_diameter */

     /*** calculate the peak flow of an impoundment ***/

    if(hydro_info)
     {
      hydroimp->drainage_area_after=currentptr->drainage_area;
      hydroimp->pipe_diameter_after=currentptr->pipe_diameter;
     }

     terh(column_number, currentptr);

     sed_flowing_in=(upland_erosion* columndata[column_number]->area)*
     (currentptr->drainage_area/columndata[column_number]->area);



     total_nit_impound=runoffpath->impound_nit *
		   (currentptr->drainage_area/impounded_area);

     total_phos_impound=runoffpath->impound_phos *
		   (currentptr->drainage_area/impounded_area);




     /*** calculate sediment yeild from an impoundment ***/
     sum_of_particle_surfaces=0.0;
     for (particle_size=1; particle_size<=5; particle_size++)
     {
      particle_fraction=soil_break_down[columndata[column_number]->soil->soil_type]
			[particle_size];

      particle_surface[particle_size] = (sed_flowing_in * particle_fraction)
				  * surface_constant[particle_size];

      sum_of_particle_surfaces += particle_surface[particle_size];
     }

     part_n_yield=0.0;
     part_p_yield=0.0;

     for (particle_size=1; particle_size<=5; particle_size++)
      {

	/*UNITS: TONS ters(,, TONS/ACRE, ) */
	/*UNITS: TONS runoff.sediment_yield */

	sed_leaving_pond = ters(currentptr, particle_size,
					upland_erosion, column_number);


	runoffpath->sediment_yield[particle_size] += sed_leaving_pond;
	runoffpath->impound_yield[particle_size] += sed_leaving_pond;
	currentptr->sediment_out[particle_size] = sed_leaving_pond;


      /* sediment_yield is in tons */
      if ( sum_of_particle_surfaces <= 0.0)
	percent_surface[particle_size] = 0.0;
      else
	percent_surface[particle_size] = particle_surface[particle_size] /
				   sum_of_particle_surfaces;


      /* Calculate the percentage of sediment that is flowing out of the
      impoundment, compared to the amount that is flowing in and generated in the
      current cell.  Since we know what percent of the nutrients each
      particle size is carrying, and we know the percent of each particle
      size that is depositied, we know the amount of nutrients that are
      depositied along with the sediment. */

      /* In the case of peat soils, the percentages for many of the
	 particle sizes are zero */

      if((sed_flowing_in != 0.0) && (sed_leaving_pond != 0.0))
       {
	particle_fraction=soil_break_down[columndata[column_number]->soil->soil_type]
			[particle_size];
	percent_flowing_out = (sed_leaving_pond) /
			    (sed_flowing_in*particle_fraction);
       }
      else
      percent_flowing_out = 0.0;


      /* N and P is in lbs */

      part_n_yield += percent_surface[particle_size] * percent_flowing_out *
		     total_nit_impound;

      part_p_yield += percent_surface[particle_size] * percent_flowing_out *
		   total_phos_impound;
     }


     impound_n_leaving+=part_n_yield;
     impound_p_leaving+=part_p_yield;


     /*UNITS: ACRES columndata.area_not_terraced */

     columndata[column_number]->area_not_terraced -=
	currentptr->drainage_area;

     currentptr = currentptr->next;

    } /* end "if not a sinkhole" */
    else
     {
      if(hydro_info)
	hydroimp->peak_flow=0.0;
     }

   if(hydro_info)
    fwrite(hydroimp,sizeof(HYDRO_IMP_REC),1,hydrofile);

  }    /* end of loop through list of impoundments */

  /* Add sediment attached N & P to total N & P at top of cell */

    runoffpath->total_n_within_cell+=impound_n_leaving;
    runoffpath->total_p_within_cell+=impound_p_leaving;

   if(sourceinfo)
   { 
    sourceactptr->sed_n_impoundments=impound_n_leaving;
    sourceactptr->sed_p_impoundments=impound_p_leaving;
   }


	    /* make sure remaining area is nonnegative */

  if (columndata[column_number]->area_not_terraced < 0)
      columndata[column_number]->area_not_terraced = 0.1;


  if (rflags.impound)
   {
    fprintf (stderr,"IMPOUND routine: \n");
    fprintf (stderr,"   Input:  %d...col_number    %d...rec_cell\n",column_number,temp_receiving_cell);
    fprintf (stderr,"           %f...upland_eros   %f...impoundmt_area\n",upland_erosion,impounded_area);
    fprintf (stderr,"   Output: %f...impound_N_leaving  %f...impound_P_leaving\n",impound_n_leaving,impound_p_leaving);
   }

  return;
}
