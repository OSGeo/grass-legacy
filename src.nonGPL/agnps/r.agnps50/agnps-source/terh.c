/******************************************************************/
/*      terh.c                                                    */
/*	was terh(ro,a,d,qp) in LOOP1.FOR                          */
/* Subroutine to calculate the peak flow from an impoundment      */
/* Relations taken from the CREAMS pond model                     */
/*                                                                */
/* Don't event try to do a dimensional analysis on this routine!! */
/*                                                                */
/******************************************************************/
 
#ifdef _DOS
 
 #include <math.h>
 #include "input.h"
 #include <stdio.h>
 #include "binary.h"
 #include "debugflg.h"

#else

#include <math.h>
#include "input.h"
#include <stdio.h>
#include "binary.h"
#include "debugflg.h"

#endif


extern COLUMN_INFOPTR *columndata;
extern int debuginfo;
extern int hydro_info;
extern HYDRO_IMP_REC_PTR	hydroimp;

extern FLAGS_BASIC              bflags;
extern FLAGS_ROUTINE            rflags;

#ifdef _UNIX_K_AND_R

 void terh(column_number, currentptr)

     int column_number;
     IMPOUND_INFO *currentptr;

#else

 void terh(int column_number, IMPOUND_INFO *currentptr)

#endif

{
int   pond_area_coeff = 7500;
float b = 1.5;
float depth;                   /* feet */
float cor;
RUNOFF_INFO *runoffpath;


  runoffpath = columndata[column_number]->runoff;

  currentptr->volume_runoff =                                 /* ft^3 */
		runoffpath->cell_run_off * IN_ACRE_TO_CU_FT *
		currentptr->drainage_area;

  depth = pow( (3 * currentptr->volume_runoff/pond_area_coeff), (1/(1+b)));

  cor = 13968.0 * pow((currentptr->pipe_diameter/12.0), 2.0);

  currentptr->peak_flow =                      /*  cfs */
		cor * sqrt(depth) / 3600.0;
  if(bflags.impound)
   fprintf (stderr,"pond depth %f(feet)  vol. runoff %f(cu ft.))  peak flow %f(cfs)\n",
	 depth,currentptr->volume_runoff,currentptr->peak_flow);

  if(hydro_info)
   {
    hydroimp->cell_runoff=runoffpath->cell_run_off;
    hydroimp->impoundment_runoff=currentptr->volume_runoff;
    hydroimp->depth=depth;
    hydroimp->pond_area_coeff=pond_area_coeff;
    hydroimp->b=b;
    hydroimp->cor=cor;
    hydroimp->peak_flow=currentptr->peak_flow;
   }



  if (rflags.terh)
   {
    fprintf (stderr,"TERH routine: \n");
    fprintf (stderr,"   Input:  %d...col_number  & Impoundment structure pointer\n",column_number);
    fprintf (stderr,"   Output: %f...vol_runoff    %f...peak_flow\n",currentptr->volume_runoff,currentptr->peak_flow);
   }

  return;
}