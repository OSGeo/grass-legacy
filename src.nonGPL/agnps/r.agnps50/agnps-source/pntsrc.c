 
#ifdef _DOS
 
 #include <stdio.h>
 #include <stdlib.h>
 #include <alloc.h>
 #include <string.h>
 #include "input.h"
 #include "debugflg.h"

#else
#include <stdio.h>
#include "input.h"
#include "debugflg.h"

#endif
 
extern COLUMN_INFOPTR *columndata;
 
extern INITIAL_INFOPTR initialptr;
extern SOURCEACCT      sourceact;
extern SOURCEACCTPTR   sourceactptr;
extern int             sourceinfo;

extern FLAGS_ROUTINE   rflags;

 
/* BUG A-4 ONLY prototyping for 6 variables, function needs 7, so added column */
 
#ifdef _UNIX_K_AND_R
 
 void  feedlot();
 void  memory_out(); /* in AGRUN.C */
 
#else
 
 void  feedlot(int number, float rainfall, FEEDLOT_INFO *current_feed_ptr,
			 float *nitrogen, float *phos, float *cod, int column);

 void  memory_out(int location, int column); /* in AGRUN.C */

#endif



/* This routine now only handles feedlot-type point sources, since     */
/*  springs and streams are better included during the routine loop    */
 
 
/*
	Date	Bug #	Prog	Desc
	6/15/93	A-4	MAK	Only sending 6 variables to feedlot
 
*/
 
#ifdef _UNIX_K_AND_R
 
 void pntsrc(column,temp_receiving_cell)
 
     int column;
     int temp_receiving_cell;
 
#else
 
  void pntsrc(int column, int temp_receiving_cell)
 
#endif
 
{
FEEDLOT_INFO *cur_feedlot;
RUNOFF_INFO  *runoffpath;
 
float feedlot_N;
float feedlot_P;
float feedlot_COD;
 
float feedlot_n_yield   = 0.0;
float feedlot_p_yield   = 0.0;
float feedlot_cod_yield = 0.0;
int   feedlot_number    = 0;
 
 
 
    runoffpath = columndata[column]->runoff;
 
 
    if (columndata[column]->feedlot != NULL)
	{
	feedlot_number    = 0;
 
	feedlot_n_yield   = 0;
	feedlot_p_yield   = 0;
	feedlot_cod_yield = 0;
 
	cur_feedlot = columndata[column]->feedlot;
 


	while (cur_feedlot != NULL)
	    {
	    feedlot_number++;

	    /*UNITS: VOID feedlot( , INCHES, , LBS, LBS, LBS) */

	    /* Bug A-4 Only sending 6 variables, but should be sending 7.
	       Added column as the seventh argument */

	    feedlot(feedlot_number, initialptr->storm_rainfall,
				cur_feedlot,
				&feedlot_N,     /* lbs */
				&feedlot_P,   /* lbs */
				&feedlot_COD,
				column);         /* lbs */



/* The next three are just for within the cell  */

	    runoffpath->soluble_nitrogen_runoff +=          /* lbs/acre */
			feedlot_N  /
			columndata[column]->area;

	    runoffpath->soluble_phosphorus_runoff +=        /* lbs/acre */
			feedlot_P /
			columndata[column]->area;

	    runoffpath->cod_runoff +=                       /* lbs/acre */
		     feedlot_COD /
		     columndata[column]->area;



	    feedlot_n_yield   += feedlot_N;
	    feedlot_p_yield   += feedlot_P;
	    feedlot_cod_yield += feedlot_COD;


	    /*UNITS: LBS runoff.soluble_nitrogen_yield */
	    /*UNITS: LBS runoff.soluble_phosphorus_yield */
	    /*UNITS: LBS runoff.soluble_cod_yield */

	    if (temp_receiving_cell > 0)   /* i.e. not a sinkhole */
		{
		runoffpath->soluble_nitrogen_yield   += feedlot_N;
		runoffpath->soluble_phosphorus_yield += feedlot_P;
		runoffpath->soluble_cod_yield        += feedlot_COD;
		}

	    cur_feedlot = cur_feedlot->next;
	    }


	if (feedlot_number > 1)
	    {
	    columndata[column]->feed_totals = (FEEDLOT_TOTALS*)
			calloc(1, sizeof(FEEDLOT_TOTALS));

	    if (columndata[column]->feed_totals == NULL)
		memory_out(11, column);

/*UNITS: LBS feed_totals.tot_feedlot_nit */

	    columndata[column]->feed_totals->tot_feedlot_nit  =
							feedlot_n_yield;

	    columndata[column]->feed_totals->tot_feedlot_phos =
							feedlot_p_yield;

	    columndata[column]->feed_totals->tot_feedlot_cod =
							feedlot_cod_yield;
	    }

     if(sourceinfo)
      {
	sourceactptr->sol_n_feedlots=feedlot_n_yield;
	sourceactptr->sol_p_feedlots=feedlot_p_yield;
	sourceactptr->sol_cod_feedlots=feedlot_cod_yield;
      }
    } /* end of if  (were there any pointsources at all */


  if (rflags.pntsrc)
   {
    fprintf (stderr,"PNTSRC routine: \n");
    fprintf (stderr,"   Input:  %d...col_number    %d...rec_column \n",column,temp_receiving_cell);
    fprintf (stderr,"   Output: %f...psrc_N     %f...psrc_P\n",feedlot_n_yield,feedlot_p_yield);
    fprintf (stderr,"           %f...psrc_COD\n",feedlot_cod_yield);
   }

    return;
}
