/******************************************************************/
/*   run_out.c                                                    */
/*   was mostly outsubs.for                                       */
/******************************************************************/


#include <stdio.h>
#include <math.h>
#include "input.h"
#include "debugflg.h"



/* Comment out the following line for the release version,   */
/*   or if you want to import the .NPS file into Quattro     */

#define _HUMAN_READABLE_FILES_



extern int columns;                   /* number of columns in the watershed */
extern int outlet_cell_number;
extern COLUMN_INFOPTR *columndata;
extern INITIAL_INFOPTR initialptr;
extern GENERAL_PESTICIDE_DATA *general_pest_info;
extern SEDIMENT_DATA outlet_sediment[7];
extern float soil_break_down[6][7];   /* [ soil_type ][ particle_type ] */

extern FLAGS_TABLE    tflags;
extern CHEM_TABLE     ctable;
extern SED_TABLE      stable;


#ifdef _UNIX_K_AND_R

 extern int rnd();     /* just a rounding function */
 void output_nps();
 void sed_nut();
 void header_info();
 void summary_info();
 void sed_info();
 void soil_loss();
 void nutrient();
 void feedlot_info();
 void pest_out();

#else

 extern int rnd(float number);     /* just a rounding function */
 void output_nps( FILE*, FILE*, int);
 void sed_nut(float,int,int,float*,float*);
 void header_info( FILE* ,float, float,float);
 void summary_info(FILE*,float,float);
 void sed_info(FILE*,float,float);
 void soil_loss(FILE*, FILE*, int);
 void nutrient( FILE* , FILE*, int);
 void feedlot_info( FILE* );
 void pest_out(FILE*, float);

#endif




float tot_sed_yield[7];



/***************************************************************************
*                               OUTPUTLOOP2N3                              *
****************************************************************************/

#ifdef _UNIX_K_AND_R

void output_nps(nps, GISfp, doGIS)

       FILE *nps;
       FILE *GISfp;
       int doGIS;

#else

void output_nps( FILE *nps, FILE *GISfp, int doGIS)

#endif

{
float  area_of_ws;     /** area of watershed in acres              **/
float  run_vol;        /** runoff volume in inches                 **/
float  converter;      /** used as a temp to convert to ppm water  **/
int    j;

    area_of_ws = columndata[outlet_cell_number]->accumulated->
					drainage_area;


    run_vol = columndata[outlet_cell_number]->accumulated->
		runoff_vol_below / (IN_ACRE_TO_CU_FT * area_of_ws);


    if (run_vol != 0.0)
	converter    = 1.0e6 / (run_vol *
				IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS);
    else
	converter = 0.0;



       for (j=1; j<6; j++)
	{
	tot_sed_yield[j] = columndata[outlet_cell_number]->
					runoff->sediment_yield[j];
	tot_sed_yield[6] += tot_sed_yield[j];

	}


    feedlot_info(nps);
    header_info(nps, run_vol, converter, area_of_ws);
    sed_info(nps, area_of_ws, converter);
    soil_loss(nps, GISfp, doGIS);
    nutrient(nps, GISfp, doGIS);

}    /*  end of outputloop2n3  */




/***************************************************************************
*                            HEADER INFO                                   *
****************************************************************************/

#ifdef _UNIX_K_AND_R

void header_info(nps, run_vol, converter, area_of_ws)

     FILE *nps;
     float run_vol;
     float converter;
     float area_of_ws;

#else

void header_info(FILE *nps,float run_vol,float converter, float area_of_ws)

#endif

{
   /** write header data to the nps file **/


   fputs("INITIAL\n",nps);

#ifdef _DOS
   fprintf (stderr,"     ... Initial Output Being Created                \r");
#endif

   fprintf(nps,"%s", initialptr->watershed);

   fprintf(nps,"%.2f %.2f %.2f %.2f %d %.3d %.2f %.2f %.2f\n",
		area_of_ws,
		initialptr->base_cell_area,
		initialptr->storm_rainfall,
		initialptr->storm_energy_intensity,
		columndata[outlet_cell_number]->cell_number,
		columndata[outlet_cell_number]->cell_division,
		run_vol,
		columndata[outlet_cell_number]->
				accumulated->runoff_flow_below,
		tot_sed_yield[6]); /* New value added to file C0006 */

   summary_info(nps,area_of_ws,converter);

}




/***************************************************************************
*                             SUMMARY INFO                                 *
****************************************************************************/

#ifdef _UNIX_K_AND_R
 
void summary_info(nps, area_of_ws, converter)
 
 
    FILE *nps;
    float area_of_ws;
    float converter;

#else
 
void summary_info(FILE *nps, float area_of_ws, float converter)
 
#endif
 
{

float sed_tot_acre = 0.0;  /** sediment total per acre                   **/
 
float sol_nit      = 0.0;  /** soluable nitrogen per acre                **/
float sol_phos     = 0.0;  /** soluable phosphorous per acre             **/
float sol_cod      = 0.0;  /** soluable cod per acre                     **/
 
float nit_ppm      = 0.0;  /** soluable nitrogen in parts per million    **/
float phos_ppm     = 0.0;  /** soluable phosphorous in parts per million **/
float cod_ppm      = 0.0;  /** soluable cod in parts per million         **/
 
float nit_sed      = 0.0;  /** sediment associated with nitrogern        **/
float phos_sed     = 0.0;  /** sediment associated with phosphorous      **/

RUNOFF_INFO *runoffpath;



    runoffpath   = columndata[outlet_cell_number]->runoff;

    sed_tot_acre = tot_sed_yield[6] / area_of_ws;

    sol_nit      = runoffpath->soluble_nitrogen_yield   / area_of_ws;
    sol_phos     = runoffpath->soluble_phosphorus_yield / area_of_ws;
    sol_cod      = runoffpath->soluble_cod_yield        / area_of_ws;

    nit_ppm      = sol_nit  * converter;
    phos_ppm     = sol_phos * converter;
    cod_ppm      = sol_cod  * converter;


    nit_sed  	=  runoffpath->total_n_cell_outlet/
		   area_of_ws;

    phos_sed	=  runoffpath->total_p_cell_outlet/
		   area_of_ws;


    fprintf( nps, "%.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n",
		nit_sed,  sol_nit,  nit_ppm,
		phos_sed, sol_phos, phos_ppm,
			  sol_cod,  cod_ppm );

    fputs("****\n",nps);
}

 
 
 
/***************************************************************************
*                             SEDIMENT INFO                                 *
****************************************************************************/

#ifdef _UNIX_K_AND_R
 
void sed_info(nps, area_of_ws, converter)

     FILE *nps;
     float area_of_ws;
     float converter;

#else

void sed_info(FILE *nps, float area_of_ws, float converter)

#endif

{
int i                = 0;
float  ave_wt_ero    = 0.0;    /* average weighted erosion     tons/acre */
float  ave_gul_ero   = 0.0;    /* average gully erosion                  */
float  del_ratio     = 0.0;    /* delivery ratio                         */
float  area_wt_yld   = 0.0;    /* area weighted average                  */
float  concen        = 0.0;    /* concentration                          */
float  fract_of_part = 0.0;    /* fraction of particles in soil          */
float  enrich_ratio  = 0.0;    /* enrichment ratio                       */


   fputs("SEDIMENT\n",nps);

   for (i=1; i<=6; i++)
	  {
#ifdef _DOS
	  fprintf (stderr,"   %d ... Sediment Output Being Created            \r",i);
#endif

	  ave_wt_ero    = outlet_sediment[i].area_weighted_erosion;
	  ave_gul_ero   = outlet_sediment[i].gully_erosion;

	  if((outlet_sediment[i].area_weighted_erosion == 0.0) &&
	     outlet_sediment[i].gully_erosion == 0.0)
	    del_ratio = 0.0;
	  else
	    del_ratio     = (tot_sed_yield[i] / (outlet_sediment[i].area_weighted_erosion +
					 outlet_sediment[i].gully_erosion) * 100);

	  if (tot_sed_yield[6] != 0)
		 fract_of_part = tot_sed_yield[i] / tot_sed_yield[6];
	  else
		 fract_of_part = 0;

	  if ( (outlet_sediment[i].area_weighted_erosion != 0) &&
	       (outlet_sediment[6].area_weighted_erosion != 0)  )
		 enrich_ratio  = fract_of_part /(outlet_sediment[i].area_weighted_erosion /outlet_sediment[6].area_weighted_erosion);
	  else
		 enrich_ratio  = 0.0;

	  concen         = converter * tot_sed_yield[i]* 2000.0 /area_of_ws;
	  area_wt_yld    = tot_sed_yield[i] / area_of_ws;
	  ave_wt_ero    /= area_of_ws;
	  ave_gul_ero   /= area_of_ws;


#ifdef _HUMAN_READABLE_FILES_

	  fprintf(nps,"%7.2f %7.2f %3d %3d %7.2f %7.2f %7.2f\n",
					    ave_wt_ero,
					    ave_gul_ero,
					    rnd(del_ratio),
					    rnd(enrich_ratio),
					    concen,
					    area_wt_yld,
					    tot_sed_yield[i]);
#else

	  fprintf(nps,"%.2f %.2f %d %d %.2f %.2f %.2f\n",
					    ave_wt_ero,
					    ave_gul_ero,
					    rnd(del_ratio),
					    rnd(enrich_ratio),
					    concen,
					    area_wt_yld,
					    tot_sed_yield[i]);


#endif


       }
   fputs("****\n",nps);
}




/***************************************************************************
*                           SOIL LOSS INFO                                 *
****************************************************************************/
 
#ifdef _UNIX_K_AND_R

void soil_loss(nps, GISfp, doGIS)
 
      FILE *nps;
      FILE *GISfp;
      int doGIS;
 
#else
 
void soil_loss(FILE *nps, FILE *GISfp, int doGIS)
 
#endif
 
{
int   i,j;
int   calculate;
float runoff_in      = 0.0; /* runnoff coming into the cell     inches */
float runoff_out     = 0.0; /* runnoff going out of the cell    inches */
float drain_area     = 0.0; /* drainage area                    acres  */
float area_runoff    = 0.0; /* runoff vol exiting  cell       in-acres */
float percent_runoff_above; /* % runoff generated above the cell       */
float sed_gen_in     = 0.0; /* sediment generated within the cell      */
float sum_sed_del    = 0.0; /* sum of sediment deliverd to channel     */
float max_sed        = 0.0; /* max betweed sediment delivered to       */
float tot_deposit    = 0.0; /* total deposition in the cell            */
float overland_ero   = 0.0; /* overland erosion in t/a                 */
float sed_available  = 0.0; /* sediment available in the cell          */
float upstream_yld[7]     ; /* upstream sediment yeild                 */
float sed_out_lbs    = 0.0; /* sediment leaving the cell in lbs        */
float deposition     = 0.0; /* deposition in the cell                  */
float max_sed_lbs    = 0.0; /* maximum sediment in lbs ??              */
float soil_type_eros = 0.0; /* I'm only guessing that this is the      */
			    /* amount of overland erosion that is      */
			    /* of the soil type in the current loop    */
float run_acre       = 0.0; /* runoff per acre for a cell              */
float sol_fract      = 0.0; /* fraction of soil type in soil           */
float equiv_runoff_value;   /* vol of water gen. within cell   in-acre */
float runoff_value;         /* vol of water entering cell      in-acre */
 

   fputs("SOIL_LOSS\n",nps);

   if (doGIS)
	fputs("******* GIS SOIL LOSS OUTPUT *******\n",GISfp);


   for (i=1; i<=columns; i++)
	{
#ifdef _DOS
	fprintf (stderr,"  %d ... Soil Loss Output Being Created         \r",i);
#endif

	equiv_runoff_value = (columndata[i]->accumulated->runoff_vol_below -
			columndata[i]->accumulated->runoff_vol_above) /
			IN_ACRE_TO_CU_FT;
 
    /*	equiv_runoff_value = columndata[i]->runoff->cell_run_off; */
 
	runoff_value = columndata[i]->accumulated->runoff_vol_above /
			IN_ACRE_TO_CU_FT;
 

	if (columndata[i]->accumulated->drainage_area >
						columndata[i]->area)
	     {
	     runoff_in = runoff_value / (columndata[i]->accumulated->
				drainage_area - columndata[i]->area);
	     }
	else
	     {
	     runoff_in = 0.0;
	     }
 
	drain_area = columndata[i]->accumulated->drainage_area;
 
	if (columndata[i]->receiving_cell_position != 0)    /* nonsinkhole */
	     {
	     area_runoff  = columndata[i]->accumulated->runoff_vol_below /
				IN_ACRE_TO_CU_FT;
 
	     if (drain_area > 0.0)
		  runoff_out = area_runoff / drain_area;      /* inches */
	     else
		  runoff_out = 0.0;
 
 
	     if (area_runoff > 0.0)
		  percent_runoff_above = 100.0 * runoff_value / area_runoff;
	     else
		  percent_runoff_above = 0.0;
	     }
	else
	     {
	     runoff_out           = 0.0;
	     percent_runoff_above = 0.0;
	     }

	sed_gen_in= columndata[i]->runoff->total_eroded_sediment / 2000;

	upstream_yld[6]   = 0.0;
	tot_sed_yield[6]  = 0.0;

	for (j=1; j<=5; j++)
	     {
	     upstream_yld[j]   = columndata[i]->runoff->
					available_sediment[j] / 2000;
	     upstream_yld[6]  += upstream_yld[j];

	     tot_sed_yield[j]  = columndata[i]->runoff->sediment_yield[j];
	     tot_sed_yield[6] += tot_sed_yield[j];

             if (tflags.sed_table)
              {
               stable.st[i].up_amount[j] = upstream_yld[j];
               if (columndata[i]->accumulated->runoff_flow_above > 0.0)
                {
                 stable.st[i].up_conc[j]   = upstream_yld[j] /
                                              columndata[i]->accumulated->runoff_flow_above;
                }
               else
                 stable.st[i].up_conc[j] = 0.0;

               stable.st[i].down_amount[j] = tot_sed_yield[j];

               if (columndata[i]->accumulated->runoff_flow_below > 0.0)
                {
                 stable.st[i].down_conc[j] = tot_sed_yield[j] /
                                              columndata[i]->accumulated->runoff_flow_below;
                }
               else
                stable.st[i].down_conc[j] = 0.0;
              }
	     }

        if (tflags.sed_table)
         {
          stable.st[i].up_amount[6]   = upstream_yld[6];
          stable.st[i].down_amount[6] = tot_sed_yield[6];

          if (columndata[i]->accumulated->runoff_flow_below > 0.0)
           {
            stable.st[i].down_conc[6] = tot_sed_yield[6] /
                                         columndata[i]->accumulated->runoff_flow_below;
           }
          else
            stable.st[i].down_conc[6] = 0.0;

          if (columndata[i]->accumulated->runoff_flow_above > 0.0)
           {
            stable.st[i].up_conc[6]   = upstream_yld[j] /
                                         columndata[i]->accumulated->runoff_flow_above;
           }
          else
           stable.st[i].up_conc[6] = 0.0;

         }

	sum_sed_del = upstream_yld[6] + columndata[i]->runoff->
					      total_eroded_sediment / 2000;

	if ((sum_sed_del<tot_sed_yield[6]) && (columndata[i]->channel->
						channel_mannings == 0.013))
	     tot_sed_yield[6] = sum_sed_del;

	max_sed = sum_sed_del;
 
	if (tot_sed_yield[6] > sum_sed_del)
	     max_sed = tot_sed_yield[6];
 

	if (max_sed > 0.0)
	     tot_deposit   = (sum_sed_del -tot_sed_yield[6]) / max_sed * 100;
	else
	     tot_deposit   = 0.0;
 
	if (fabs(tot_deposit) < 0.5)
	     tot_deposit   = 0.0;
 
	overland_ero = columndata[i]->runoff->total_eroded_sediment /
				(2000.0 * columndata[i]->area);
 
	for (j=1; j<=6; j++)
	     {
	     sol_fract = soil_break_down[columndata[i]->soil->soil_type][j];
	     sed_available = upstream_yld[j] * 2000.0 +
		    (columndata[i]->runoff->total_eroded_sediment
		     * sol_fract * columndata[i]->area_not_terraced /
		     columndata[i]->area) + (columndata[i]->runoff->
		     impound_yield[j] * 2000);
	     sed_out_lbs   = tot_sed_yield[j] * 2000.0;
	     deposition    = 0;
 
 
/* a mannings of .013 is concrete and therefore there is no scouring */
 
	     if ((sed_out_lbs >= sed_available) &&
		      (columndata[i]->channel->channel_mannings == .013))
/*	       || (calculate == FALSE)) */
		  {
		   sed_out_lbs       = sed_available;
		   tot_sed_yield[j]  = sed_out_lbs / 2000.0;
		   }

	     max_sed_lbs = sed_available;
 
	     if (sed_out_lbs > sed_available)
		 max_sed_lbs = sed_out_lbs;
 
	     if (max_sed_lbs > 0.0)
		 deposition  = (sed_available - sed_out_lbs) /
					      max_sed_lbs * 100.0;
	     else
		 deposition  = 0.0;
 
	     if (fabs(deposition) < 0.5)
		 deposition  = 0.0;

	     sed_gen_in = columndata[i]->runoff->total_eroded_sediment *
						sol_fract / 2000.0;

             if (tflags.sed_table)
              {
               stable.st[i].tot_in_cell_src[j] = sed_gen_in;
              }
	     soil_type_eros = overland_ero * sol_fract;
 
	     if (j==1)
		 {
		 run_acre = equiv_runoff_value / columndata[i]->area;



#ifdef _HUMAN_READABLE_FILES_

		 fprintf(nps,"%2d %.3d %.2f %4.2f %4.2f %.2f %4.2f %.2f %5.1f\n",
			 columndata[i]->cell_number,
			 columndata[i]->cell_division,
			 drain_area,
			 columndata[i]->runoff->cell_run_off,
			 runoff_in,
			 columndata[i]->accumulated->runoff_flow_above,
			 runoff_out,
			 columndata[i]->accumulated->runoff_flow_below,
			 percent_runoff_above);

		 fprintf(nps,"      %4.2f %6.2f %5.2f %6.2f %4d\n", soil_type_eros,
				  upstream_yld[j],  sed_gen_in,
				  tot_sed_yield[j], rnd(deposition));

#else
		 fprintf(nps,"%d %.3d %.2f %.2f %.2f %.2f %.2f %.2f %.1f\n",
			 columndata[i]->cell_number,
			 columndata[i]->cell_division,
			 drain_area,
			 run_acre,
			 runoff_in,
			 columndata[i]->accumulated->runoff_flow_above,
			 runoff_out,
			 columndata[i]->accumulated->runoff_flow_below,
			 percent_runoff_above);
 
		 fprintf(nps,"%.2f %.2f %.2f %.2f %d\n", soil_type_eros,
				  upstream_yld[j],  sed_gen_in,
				  tot_sed_yield[j], rnd(deposition));
 
#endif
 
 
		 }
	     else
		 {
#ifdef _HUMAN_READABLE_FILES_

		 fprintf(nps,"      %4.2f %6.2f %5.2f %6.2f %4d\n", soil_type_eros,
				  upstream_yld[j],  sed_gen_in,
				  tot_sed_yield[j], rnd(deposition));
#else
 
		 fprintf(nps,"%.2f %.2f %.2f %.2f %d\n", soil_type_eros,
				  upstream_yld[j],  sed_gen_in,
				  tot_sed_yield[j], rnd(deposition));
#endif
 
 
 
		 if( doGIS && (j==6) )
		     fprintf(GISfp,
			 "%d %.3d %d %.2f %.2f %d %.2f %d %.1f %.2f %.2f %.2f %.2f %d\n",
			 columndata[i]->cell_number,
			 columndata[i]->cell_division,
			 rnd(drain_area),
			 run_acre,
			 runoff_in,
			 rnd(columndata[i]->accumulated->runoff_flow_above),
			 runoff_out,
			 rnd(columndata[i]->accumulated->runoff_flow_below),
			 percent_runoff_above,
			 soil_type_eros,
			 upstream_yld[j],
			 sed_gen_in,
			 tot_sed_yield[j],
			 rnd(deposition)   );
		 }
	     } /* end of for loop indexed by particle type */
	}      /* end of for loop indexed by collumn */
   fputs("****\n",nps);
}
/***************************************************************************
*                         PESTICIDE INFORMATION                            *
****************************************************************************/

#ifdef _UNIX_K_AND_R

 void pest_out(nps, converter)

     FILE *nps;
     float converter;

#else

 void pest_out(FILE *nps,float converter)

#endif
{
int i;
int pest_cell_count  =0;
float drain_area     = 0.0;  /* drainage area of a cell           acres  */
float pest_sol_con_exit;     /* exit conc. of soluble pesticide    ppm   */
float pest_sed_con_exit;     /* exit conc. of sed-attatched pest.  ppm   */
PESTICIDE_INFO *pest;

 
fputs("PESTICIDE\n",nps);
 
 for (i=1; i<=columns; i++)
    {

/* Output the pesticide info.  If no pesticide was applied in a cell, most */
/*     of these numbers will be zero.                                      */


       drain_area = columndata[i]->accumulated->drainage_area;

       pest = columndata[i]->pesticide;

       if (pest->soluble_pest_exit >0)
#ifdef _DOS
      fprintf (stderr," %d...Pesticide Output Being Created              \r",i);
#endif


       if (drain_area > 0.0)
	   pest_sol_con_exit = pest->soluble_pest_exit *
						converter / drain_area;
       else
	   pest_sol_con_exit = 0.0;


       if (tot_sed_yield[6] > 0.0)
	   pest_sed_con_exit = pest->sediment_pest_exit /
				(tot_sed_yield[6] * 1.0e6);
       else
	   pest_sed_con_exit = 0.0;


      if (tflags.chem_table)
       {
        ctable.ct[i].Pest_dis_local = pest->soluble_pest;
        ctable.ct[i].Pest_dis_out   = pest->soluble_pest_exit;

        ctable.ct[i].Pest_att_local = pest->sediment_pest;
        ctable.ct[i].Pest_att_out   = pest->sediment_pest_exit;

        ctable.ct[i].Pest_tot_local = ctable.ct[i].Pest_dis_local   +
                                       ctable.ct[i].Pest_att_local;
        ctable.ct[i].Pest_tot_out   = ctable.ct[i].Pest_dis_out     +
                                       ctable.ct[i].Pest_att_out;
       }

	 /* convert to lbs/acre x 10^-3 */

       pest->soluble_pest       /= columndata[i]->area;
       pest->sediment_pest      /= columndata[i]->area;


       if (drain_area > 0.0)
	   {
	   pest->sediment_pest_exit /= drain_area;
	   pest->soluble_pest_exit  /= drain_area;
	   }
       else
	   {
	   pest->sediment_pest_exit = 0.0;
	   pest->soluble_pest_exit  = 0.0;
	   }



       if ((pest->soluble_pest_exit) || (pest->sediment_pest_exit))
	{
	 pest_cell_count ++;
 
	 if(pest_cell_count == 1)
	    fprintf(nps,"%d         %s \n",
	    columndata[i]->pesticide_type,
		general_pest_info->pesticide_name);
 
#ifdef _HUMAN_READABLE_FILES_
 
 
	 fprintf(nps,"      %d %.3d %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f\n",
		  columndata[i]->cell_number,
		  columndata[i]->cell_division,
		  drain_area,
		  pest->soluble_pest,
		  pest->soluble_con,
		  pest->soluble_pest_per,
		  pest->soluble_pest_exit,
		  pest_sol_con_exit);
 
	 fprintf(nps,"        %5.2f %5.2f %5.2f %5.2f %5.2f",
		  pest->sediment_pest,
		  pest->sediment_con,
		  pest->sediment_pest_per,
		  pest->sediment_pest_exit,
		  pest_sed_con_exit);
 
 
	 fprintf(nps, "        %5.2f %5.2f\n",
		  pest->percolated,
		  pest->percolation_per);


#else
 
	 fprintf(nps,"%d %.3d %5.2f %5.2f %5.2f %5.2f %5.2f %5.2f\n",
		  columndata[i]->cell_number,
		  columndata[i]->cell_division,
		  drain_area,
		  pest->soluble_pest,
		  pest->soluble_con,
		  pest->soluble_pest_per,
		  pest->soluble_pest_exit,
		  pest_sol_con_exit);
 
	 fprintf(nps,"%5.2f %5.2f %5.2f %5.2f %5.2f",
		  pest->sediment_pest,
		  pest->sediment_con,
		  pest->sediment_pest_per,
		  pest->sediment_pest_exit,
		  pest_sed_con_exit);
 
 
	 fprintf(nps, "%5.2f %5.2f\n",
		  pest->percolated,
		  pest->percolation_per);
 
 
 
 
#endif
	}
 }
 if(pest_cell_count == 0)
   fprintf(nps,"0\n");
 fputs("****\n",nps);
}

/***************************************************************************
*                           NUTRIENT INFO                                  *
****************************************************************************/
 
#ifdef _UNIX_K_AND_R
 
void nutrient(nps, GISfp, doGIS)
 
     FILE *nps;
     FILE *GISfp;
     int doGIS;
 
#else
 
void nutrient(FILE *nps, FILE *GISfp, int doGIS)
 
#endif
 
{
int i,j;
float drain_area     = 0.0;  /* drainage area of a cell           acres  */
float runoff_out     = 0.0;  /* runoff volume out of the cell     inches */
float converter      = 0.0;  /* converts lbs/acre to ppm water           */
float area_runoff    = 0.0;  /* vol of runoff out of the cell    acre-in */
float sed_yld_tpa    = 0.0;  /* sediment yield in                tons/ac */
float cell_nit_sed   = 0.0;  /* nitrogen sediment per cell       lbs/ac  */
float cell_phos_sed  = 0.0;  /* phosphorous sediment per cell    lbs/ac  */
float nit_sed        = 0.0;  /* nitrogen sediment                lbs/ac  */
float phos_sed       = 0.0;  /* phosphorous sediment             lbs/ac  */
float overland_eros  = 0.0;  /* overland erosion per cell       tons/ac  */
float tot_sol_nit    = 0.0;  /* total soluable nitrogen          lbs/ac  */
float tot_sol_phos   = 0.0;  /* total soluable phosphorous       lbs/ac  */
float tot_sol_cod    = 0.0;  /* total soluable c.o.d.            lba/ac  */
float tot_nit_ppm    = 0.0;  /* total soluable nitrogen           ppm    */
float tot_phos_ppm   = 0.0;  /* total soluable phosphorous        ppm    */
float tot_cod_ppm    = 0.0;  /* total soluable c.o.d              ppm    */
float nit_run        = 0.0;  /* soluable N in runoff from cell   lbs/ac  */
float phos_run       = 0.0;  /* soluable P in runoff from cell   lbs/ac  */
float cod_run        = 0.0;  /* soluable COD in runoff from cell lbs/ac  */


   fputs("NUTRIENT\n",nps);

   if (doGIS)
       fputs("******* GIS NUTRIENT OUTPUT *******\n",GISfp);


   for (i=1; i<=columns; i++)
       {

#ifdef _DOS
       fprintf (stderr,"  %d ... Nutrient Output Being Created           \r",i);
#endif

       tot_sed_yield[6] = 0.0;



       for (j=1; j<=5; j++)
	    tot_sed_yield[6] += columndata[i]->runoff->sediment_yield[j];



       drain_area = columndata[i]->accumulated->drainage_area;

       if (columndata[i]->receiving_cell_position != 0)
	    {
	    area_runoff = columndata[i]->accumulated->runoff_vol_below /
					IN_ACRE_TO_CU_FT;

	    if (drain_area > 0.0)
		 {
		 runoff_out = area_runoff / drain_area;
 
		 if (runoff_out > 0.0)
		      converter = 1.0e6 / (runoff_out *
				IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS);
		 else
		      converter = 0.0;
		 }
 
	    else
		 {
		 runoff_out = 0.0;
		 converter  = 0.0;
		 }
	    }
       else /* Sinkhole */
	    {
	    runoff_out = 0.0;
	    converter  = 0.0;
	    }
 
       if (drain_area > 0.0)
	    sed_yld_tpa = tot_sed_yield[6] / drain_area;
       else
	    sed_yld_tpa = 0.0;
 
 
 
       /* Calc. for the outlet of the cell */
 
/*       sed_nut( sed_yld_tpa,
		columndata[i]->soil->soil_type,
		i,
		&nit_sed, &phos_sed ); */
 
	/**** Calculate cell sediment N and P ****/
	/* Calculate for within the current cell */
 
 /*      overland_eros = columndata[i]->runoff->total_eroded_sediment /
					( 2000.0 * columndata[i]->area); */
   /*    sed_nut( overland_eros,
	       columndata[i]->soil->soil_type,
	       i,
	       &cell_nit_sed, &cell_phos_sed); */
 
	       cell_nit_sed = columndata[i]->runoff->total_n_within_cell/
			      columndata[i]->area;
	       cell_phos_sed = columndata[i]->runoff->total_p_within_cell/
			      columndata[i]->area;;
	       nit_sed = columndata[i]->runoff->total_n_cell_outlet/
			      drain_area;
	       phos_sed = columndata[i]->runoff->total_p_cell_outlet/
			      drain_area;
 
 
       /* Soluble Nitrogen */
 
       if (drain_area > 0.0)
	    {
	    tot_sol_nit  = columndata[i]->runoff->soluble_nitrogen_yield /
						drain_area;
	    tot_sol_phos = columndata[i]->runoff->soluble_phosphorus_yield /
						drain_area;
	    tot_sol_cod  = columndata[i]->runoff->soluble_cod_yield /
						drain_area;
	    }
       else
	    {
	    tot_sol_nit  = 0.0;
	    tot_sol_phos = 0.0;
	    tot_sol_cod  = 0.0;
	    }
 
 
       /* Convert these to ppm */
 
       tot_nit_ppm  = tot_sol_nit  * converter;
       tot_phos_ppm = tot_sol_phos * converter;
       tot_cod_ppm  = tot_sol_cod  * converter;


       nit_run  = columndata[i]->runoff->soluble_nitrogen_runoff;
       phos_run = columndata[i]->runoff->soluble_phosphorus_runoff;
       cod_run  = columndata[i]->runoff->cod_runoff;




    if (tflags.chem_table)
     {
      ctable.ct[i].N_dis_local = columndata[i]->runoff->soluble_nitrogen_runoff
                                  * columndata[i]->area;
      ctable.ct[i].N_dis_out   = columndata[i]->runoff->soluble_nitrogen_yield;
      ctable.ct[i].N_att_local = columndata[i]->runoff->total_n_within_cell;
      ctable.ct[i].N_att_out   = columndata[i]->runoff->total_n_cell_outlet;
      ctable.ct[i].N_tot_local = ctable.ct[i].N_dis_local   +
                                       ctable.ct[i].N_att_local;
      ctable.ct[i].N_tot_out   = ctable.ct[i].N_dis_out   +
                                  ctable.ct[i].N_att_out;


      ctable.ct[i].P_dis_local = columndata[i]->runoff->soluble_phosphorus_runoff
                                  * columndata[i]->area;
      ctable.ct[i].P_dis_out   = columndata[i]->runoff->soluble_phosphorus_yield;
      ctable.ct[i].P_att_local = columndata[i]->runoff->total_p_within_cell;
      ctable.ct[i].P_att_out   = columndata[i]->runoff->total_p_cell_outlet;
      ctable.ct[i].P_tot_local = ctable.ct[i].P_dis_local   +
                                       ctable.ct[i].P_att_local;
      ctable.ct[i].P_tot_out   = ctable.ct[i].P_dis_out   +
                                  ctable.ct[i].P_att_out;


      ctable.ct[i].COD_local_lbs = columndata[i]->runoff->cod_runoff
                                    * columndata[i]->area;

      if ((ctable.ct[i].COD_local_lbs > 0.0) && (columndata[i]->runoff->cell_run_off))
        ctable.ct[i].COD_local_ppm = 1000000 * ctable.ct[i].COD_local_lbs /
                                      (columndata[i]->runoff->cell_run_off *
                                       columndata[i]->area *
                                       IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS);

      ctable.ct[i].COD_out_lbs   = columndata[i]->runoff->soluble_cod_yield;
      ctable.ct[i].COD_out_ppm   = tot_cod_ppm;

     }






#ifdef _HUMAN_READABLE_FILES_
 
       fprintf(nps,"%2d %.3d %.2f      %5.2f %5.2f    %5.2f %5.2f %6.2f\n",
				columndata[i]->cell_number,
				columndata[i]->cell_division,
				drain_area,
				cell_nit_sed,
				nit_sed,
				nit_run,
				tot_sol_nit,
				tot_nit_ppm);
 
       fprintf(nps,"        %5.2f %5.2f    %5.2f %5.2f %5.2f         %6.2f %6.2f %6.2f\n",
				cell_phos_sed,
				phos_sed,
				phos_run,
				tot_sol_phos,
				tot_phos_ppm,
				cod_run,
				tot_sol_cod,
				tot_cod_ppm);
#else
 
       fprintf(nps,"%d %.3d %.2f %.2f %.2f %.2f %.2f %.2f\n",
				columndata[i]->cell_number,
				columndata[i]->cell_division,
				drain_area,
				cell_nit_sed,
				nit_sed,
				nit_run,
				tot_sol_nit,
				tot_nit_ppm);
 
       fprintf(nps,"%.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n",
				cell_phos_sed,
				phos_sed,
				phos_run,
				tot_sol_phos,
				tot_phos_ppm,
				cod_run,
				tot_sol_cod,
				tot_cod_ppm);
#endif
 
 
 
 
       if (doGIS)
	    fprintf(GISfp,
		"%d %.3d %d%.2f %.2f %.2f %.2f %d %.2f %.2f %.2f %.2f %d %.2f %.2f %d\n",
		columndata[i]->cell_number,
		columndata[i]->cell_division,
		rnd(drain_area),
		cell_nit_sed,
		nit_sed,
		nit_run,
		tot_sol_nit,
		rnd(tot_nit_ppm),
		cell_phos_sed,
		phos_sed,
		phos_run,
		tot_sol_phos,
		rnd(tot_phos_ppm),
		cod_run,
		tot_sol_cod,
		rnd(tot_cod_ppm));
       }
   fputs("****\n",nps);
   pest_out(nps,converter);
}

 
 
 
/***************************************************************************
*                          	FEEDLOT INFO                                   *
****************************************************************************/
 
 
#ifdef _UNIX_K_AND_R
 
void feedlot_info(nps)
 
     FILE *nps;
 
#else
 
void feedlot_info(FILE *nps)
 
#endif
 
{
int i;
FEEDLOT_INFO *temp;
FEEDLOT_TOTALS *totalptr;
 
 
 
   fputs("FEEDLOT\n",nps);
 
   for (i=1; i<=columns; i++)
       {
 
       temp     = columndata[i]->feedlot;
       totalptr = columndata[i]->feed_totals;
       while (temp!=NULL)
	   {
#ifdef _DOS
	    fprintf (stderr,"  %d ... Feedlot Output Being Created          \r",i);
#endif

	    fprintf(nps,"%d %.3d %.3f %.3f %.3f %.3f %.3f %.3f %d\n",
					     columndata[i]->cell_number,
					     columndata[i]->cell_division,
					     temp->n_conc_discharge,
					     temp->p_conc_discharge,
					     temp->cod_conc_discharge,
					     temp->n_discharge_lbs,
					     temp->p_discharge_lbs,
					     temp->cod_discharge_lbs,
					     temp->feedlot_rating_number);

	   if (columndata[i]->feed_totals != NULL)
		 fprintf(nps,"TOTL %.2f %.2f %.2f\n",
					   totalptr->tot_feedlot_nit,
					   totalptr->tot_feedlot_phos,
					   totalptr->tot_feedlot_cod);
	   temp = temp->next;
	   }
       }
   fputs("****\n",nps);
}