/****************** Documentation Start *********************
 
NAME:  LOOP1.C
 
SYNOPSIS:
   This procedure runs the calculations that are not dependent on
   the routing of the model.
 
*********************************************/
 
 
 
/* Header files included in procedure */

#ifdef _DOS
 
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include <dos.h>
 #include "input.h"
 #include "binary.h"
 #include "debugflg.h"

#else

#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif
 
/*#include <memcheck.h> */
 

/* GLOBAL VARIABLES */

extern int              	debuginfo;
extern int 			columns;
extern COLUMN_INFOPTR   	*columndata;
extern INITIAL_INFOPTR  	initialptr;
extern SEDIMENT_DATA 		outlet_sediment[7];
extern int 			outlet_cell_number;
extern int              	error_log;
extern FILE             	*errorfp;
extern float            	soil_break_down[6][7];
extern GENERAL_PESTICIDE_DATA 	*general_pest_info;

extern int 			hydro_info;
extern int			sourceinfo;
extern int 			hydro_file_open;
extern int			source_acct_open;
extern char 			hydro_out_file[128];
extern char			source_acct[128];
extern HYDRO_REC_PTR		hydro;
extern HYDRO_REC		hydrodata;
extern SOURCEACCT		sourceact;
extern SOURCEACCTPTR		sourceactptr;
extern FILE			*hydrofile;
extern FILE			*sourcefile;

extern FLAGS_BASIC   bflags;
extern FLAGS_ROUTINE rflags;

extern FLAGS_TABLE   tflags;
extern FILE         *vfy1;
extern HYDRO_TABLE   htable;

/* Function Prototypes */

#ifdef _UNIX_K_AND_R

 void  rclmap();
 void  calc_sed_nut();
 void  length();
 void  xeros();
 void  curve_number_runoff();
 float ovrld();
 void  slnut();
 void  impound();
 void  pntsrc();
 void  loop1tr55();
 void  pesticide();

#else

 void  rclmap(int);
 void  calc_sed_nut(int,float*,float*,float);
 void  length(int);
 void  xeros(int,float,int,float,float,float,float,float*);
 void  curve_number_runoff(float,float,int,char[20],float*);
 float ovrld(float,int,float);
 void  slnut(int, float, float*, float*);
 void  impound(int, int, float, float);
 void  pntsrc(int, int);
 void  loop1tr55(int, float*);
 void  pesticide( int soil_type, float rainfall_inches, float runoff_inches,
	   float sediment_lbs_per_acre, float cell_area_acres,
	   PESTICIDE_INFO *pest, GENERAL_PESTICIDE_DATA *genpest,
	   int);

#endif
 

 
void loop1()
{
 /***************************************************

 DESCRIPTION:

 RETURNS:

 NOTES:

 DATA STORES:

  columndata[column_number]->cell_division     [R]
  columndata[column_number]->area              [C]
  columndata[column_number]->area_not_terraced [C]
  initialptr->base_cell_area                   [R]

 HISTORY:

Date    	Bug#   Prog   Description

06/22/93	C0007	MAK	Put in multiple gullies
11/03/93	C0018	MAK	Added long filenames


 *************** Documentation End ************************/

 /* LOCAL VARIABLES IN LOOP1.C */

 int 		column_number = 0;
 GULLY_INFO      *gullyptr;
 IMPOUND_INFO    *currentptr;
 PESTICIDE_INFO  *pest;
 MANAGEMENT_INFO *mgmtstruct;
 SLOPE_INFO      *slopestruct;
 SOIL_INFO       *soilstruct;
 RUNOFF_INFO    *runoffstruct;
 int  		temp_receiving_cell;
 int            error_code;
 char           problem[20];
 float 		upland_erosion;                     /* tons/acre */
 float           overland_duration;                  /* hours     */
 float 		particle_fraction;
 float          total_gully_sediment = 0.0;
 int             division1,division2,division3;
 float          overland_sediment;
 float 		runoff;                             /* inches    */
 float          impounded_area;
 int 		particle_size;
 float 		nitrogen_runoff;                    /* lbs/acre  */
 float 	 	phosphorus_runoff;                  /* lbs/acre  */
 float          overland_nit;
 float          overland_phos;
 float 		sed_in_cell;
 float		total_cell_sed;
 
 
 /*  Start of the program */
 
 column_number = 1;
 
 while( column_number <= columns )
  {

#ifdef _DOS
   fprintf (stderr,"  %d  ... Loop #1 : Executing Loop#1            \r",column_number);
#endif

   gullyptr = columndata[column_number]->gully;
   mgmtstruct  =columndata[column_number]->management;
   slopestruct =columndata[column_number]->slope;
   soilstruct  =columndata[column_number]->soil;
   runoffstruct=columndata[column_number]->runoff;

 
 
   /***** 1.1  CALCULATE AREA FOR EACH CELL ******
 
   This part of the program takes the cell divisions and the base cell
   area as input and calculates the area of each cell.  This is based on
   the number of times each cell has been divided.  Note that the AGNPS
   cell's are square.  This means that when a cell is divided, each
   division results in 1/4 the orignal base cell area.

   ERROR CODE INFORMATION:  0 = no errors
			    1 = problem with division 1
			    2 = problem with division 2
			    3 = problem with division 3
			    4 = cell area <= 0
			    5 = base cell area <=0
			      */

 
 
 
   division1 = columndata[column_number]->cell_division/100;
   division2 = (columndata[column_number]->cell_division -(division1*100))/10;
   division3 = (columndata[column_number]->cell_division -
					(division1*100) - (division2*10) );
 
 
 
   /* Calculate area of each divided cell if present */
   /*UNITS: ACRES columndata.area, initialptr.base_cell_area */
 
   if (division3 > 0)
     columndata[column_number]->area = initialptr->base_cell_area/64;

   else if (division2 > 0)
     columndata[column_number]->area = initialptr->base_cell_area/16;
 
   else if (division1 > 0)
     columndata[column_number]->area = initialptr->base_cell_area/4;
 
   else
     columndata[column_number]->area = initialptr->base_cell_area;
 
 
   /*UNITS: ACRES columndata.area_not_terraced */
 
   columndata[column_number]->area_not_terraced =
					columndata[column_number]->area;
  /* ERROR ROUTINE FOR 1.1 */
   if(error_log == TRUE)
   {
     error_code = 0;
     if((division1 >4) || (division1<0)) error_code=1;
     if((division2 >4) || (division2<0)) error_code=2;
     if((division3 >4) || (division3<0)) error_code=3;
     if(columndata[column_number]->area <= 0) error_code=4;
     if(initialptr->base_cell_area <= 0)      error_code=5;
     if(error_code > 0)
      {
       fprintf(errorfp,"Error Occured in the Cell Division routine!!\n");
       fprintf(errorfp,"Error occured while processing cell..%d \n",column_number);
       fprintf(errorfp,"Routine is located in loop 1. \n");
       fprintf(errorfp,"========================================================= \n\n");

       if(error_code == 1)
	 strcpy(problem,"Division 1 not in the range 0-4");
       if(error_code == 2)
	 strcpy(problem,"Division 2 not in the range 0-4");
       if(error_code == 3)
	 strcpy(problem,"Division 3 not in the range 0-4");
       if(error_code == 4)
	 strcpy(problem,"Area of cell is less than or equal to zero");
       if(error_code == 5)
	 strcpy(problem,"Base Cell Area is less than or equal to 0");
 
 
       fprintf(errorfp,"ERROR #%d    %s \n\n",error_code,problem);
       fprintf(errorfp,"========================================================= \n");
 
       fprintf(errorfp,"Data Dump........\n");
       fprintf(errorfp,"INPUT:  Division 1     (flag):     %d \n",division1);
       fprintf(errorfp,"        Division 2     (flag):     %d \n",division2);
       fprintf(errorfp,"        Division 3     (flag):     %d \n",division3);
       fprintf(errorfp,"        Base Cell Area (acres):    %f \n\n",
	 initialptr->base_cell_area);
       fprintf(errorfp,"OUTPUT: Area           (acres):    %f \n",
	  columndata[column_number]->area);
       fprintf(errorfp,"        Area not terraced (acres): %f \n",
		columndata[column_number]->area_not_terraced);
       fprintf (stderr,"Error encountered-Program stopped-Check error.log");
       fclose(errorfp);
       sleep(5);
       exit(1);
    }
   }
 
/*   if(bflags.cell_info)
    {
     fprintf (stderr,"****\n");
     fprintf (stderr,"1.1 CALCULATED AREA OF CURRENT CELL\n");
     fprintf (stderr,"INPUTS: Division 1=%d  Division 2=%d  Division 3=%d Base Cell Area=%f(acres)\n",
	   division1,division2,division3,initialptr->base_cell_area);
     fprintf (stderr,"OUTPUTS: Cell Area = %f (acres) Error Code= %d \n\n",
	    columndata[column_number]->area,error_code);
    }
 
*/
   /** 1.2  LOCATE RECEIVING CELL POSITION **/
 
   error_code=0;
 
   if(column_number < 0) error_code=1;

   rclmap(column_number);

   if(bflags.cell_info)
    {
     fprintf (stderr,"****\n");
     fprintf (stderr,"1.2 LOCATE RECEIVING CELL POSITION\n");
     fprintf (stderr,"INPUTS: Column Number= %d\n",column_number);
     fprintf (stderr,"OUTPUTS: Receiving Cell = %d  Error Code %d \n\n",
	    columndata[column_number]->receiving_cell_position,error_code);
    }
 
 

    /**** 1.3  CHECK FOR A SINK HOLE *****/

    if (columndata[column_number]->receiving_cell_position ==  column_number)
     {
      columndata[column_number]->receiving_cell_position = 0;
      if(bflags.cell_info)
       {
	fprintf (stderr,"****\n");
	fprintf (stderr,"1.3 CHECK FOR A SINKHOLE\n");
	fprintf (stderr,"INPUTS: Column Number %d\n",column_number);
	fprintf (stderr,"OUTPUTS: SINKHOLE\n\n");
       }
     }
 
 
    temp_receiving_cell = columndata[column_number]->
						receiving_cell_position;
 
 
    /**** 1.4  SET UP THE OUTLET CELL *****/
 
    /* sets outlet to one more than largest cell number */
 
    if (temp_receiving_cell > columns)
     {
      temp_receiving_cell = columns + 1;
      columndata[column_number]->receiving_cell_position = temp_receiving_cell;
      outlet_cell_number = column_number;
 
      if(bflags.cell_info)
       {
	fprintf (stderr,"****\n");
	fprintf (stderr,"1.4 SET UP THE OUTLET CELL\n");
	fprintf (stderr,"INPUTS: Number of Columns %d  Receiving Cell %d\n",
		    columns,temp_receiving_cell);
	fprintf (stderr,"OUTPUTS:  THIS IS THE OUTLET CELL\n\n");
       }
     }

    /*** 1.5  ADJUST NUMBER OF CELLS RUNNING INTO A RECEIVING CELL ***/
 
    if (temp_receiving_cell > 0)
	columndata[temp_receiving_cell]->primary_cell++;
 
    if(bflags.cell_info)
     {
      fprintf (stderr,"****\n");
      fprintf (stderr,"1.5 ADJUST CELLS FLOWING INTO CELL\n");
      fprintf (stderr,"INPUTS: Receiving Cell %d\n",temp_receiving_cell);
      fprintf (stderr,"OUTPUTS: Number of Cells Flowing in %d\n",
		 columndata[temp_receiving_cell]->primary_cell);
      fprintf (stderr,"This number may change once the routing is complete!\n\n");
     }
 
 
    /*** SET UP LAND SLOPE VALUES (should not be here ***/
 
    /*UNITS: PERCENT slope.average_land_slope */

    if (slopestruct->average_land_slope == 0)   /* division by 0 */
	slopestruct->average_land_slope = 0.1;
 
    /* 1.6  CALCULATE CHANNEL CHARACTERISTICS IF USER HAS NOT INPUT THEM */
 
    /* This procedure also checks to see if values fall within a
       minimum and maximum values */


    length(column_number);
 
 

    /**** 2.1  CALCULATE THE UPLAND EROSION ******/
 
 
    /*UNITS: TONS/ACRE xeros(  )  */


    if (rflags.xeros)
     {
      fprintf (stderr,"%d ",column_number);
     }

    xeros(slopestruct->slope_shape_code,
	  slopestruct->average_land_slope,
	  slopestruct->slope_length,
	  initialptr->storm_energy_intensity,
	  soilstruct->soil_erodibility_factor,
	  mgmtstruct->cropping_factor,
	  mgmtstruct->practice_factor,
	  &upland_erosion);


 /*** 1.8  CALCULATE THE VOLUME OF RUNOFF FROM CELL ***/

    /* This section calculates the amount of runoff in inch-acres
       using the curve number method. */

    /*UNITS: INCHES ro( NOD, INCHES ) */
    /*UNITS: NOD management.curve_number */

   if(mgmtstruct->curve_number == 100)
    {
      runoff = initialptr->storm_rainfall;
      if(hydro_info)
       {
	hydro->runoff=runoff;
	hydro->curve_number=(float)mgmtstruct->curve_number;
	hydro->storm_rainfall=initialptr->storm_rainfall;
	hydro->retention_factor=0.0;
	hydro->equation_top=0.0;
	hydro->equation_bottom=0.0;
       }
    }
   else
     curve_number_runoff((float)mgmtstruct->curve_number,
			      initialptr->storm_rainfall,
			      column_number,
			      "loop1",
			      &runoff);


   if (sourceinfo)
      sourceactptr->runoff_volume=runoff*columndata[column_number]->area;

    runoffstruct->cell_run_off = runoff;

    /*UNITS: INCHES runoff.cell_run_off */


    /*** 1.9  CALCULATE THE OVERLAND FLOW DURATION ***/

    /*UNITS: SEC runoff.overland_flow_duration */
    /*UNITS: SEC ovrld( PERCENT, FEET, NOD ) */
    /*UNITS: FEET slope.slope_length */

    if (initialptr->calc_method == CREAMS)
	runoffstruct->overland_flow_duration =
	ovrld( slopestruct->average_land_slope,
	slopestruct->slope_length,
	mgmtstruct->surface_condition_constant);

    else

    /* We are sending along a zero for the channel length, because
      we do not need to calculate the channel flow duration.  In
      this part of the model, we only calculate the overland and
      shallow flow duration and the channel length is only needed
      in the calculation of the channel duration, which is used in
      loops 2 and 3  */

    /*UNITS: VOID loop1tr55( , HOURS ) */

     {
      loop1tr55(column_number, &overland_duration);


      /* the 3600.0 coverts hours to seconds */

      runoffstruct->overland_flow_duration = 3600 * overland_duration;
     }

   if (hydro_info)
    {
      hydro->calc_method=initialptr->calc_method;
      hydro->temp_receiving_cell=temp_receiving_cell;
      hydro->column_number=column_number;
      hydro->impoundments=columndata[column_number]->num_impoundments;
      if(!hydro_file_open)
       {
	hydrofile=fopen(hydro_out_file,"wb");
	fwrite(&columns,sizeof(int),1,hydrofile);
	hydro_file_open=1;
       }

      fwrite(hydro,sizeof(HYDRO_REC),1,hydrofile);
    }



/*** 1.10  CALCULATE SEDIMENT INFORMATION ***/

     /*UNITS: LBS runoff.total_eroded_sediment */

     if(bflags.sediment)
      {
       fprintf (stderr,"****\n");
       fprintf (stderr,"1.10 CALCULATE SEDIMENT INFORMATION...Col %d:\n",column_number);
      }

     runoffstruct->total_eroded_sediment  =
	  upland_erosion * 2000.0 * columndata[column_number]->area;

     if(bflags.sediment)
      {
       fprintf (stderr,"INPUTS: Total Eroded Sediment from Cell %f\n",
	      runoffstruct->total_eroded_sediment);
      }


	/* Change # 0007 This change allows for multiple gullies */

	total_gully_sediment = 0.0;

	while(gullyptr != NULL)
	 {
	  if(bflags.sediment)
	   {
	     fprintf (stderr,"INTER: <FOUND A GULLY IN THIS CELL>\n");
	     fprintf (stderr," Gully Soil Type... %d\n\n",gullyptr->gully_soil);
	     fprintf (stderr," Part Size  Fraction   Avail Sediment   Gully Erosion\n");
	   }


	  for (particle_size=1; particle_size<=5; particle_size++)
	    {
	     /* calc. amount of sediment for each particle size */

	     particle_fraction =
		soil_break_down[gullyptr->gully_soil][particle_size];

	     /*UNITS: LBS runoff.available_sediment */

	     runoffstruct->available_sediment[particle_size] +=
		gullyptr->gully_source * particle_fraction * 2000.0;


	    if (sourceinfo)
	     {
	       if(particle_size==1)
	       sourceactptr->clay_gully+=gullyptr->gully_source* particle_fraction *
				   2000;
	       if(particle_size==2)
	       sourceactptr->silt_gully+=gullyptr->gully_source* particle_fraction *
				   2000;
	       if(particle_size==3)
	       sourceactptr->sagg_gully+=gullyptr->gully_source* particle_fraction *
				   2000;
	       if(particle_size==4)
	       sourceactptr->lagg_gully+=gullyptr->gully_source* particle_fraction *
				   2000;
	       if(particle_size==5)
	       sourceactptr->sand_gully+=gullyptr->gully_source* particle_fraction *
				   2000;
	     }

	     /*UNITS: TONS columndata.gully_source */
	     /*UNITS: TONS outlet_sediment.gully_erosion */

	     outlet_sediment[particle_size].gully_erosion =
		gullyptr->gully_source * particle_fraction +
		outlet_sediment[particle_size].gully_erosion;

	     if(bflags.sediment)
	      {
	       fprintf (stderr,"%d    %f      %f               %f\n",particle_size,
	       particle_fraction,runoffstruct->available_sediment[particle_size],
	       outlet_sediment[particle_size].gully_erosion);

	      }
	    }


	  total_gully_sediment += gullyptr->gully_source * 2000.0;

	  outlet_sediment[6].gully_erosion =
	       outlet_sediment[6].gully_erosion +
	       gullyptr->gully_source;


	  if(bflags.sediment)
	    fprintf (stderr,"OUTPUT: Total Gully Erosion= %f\n",
		    outlet_sediment[6].gully_erosion);

	  gullyptr=gullyptr->next;
	 }

 /* CALCULATE AMOUNT OF OVERLAND SEDIMENT THAT FLOWS INTO IMPOUNDMENTS */

     currentptr=columndata[column_number]->impound;
     impounded_area=0.0;

     while(currentptr !=NULL)
      {
       if(temp_receiving_cell !=0)
	 impounded_area+=currentptr->drainage_area;

       currentptr=currentptr->next;
      }


 /***       CALCULATE SOIL NUTRIENT VALUES IN THE SEDIMENT ***/

 calc_sed_nut(column_number, &overland_nit, &overland_phos,total_gully_sediment);


 /* Calculate amount flowing into impoundments */

 runoffstruct->impound_nit=overland_nit*
			   (impounded_area/columndata[column_number]->area);
 runoffstruct->impound_phos=overland_phos*
			   (impounded_area/columndata[column_number]->area);

 /* Calculate the amount of sed n and p that doesn't flow into impoundment */

 if(sourceinfo)
  {
   sourceactptr->sed_n_overland=overland_nit-runoffstruct->impound_nit;
   sourceactptr->sed_p_overland=overland_phos-runoffstruct->impound_phos;
  }

   if(sourceinfo)
   {
    sourceactptr->sed_n_gully=runoffstruct->total_n_within_cell-
       overland_nit;

    sourceactptr->sed_p_gully=runoffstruct->total_p_within_cell-
       overland_phos;
   }

 /* Subtract amount flowing into impoundments */

 runoffstruct->total_n_within_cell-=runoffstruct->impound_nit;
 runoffstruct->total_p_within_cell-=runoffstruct->impound_phos;







 /*** 1.11  CALCULATE SOIL NUTRIENT VALUES IN RUNOFF ***/

	/*UNITS: VOID slnut( , INCHES, LBS/ACRE, LBS/ACRE ) */


	slnut( column_number,
	       runoff,               /* inches   */
	       &nitrogen_runoff,     /* lbs/acre */
	       &phosphorus_runoff);  /* lbs/acre */
 
	if (bflags.nutrient)
	    fprintf (stderr,"Nitrogen RO= %f lbs/acre Phosphorus RO= %f lbs/acre\n",
					nitrogen_runoff,phosphorus_runoff);
 

 
	/*UNITS: LBS/ACRE runoff.soluble_nitrogen_runoff */
	/*UNITS: LBS/ACRE runoff.soluble_phosphorus_runoff */
	/*UNITS: LBS/ACRE runoff.cod_runoff */
 
	runoffstruct->soluble_nitrogen_runoff   = nitrogen_runoff;
 
	runoffstruct->soluble_phosphorus_runoff = phosphorus_runoff;
 
	runoffstruct->cod_runoff = mgmtstruct->cod_factor *
		    runoff * (IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS)/1.0e6;





	if(bflags.nutrient)
	 fprintf (stderr,"OUTPUT:nitrogen runoff %f(kg/ha)  phos. runoff %f(kg/ha) cod runoff %f(kg/ha)\n\n",
	 nitrogen_runoff,phosphorus_runoff,runoffstruct->cod_runoff);

 /*** 1.12  CALCULATE SOLUBLE NUTRIENT YIELD (IN LBS) ***/

	/*UNITS: LBS runoff.soluble_nitrogen_yield */
	/*UNITS: LBS runoff.soluble_phosphorus_yield */
	/*UNITS: LBS runoff.soluble_cod_yield */

	if(bflags.nutrient)
	 {
	  fprintf (stderr,"****\n");
	  fprintf (stderr,"1.12 SOLUBLE NUTRIENT YIELD...Col %d:\n",column_number);
	 }


	runoffstruct->soluble_nitrogen_yield = nitrogen_runoff *
			columndata[column_number]->area;

	runoffstruct->soluble_phosphorus_yield = phosphorus_runoff *
			columndata[column_number]->area;

	runoffstruct->soluble_cod_yield = mgmtstruct->cod_factor *
		  runoff * (IN_ACRE_TO_CU_FT * CU_FT_H2O_IN_LBS) *
		  columndata[column_number]->area / 1.0e6;




 /*** IF ITS A SINKHOLE, WE WON'T PASS ANY NUTRIENTS DOWN THE CHANNEL ***/

/* If it is a sinkhole, we must still calculate the amount of nutrients
   in the cell.  The decay.c procedure takes care of not routing the
   nutrients to the next cell */

 /*** 1.13  CALCULATE IMPOUNDMENTS ***/

	/*UNITS: VOID impound( , , TONS/ACRE ) */

  impound(column_number, temp_receiving_cell, upland_erosion, impounded_area);


  runoffstruct->sol_impound_nit = runoffstruct->soluble_nitrogen_runoff *
		     impounded_area;

  runoffstruct->sol_impound_phos = runoffstruct->soluble_phosphorus_runoff *
		  impounded_area;

  runoffstruct->sol_impound_cod = runoffstruct->cod_runoff * impounded_area;





	if(sourceinfo)
	 {
	  sourceactptr->sol_n_overland *= columndata[column_number]->area_not_terraced;
	  sourceactptr->sol_n_fertilizer *= columndata[column_number]->area_not_terraced;
	  sourceactptr->sol_p_overland *= columndata[column_number]->area_not_terraced;
	  sourceactptr->sol_p_fertilizer *= columndata[column_number]->area_not_terraced;
	  sourceactptr->sol_cod_overland = runoffstruct->soluble_cod_yield;
	 }



  sed_in_cell=runoffstruct->total_eroded_sediment*
	   ((columndata[column_number]->area-impounded_area)/
	    columndata[column_number]->area);

  for (particle_size=1; particle_size<=5; particle_size++)
	    {
	     /* calc. amount of sediment for each particle size */

	     particle_fraction =
		soil_break_down[soilstruct->soil_type][particle_size];

	     total_cell_sed=(sed_in_cell*particle_fraction)+
			  (runoffstruct->impound_yield[particle_size]*2000.0);

	      if (sourceinfo)
	      {
	       if(particle_size==1)
	       sourceactptr->clay_sheet=total_cell_sed;

	       if(particle_size==2)
	       sourceactptr->silt_sheet=total_cell_sed;

	       if(particle_size==3)
	       sourceactptr->sagg_sheet=total_cell_sed;

	       if(particle_size==4)
	       sourceactptr->lagg_sheet=total_cell_sed;

	       if(particle_size==5)
	       sourceactptr->sand_sheet=total_cell_sed;
	      }

	     }


 /*** 1.14  CALCULATE FOR POINT SOURCES ***/

	pntsrc(column_number, temp_receiving_cell);


 /*** 1.15  CALCULATE PESTICIDE RESULTS ***/

	if (general_pest_info != NULL)
	    {
	    if (columndata[column_number]->
				pesticide->application_rate > 0.0)
		{
		pest = columndata[column_number]->pesticide;
		pesticide( columndata[column_number]->soil->soil_type,
			   initialptr->storm_rainfall,
			   runoffstruct->cell_run_off,
			   runoffstruct->total_eroded_sediment,
			   columndata[column_number]->area,
			   pest,
			   general_pest_info,
			   column_number );
		}
	    /*UNITS: INCHES initialptr.storm_rainfall */
	    }


 /***  WRITE SOURCE ACCOUNTING FILES ***/

   if(sourceinfo)
    {
      sourceactptr->cell_number=columndata[column_number]->cell_number;
      sourceactptr->cell_division=columndata[column_number]->cell_division;
      sourceactptr->receiving_number=columndata[column_number]->receiving_cell_number;
      sourceactptr->receiving_div=columndata[column_number]->receiving_cell_division;
      sourceactptr->flow_direction=columndata[column_number]->flow_direction;


      if(!source_acct_open)
	{
	 sourcefile=fopen(source_acct,"wb");
	 source_acct_open=1;
	}

	fwrite(sourceactptr,sizeof(SOURCEACCT),1,sourcefile);

      sourceactptr->cell_number=0;
      sourceactptr->cell_division=0;
      sourceactptr->receiving_number=0;
      sourceactptr->receiving_div=0;
      sourceactptr->flow_direction=0;
      sourceactptr->clay_sheet=0.0;
      sourceactptr->clay_gully=0.0;
      sourceactptr->silt_sheet=0.0;
      sourceactptr->silt_gully=0.0;
      sourceactptr->sagg_sheet=0.0;
      sourceactptr->sagg_gully=0.0;
      sourceactptr->lagg_sheet=0.0;
      sourceactptr->lagg_gully=0.0;
      sourceactptr->sand_sheet=0.0;
      sourceactptr->sand_gully=0.0;
      sourceactptr->sed_n_overland=0.0;
      sourceactptr->sed_n_gully=0.0;
      sourceactptr->sed_n_impoundments=0.0;
      sourceactptr->sed_p_overland=0.0;
      sourceactptr->sed_p_gully=0.0;
      sourceactptr->sed_p_impoundments=0.0;
      sourceactptr->sol_n_overland=0.0;
      sourceactptr->sol_n_fertilizer=0.0;
      sourceactptr->sol_n_feedlots=0.0;
      sourceactptr->sol_p_overland=0.0;
      sourceactptr->sol_p_fertilizer=0.0;
      sourceactptr->sol_p_feedlots=0.0;
      sourceactptr->sol_cod_overland=0.0;
      sourceactptr->sol_cod_feedlots=0.0;
      sourceactptr->runoff_volume=0.0;
    }





	column_number++;
	}


    return;
}
