
/****************** DOCUCMENTATION START *****************
 
NAME: readinput.c
 
SYNOPSIS:
 
     This procedure reads in the data from the data file and puts
     it in the data structure.  It also calls the setstruct procedure
     which sets up the dynamic structure for the system.
 
INCLUDE FILES:

*/

#ifdef _DOS

 #include <mem.h>
 #include <stdio.h>
 #include <string.h>
 #include <stdlib.h>
 #include <alloc.h>
 #include "input.h"
 #include "binary.h"
 #include "debugflg.h"

#else

#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif

/*#include <memcheck.h>*/

/* This is the structure for the input format records */

typedef struct
 {
   char  watershed_name[100];
   char  description[100];
   float base_cell_area;
   int   number_of_base_cells;
   int   columns;
   int   calc_method;
   int   geomorphic_calc;
   int   k_coef;
   float k_coef_value;
   float prepeak_ro_fraction;
   float k_triangle;
   char  storm_type[4];
   float storm_energy_intensity;
   float storm_duration;
   float storm_rainfall;
   float rainfall_nitrogen;
   }INIT_REC;


/*

GLOBAL VARIABLES:

*/

extern int columns;
extern int init_info;
extern int error_log;
extern int input_info;
extern int hydro_info;
extern int sourceinfo;
extern int sediment_info;
extern int pest_info;
extern int nut_info;
extern int out_info;
extern char pest_out_file[128];
extern char pest_out_file2[128];
extern char hydro_out_file[128];
extern char hydro_out_file2[128];
extern char source_acct[128];
extern char source_acct2[128];
extern char init_out[128];
extern COLUMN_INFOPTR         *columndata;
extern INITIAL_INFOPTR        initialptr;


/* External Variables for Binary Ooutput Files */

extern PEST_REC_PTR	      pest_ptr;
extern PEST_REC		      pest_data;

extern PEST_ROUTE_REC_PTR     pestroute;
extern PEST_ROUTE_REC         pestroutedata;

extern HYDRO_REC_PTR	      hydro;
extern HYDRO_REC	      hydrodata;

extern HYDRO_IMP_REC_PTR      hydroimp;
extern HYDRO_IMP_REC	      hydroimpdata;

extern HYDRO_ROUTE_REC	      hydro_route_data;
extern HYDRO_ROUTE_REC_PTR    hydro_route;

extern POINT_SOURCE_REC	      point_source_data;
extern POINT_SOURCE_REC_PTR   point_source_rec;

extern IMPOUND_ROUTE_REC      impound_data_rec;
extern IMPOUND_REC_PTR	      impound_data_ptr;

extern SOURCEACCTPTR          sourceactptr;
extern SOURCEACCT	      sourceact;

extern SOURCEACCT2PTR	      sourceact2ptr;
extern SOURCEACCT2	      sourceact2;

extern GENERAL_PESTICIDE_DATA *general_pest_info;

extern FLAGS_TABLE            tflags;
extern HYDRO_TABLE            htable;


/*
FUNCTION PROTOTYPES:

*/

#ifdef _UNIX_K_AND_R

 void   memory_out();
 void   setstruc();

#else

 void   memory_out(int location, int column_number);
 void   setstruc(void);

#endif


#ifdef _UNIX_K_AND_R

int readinput(fp1,sdataname)

   FILE *fp1;
   char sdataname[128];

#else

 int readinput( FILE *fp1, char sdataname[128])

#endif
{
 /******************************************

 DESCRIPTION:  This part of the system reads in all the information for
	       AGNPS to use.

 RETURNS:

 NOTES:  See the input file format sheets.

 DATA STORES:

 HISTORY:

 Date    	Bug#	Prog	Description
 06/28/93  	  	MAK	Finished coding change for binary file line
 09/09/93       C0003	MAK	Put in division by 100 for k_coef_value
 09/22/93       c0007   MAK	Put in multiple gullies
 10/05/93	C0008	MAK	Change buffer slope length to integer
 10/12/93	C0014	MAK	Changed order of input of gully source
 11/03/93	C0018	MAK	Added long filenames

 LOCAL VARIABLES:
 */




 char  fp1buffer[150];
 char  point_source_indicator;

 float k_coef_value;          /* either prepeak_RO_fraction or K_triangle */

 int   num_gullies;
 int   column_number;
 int   namelength;
 int   counter;
 int   impoundments;
 int   k_coef;                /* identifies what k_coef_value means      */
 int   location;
 int   gullies;
 int   num_feedlots;
 int   num_nonfeedlots;
 int   point_source;
 int   source_hold;
 int   i;                     /* Local index counter...    -JW  04/10/95 */

 CHANNEL_INFO     *channelstruct        = NULL;
 FILE             *initial;
 FEEDLOT_INFO     *current_feed_ptr     = NULL;
 GULLY_INFO       *gullyptr             = NULL;
 INIT_REC         init_data;
 IMPOUND_INFO     *impoundptr           = NULL;
 MANAGEMENT_INFO  *mgmtstruct           = NULL;
 NONFEEDLOT_INFO  *current_nonfeed_ptr  = NULL;
 PESTICIDE_INFO   *pestptr              = NULL;
 SLOPE_INFO       *slopestruct          = NULL;
 SOIL_INFO        *soilstruct           = NULL;


  pest_ptr = NULL;
  pestroute = NULL;
  hydro = NULL;
  hydro_route = NULL;
  sourceactptr=NULL;
  sourceact2ptr=NULL;


 /* LINE1: Read in the version code information line.  We don't need it at
    this time, so it is only read off of the file */

 fgets(fp1buffer, 149, fp1);

 /* LINE2: Read in the binary file information */

 fgets(fp1buffer, 149, fp1);


 sscanf(fp1buffer,"%d%d%d%d%d%d",
	&error_log,
	&source_hold,
	&hydro_info,
	&sediment_info,
	&nut_info,
	&pest_info);


       /* check to see if the user has turned the source accounting on
	  in the parameters or if they have turned it on in the actual
	  data file.  We use the source_hold variable since, that way
	  we can only change it if the value is a one.  We don't want
	  to have the value set in the parameter list to true and then
	  read in the data file with a value of false, thus changing
	  the flag from what it was. */

	if(source_hold==1)
	  sourceinfo=1;



	 if (pest_info)
	   {
	     strcpy(pest_out_file,sdataname);
	     namelength = strlen(pest_out_file);
	     pest_out_file[namelength-3] = 'p';
	     pest_out_file[namelength-2] = 's';
	     pest_out_file[namelength-1] = 't';

	     pest_ptr = &pest_data;

	     strcpy(pest_out_file2, sdataname);
	     namelength = strlen(pest_out_file2);
	     pest_out_file2[namelength-3] = 'p';
	     pest_out_file2[namelength-2] = 's';
	     pest_out_file2[namelength-1] = 'r';

	     pestroute = &pestroutedata;

	   }

	 if (sourceinfo)
	   {
	     strcpy(source_acct,sdataname);
	     namelength = strlen(source_acct);
	     source_acct[namelength-3] = 's';
	     source_acct[namelength-2] = 'r';
	     source_acct[namelength-1] = 'c';

	     sourceactptr = &sourceact;

	     strcpy(source_acct2, sdataname);
	     namelength = strlen(source_acct2);
	     source_acct2[namelength-3] = 'd';
	     source_acct2[namelength-2] = 'e';
	     source_acct2[namelength-1] = 'p';

	     sourceact2ptr = &sourceact2;

	   }




	 if (hydro_info)
	   {
	     strcpy(hydro_out_file,sdataname);
	     namelength = strlen(hydro_out_file);
	     hydro_out_file[namelength-3] = 'h';
	     hydro_out_file[namelength-2] = 'y';
	     hydro_out_file[namelength-1] = 'd';

	     hydro = &hydrodata;
	     hydroimp=&hydroimpdata;


	     strcpy(hydro_out_file2,sdataname);
	     namelength = strlen(hydro_out_file2);
	     hydro_out_file2[namelength-3] = 'h';
	     hydro_out_file2[namelength-2] = 'y';
	     hydro_out_file2[namelength-1] = 'r';

	     hydro_route = &hydro_route_data;
	     impound_data_ptr=&impound_data_rec;
	     point_source_rec=&point_source_data;

	   }


 /* LINE3: Read in the watershed name from the data file (header)*/

 memset(initialptr->watershed,0,100);
 fgets(initialptr->watershed, 99, fp1);


 /* LINE4: Read in the watershed description from the data file (header)*/

 memset(initialptr->description,0,100);
 fgets(initialptr->description,99, fp1);


 /* LINE5: Read in the header line from the data file (header) */

 memset(fp1buffer,0,150); /* clean the buffer */
 fgets(fp1buffer,149,fp1); /* read 2nd line of .dat into a buffer */
 
 sscanf(fp1buffer,"%f%d%d%d%d%d%f",
	&initialptr->base_cell_area,
	&initialptr->number_of_base_cells,
	&columns,
	&initialptr->calc_method,
	&initialptr->geomorphic_calc,
	&k_coef,
	&k_coef_value);
 
 
 
 /* CALCULATE THE HYDROGRAPH INFORMATION
 
 This is done as follows..
 triangular hydrograph parameters
 2.0   = Fs = factor based on hydrograph shape
 645.3 = C  = conversion cfs/mi^2 to in/hr */
 
 if (k_coef)                                     /* User gave us K */
  {
   initialptr->K_triangle = k_coef_value;
   initialptr->prepeak_RO_fraction  = k_coef_value/(2.0 * 645.3);
  }
 else                                            /* User gave us Vp */
  {
   /* Change C0003 */
   /* k-coef_value divided by 100 to get into decimal percent */
   initialptr->prepeak_RO_fraction = (k_coef_value/100);
   initialptr->K_triangle = (k_coef_value/100) * 2.0 * 645.3;
  }
 
 
 /*LINE6:  Read in line 6 of the header  */
 
 memset(fp1buffer,0,150);
 memset(initialptr->storm_type,0,4);
 fgets(fp1buffer,149,fp1);
 
 sscanf(fp1buffer,"%s%f%f%f%f",
	initialptr->storm_type,
	&initialptr->storm_energy_intensity,
	&initialptr->storm_duration,
	&initialptr->storm_rainfall,
	&initialptr->rainfall_nitrogen);
 
 /* add the binary file print here */
 
 if(init_info == TRUE)
  {
   initial = fopen(init_out,"wb");
   for (location=0; location <= 99; location++)
    {
      init_data.watershed_name[location] = initialptr->watershed[location];
      init_data.description[location] = initialptr->description[location];
    }
 
   init_data.base_cell_area = initialptr->base_cell_area;
   init_data.number_of_base_cells = initialptr->number_of_base_cells;
   init_data.columns = columns;
   init_data.calc_method = initialptr->calc_method;
   init_data.geomorphic_calc = initialptr->geomorphic_calc;
   init_data.k_coef = k_coef;
   init_data.k_coef_value = k_coef_value;
   init_data.prepeak_ro_fraction = initialptr->prepeak_RO_fraction;
   init_data.k_triangle = initialptr->K_triangle;
   for(location=0;location<=3;location++)
    init_data.storm_type[location] = initialptr->storm_type[location];
   init_data.storm_duration = initialptr->storm_duration;
   init_data.storm_rainfall = initialptr->storm_rainfall;
   init_data.rainfall_nitrogen = initialptr->rainfall_nitrogen;
 
   fwrite(&init_data,sizeof(INIT_REC),1,initial);
   fclose(initial);

  }

  if (tflags.hydro_table)   /* Added for hydrology table verification... */
   {                        /*                            -JW   04/10/95 */
    for (i = 1; i <= 9; ++i)
     htable.ht[i].total_cell_area = initialptr->base_cell_area;
   }
 
/*  fprintf (stderr,"There is %lu bytes after the header info is loaded. \n",
     (unsigned long) coreleft()); */
 
 
 
 setstruc();
 
 column_number = 1;
 
 
 while (column_number <= columns)
  {
   mgmtstruct    = columndata[column_number]->management;
   slopestruct   = columndata[column_number]->slope;
   soilstruct    = columndata[column_number]->soil;
   channelstruct = columndata[column_number]->channel;
 

#ifdef _DOS
   fprintf (stderr,"  %d ... Reading in Cell Information                     <AGNPS 5.00>\r",column_number);
#endif

 
   /* LINE1:  Read in the first line of the required AGNPS data */
 
 
   memset(fp1buffer,0,150);
   fgets(fp1buffer,149,fp1);
 
   sscanf(fp1buffer,"%d%d%d%d%d%f%f%d",
	  &columndata[column_number]->cell_number,
	  &columndata[column_number]->cell_division,
	  &columndata[column_number]->receiving_cell_number,
	  &columndata[column_number]->receiving_cell_division,
	  &columndata[column_number]->flow_direction,
	  &mgmtstruct->curve_number,
	  &slopestruct->average_land_slope,
	  &slopestruct->slope_shape_code);
 
   if (tflags.hydro_table)
    {
     htable.ht[column_number].receiving_cell =
                         columndata[column_number]->receiving_cell_number;
    }
 
 
   /* Set the slope shape code, if the cell is water */
 
   if ((slopestruct->slope_shape_code < 1) && (soilstruct->soil_type == 0))
	slopestruct->slope_shape_code = 1;
 
 
   /* LINE2: Read in the second line of the required AGNPS data */
 
   memset(fp1buffer,0,150);
   fgets(fp1buffer,149,fp1);
 
   sscanf(fp1buffer,"%d%f%f%f%f%f%d",
	  &slopestruct->slope_length,
	  &columndata[column_number]->overland_mannings,
	  &soilstruct->soil_erodibility_factor,
	  &mgmtstruct->cropping_factor,
	  &mgmtstruct->practice_factor,
	  &mgmtstruct->surface_condition_constant,
	  &mgmtstruct->cod_factor);
 
 
   if (slopestruct->slope_length < 1) /* To avoid divide by zero */
       slopestruct->slope_length = 1;


   /* LINE3: read in the third line of required agnps input */


	memset(fp1buffer,0,150);
	fgets(fp1buffer,149,fp1);

	sscanf(fp1buffer,"%d%d%d%d%d%d%d",
	     &soilstruct->soil_type,
	     &mgmtstruct->fertilizer_level,
	     &columndata[column_number]->pesticide_type,
	     &columndata[column_number]->num_point_sources,
	     &num_gullies,
	     &columndata[column_number]->num_impoundments,
	     &channelstruct->channel_indicator);
 
 
 
	if (soilstruct->soil_type<1) /* Makes water into last instead of first */
	    soilstruct->soil_type = 5;
 
 
 
/* LINE1: Read in the optional soil information */
 
     if(soilstruct->soil_type != 5)
      {
	memset(fp1buffer,0,150);
	fgets(fp1buffer,149,fp1);
 
	sscanf(fp1buffer,"%*s%f%f%f%f",
	     &soilstruct->base_soil_nitrogen,
	     &soilstruct->base_soil_phosphorus,
	     &soilstruct->soil_pore_nitrogen,
	     &soilstruct->soil_pore_phosphorus);
      }
     else
     /* If the cell is a water cell, then the nitrogen and phosphorus
	from the soil and soil pores is set to zero. */
      {
	soilstruct->base_soil_nitrogen=0.0;
	soilstruct->base_soil_phosphorus=0.0;
	soilstruct->soil_pore_nitrogen=0.0;
	soilstruct->soil_pore_phosphorus=0.0;
      }
 

 
/* LINE2: Read in the optional soil information line 2 */
 
 
      if(soilstruct->soil_type != 5)
       {
	 memset(fp1buffer,0,150);
	 fgets(fp1buffer,149,fp1);
 
	 sscanf(fp1buffer,"%f%f%f%f%d",
	     &soilstruct->nitrogen_runoff_extraction,
	     &soilstruct->phosphorus_runoff_extraction,
	     &soilstruct->nitrogen_leaching_extraction,
	     &soilstruct->phosphorus_leaching_extraction,
	     &soilstruct->soil_organic_matter);
 
       }
      else
      /* If the cell is a water cell, then the extraction coefficients
	 are set to zero */
       {
	 soilstruct->nitrogen_runoff_extraction=0.0;
	 soilstruct->phosphorus_runoff_extraction=0.0;
	 soilstruct->nitrogen_leaching_extraction=0.0;
	 soilstruct->phosphorus_leaching_extraction=0.0;
       }
 
 
 
 
/* LINE1: Read in the optional fertilizer information */
 
       if(mgmtstruct->fertilizer_level > 0)
       {
	 memset(fp1buffer,0,150);
	 fgets(fp1buffer,149,fp1);
 
	 sscanf(fp1buffer,"%*s%f%f%d%d",
	    &mgmtstruct->nitrogen_application_rate,
	    &mgmtstruct->phosphorus_application_rate,
	    &mgmtstruct->nitrogen_availability,
	    &mgmtstruct->phosphorus_availability);
 
 
       }
 
 
	 /* Every cell needs a pesticide structure now */
 
	pestptr = columndata[column_number]->pesticide =
		      (PESTICIDE_INFO*) calloc(1,sizeof(PESTICIDE_INFO));
 
   /* If pesticide has been applied, then read in pesticide optional info */
 
   if (columndata[column_number]->pesticide_type > 0)
	    {
 
 
	    if (general_pest_info == NULL)
		general_pest_info = (GENERAL_PESTICIDE_DATA*) calloc
			      (1, sizeof(GENERAL_PESTICIDE_DATA));
 
   /* LINE1: Read in the first line of pesticide information */
 
	    memset(fp1buffer,0,150);
	    fgets(fp1buffer,149,fp1);
 
	    sscanf(fp1buffer,"%*s%s",
			general_pest_info->pesticide_name);

  /* LINE2: Read in the second line of optional pesticide information */
 
	    memset(fp1buffer,0,150);
	    fgets(fp1buffer,149,fp1);
 
	    sscanf(fp1buffer,"%f%f%f%f%f",
		 &pestptr->time_of_application,
		 &pestptr->time_since_application,
		 &pestptr->application_rate,
		 &pestptr->application_efficiency,
		 &pestptr->canopy_cover);
 
 
   /* LINE3: Read in the third line of optional pesticide information */
 
 
	    memset(fp1buffer,0,150);
	    fgets(fp1buffer,149,fp1);
 
	    sscanf(fp1buffer,"%f%f%f%f%f%f",
		 &pestptr->initial_soil_residue,
		 &general_pest_info->soil_residue_halflife,
		 &pestptr->incorporation_depth,
		 &pestptr->incorporation_efficiency,
		 &general_pest_info->solubility,
		 &general_pest_info->organic_carbon_sorption);
 
 
 
   /* LINE4: Read in the fourth line of optional pesticide information */

 
	    memset(fp1buffer,0,150);
	    fgets(fp1buffer,149,fp1);

	    sscanf(fp1buffer,"%f%f%f%f",
		 &pestptr->foliar_residue,
		 &general_pest_info->foliar_washoff_threshold,
		 &general_pest_info->foliar_washoff_fraction,
		 &general_pest_info->foliar_residue_halflife);
	    }
 
 


   /* If there is a point source, then read optional information */


	if (columndata[column_number]->num_point_sources > 0)
	    {

            if (tflags.hydro_table)
             {
              htable.ht[column_number].feedlot_area = 0.0;
             }

	    num_feedlots    = 0;
	    num_nonfeedlots = 0;

	    for (point_source=1; point_source <=
			columndata[column_number]->num_point_sources;
			point_source++)
		{


		/* read in the first line of the point source to see
		   if it is a feedlot or non-feedlot */


		memset(fp1buffer,0,150);
		fgets(fp1buffer,149,fp1);


		point_source_indicator = fp1buffer[0];

		/* The point source is a non-feedlot point source */

		if (point_source_indicator == 'N')
		    {
		    if (num_nonfeedlots == 0)
			{
			columndata[column_number]->nonfeedlot =
			      (NONFEEDLOT_INFO*) calloc(1,
			      sizeof(NONFEEDLOT_INFO));

			if (columndata[column_number]->nonfeedlot == NULL)
			    memory_out(7, column_number);

			current_nonfeed_ptr=columndata[column_number]->nonfeedlot;
			}
		    else
			{
			current_nonfeed_ptr->next = (NONFEEDLOT_INFO*)
					calloc(1,sizeof(NONFEEDLOT_INFO));

			if (current_nonfeed_ptr->next == NULL)
			    memory_out(8, column_number);

			current_nonfeed_ptr=current_nonfeed_ptr->next;
			}

		    sscanf(fp1buffer,"%*s%f%f%f%f%d",
		       &current_nonfeed_ptr->water_discharge,
		       &current_nonfeed_ptr->nitrogen_concentration,
		       &current_nonfeed_ptr->phosphorus_concentration,
		       &current_nonfeed_ptr->cod_concentration,
		       &current_nonfeed_ptr->enter_at_top);

		    num_nonfeedlots++;

		    }  /* end if pointsource code == N */

		else /* The point source is a feedlot */
		    {
		    if (num_feedlots == 0)
			{
			columndata[column_number]->feedlot =
			    (FEEDLOT_INFO*) calloc(1,sizeof(FEEDLOT_INFO));

			if (columndata[column_number]->feedlot == NULL)
			     memory_out(9, column_number);

			current_feed_ptr = columndata[column_number]->
								feedlot;
			}
		    else
			{
			current_feed_ptr->next = (FEEDLOT_INFO*)
					calloc(1,sizeof(FEEDLOT_INFO));

			if (current_feed_ptr->next == NULL)
			    memory_out(10, column_number);

			current_feed_ptr = current_feed_ptr->next;
			}


		    sscanf(fp1buffer,"%*s%f%f%f%f%f%f",
			  &current_feed_ptr->area,
			  &current_feed_ptr->curve_number,
			  &current_feed_ptr->roofed_area,
			  &current_feed_ptr->feedlot_nitrogen,
			  &current_feed_ptr->feedlot_phosphorus,
			  &current_feed_ptr->feedlot_cod);


                    if (tflags.hydro_table)
                     {
                      htable.ht[column_number].feedlot_area +=
                                           (current_feed_ptr->area +
                                            current_feed_ptr->roofed_area);
                     }



   /* LINE2: Read in the second line of the optional feedlot input */

		    memset(fp1buffer,0,150);
		    fgets(fp1buffer,149,fp1);

/* Bug # A-6
   This bug was reading the grass inputs before the overland inputs,
   This should have been the other way around.  The code was changed
   so that the overland variables were read first and then the grass
   variables
   */

		    sscanf(fp1buffer,"%d%f%f%f%f%f%f",
			  &current_feed_ptr->use_buffer_inputs,
			  &current_feed_ptr->decrease_n_overland,
			  &current_feed_ptr->decrease_p_overland,
			  &current_feed_ptr->decrease_cod_overland,
			  &current_feed_ptr->decrease_n_grass,
			  &current_feed_ptr->decrease_p_grass,
			  &current_feed_ptr->decrease_cod_grass);

   /* LINE3: (REPEATING) area characteristics for the 6 subareas */

		    for ( counter=1; counter<=6; counter++)
			{

			  memset(fp1buffer,0,150);
			  fgets(fp1buffer,149,fp1);

			  sscanf(fp1buffer,"%f%f%f%f",
			     &current_feed_ptr->area2_area[counter],
			     &current_feed_ptr->area2_curve_number[counter],
			     &current_feed_ptr->area3_area[counter],
			     &current_feed_ptr->area3_curve_number[counter]);

                          if (tflags.hydro_table)
                           {
                            htable.ht[column_number].feedlot_area +=
                                    (current_feed_ptr->area2_area[counter] +
                                     current_feed_ptr->area3_area[counter]);
                           }

			}


   /* REPEATING -Read in the info for the 3 buffer areas */

		    for (counter=1; counter<=3; counter++)
			{

			  memset(fp1buffer,0,150);
			  fgets(fp1buffer,149,fp1);

    /* Change buffer slope length to integer */

			  sscanf(fp1buffer,"%f%f%d",
				&current_feed_ptr->buffer_slope[counter],
				&current_feed_ptr->buffer_surface_constant[counter],
				&current_feed_ptr->buffer_flow_length[counter]);
 
			  if (current_feed_ptr->buffer_flow_length[counter] == 0.0)
			    current_feed_ptr->buffer_flow_length[counter]  = 1.0;
 
			}
 
 
 
   /* REPEATING - Read in the info for up to three animals in the feedlot
      Note: That there are always three types of animals read in */
 
		    for(counter=1; counter<=3; counter++)
			{
 
			  memset(fp1buffer,0,150);
			  fgets(fp1buffer,149,fp1);
 
			  sscanf(fp1buffer,"%f%f%f%f",
				&current_feed_ptr->number_of_animals[counter],
				&current_feed_ptr->cod_factor[counter],
				&current_feed_ptr->animal_phosphorus[counter],
				&current_feed_ptr->animal_nitrogen[counter]);
 
			}
 
 
 
		    num_feedlots++;
 
		    } /* end of else for feedlot */
 
		} /* end for on point sources */
 
	    } /* End if then for point sources */



   /* LINE1: Read optional gully line, if the user has put in
    additional erosion */


  /* Change #c0007 Allow the user to enter multiple gullies through the use
     of a linked list of gullies.
  */

  if(num_gullies >0)
     {

	 gullyptr = columndata[column_number]->gully =
		      (GULLY_INFO*) calloc( 1, sizeof(GULLY_INFO));

	 gullyptr->next=NULL;

	 for(gullies=1; gullies <=num_gullies;
	      gullies++)
	    {
	     if (gullies > 1)
		 {
		  gullyptr->next=(GULLY_INFO*) calloc(1,sizeof(GULLY_INFO));
		  gullyptr=gullyptr->next;
		  gullyptr->next=NULL;
		 }



	    memset(fp1buffer,0,150);
	    fgets(fp1buffer,149,fp1);

	    sscanf(fp1buffer,"%*s%*d%d%d%f%f",
		 &gullyptr->gully_source, /* Added this variable C0007*/
		 &gullyptr->gully_soil,
		 &gullyptr->gully_nitrogen,
		 &gullyptr->gully_phos);

	   }
     }

    /* LINE1: Read optional impoundment line if present */


	if (columndata[column_number]->num_impoundments > 0)
	    {
	    impoundptr = columndata[column_number]->impound =
			(IMPOUND_INFO*) calloc(1,sizeof(IMPOUND_INFO));

	    impoundptr->next = NULL;
 
	    for (impoundments=1;impoundments <=
		  columndata[column_number]->num_impoundments;
		  impoundments++)
		{
		if (impoundments > 1)
		    {
		    impoundptr->next=(IMPOUND_INFO*) calloc(1,
						sizeof(IMPOUND_INFO));
		    impoundptr=impoundptr->next;
		    impoundptr->next=NULL;
		    }

		memset(fp1buffer,0,150);
		fgets(fp1buffer,149,fp1);

		sscanf(fp1buffer,"%*s%f%f%f",
			 &impoundptr->drainage_area,    /* acres      */
			 &impoundptr->pipe_diameter,    /* inches     */
			 &impoundptr->infiltration);

                if (tflags.hydro_table)
                 {
                  htable.ht[column_number].impoundment_area +=
                                              impoundptr->drainage_area;
                 }

		}
	    }
 
 
 
 
 
	  /* LINE1: Read in first line of Channel information */
 
	  memset(fp1buffer,0,150);
	  fgets(fp1buffer,149,fp1);
 
	  sscanf(fp1buffer,"%*s%f%f%f%f%f%f",
	     &channelstruct->width,
	     &channelstruct->width_coef,
	     &channelstruct->width_exp,
	     &channelstruct->depth,
	     &channelstruct->depth_coef,
	     &channelstruct->depth_exp);
 
	 /* LINE2: Read in the second line of the channel information */
 
 
	 memset(fp1buffer,0,150);
	 fgets(fp1buffer,149,fp1);
 
	 sscanf(fp1buffer,"%f%f%f%f%f",
	     &channelstruct->channel_length,
	     &channelstruct->length_coef,
	     &channelstruct->length_exp,
	     &channelstruct->channel_slope,
	     &channelstruct->channel_side_slope);
 
 
	 /* LINE3: Read in the third line of the channel information */
	 /* Put this part back in once Basil fixes the spreadsheet to
	    handle water cells. */
 
 
	memset(fp1buffer,0,150);
	fgets(fp1buffer,149,fp1);

	sscanf(fp1buffer,"%f%d%f%f%f",
	     &channelstruct->channel_mannings,
	     &channelstruct->agnps_decay,
	     &channelstruct->per_n_decay,
	     &channelstruct->per_p_decay,
	     &channelstruct->per_cod_decay);
 
 
      /* read in the fourth line of the channel info */
 
	 memset(fp1buffer,0,150);
	 fgets(fp1buffer,149,fp1);
 
       if(channelstruct->channel_indicator > 0)
	{
	 sscanf(fp1buffer,"%d%d%d%d%d",
	     &channelstruct->clay_indicator,
	     &channelstruct->silt_indicator,
	     &channelstruct->small_agg_indicator,
	     &channelstruct->large_agg_indicator,
	     &channelstruct->sand_indicator);
	}
       else
	{
	  channelstruct->clay_indicator = 0;
	  channelstruct->silt_indicator = 0;
	  channelstruct->small_agg_indicator = 0;
	  channelstruct->large_agg_indicator = 0;
	  channelstruct->sand_indicator =0;
	}
 
 
	column_number++;
 
 
/*      fprintf (stderr,"There is %lu bytes after loading up cell num %d \n",
       (unsigned long) coreleft(),column_number); */
 
 
	}
 
    rewind(fp1);
 
    return (1);
}
