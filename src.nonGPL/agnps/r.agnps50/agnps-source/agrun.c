/***** Documentation Start ********

NAME: AGRUN.C

SYNOPSIS:

  This procedure is the main procedure that calls all the other
  procedures for the agnps model.  It also checks for the various
  command line functions that could be defined from the command line.


HEADER FILES:
*/
#ifdef _DOS

  #include <stdio.h>
  #include <dos.h>
  #include <string.h>
  #include <stdlib.h>
  #include <conio.h>
  #include "input.h"
  #include <alloc.h>
  #include "binary.h"
  #include "debugflg.h"

#else

#include <stdio.h>
#include "input.h"
#include "binary.h"
#include "debugflg.h"

#endif

#include "gis.h"


/*
FUNCTION PROTOTYPES:
*/
#ifdef _UNIX_K_AND_R

  void  init_db_file();
  void  read_db_file();         /* New debug flag file -JW 03/29/95... */
  int   readinput();
  void  loop1();
  void  routing_loop();
  void  output_nps();
  int   locate_source_cells();
  void  memory_out();
  int   main();


#else

  void  init_db_file(FLAGS_TABLE *tflags, FLAGS_BASIC *bflags,
		     FLAGS_ROUTINE *rflags);
  void  read_db_file(FILE *dbfp, FLAGS_TABLE *tflags, FLAGS_BASIC *bflags,
		     FLAGS_ROUTINE *rflags);
				 /* New debug flag file -JW 03/29/95... */
  int   readinput(FILE *fp1,char [128]);
  void  loop1(void);
  void  routing_loop(int outlet_column);
  void  output_nps(FILE *nps, FILE *GISfp, int doGIS);
  int   locate_source_cells( int num_cols, SINKHOLEPTR first );
  void  memory_out(int location, int column_number);
  int   main(int argc, char *argv[] );

#endif


/*
GLOBAL VARIABLES:
 */

int                      columns;
float                    sed_yield_for_j[6];
int                      pest_file_open = FALSE;
int                      pest_file_open2 = FALSE;
int                      hydro_file_open = FALSE;
int                      hydro2_file_open= FALSE;
int                      source_acct_open=FALSE;
int                      source_acct_open2=FALSE;
int                      outlet_cell_number;
int                      sedimentinfo=FALSE;
int                      nutrientinfo=FALSE;
int                      debuginfo=FALSE;
int                      sourceinfo=FALSE;
char                     init_out[128];
char                     pest_out_file[128];
char                     pest_out_file2[128];
char                     hydro_out_file[128];
char                     hydro_out_file2[128];
char                     source_acct[128];
char                     source_acct2[128];

/* Enhancement E-2 */
int                      init_info      = FALSE;
int                      error_log      = FALSE;
int                      input_info     = FALSE;
int                      hydro_info     = FALSE;
int                      sediment_info  = FALSE;
int                      pest_info      = FALSE;
int                      nut_info       = FALSE;
int                      out_info       = FALSE;
int                      hydroinfo      = FALSE;


SEDIMENT_DATA            outlet_sediment[7];
SEDIMENT_INFO            sediment[6];
INITIAL_INFO             initial_input;
INITIAL_INFOPTR          initialptr;

/* Set up binary source structures */

PEST_REC_PTR             pest_ptr;
PEST_REC                 pest_data;

PEST_ROUTE_REC_PTR       pestroute;
PEST_ROUTE_REC           pestroutedata;

HYDRO_REC_PTR            hydro;
HYDRO_REC                hydrodata;

HYDRO_IMP_REC            hydroimpdata;
HYDRO_IMP_REC_PTR        hydroimp;

HYDRO_ROUTE_REC          hydro_route_data;
HYDRO_ROUTE_REC_PTR      hydro_route;

POINT_SOURCE_REC_PTR     point_source_rec;
POINT_SOURCE_REC         point_source_data;

SOURCEACCTPTR            sourceactptr;
SOURCEACCT               sourceact;

SOURCEACCT2PTR           sourceact2ptr;
SOURCEACCT2              sourceact2;

IMPOUND_ROUTE_REC        impound_data_rec;
IMPOUND_REC_PTR          impound_data_ptr;

COLUMN_INFOPTR           *columndata;
SINKHOLE                 first_sinkhole;
GENERAL_PESTICIDE_DATA   *general_pest_info = NULL;
FILE                     *errorfp;
FILE                     *pestfile;
FILE                     *pestfile2;
FILE                     *hydrofile;
FILE                     *hydrofile2;
FILE                     *sourcefile;
FILE                     *sourcefile2;
FILE   *hyd;
FILE   *imp;

/*--------------------------------John's additional DEBUG.FLG variables----*/
			       /*                                          */
 FLAGS_TABLE    tflags;        /* Verification table pntr (see debugflg.h) */
 FLAGS_BASIC    bflags;        /* Basic flags struct pntr (see debugflg.h) */
 FLAGS_ROUTINE  rflags;        /* Routine flg struct pntr (see debugflg.h) */

 HYDRO_TABLE    htable;        /* Hydrology output verification table.     */
 SED_TABLE      stable;        /* Sediment output verification table.      */
 CHEM_TABLE     ctable;        /* Chemical output verification table.      */
			       /*                                          */
/*--------------------------------Added to 4.03c on 03/29/95 (now 5.00)----*/


/*--------------------------------John's additional DEBUG.FLG variables----*/
			       /*                                          */
 FILE           *dbfp = NULL;  /* Debug file pointer.                      */
			       /*                                          */
 FILE           *vfy1 = NULL;  /* Verification file pointer #1...hydrology.*/
 FILE           *vfy2 = NULL;  /* Verification file pointer #2...sediment. */
 FILE           *vfy3 = NULL;  /* Verification file pointer #3...chemicals.*/
 FILE           *vfy4 = NULL;  /* Verification file pointer #4...feedlot.  */
 FILE           *vfy5 = NULL;  /* Verification file pointer #5...impoundmt.*/
 FILE           *vfy6 = NULL;  /* Verification file pointer #6...sed_traps.*/
			       /*                                          */
			       /*                                          */
/*--------------------------------Added to 4.03c on 03/29/95 (now 5.00)----*/


/*
CONSTANTS:
*/

/*********************** SOIL BREAK DOWN matrix  *************************
 *             array indexes = [soil_type][particle_type]                *
 * array indexes of [0] are not used.  Thus, the values 9.9 are entered. */

float soil_break_down[6][7] = {{9.9, 9.9,  9.9,  9.9,  9.9,  9.9,  1.0},
			       {9.9, 0.02, 0.02, 0.16, 0.2,  0.6,  1.0},
			       {9.9, 0.05, 0.08, 0.5,  0.31, 0.06, 1.0},
			       {9.9, 0.1,  0.06, 0.57, 0.25, 0.02, 1.0},
			       {9.9, 1.0,  0.0,  0.0,  0.0,  0.0,  1.0},
			       {9.9, 0.0,  0.0,  0.0,  0.0,  0.0,  0.0}};

/* INPUT PARAMETERS: (Also See Data Dictionary) */
#ifdef _UNIX_K_AND_R
int main(argc,argv)

  int argc;        /* filename of the input file (from command line) */
  char *argv[];    /* flag to check for GIS output, and debug information
		      such as hydrology, sediment */
#else

int main(

  int argc,        /* filename of the input file (from command line) */
  char *argv[])    /* flag to check for GIS output, and debug information
		      such as hydrology, sediment */
#endif

{

 /*

 OUTPUT PARAMETERS:
      none

 LOCAL VARIABLES:
 */

 char   sDATANAME[128];        /* This is the name of the input data file  */
 char   dataout[128];          /* This is the name of the output data file */
 char   line_buffer[100];      /* Line buffer used for reading # cells (VT)*/
 int    peek_cols;             /* Number of columns from peek feature...   */
 float  peek_float;            /* Temp vars used in peek...                */
 int    peek_int;              /*                                          */
 int    doGIS          = FALSE;/* Flag for GIS output 1=GIS output 0=no GIS*/
 int    namelength;            /* Length of the */
 int    argument_check;
 FILE   *fp1;                  /* File pointer that points to the current  */
			       /* location in the file.                    */
 FILE   *GISfp         = NULL;
 char   GISout[128];
 FILE   *nps;
 int    outlet_column;         /* This will contain the number of the column*/
			       /* where the outlet cell is located          */
 int    no_errors=0;

 int    i = 0;                 /* Temporary index counter.                  */
 char   vt_1name[15];          /* Filename for verification output table 1. */
 char   vt_2name[15];          /* Filename for verification output table 2. */
 char   vt_3name[15];          /* Filename for verification outpuf table 3. */

 struct GModule *module;
 struct Option *input;
 struct Flag *gis, *source;
 struct Flag *sediment, *nutrient, *debug, *hydro;


/*

 DESCRIPTION:

  This is the main routine for the agnps ver. 4.00 SCS C-code.
  This program calls all the required subroutines for the model
  portion of the system.  There is also a spreadsheet that this
  links to for input.  The first part of this file works with the
  command line inputs to create the input and output filenames.
  The input file should have a .DAT extension.  The output file will
  have a .NPS extension and if the user selected a GIS file to be
  created, it will have a .GIS extension.  This procedure also sets
  the debug flags on if the user has selected them.  These variables
  are set as global so the model knows what the user selected as debug
  information outputs.  The program then calls the procedure to input
  the data from the .DAT file.  The program


 RETURNS:

 NOTES:

 DATA STORES:

 HISTORY:

 Date           Bug#    Prog    Desc
 9/21/92                MAK     created finished version.
 2/22/93                MAK     added the debug indicator to print out debug
				information.
 3/11/93                MAK     added documentation to the file.
 3/15/93                MAK     added initial data output file .
 9/10/93        C0004   MAK     changed check for para. to 3 instead of 2
11/03/93        C0018   MAK     Added long filenames
11/04/93        C0018   MAK     Added program run flag


 SEE ALSO:

 readinput, loop1, routing loop, output nps, locate source cells

 ************** Documentation End ***********/


 /****** DEAL WITH COMMAND LINE ARGUMENTS *******/

 G_gisinit(argv[0]);

 module = G_define_module();
 module->description =
	 "Assess nonpoint source pollution origination and movement in a watershed.";

 input = G_define_option();
 input->key            = "input";
 input->type           = TYPE_STRING;
 input->required       = YES;
 input->description    = "Data file to be analyzed";

 gis = G_define_flag();
 gis->key              = 'g';
 gis->description      = "Create GIS formatted output file";

 source = G_define_flag();
 source->key           = 'a';
 source->description   = "Enable source accounting";

 sediment = G_define_flag();
 sediment->key         = 's';
 sediment->description = "Output sediment information";

 nutrient = G_define_flag();
 nutrient->key         = 'n';
 nutrient->description = "Output nutrient information";

 hydro = G_define_flag();
 hydro->key            = 'h';
 hydro->description    = "Output hydro information";

 debug = G_define_flag();
 debug->key            = 'd';
 debug->description    = "Output debug information";


 if (G_parser(argc, argv))
  exit(1);

  {
 /* Change C0004 */

   strcpy(sDATANAME, input->answer);
   namelength = strlen(input->answer);

   strcpy(dataout,  input->answer);
   strcpy(&dataout[namelength - 3], "nps");
   fp1 = fopen(sDATANAME,"r");  /* Open up the input file */
   nps = fopen(dataout, "w");  /* Open up the output file */


   /* Peek into the data file for # of cells check...<Cheesy patch>  */
   if (fp1 != NULL)
    {
     fgets(line_buffer,99,fp1);
     fgets(line_buffer,99,fp1);
     fgets(line_buffer,99,fp1);
     fgets(line_buffer,99,fp1);
     fgets(line_buffer,99,fp1);
     sscanf(line_buffer," %f  %d  %d ",&peek_float,&peek_int,&peek_cols);
     line_buffer[0] = '\0';
     fclose(fp1);
    }
   else
    {
     fprintf (stderr,"Error: Data file [%s] not present...\n",sDATANAME);
     return(1);
    }
   /******************************************************************/


   fp1 = fopen(sDATANAME,"r");  /* Open up the input file */

   if ( ((dbfp = fopen("DEBUG.FLG","r")) != NULL) && (peek_cols < 9))
    {
     /*tflags = (FLAGS_TABLE*) calloc(1, sizeof(FLAGS_TABLE));*/
     init_db_file(&tflags,&bflags,&rflags);
     read_db_file(dbfp,&tflags,&bflags,&rflags);

     if (tflags.hydro_table)           /* Hydrology verif output table. */
      {
       strcpy(vt_1name,  input->answer);
       strcpy(&vt_1name[namelength - 3], "vt1");

       vfy1 = fopen(vt_1name,"w+");
       strcpy(htable.ht[0].name,sDATANAME);
       vsetup_1(vfy1,htable.ht[0].name);
       for (i = 1; i <= 9; ++i)
	t1setup_1(&htable.ht[i]);
      }

     if (tflags.sed_table)           /* Sediment verif output table. */
      {
       strcpy(vt_2name,  input->answer);
       strcpy(&vt_2name[namelength - 3], "vt2");
/*     for (i = 1; i <= 9; ++i)
	stable->st[i]->column_id = calloc(1,sizeof(int));
*/

       vfy2 = fopen(vt_2name,"w+");
       strcpy(stable.st[0].name,sDATANAME);
       vsetup_2(vfy2,stable.st[0].name);
       for (i = 1; i <= 9; ++i)
	{
	 t2setup_2(&stable.st[i]);
	}
      }
     if (tflags.chem_table)            /* Chemical verif output table.  */
      {
       strcpy(vt_3name,  input->answer);
       strcpy(&vt_3name[namelength - 3], "vt3");

       vfy3 = fopen(vt_3name,"w+");
       strcpy(ctable.ct[0].name,sDATANAME);
       vsetup_3(vfy3,ctable.ct[0].name);
       for (i = 1; i <= 9; ++i)
	t3setup_3(&ctable.ct[i]);
      }

     fclose(dbfp);
    }

   if ( gis->answer)             /* GIS file? */
    {
     doGIS = TRUE;
     strcpy(GISout, input->answer);
     strcpy(&GISout[namelength - 3], "gis");
     GISfp = fopen(GISout, "w");
    }

      strcpy(init_out, input->answer);
      strcpy(&init_out[namelength-3], "ini");

      if (source->answer)
	sourceinfo=TRUE;

      if (sediment->answer)
       sedimentinfo = TRUE;

      if (nutrient->answer)
       nutrientinfo = TRUE;

      if (debug->answer)
       debuginfo = TRUE;

      if (hydro->answer)
       hydroinfo = TRUE;

  if(hydroinfo)
   {
    strcpy(dataout,  input->answer);
    strcpy(&dataout[namelength-3], "imp");
    imp = fopen(dataout, "w");  /* Open up the output file */

    strcpy(dataout,  input->answer);
    strcpy(&dataout[namelength-3], "hyd");
    hyd = fopen(dataout, "w");  /* Open up the output file */

    fprintf(hyd,"Column  Increment	Top Flow	Bottom Flow	Duration\n");
    fprintf(hyd,"                         cfs               cfs		  secs   \n");

    fprintf(imp,"Column  Imp.     Number of   Imp. Peak Flow	Base Duration    Duration\n");
    fprintf(imp,"                 Increments        cfs              secs           secs\n");


   }


   initialptr = &initial_input; /* set the pointer to the structure */


   /*** READ IN WATERSHED INFORMATION FROM THE CELL ***/



   readinput(fp1,sDATANAME);

   if(error_log == TRUE)
   {
     errorfp = fopen("error.log","w");
     fprintf(errorfp,"No Errors Detected");
     rewind(errorfp);
   }


   /*** CALCULATE CELL INFORMATION ***/


   loop1();

   if(pest_info)
      fclose(pestfile);
   if(hydro_info)
     fclose(hydrofile);

   outlet_column = locate_source_cells( columns, &first_sinkhole );

   /*** ROUTE THROUGH CELL ***/

   routing_loop( outlet_column );

   if (pestfile2 != NULL)
     fclose(pestfile2);

   /*** OUTPUT RESULTS ***/

   output_nps(nps, GISfp, doGIS);

/*   puts("\nWe've finished the program!!!\n"); */

   fclose(nps);

   if (debuginfo)
     {
      fclose(hyd);
      fclose(imp);
     }

   if(error_log == TRUE)
     fclose(errorfp);

   if (doGIS)
      fclose(GISfp);
  }

 if (tflags.hydro_table && columns <= 9)
  {
   for (i = 1; i <= columns; ++i)
    {

#ifdef _DOS
      fprintf (stderr,"  %d  ... Creating Water Verification Table :     \r",i);
#endif

     htable.ht[i].column_id = i;

     /*************************************** Last minute conversions ***/

					/* Convert area to % total area */
     htable.ht[i].feedlot_area = htable.ht[i].feedlot_area /
					  htable.ht[i].total_cell_area;

					/* Convert area to % total area */
     htable.ht[i].impoundment_area = htable.ht[i].impoundment_area /
					  htable.ht[i].total_cell_area;

     htable.ht[i].pk_dis_outlet_iph = htable.ht[i].pk_dis_outlet_cfs /
				     (htable.ht[i].drainage_area * 1.008333);

     htable.ht[i].tot_ro_outlet_af = htable.ht[i].tot_ro_outlet_in *
				     htable.ht[i].drainage_area / 12;

     htable.ht[i].time_to_pk = htable.ht[i].time_to_pk  /  3600;

     htable.ht[i].time_to_base = htable.ht[i].time_to_base  /  3600;

     populate_vfy1(vfy1,&htable.ht[i]);

    }

   fclose(vfy1);
  }



 if (tflags.sed_table && columns <= 9)
  {

/*stable.st[1].num_partitions = 8;*/

   vfinish_2(vfy2,stable.st[1].num_partitions);

   for (i = 1; i <= columns; ++i)
    {

#ifdef _DOS
      fprintf (stderr,"  %d  ... Creating Sediment Verification Table :    \r",i);
#endif

     stable.st[i].column_id = i;

     /*************************************** Last minute conversions ***/

					/* Convert area to % total area */
/*     htable.ht[i].feedlot_area = htable.ht[i].feedlot_area /
					  htable.ht[i].total_cell_area;
*/


     populate_vfy2(vfy2,&stable.st[i]);

    }


   fclose(vfy2);
  }



 if (tflags.chem_table && columns <= 9)
  {
   for (i = 1; i <= columns; ++i)
    {

#ifdef _DOS
      fprintf (stderr,"  %d  ... Creating Chemical Verification Table :  \r",i);
#endif

     ctable.ct[i].column_id = i;

     /*************************************** Last minute conversions ***/

     populate_vfy3(vfy3,&ctable.ct[i]);
    }

   fclose(vfy3);
  }

 if (tflags.hydro_table && columns > 9)
   fclose(vfy1);

 if (tflags.sed_table && columns > 9)
   fclose(vfy2);

 if (tflags.chem_table && columns > 9)
   fclose(vfy3);


/* mc_endcheck ();*/

 return(0);
}









#ifdef _UNIX_K_AND_R

void memory_out(location, column_number)

   int location;
   int column_number;

#else
 void memory_out(int location, int column_number)

#endif

{
    switch (location)
	{
	case 0:  fprintf (stderr,"\nMemory error in initial calloc...column #%d\n",column_number);
		 break;
	case 1:  fprintf (stderr,"\nMemory error in mallocing COLLUMN_INFO for column %d\n",column_number);
		 break;
	case 2:  fprintf (stderr,"\nMemory error in mallocing CHANNEL_INFO for column %d\n",column_number);
		 break;
	case 3:  fprintf (stderr,"\nMemory error in mallocing MANAGEMENT_INFO for column %d\n",column_number);
		 break;
	case 4:  fprintf (stderr,"\nMemory error in mallocing SLOPE_INFO for column %d\n",column_number);
		 break;
	case 5:  fprintf (stderr,"\nMemory error in mallocing SOIL_INFO for column %d\n",column_number);
		 break;
	case 6:  fprintf (stderr,"\nMemory error in mallocing RUNOFF_INFO for column %d\n",column_number);
		 break;
	case 7:  fprintf (stderr,"\nMemory error in mallocing NONFEEDLOT_INFO for column %d\n",column_number);
		 break;
	case 8:  fprintf (stderr,"\nMemory error in mallocing more NONFEEDLOT_INFO for column %d\n",column_number);
		 break;
	case 9:  fprintf (stderr,"\nMemory error in mallocing FEEDLOT_INFO for column %d\n",column_number);
		 break;
	case 10: fprintf (stderr,"\nMemory error in mallocing more FEEDLOT_INFO for column %d\n",column_number);
		 break;
	case 11: fprintf (stderr,"\nMemory error in mallocing FEEDLOT_TOTALS for column %d\n",column_number);
		 break;
	case 12: fprintf (stderr,"\nMemory error in mallocing flow_path_upto in loop3\n");
		 break;
	case 13: fprintf (stderr,"\nMemory error in mallocing flow_path_out in loop3\n");
		 break;
	case 14: fprintf (stderr,"\nMemory error in mallocing sum_runoff_in in loop3\n");
		 break;
	case 15: fprintf (stderr,"\nMemory error in mallocing num_cells_drain_in in loop3\n");
		 break;
	default: fprintf (stderr,"\nGeneral out of memory error in location %d for cell %d.\n", location, column_number);
	}
    fprintf (stderr,"Program terminated\n");
    exit(1);
}
