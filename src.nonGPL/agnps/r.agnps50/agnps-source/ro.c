/*********** Documentation Start **************
 
$NAME$
NAME: ro.c
 
$PATHS$
    hydrology\runoff
$1$
SYNOPSIS:
 
  This procedure calculates the runoff volume on an area basis.  This
  procedure is called several times. It is called for each cell and
  is also called for feedlots.
 
 
HEADER FILES:
*/
 
#ifdef _DOS
 
 #include <math.h>
 #include <dos.h>
 #include "input.h"
 #include <stdio.h>
 #include <stdlib.h>
 #include <string.h>
 #include "binary.h"
 #include "debugflg.h"

#else

#include <math.h>
#include "input.h"
#include <stdio.h>
#include "binary.h"
#include "debugflg.h"

#endif
 
 
/*
EXTERNAL VARIABLES
*/
 
extern FILE *errorfp;
extern int error_log;
extern HYDRO_REC_PTR    hydro;
extern int hydro_info;

extern FLAGS_ROUTINE    rflags;

/*UNITS: INCHES runoff( NOD, INCHES ) */

#ifdef _UNIX_K_AND_R

 void curve_number_runoff(curve_number, storm_rainfall, column,
			location,runoff)

	 float curve_number;
	 float storm_rainfall;
	 int   column;
	 char  location[20];
	 float *runoff;

#else

 void curve_number_runoff( float curve_number,       /* dimensionless */
			  float storm_rainfall,     /* inches */
			  int   column,
			  char  location[20],
			  float *runoff)            /* inches */
 
#endif
 
 
{
/*
INPUT PARAMETERS:
  curve_number   (float) user input curve number  [dimensionless]
  storm_rainfall (float) user input storm rainfall for the storm [inches]
  column         (int)   column number of current cell (error output)
  location[20]   (string)location of error (error output)
 
      Note: curve number was converted to a float before it was sent
	    from loop1.

OUTPUT PARAMETERS:
  runoff   (float) volume of runoff from current area [inches]
 
LOCAL VARIABLES:
*/
 
double retention_factor = 0.0;   /* inches */
double equation_top    =0.0;
double equation_bottom = 0.0;
char   problem[30];
int    error_code = 0;

/*
 
RETURNS:
  error_code (integer)  error code for the procedure
 
      0 = No Errors
      1 = Curve Number    >  100
      2 = Curve Number    <= 0
      3 = Storm Rainfall  <= 0
      4 = Equation Bottom <= 0
      5 = Runoff         <= 0
 
NOTES:
  This equation is based on the curve number method calculations. See
  the AGNPS flow documents and the data dictionary, along with source
  documentation.  The source used is the TR55 technical release
  from the Soil Conservation Service, equations 2-3 and 2-4.  The equations
  come from the SCS runoff curve number method in NEH-4 (SCS 1985).
 
DATA STORES:
 
HISTORY:
 
	Date	Bug#	Prog	Desc
 
	9/25/92 none	MAK	finished coding
       10/06/92 none    MAK     added boundary checking
       10/06/92 none    MAK     tested proc., no errors detected
 
 
SEE ALSO:
  loop1 (called)
 
********** Documentation end **********/
 
 
 /**************** ERROR CHECK ROUTINE *****************/
 
 if(curve_number == 100)
   *runoff = storm_rainfall;

 else
  {
    error_code = 0;

  /* The maximum curve number is 100 */

    if(curve_number > 100.0)
	error_code = 1;

    if(curve_number <=0.0)
	error_code = 2;

    if(storm_rainfall <= 0.0)
	error_code = 3;


/*** 1.1.1  CALCULATE RETENTION VALUE ***/

 /* This calculates the amount of water that doesn't run off.
   Retention factor should be greater than or equal to zero */

    retention_factor = (1000.0/curve_number) - 10.0;

/*** 1.1  CALCULATE VOLUME OF RUNOFF ***/

  /* calculate numerator */

  equation_top   = storm_rainfall - 0.2 * retention_factor;



  /* calculate denominator */

  equation_bottom = storm_rainfall + 0.8 * retention_factor;




  if(equation_top <= 0.0) /* retention factor is greater than rainfall */
   *runoff = 0.0;
  else
   {
    if(equation_bottom <= 0.0)
      error_code = 4;
    else
      *runoff = (pow(equation_top,2.0))/equation_bottom;
   }


  if(*runoff < 0.0)
    error_code = 5;

  }

  if(hydro_info)
   {
    hydro->curve_number=curve_number;
    hydro->storm_rainfall=storm_rainfall;
    hydro->retention_factor=retention_factor;
    hydro->equation_top=equation_top;
    hydro->equation_bottom=equation_bottom;
    hydro->runoff=*runoff;
   }

/* Print out the error to an error file */

 if(error_log == TRUE)
  {
   if(error_code > 0)
    {
     fprintf(errorfp,"Error Occured in the runoff routine!!\n");
     fprintf(errorfp,"Error occured while processing cell..%d \n",column);
     fprintf(errorfp,"Routine was called from..%s \n",location);
     fprintf(errorfp,"========================================================= \n\n");
 
     if(error_code == 1)
      strcpy(problem,"Curve Number > 100");
     if(error_code == 2)
      strcpy(problem,"Curve Number <= 0");
     if(error_code == 3)
      strcpy(problem,"Storm Rainfall <= 0.0");
     if(error_code == 4)
      strcpy(problem,"Equation Denominator = 0.0");
     if(error_code == 5)
      strcpy(problem,"Runoff <= 0.0");
 
     fprintf(errorfp,"ERROR #%d    %s \n\n",error_code,problem);
     fprintf(errorfp,"========================================================= \n");
 
     fprintf(errorfp,"Data Dump........\n");
     fprintf(errorfp,"INPUT:  Curve Number     (dimensionless):     %f \n",curve_number);
     fprintf(errorfp,"        Storm Rainfall   (inches):            %f \n\n",storm_rainfall);
     fprintf(errorfp,"INTER:  Retention Factor: %f \n",retention_factor);
     fprintf(errorfp,"        Equation Top:     %f \n",equation_top);
     fprintf(errorfp,"        Equation Bottom:  %f \n\n",equation_bottom);
     fprintf(errorfp,"OUTPUT: Runoff:          (inch-acres)         %f \n",*runoff);
     fprintf (stderr,"Error encountered-Program stopped-Check error.log");
     sleep(10);
    exit(1);
    fclose(errorfp);
   }
  }

  if (rflags.curve_ro)
   {
    fprintf (stderr,"CURVE#_RUNOFF routine: column #%d \n",column);
    fprintf (stderr,"   Input:  %f...curve_number    %f...storm_rainfall\n",curve_number,storm_rainfall);
    fprintf (stderr,"           %s...location   \n",location);
    fprintf (stderr,"   Output: %f...retention_factor  %f...runoff(in)\n",retention_factor,*runoff);
   }


/* Finished printing out error message to error file */
 
/*
  if ((hydro_info) || (bflags.flow))
    {
     fprintf (stderr,"****\n");
     fprintf (stderr,"1.8   SCS curve number routine output\n ");
     fprintf (stderr,"INPUT:  Storm Rainfall = %f in.  Curve Number = %f\n",storm_rainfall,
	     curve_number);
     fprintf (stderr,"OUTPUT: Retention Factor = %f in  Runoff = %f in. Error Code = %d\n\n",
	     retention_factor,*runoff,error_code);
    }
*/

}

/* $END$ */
