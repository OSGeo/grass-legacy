/*=============================*/
/*           NETS              */
/*                             */
/* a product of the AI Section */
/* NASA, Johnson Space Center  */
/*                             */
/* principal author:           */
/*       Paul Baffes           */
/*                             */
/* contributing authors:       */
/*      Bryan Dulock           */
/*      Chris Ortiz            */
/*=============================*/


/*
----------------------------------------------------------------------
  Code For Dribbling output to User (Prefix = D_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) externed functions
  (3) global variables
  (4) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include "common.h"
#include "weights.h"
#include "layer.h"
#include "net.h"
#include "netio.h"


/*
----------------------------------------------------------------------
  EXTERNED FUNCTIONS AND GLOBALS
----------------------------------------------------------------------
  Below are the functions defined in other files which are used by the
  code here. They are organized by section.
----------------------------------------------------------------------
*/
extern  void    IO_my_get_string();
extern  float   C_Sint_to_float();
extern  void    N_save_wts();
extern  void    N_query_net();
extern  void    IO_print();
extern  void    IO_insert_format();

extern  char    IO_wkstr[MAX_LINE_SIZE];
extern  char    IO_str[MAX_LINE_SIZE];


/*
----------------------------------------------------------------------
  GLOBALS
----------------------------------------------------------------------
  The list of globals here are for the dribble facility of NETS. The idea 
  is to be able to save error values and weights and test case results 
  DURING the training to see how things are going. Five files are needed, 
  and eight flags. Four of the flags are referenced in "teach.c" to  
  determine if the D_dribble function should be called.
----------------------------------------------------------------------
*/
char           maxerr_file[MAX_WORD_SIZE]; /* These three strings used  */
char           rmserr_file[MAX_WORD_SIZE]; /* for dribbling OUTPUT as   */
char           weights_file[MAX_WORD_SIZE];/* the net runs.             */
char           tests_file[MAX_WORD_SIZE];  
char           tests_in[MAX_WORD_SIZE];    /* INPUT file for test runs  */

int            SAVE_MAXERR;    /* These eight used as flags for the     */
int            SAVE_RMSERR;    /* dribble option                        */
int            SAVE_WEIGHTS;   
int            SAVE_TESTS;
int            MOD_MAXERR;     /* the MOD globals dictate how often the */
int            MOD_RMSERR;     /* the files will be updated (in cycles) */
int            MOD_WEIGHTS;    
int            MOD_TESTS;


/*
======================================================================
  ROUTINES IN BUILDNET.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "D_" indicating that it is defined in the 
  "buildnet.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------       
    void                        D_initialize                          
    void                        D_dribble
    int                         D_set_dribble
    void                        D_dribble_status
    void                        D_maxerr_dribble
    void                        D_rmserr_dribble
    void                        D_weights_dribble
    void                        D_tests_dribble
======================================================================
*/


void  D_initialize()
/*
----------------------------------------------------------------------
  The beginning values of the dribble filenames are set
  to the empty string by this routine.                 
----------------------------------------------------------------------
*/
BEGIN
   /*--------------------------------*/
   /* set dribble parameters to NULL */
   /*--------------------------------*/
   maxerr_file[0]  = ENDSTRING;  
   rmserr_file[0]  = ENDSTRING;
   weights_file[0] = ENDSTRING;
   tests_file[0]   = ENDSTRING;
   tests_in[0]     = ENDSTRING;

   /*------------------------------------------*/
   /* set the dribble parameters to OFF, and 1 */
   /*------------------------------------------*/
   SAVE_MAXERR  = FALSEE;
   SAVE_RMSERR  = FALSEE;
   SAVE_WEIGHTS = FALSEE;
   SAVE_TESTS   = FALSEE;
   MOD_MAXERR   = 1;
   MOD_RMSERR   = 1;
   MOD_WEIGHTS  = 1;
   MOD_TESTS    = 1;

END /* D_initialize */


void  D_dribble_status()
/*
----------------------------------------------------------------------
  Prints out a prompt indicating the status of the dribble parameters.
  For any parameters which are ON, the corresponding MOD and filename
  values are also printed. Then, it asks whether or not to continue
  prompting the user for info (namely, it asks whether or not the
  user wants to change the values of the parameters).
----------------------------------------------------------------------
*/
BEGIN
   void  D_maxerr_dribble(), D_weights_dribble(), D_tests_dribble(),
         D_rmserr_dribble();
   char  temp_str[MAX_WORD_SIZE];
   
   /*----------------------------*/
   /* first print header of info */
   /*----------------------------*/
   sprintf(IO_str, "\n   Current Dribble Parameters Are:");
   IO_print(0);
   sprintf(IO_str, "\n     Parameter  State  Cycles  Output File");
   IO_print(0);
   sprintf(IO_str, "\n                -----  ------  -----------");
   IO_print(0);
   
   /*---------------------------------------*/
   /* then print status of three parameters */
   /*---------------------------------------*/
   if (SAVE_MAXERR == TRUEE)
      sprintf(IO_str, "\n     Max Error: ON %8d     %s", MOD_MAXERR, maxerr_file);
   else sprintf(IO_str, "\n     Max Error: OFF");
   IO_print(0);
   if (SAVE_RMSERR == TRUEE)
      sprintf(IO_str, "\n     RMS Error: ON %8d     %s", MOD_RMSERR, rmserr_file);
   else sprintf(IO_str, "\n     RMS Error: OFF");
   IO_print(0);
   if (SAVE_WEIGHTS == TRUEE)
      sprintf(IO_str, "\n     Weights:   ON %8d     %s", MOD_WEIGHTS, weights_file);
   else sprintf(IO_str, "\n     Weights:   OFF");
   IO_print(0);
   if (SAVE_TESTS == TRUEE) BEGIN
      sprintf(IO_str, "\n     Tests:     ON %8d     %s", MOD_TESTS, tests_file);
      IO_print(0);
      sprintf(IO_str, " (test file: %s)", tests_in);
   ENDIF
   else sprintf(IO_str, "\n     Tests:     OFF");
   IO_print(0);
         
   /*--------------------------------------------------*/
   /* finally, prompt user for update of dribble parms */
   /*--------------------------------------------------*/
   sprintf(IO_str, "\n\n   Change dribble parameters(y/n default=n): ");
   IO_print(0);
   IO_my_get_string(temp_str);
   if ((temp_str[0] == 'y') || (temp_str[0] == 'Y')) BEGIN
      D_maxerr_dribble();
      D_rmserr_dribble();
      D_weights_dribble();
      D_tests_dribble();
   ENDIF

END /* D_dribble_status */

 
void  D_maxerr_dribble()
/*
----------------------------------------------------------------------
   This routine prompts the user for info concerning whether or not to
   turn ON or OFF the SAVE_WEIGHTS flag (of NETMAIN.C) and, consequently,
   for the filename to use as a repository for the weights as the net
   is learning.
----------------------------------------------------------------------
*/
BEGIN
   int          D_set_dribble();
   FILE         *fp;
   static char  name[11] = "Max Errors";

   /*-------------------------*/
   /* get new parameter value */
   /*-------------------------*/
   SAVE_MAXERR = D_set_dribble(SAVE_MAXERR, &MOD_MAXERR,
                                name, maxerr_file);

   /*--------------------------------------*/
   /* if set, the get filename for errors  */
   /* then open file and write out header  */
   /*--------------------------------------*/
   if (SAVE_MAXERR == TRUEE) BEGIN
      if ((fp = fopen(maxerr_file, "wt")) == NULL) BEGIN
         sprintf(IO_str, "\n*** error opening file %s ***", maxerr_file);
         IO_print(0);
         SAVE_MAXERR = FALSEE;
         return;
      ENDIF
      fprintf(fp, "-- Max Errors Dribble File --\n");
      fclose(fp);
   ENDIF

END /* D_maxerr_dribble */


void  D_rmserr_dribble()
/*
----------------------------------------------------------------------
   This routine prompts the user for info concerning whether or not to
   turn ON or OFF the SAVE_WEIGHTS flag (of NETMAIN.C) and, consequently,
   for the filename to use as a repository for the weights as the net
   is learning.
----------------------------------------------------------------------
*/
BEGIN
   int          D_set_dribble();
   FILE         *fp;
   static char  name[11] = "RMS Errors";

   /*-------------------------*/
   /* get new parameter value */
   /*-------------------------*/
   SAVE_RMSERR = D_set_dribble(SAVE_RMSERR, &MOD_RMSERR,
                                name, rmserr_file);

   /*--------------------------------------*/
   /* if set, the get filename for errors  */
   /* then open file and write out header  */
   /*--------------------------------------*/
   if (SAVE_RMSERR == TRUEE) BEGIN
      if ((fp = fopen(rmserr_file, "wt")) == NULL) BEGIN
         sprintf(IO_str, "\n*** error opening file %s ***", rmserr_file);
         IO_print(0);
         SAVE_RMSERR = FALSEE;
         return;
      ENDIF
      fprintf(fp, "-- RMS Errors Dribble File --\n");
      fclose(fp);
   ENDIF

END /* D_rmserr_dribble */


void  D_weights_dribble()
/*
----------------------------------------------------------------------
   This routine prompts the user for info concerning whether or not to
   turn ON or OFF the SAVE_WEIGHTS flag (of NETMAIN.C) and, consequently,
   for the filename to use as a repository for the weights as the net
   is learning.
----------------------------------------------------------------------
*/
BEGIN
   int          D_set_dribble();
   static char  name[8] = "Weights";

   /*----------------------------*/
   /* set parameter to new value */
   /*----------------------------*/
    SAVE_WEIGHTS = D_set_dribble(SAVE_WEIGHTS, &MOD_WEIGHTS, 
                                  name, weights_file);
       
END /* D_weights_dribble */


void  D_tests_dribble()
/*
----------------------------------------------------------------------
   This routine prompts the user for info concerning whether or not to
   turn ON or OFF the SAVE_TESTS flag (of NETMAIN.C) and, consequently,
   for the filename to use as a repository for the test cases as the net
   is learning.
----------------------------------------------------------------------
*/
BEGIN
   int          D_set_dribble();
   FILE         *fp;
   char         temp_str[MAX_WORD_SIZE];
   static char  name[7] = "Tests";
   

   /*-------------------------*/
   /* get new parameter value */
   /*-------------------------*/
   SAVE_TESTS = D_set_dribble(SAVE_TESTS, &MOD_TESTS, 
                               name, tests_file);

   /*--------------------------------------*/
   /* if set, the get filename for errors  */
   /* then open file and write out header  */
   /*--------------------------------------*/
   if (SAVE_TESTS == TRUEE) BEGIN
      sprintf(IO_str, "\n      Enter INPUT filename for testing network");
      IO_print(0);
      sprintf(IO_str, "(default=%s): ", tests_in);
      IO_print(0);
      IO_my_get_string(temp_str);

      /*-------------------------------------*/
      /* set filename if default is not used */
      /*-------------------------------------*/
      if (temp_str[0] != ENDSTRING) strcpy(tests_in, temp_str);
         
      /*----------------------------------------*/
      /* after assigning tests_in, check to see */
      /* if it's still empty (default tests_in  */
      /* is empty and would not be set above)   */
      /*----------------------------------------*/
      if (tests_in[0] == ENDSTRING) BEGIN
         sprintf(IO_str, "\n      *** can't open file  ***");
         IO_print(0);
         SAVE_TESTS = FALSEE;
         return;
      ENDIF

      fp = fopen(tests_file, "wt");
      fprintf(fp, "-- Tests Dribble File --\n");
      fclose(fp);
   ENDIF
   
END /* D_tests_dribble */


int  D_set_dribble(last_val, mod_val, parm_name, filename)
int   last_val;
int   *mod_val;
char  *parm_name, *filename;
/*
----------------------------------------------------------------------
   Takes care of prompting the user for any of the dribble parameters
   which can be set to ON or OFF. A return value is provided which 
   indicates what the new value of the parameter should be.
   
   Note also that the incoming "filename" parameter may be altered by
   this routine if the user answers "ON" to the parameter and answers
   a new string value for the dribble filename.
   
   Finally, the mod_val parameter is passed in as a pointer to one of
   the three "MOD_..." globals defined in NETMAIN.C (eg MOD_ERRORS).
   If a filename is successfully entered by the user, then the last 
   part of this routine updates the corresponding mod number, again
   according to user response.
----------------------------------------------------------------------
*/
BEGIN
   int   rval, old_mod;
   char  temp_str[MAX_WORD_SIZE];

   /*---------------------------------------*/
   /* print prompt with old parameter value */
   /*---------------------------------------*/
   sprintf(IO_str, "\n   Set %s parameter", parm_name);
   IO_print(0);
   sprintf(IO_str, "(default=%s): ", (last_val == TRUEE ? "ON" : "OFF"));
   IO_print(0);
   IO_my_get_string(temp_str);
 
   /*-------------------------------------*/
   /* set return value by user's response */
   /*-------------------------------------*/
   if (temp_str[0] == ENDSTRING)
      rval = last_val;
   else if ( (strcmp(temp_str, "on") == 0) || (strcmp(temp_str, "ON") == 0) )
      rval = TRUEE;
   else
      rval = FALSEE;
      
   /*------------------------------------*/
   /* if set, the get filename for the   */
   /* dribble output with old as default */
   /*------------------------------------*/
   if (rval == TRUEE) BEGIN
      sprintf(IO_str, "\n      Enter OUTPUT filename for saving %s", parm_name);
      IO_print(0);
      sprintf(IO_str, "(default=%s): ", filename);
      IO_print(0);
      IO_my_get_string(temp_str);

      /*-------------------------------------*/
      /* set filename if default is not used */
      /*-------------------------------------*/
      if (temp_str[0] != ENDSTRING) strcpy(filename, temp_str);
         
      /*----------------------------------------*/
      /* after assigning filename, check to see */
      /* if it's still empty (default filename  */
      /* is empty and would not be set above)   */
      /*----------------------------------------*/
      if (filename[0] == ENDSTRING) BEGIN
         sprintf(IO_str, "\n      *** can't open file  ***");
         IO_print(0);
         rval = FALSEE;
      ENDIF
   ENDIF
   
   /*------------------------------------------*/
   /* if all still go for change, then get new */
   /* mod value for saving to the given file   */
   /*------------------------------------------*/
   old_mod = *mod_val;
   if (rval == TRUEE) BEGIN
      sprintf(IO_str, "\n      Enter cycle increment for saving %s", parm_name);
      IO_print(0);
      sprintf(IO_str, "(default=%d): ", *mod_val);
      IO_print(0);
      if (gets(temp_str) != NULL)
         sscanf(temp_str, "%d", mod_val);
      if (*mod_val < 1) BEGIN
         sprintf(IO_str, "\n      *** cycle number cannot be < 1; default used ***\n");
         IO_print(0);
         *mod_val = old_mod;
      ENDIF
   ENDIF
   
   return(rval);

END /* D_set_dribble */


void  D_dribble(ptr_net, num_cycles, cur_error, grad_err)
Net    *ptr_net;
int    num_cycles;
Sint   cur_error;
float  grad_err;
/*
----------------------------------------------------------------------
  This routine takes the current cycle number and error value as input
  from the T_teach_net routine and prints out information, if any, which
  should appear in the dribble files. This is done by first noting if 
  any of the MOD numbers for the dribble 
----------------------------------------------------------------------
*/
BEGIN
   FILE  *fp;
   void  D_next_wts_file();


   /*---------------------------------------------*/
   /* if SAVE_MAXERR is ON and we're at the right */
   /* cycle,  output "(cycle number, max error)"  */
   /*---------------------------------------------*/
   if ( (SAVE_MAXERR == TRUEE)
        && ((num_cycles % MOD_MAXERR) == 0) ) BEGIN
      fp = fopen(maxerr_file, "at");
      sprintf(IO_wkstr, "\n(%d %%.f)", num_cycles);
      IO_insert_format(IO_wkstr);
      fprintf(fp, IO_wkstr, C_Sint_to_float(cur_error) );
      fclose(fp);
   ENDIF
   
   /*---------------------------------------------*/
   /* if SAVE_RMSERR is ON and we're at the right */
   /* cycle,  output "(cycle number, RMS error)"  */
   /*---------------------------------------------*/
   if ( (SAVE_RMSERR == TRUEE)
        && ((num_cycles % MOD_RMSERR) == 0) ) BEGIN
      fp = fopen(rmserr_file, "at");
      sprintf(IO_wkstr, "\n(%d %%.f)", num_cycles);
      IO_insert_format(IO_wkstr);
      fprintf(fp, IO_wkstr, grad_err);
      fclose(fp);
   ENDIF
   
   /*----------------------------------------------*/
   /* if SAVE_WEIGHTS is ON and we're at the right */
   /* cycle, then printout a "please wait..." mesg */
   /* and save the weights using N_save_wts        */
   /*----------------------------------------------*/
   if ( (SAVE_WEIGHTS == TRUEE)
        && ((num_cycles % MOD_WEIGHTS) == 0) ) BEGIN
      /* D_next_wts_file(weights_file); */
      sprintf(IO_str, "\n*** currently dribbling weights to %s...", weights_file);
      IO_print(0);
      N_save_wts(ptr_net, weights_file, FAST_FORMAT);
      sprintf(IO_str, "done.");
      IO_print(0);
   ENDIF
   
   /*--------------------------------------------*/
   /* If SAVE_TESTS is ON and we're at the right */
   /* cycle, then append a test case to the end  */
   /* of the tests_file, along with the cycle    */
   /*--------------------------------------------*/
   if ( (SAVE_TESTS == TRUEE)
        && ((num_cycles % MOD_TESTS) == 0) ) BEGIN
      /*----------------------------------------*/
      /* open file, output cycle number and "(" */
      /*----------------------------------------*/
      fp = fopen(tests_file, "at");
      fprintf(fp, "\n*** CYCLE: %d ***", num_cycles);

      /*----------------------------------------*/
      /* output results from propagation; close */
      /*----------------------------------------*/
      N_query_net(ptr_net, tests_in, fp, -1);
      fclose(fp);
   ENDIF

END /* D_dribble */


void  D_next_wts_file(filename)
char *filename;
/*
----------------------------------------------------------------------
  Right now, this routine is functional, but not part of the normal
  NETS program (ie, it isn't documented anywhere). It is only called
  by the D_dribble routine and its function is to save the weights
  to DIFFERENT files during learning rather than to the same file over
  and over again. I had thought this would be useful to be able to see
  weights from different stages of learning, but so far this feature 
  has done little more than create lots of useless files.
  Note that the "tail" variable is static. Thus this code currently
  only works for the first learning cycle, ie, if you create a new
  network after saving some weights, tail will pick up where it left
  off.
----------------------------------------------------------------------
*/
BEGIN
   static  int tail = 1;
   int     i;
   char    temp[MAX_WORD_SIZE];
   
   if (filename[0] == ENDSTRING) BEGIN
      tail = 1;
      return;
   ENDIF
   
   for (i = 0; i < MAX_WORD_SIZE; i++) BEGIN
      if (filename[i] == '.') break;
      temp[i] = filename[i];
   ENDFOR
   temp[i] = ENDSTRING;
   
   if (tail < 10)
      sprintf(filename, "%s.00%d", temp, tail);
   else if (tail < 100)
      sprintf(filename, "%s.0%d", temp, tail);
   else 
      sprintf(filename, "%s.%d", temp, tail);
      
   tail++;

END /* D_nextawts_file */

