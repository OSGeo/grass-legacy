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
  Code For Standard IO prompting routines (Prefix = IO_)
----------------------------------------------------------------------
  This code is divided into 3 major sections:

  (1) include files
  (2) global variables
  (3) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include "common.h"
#include "netio.h"
#define YES 1
#define NO 0


/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
  The first global value below is a temporary work string needed by
  almost all of the "my_get..." routines. This string is read in by
  gets and then scanned for whatever the particular routines is looking
  for. Following this are three global variables which contain the default
  file names for the net's configuration, io pairs, and final weights
  are declared here. These are NOT STATIC, so that they may be referenced
  as EXTERN variables by other files.
  7-3-89 I added the "buf_str" buffer for doing output in a more general
  way (see IO_print). The idea is to provide a string which all files
  can use for doing output.
----------------------------------------------------------------------
*/
static char    in_string[MAX_LINE_SIZE];  /* used for 'my' IO routines   */
                                          /* and IO_get_float_from_file  */
char           IO_str[MAX_LINE_SIZE];     /* two strings for output      */
char           IO_wkstr[MAX_LINE_SIZE];
char           net_config[MAX_WORD_SIZE]; /* These are also referenced   */
char           net_iop[MAX_WORD_SIZE];    /* outside, in netmain.c and   */
char           net_fwt[MAX_WORD_SIZE];    /* net.c (see doc netmain.c)   */
char           net_pwt[MAX_WORD_SIZE];    /* also (see IO_set_filenames) */
static         int  more_counter;         /* # lines IO_more has printed */


/*
======================================================================
  ROUTINES IN NETIO.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "IO_" indicating that it is defined in the 
  "netio.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    void                        IO_print                         
    void                        IO_insert_format                        
    void                        IO_update_str
    int                         IO_reset_more
    int                         IO_more                         
    float                       IO_my_get_float                         
    float                       IO_get_default_float                         
    int                         IO_my_get_int
    int                         IO_get_default_int
    int                         IO_get_num_cycles
    void                        IO_my_get_string (returns string via args)
    void                        IO_set_filenames
    long                        IO_find_file_size
    void                        IO_get_io_name
    void                        IO_get_wts_name
======================================================================
*/


void  IO_print(target)
int    target;
/*
----------------------------------------------------------------------
  This routine is the main routing routine for all output from the 
  program. In anticipation of a variety of interfaces, I have included
  a "target" variable which indicates the location to which the string
  should be printed (eg, a window).
  Note that this window defaults to printing the contents of the global
  variable "IO_str" which is visible to all files expressly for the 
  purpose of doing output.
----------------------------------------------------------------------
*/
BEGIN
   printf("%s", IO_str);
   
END /* IO_print */


void  IO_insert_format(str)
char  str[];
/*
----------------------------------------------------------------------
  After adding the option to compile with either floating point or
  Sints (release 2.0), I realized that I would have different precision
  for my floating point numbers depending on the format chosen. Sint
  format yields only 3 decimal points of precision whereas floating
  point can yield 6 (and sometimes more). Thus, all my %7.3f floating
  point format statements for printf had to be changed! 
  To effect the change, I decided to replace all floating point formats
  with the string "%.f" which looks like a blank format. Then, this
  routine is called to insert the proper numbers before and after the
  period. That way, I can format the string generically, without 
  worrying about floats or Sints.
----------------------------------------------------------------------
*/
BEGIN
   int   i,j,k;
   void  IO_update_str();
   
   i = 0;
   /*---------------------------------------*/
   /* look for all occurrences of "%.f" and */
   /* call update to insert the numbers     */
   /*---------------------------------------*/
   while (str[i] != '\0') BEGIN
      if ((str[i] == '%') && (str[i+1] == '.') && (str[i+2] == 'f'))
         IO_update_str(str, i);
      i++;
   ENDWHILE
   
END /* IO_insert_format */


void  IO_update_str(str, loc)
char  str[];
int   loc;
/*
----------------------------------------------------------------------
  Once a string has been found with a "%.f" substring, this routine is
  called to insert numbers before and after the period. The numbers 
  inserted depend upon whether Sints or floats are being used. Sints
  yield only 3 decimal points of precision, thus the formatting is 
  "%7.3f" meaning a field 7 wide, with three places after the decimal.
  (remember the Sint can be as large as 31 and can be positive or 
  negative). Floats can yield up to 6 points of precision, thus their
  format is "%10.6f" (ie, a field 3 wider and 3 more points of precision).
----------------------------------------------------------------------
*/
BEGIN
   static char  save[80];
   int   i, j;
   
   /*------------------------------------------------*/
   /* first save off everything after the "f" in str */
   /*------------------------------------------------*/
   i = loc+3;
   j = 0;
   while (str[i] != '\0')
      save[j++] = str[i++];
   save[j] = '\0';
   
   /*----------------------------------------*/
   /* now insert the proper format note that */
   /* "j" is also set for copying info back  */
   /* from the save string.                  */
   /*----------------------------------------*/
#if  USE_SCALED_INTS
   str[loc+1] = '7';
   str[loc+2] = '.';
   str[loc+3] = '3';
   str[loc+4] = 'f';
   j = loc+5;
#else
   str[loc+1] = '1';
   str[loc+2] = '0';
   str[loc+3] = '.';
   str[loc+4] = '6';
   str[loc+5] = 'f';
   j = loc+6;
#endif

   /*-------------------------------------------*/
   /* copy everything back from the save string */
   /*-------------------------------------------*/
   i = 0;
   while (save[i] != '\0')
      str[j++] = save[i++];
   str[j] = '\0';
   
END /* IO_update_str */


void  IO_reset_more()
/*
----------------------------------------------------------------------
  Resets the number of lines that more has printed back to zero. This
  procedure should be called before using procedures which call IO_more.
----------------------------------------------------------------------
*/
BEGIN
   more_counter = 0;
   
END /* IO_reset_more */


int  IO_more(target)
int  target;
/*
----------------------------------------------------------------------
 This procedure uses the common interface IO_print. The procedure will
 wait and ask the user to enter Y/N after MAX_DISPLAY lines have been
 displayed.
----------------------------------------------------------------------
*/
BEGIN
   int i,status;
   char response[10];

   status = OKAY;
   IO_print (target);
   for ( i=0; i<= strlen ( IO_str ); i++ )
      if ( IO_str[i] == '\n' ) more_counter ++;
   
   if (more_counter >= MAX_DISPLAY) BEGIN
      more_counter = 0;

      sprintf(IO_str, "-------------- Continue ? (Y/N) --------------");
      IO_print(target);
      
      fgets(response,10, stdin);
      if ( response[0] == 'N' || response[0] == 'n' ) status = ERROR;
   ENDIF
   
   return(status);
   
END /* IO_more */


float  IO_my_get_float()
/*
----------------------------------------------------------------------
 Just a little routine, used often, for getting a floating number     
  input from the screen.  It loops until it gets the float.           
----------------------------------------------------------------------
*/
BEGIN
   float  num;

   while (TRUEE) BEGIN
      if ( (gets(in_string) != NULL)
           && (sscanf(in_string, "%f", &num) == 1) )
         break;
      else BEGIN
         sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
         IO_print(0);
      ENDELSE
   ENDWHILE
   return(num);

END /* IO_my_get_float */


float  IO_get_default_float(default_num)
float  default_num;
/*
----------------------------------------------------------------------
 This float-getting routine takes a default value which it returns if
  the user does not type an floating point value.
----------------------------------------------------------------------
*/
BEGIN
   float  num;

   while (TRUEE) BEGIN
       gets(in_string);

	in_string[0] = ENDSTRING;

      if (in_string[0] == ENDSTRING)
         return(default_num);
      if (sscanf(in_string, "%f", &num) == 1)
         return(num);
      else BEGIN
         sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
         IO_print(0);
      ENDELSE
   ENDWHILE

END /* IO_get_default_float */


int  IO_my_get_int()
/*
----------------------------------------------------------------------
 Just a little routine, used often, for getting an integer input from 
  the screen.  It loops until it gets the int.                        
----------------------------------------------------------------------
*/
BEGIN
   int  num;

   while (TRUEE) BEGIN
      if ( (gets(in_string) != NULL)
           && (sscanf(in_string, "%d", &num) == 1) )
         break;
      else BEGIN
         sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
         IO_print(0);
      ENDELSE
   ENDWHILE
   return(num);

END /* IO_my_get_int */


int  IO_get_default_int(default_int)
int  default_int;
/*
----------------------------------------------------------------------
 This integer-getting routine takes a default value which it returns if
  the user does not type an integer value.
----------------------------------------------------------------------
*/
BEGIN
   int  num;

   while (TRUEE) BEGIN
/*      gets(in_string); */
	in_string[0] = ENDSTRING;
      if (in_string[0] == ENDSTRING)
         return(default_int);
      if (sscanf(in_string, "%d", &num) == 1)
         return(num);
      else BEGIN
         sprintf(IO_str, "\n   sorry, I don't understand.  Try again: ");
         IO_print(0);
      ENDELSE
   ENDWHILE

END /* IO_get_default_int */


int  IO_get_num_cycles()
/*
----------------------------------------------------------------------
  This routine is used only during the "t" option of the main loop
  (see netmain.c) and reads the desired number of cycles.
----------------------------------------------------------------------
*/
BEGIN
   int  num;

   if (gets(in_string) != NULL)
      if (sscanf(in_string, "%d", &num) == 1)
         return(num);
   else
      return(MAX_CYCLES);

END /* IO_get_num_cycles */


void  IO_my_get_string(the_string)
char  *the_string;
/*
----------------------------------------------------------------------
 Like the two routines above, this guy serves to get input from the   
  terminal, in this case a string.  This routine is somewhat different
  though, in that it requires that the place holder for the string be 
  PASSED IN, rather than creating a result locally which is returned. 
----------------------------------------------------------------------
*/
BEGIN
   while (gets(the_string) == NULL) BEGIN
      sprintf(IO_str, "\n     sorry, I don't understand.  Try again: ");
      IO_print(0);
   ENDWHILE

END /* IO_my_get_string */


void  IO_set_filenames()
/*
----------------------------------------------------------------------
 This routine prompts the user for the name of the file containing    
  the configuration specifics for the net to be created.  From that   
  name, several default file names are computed and stored into       
  gloabal strings.  These defaults then become options for filenames  
  which will hold input and output information.                       
 Assuming an input of the form "name.ext" the following defaults are  
  generated:                                                          
                                                                      
     filename                      function                           
     --------                      --------                           
     name.iop                      holds the input/output pairs to be 
                                     in training the net              
     name.wts                      holds the weights for the net; ie, 
                                     after training all the resultant 
                                     weights would be stored here     
----------------------------------------------------------------------
 I made a change to the naming conventions here (8-17-89) due to the
  addition of PORTABLE weights files. That is, since a user may now 
  store weights in two formats, I wanted the naming conventions to 
  reflect that fact. I settled on the following two:
  
     name.fwt                      holds weights for a network saved
                                     in FAST_FORMAT (see N_save_wts).
     name.pwt                      holds the weights for a newtork
                                     saved in PORTABLE_FORMAT.
                                     
  Note that these two filenames REPLACE the "name.wts" file.
----------------------------------------------------------------------
*/
BEGIN
   void  IO_my_get_string();
   int   i;
   extern int DELETE, RANDOM;

   strcpy(net_config,"site.net");  /* Name of netfile created in nntool.c */
 
   if(DELETE)
   	strcpy(net_iop, "d_train");
   else {
	if(RANDOM)
		strcpy(net_iop,"train");
	else
		strcpy(net_iop,"o_train");
   }
   strcpy(net_fwt, net_config);
   strcpy(net_pwt, net_config);
   
   for (i = 0; i < MAX_WORD_SIZE; i++)
      if (net_config[i] == '.') break;
   
   if (i > (MAX_WORD_SIZE - 5)) BEGIN       /* If filename+ext too long */
      net_iop[MAX_WORD_SIZE - 5] = '.';     /* ext are 3 chars, so a '.'*/
      net_fwt[MAX_WORD_SIZE - 5] = '.';     /* must occur before the    */
      net_pwt[MAX_WORD_SIZE - 5] = '.';     /* last 3 chars at least!   */
      i = MAX_WORD_SIZE - 4;                /* So (3 + '.' + '\0') = 5  */
   ENDIF                               

   else                                     /* if no problem, just incr */
      i++;                                  /* i past the period        */

/*   net_iop[i]   = 'i';
   net_iop[i+1] = 'o';
   net_iop[i+2] = 'p';
   net_iop[i+3] = ENDSTRING; */
   net_fwt[i]   = 'f';
   net_fwt[i+1] = 'w';
   net_fwt[i+2] = 't';
   net_fwt[i+3] = ENDSTRING;
   net_pwt[i]   = 'p';
   net_pwt[i+1] = 'w';
   net_pwt[i+2] = 't';
   net_pwt[i+3] = ENDSTRING;

END /* IO_set_filenames */


long  IO_find_file_size(filename)
char  *filename;
/*
----------------------------------------------------------------------
 All this routine does is open up a file and find its length in bytes.
 If the file can't be opened, then a length of 0 is returned.
----------------------------------------------------------------------
*/
BEGIN
   FILE  *fp;
   long  length;
   
   fp = NULL;
   
   /*----------------------------------------*/
   /* first, open the file "filename" and    */
   /* if there is an error quit, returning 0 */
   /*----------------------------------------*/
   if ( (fp = fopen(filename, "rb")) == NULL )
      return(0L);

   /*--------------------------------------------*/
   /* otherwise, seek the end of the file. Note  */
   /* that the zero here is an offset and the 2  */
   /* indicates that end-of-file is sought (see  */
   /* "fseek" documentation. Thus we are looking */
   /* for the position 0 bytes from the end of   */
   /* the file.                                  */   
   /*--------------------------------------------*/
   fseek(fp, 0L, 2);
   
   /*--------------------------------------------*/
   /* use ftell to indicate the current position */
   /* of the file pointer, in bytes, from the    */
   /* beginning of the file.                     */
   /*--------------------------------------------*/
   length = ftell(fp);
   
   fclose(fp);
   return(length);
   
END /* IO_find_file_size */


void  IO_get_io_name()
/*
----------------------------------------------------------------------
 This routine prompts the user for the name of the file containing    
  list of io pairs to be used in training the net.  A default name is 
  printed out using the first part of the name given the last time    
  the "set_filenames" routine above was called.  The user may either  
  default to the name printed out (currently in net_iop) or he/she    
  may enter a new name (which then becomes the default).              
----------------------------------------------------------------------
*/
BEGIN
   void  IO_my_get_string();
   char  temp_str[MAX_WORD_SIZE];
   extern int DELETE, RANDOM;

/*   sprintf(IO_str, "\n   Enter name of file containing I/O pairs ");
   IO_print(0);
   sprintf(IO_str, "(default=%s): ", net_iop);
   IO_print(0);
   IO_my_get_string(temp_str); */

   if(DELETE == YES)
	strcpy(net_iop,"d_train");
   else {
   	if(RANDOM == YES)
   	  	strcpy(net_iop,"train");
   	else
     		strcpy(net_iop,"o_train");
   }

END /* IO_get_io_name */

void  IO_get_wts_name(format)
int  format;
/*
----------------------------------------------------------------------
 This routine prompts the user for the name of the file containing    
  list of io pairs to be used in training the net.  A default name is 
  printed out using the first part of the name given the last time    
  the "set_filenames" routine above was called.  The user may either  
  default to the name printed out (currently in net_iop) or he/she    
  may enter a new name (which then becomes the default).              
----------------------------------------------------------------------
 (8-17-89) I added a parameter to this routine so that the correct 
----------------------------------------------------------------------
*/
BEGIN
   void  IO_my_get_string();
   char  temp_str[MAX_WORD_SIZE];

   sprintf(IO_str, "(default=%s): ", ((format == FAST_FORMAT) ? net_fwt : net_pwt) );
   IO_print(0);
   IO_my_get_string(temp_str);
 
   if (temp_str[0] != ENDSTRING) BEGIN
      if (format == FAST_FORMAT)
         strcpy(net_fwt, temp_str);
      else strcpy(net_pwt, temp_str);
   ENDIF

END /* IO_get_wts_name */
