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
  All System-Dependent Code Resides Here (Prefix = sys_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDES AND EXTERNS
----------------------------------------------------------------------
*/
#include     "common.h"
#include     "weights.h"
#include     "layer.h"
#include     "net.h"
#include     "netio.h"
extern void  IO_print();
extern char  IO_str[MAX_LINE_SIZE];


/*
======================================================================
  ROUTINES IN SYSDEP.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "sys_" indicating that it is defined in the 
  "sysdep.c" file.  The types returned by the routines are also shown here
  so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
    char *                      sys_long_alloc
    char *                      sys_alloc
    void                        sys_long_free
    void                        sys_free
    float                       sys_get_time
    void                        sys_init_rand
    void                        sys_delete_file
    void                        sys_rename_file
======================================================================
*/


char  *sys_long_alloc(size)
long  size;
/*
----------------------------------------------------------------------
  Although not really system dependent code, the get_mem routine was
  placed here because its activities are usually associated with system
  calls. All this routine does is take the place of "malloc" so that
  I can check for lack of space and handle the error gracefully, ie,
  by printing out a message before quiting the program.
  
  The trick here is to do the right kind of malloc. Normally, the 
  typical "malloc" function which comes with the compiler takes an
  unsigned integer as an argument. Thus you cannot pass it a long 
  argument. 
----------------------------------------------------------------------
*/
BEGIN
   char          *r, *sys_check_alloc();
   unsigned int  test;
   
   /*-----------------------------------------*/
   /* allocate the memory in chars, using the */
   /* proper routine depending on compiler    */
   /*-----------------------------------------*/
   if (size > 0)
#if LSPEED
      r = mlalloc(size);
#endif  

#if IBM_TBC
      r = farmalloc(size);
#endif

#if IBM_MSC
      r = halloc(size,1);
#endif
  
#if (! LSPEED) && (! IBM_TBC) && (! IBM_MSC)
   BEGIN
      test = (unsigned int) size;
      if (test != size)
         r= NULL;
      else r = (char *) malloc(test);
   ENDIF
#endif

   else 
      r = NULL;
   
   /*---------------------------------------------*/
   /* then check to see that the memory was there */
   /*---------------------------------------------*/
   return(sys_check_alloc(r));
      
END /* sys_long_alloc */


char  *sys_alloc(size)
unsigned int  size;
/*
----------------------------------------------------------------------
  This routine is called for memory allocation when the caller KNOWS
  that a large memory allocation is not necessary.
----------------------------------------------------------------------
*/
BEGIN
   char  *r, *sys_check_alloc();

   if (size > 0)
      r = (char *) malloc(size);
   else 
      r = NULL;
   return(sys_check_alloc(r));
      
END /* sys_alloc */


char  *sys_check_alloc(ptr_mem)
char  *ptr_mem;
/*
----------------------------------------------------------------------
  Just a subroutine called by the allocation routines above to check if
  the pointer to memory is NULL. If so, then out of memory, thus exit
  the program.
----------------------------------------------------------------------
*/
BEGIN
   /*---------------------------------------*/
   /* if the result is NULL exit with error */
   /*---------------------------------------*/
   if (ptr_mem == NULL) BEGIN
      sprintf(IO_str, "\n*** out of memory ***\n");
      IO_print(0);
      exit(1);
   ENDIF
   return(ptr_mem);
      
END /* sys_check_alloc */

  
void  sys_long_free(ptr)
char *ptr;
/*
----------------------------------------------------------------------
  This free routine is the sister of the sys_get_mem routine. It calls
  the proper routine for freeing the memory, depending upon the compiler
  used. 
----------------------------------------------------------------------
*/
BEGIN
 
#if LSPEED
   free(ptr);
#endif  
  
#if IBM_TBC
   farfree(ptr);
#endif

#if IBM_MSC
   hfree(ptr);
#endif

#if (! LSPEED) && (! IBM_TBC) && (! IBM_MSC)
   free(ptr);
#endif

END /* sys_long_free */


void  sys_free(ptr)
char  *ptr;
/*
----------------------------------------------------------------------
  Just to keep everything parallel, I wrote this "normal case" free 
  routine which is the sister to the sys_alloc routine above. All this 
  does is call the generic free routine of the system.
----------------------------------------------------------------------
*/
BEGIN
   free(ptr);
   
END /* sys_free */


float  sys_get_time()                          
/*
----------------------------------------------------------------------
 float sys_get_time()                                                     
                                                                      
 Returns a floating point value of the current time.  Taken from      
  the CLIPS code, which is written in a more portable form there.     
 Note that there are several '#if' statements here.  These look for   
  values defined within 'common.h' to see whether or not they should  
  compile.  That is, if "VMS" is defined to 1 then any code           
  surrounded by a '#if VMS' statement will be compiled.  This is a    
  simple way to make code portable.                                   
----------------------------------------------------------------------
*/
BEGIN
#if   VMS || IBM_MSC || UNIX_BER
   float sec, msec, time;
   long   temp;
   struct timeb time_pointer;

   ftime(&time_pointer);
   temp = time_pointer.time;
   temp = temp - ((temp/10000) * 10000);
   sec  = (float) temp;
   msec = (float) time_pointer.millitm;
   return(sec + (msec / 1000.0));
#endif

#if   UNIX_ATT 
   long t_int;
   float t;
   struct tms buf;

   t_int = times(&buf);
   t = (float) t_int / 60.0;
   return(t);
#endif

#if   IBM_TBC
   struct time  now;
   float        t;
   
   gettime(&now);
   t = ((float)now.ti_hour * 3600.0)
       + ((float)now.ti_min * 60.0)
       + (float)now.ti_sec;
   return(t);
#endif

#if   LSPEED
   struct tm  *now;
   float        t;
   
   now = localtime(0L);
   t = ((float)now->tm_hour * 3600.0)
       + ((float)now->tm_min * 60.0)
       + (float)now->tm_sec;
   return(t);
#endif

#if  WIZARD
   time_t  the_time;
   
   return( (float)time(&the_time) );
#endif

END /* sys_get_time */


void  sys_init_rand()
/*
----------------------------------------------------------------------
  SYS_INIT_RAND will initialize the random number generator
                 
  Author : Chris Ortiz ( MPAD/AI NASA )
  Date   : 2-Jun-1989
  Cyclomatic Complexity = 1

  Variables Used
  --------------
     tm         - Structure Time is stored in.
     seed       - Random Number generator seed.
     
  Procedures / Functions
  ----------------------
     Srand     - Initialize Random number generator.
     Localtime - Gets Local Time

  Algorithm
  ---------
     1. Get current time.
     2. Multiply minutes by the seconds to come up with a seed.
     3. Set random number generator with seed.
----------------------------------------------------------------------
*/
BEGIN
#if   LSPEED
   struct tm  *time;
   float      seed;
   
   time = localtime(0L);
   seed = ((float)time->tm_min * (float)time->tm_sec);
   srand ((int)seed);
#endif

#if   IBM_TBC
   srand( (long)sys_get_time() );
#endif

#if   VMS || IBM_MSC || UNIX_BER
   struct timeb time_pointer;

   ftime(&time_pointer);
   srand( (unsigned)time_pointer.time );
#endif

#if   UNIX_ATT 
   long t_int;
   struct tms buf;

   t_int = times(&buf);
   srand( (unsigned)t_int );
#endif

#if  WIZARD
   time_t  t_int;
   
   srand( (unsigned int)time(&t_int) );
#endif

END /* sys_init_rand */


void  sys_delete_file(filename)
char  *filename;
/*
----------------------------------------------------------------------
  Deletes the file with the name "filename" from the system. For now,
  it only deletes from the same directory.
----------------------------------------------------------------------
*/
BEGIN
#if  LSPEED || VMS || IBM_MSC || IBM_TBC || WIZARD
   remove(filename);
#endif

#if  UNIX_ATT || UNIX_BER
   unlink(filename);
#endif

END /* sys_delete_file */


void  sys_rename_file(oldname, newname)
char  *oldname, *newname;
/*
----------------------------------------------------------------------
  Renames the file with the name "oldname" to the new name given in 
  the second parameter.
----------------------------------------------------------------------
*/
BEGIN
#if  !UNIX_ATT
   rename(oldname, newname);
#endif

#if  UNIX_ATT
   char  command[80];
   
   /*------------------------------------------*/
   /* for AT&T Unix, make a system call to get */
   /* the unix command "mv" to do the renaming */
   /*------------------------------------------*/
   sprintf(command, "mv %s %s", oldname, newname);
   system(command);
#endif

END /* sys_rename_file */
