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
  Conversion Code for the Net Program (Prefix = C_)
----------------------------------------------------------------------
  This file is a collection place for all those routines are used for
  converting between types.  This code is divided into 2 major sections:

  (1) include files
  (2) subroutines
 
  Each section is further explained below.
----------------------------------------------------------------------
*/


/*
----------------------------------------------------------------------
  INCLUDE FILES
----------------------------------------------------------------------
*/
#include  "common.h"


/*
======================================================================
  ROUTINES IN CONVERT.C                                                   
======================================================================
  The routines in this file are grouped below by function.  Each routine
  is prefixed by the string "C_" indicating that it is defined in the 
  "convert.c" file.  The types returned by the routines are also shown 
  here so that cross checking is more easily done between these functions
  and the other files which intern them.


  Type Returned                 Routine                                 
  -------------                 -------                                 
                                                                      
  CONVERSION ROUTINES                                                   
    Sint                        C_float_to_Sint
    Sint                        C_double_to_Sint
    Sint                        C_int_to_Sint
    float                       C_Sint_to_float
    double                      C_Sint_to_double
    int                         C_Sint_to_int
======================================================================
*/


Sint  C_float_to_Sint(num)
float  num;
/*
----------------------------------------------------------------------
 This routine converts a floating point number into our internal rep- 
  resentation for floating point.  The whole reason behind all this   
  trouble is the goal of being able to do floating point arithmetic   
  with integers (because integer operations are faster).  Since most  
  of the time spent by the teaching phase will be spent doing this    
  type of arithmetic (probably 90% of the time!), using integers is   
  extremely important.                                                
 To convert is straightforward: first, consider the fact that you have
  a decimal number, ie some number / 10.  To convert to the equivalent
  Sint representation, just use dimensional analysis a-la chemistry 1 
  where you have  x_decimal/10 = x_Sint/1024.  Thus x_Sint is just    
  x_decimal/10 * 1024. Since floats are already in decimal form (ie,  
  3.2 in decimal is 3.2 not 3.2/10) all we need to do is return       
  num * 1024 (cast into the correct type, of course).                 
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   return( (Sint) (num * (float)SINT_SCALE) );
#else
   return((Sint)num);
#endif

END /* C_float_to_Sint */


Sint  C_double_to_Sint(num)
double  num;
/*
----------------------------------------------------------------------
 This routine converts a double into a Sint                           
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   return( (Sint) (num * (double)SINT_SCALE) );
#else
   return((Sint)num);
#endif

END /* C_double_to_Sint */


Sint  C_int_to_Sint(num)
int  num;
/*
----------------------------------------------------------------------
 This routine converts an integer into a Sint.                        
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   return( (Sint) (num * SINT_SCALE) );
#else
   return( (Sint)num );
#endif

END /* C_int_to_Sint */


float  C_Sint_to_float(num)
Sint  num;
/*
----------------------------------------------------------------------
 converts a Sint (special integer) to a  floating point number. Note 
 that this cannot be done with right-shifting since that would throw
 away all the decimal information contained in the Sint.
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   return( (float) (num / (float)SINT_SCALE) );
#else
   return( (float)num );
#endif

END /* C_Sint_to_float */


double  C_Sint_to_double(num)
Sint  num;
/*
----------------------------------------------------------------------
 converts a Sint (special integer) to a double floating point number  
----------------------------------------------------------------------
*/
BEGIN   
#if  USE_SCALED_INTS
   return( (double) (num / (double)SINT_SCALE) );
#else
   return( (double)num );
#endif

END /* C_Sint_to_double */


int  C_Sint_to_int(num)
Sint  num;
/*
----------------------------------------------------------------------
 converts a Sint (special integer) to an integer for printing         
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   return( (int) (num / SINT_SCALE) );
#else
   return( (int)num );
#endif

END /* C_Sint_to_int */

