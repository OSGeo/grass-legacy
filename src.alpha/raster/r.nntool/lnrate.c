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
  Code For Cosecant Learning Rate Adjustment (Prefix = LR_)
----------------------------------------------------------------------
  This code is divided into 4 major sections:

  (1) include files
  (2) externed routines
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
#include  "common.h"


/*
----------------------------------------------------------------------
  EXTERNED ROUTINES
----------------------------------------------------------------------
*/
extern Sint  C_float_to_Sint();


/*
======================================================================
  ROUTINES IN LNRATE.C
======================================================================
  The only routine in this file is one which takes a Sint value which
  represents a sum and returns the semilinear activation value.

  Type Returned                 Routine                                 
  -------------                 -------                                 
    float                       LR_learn_default();
    float                       LR_momentum_default();
    float                       LR_scale_default();
    float                       LR_learn_from_scale();
    D_Sint                      LR_calc_learn_rate();
======================================================================
*/


/*
----------------------------------------------------------------------
  GLOBAL VARIABLES
----------------------------------------------------------------------
  The only global variable used by this code is a table needed for storing
  all of the cosecant values between 0 and pi/2. This table is generated
  to help speed things up when learning rates are being calculated. Rather
  than run the rather slow (1/sin(x)) function, a table reference is 
  made which will generate the appropriate answer.  In short, we pre-
  calculate all possible values for the cosecant and store them in a table 
  using the incoming value as a lookup.
  Below are three global variables used in this file. The first two are
  used to offset the cosecant function by scale (ie, make the function
  more or less steep) and by shifting (ie, moving the cosecant function
  up or down the y-axis). Following that is a pre-calculated set of 
  half the cosecant values (the cosecant is symetric so only half are
  needed). Once referenced, these values are multiplied by  LR_SCALE 
  and added to LR_SHIFT to yield the final result. 
----------------------------------------------------------------------
*/       

static float  cosec[] = {
325.950, 162.976, 108.651,  81.489,  65.192,  54.328,  46.568,  40.748,
 36.221,  32.600,  29.637,  27.169,  25.080,  23.289,  21.738,  20.380,
 19.182,  18.118,  17.165,  16.308,  15.532,  14.827,  14.183,  13.594,
 13.051,  12.550,  12.086,  11.655,  11.254,  10.880,  10.530,  10.202,
  9.894,   9.604,   9.331,   9.073,   8.828,   8.597,   8.378,   8.169,
  7.971,   7.782,   7.602,   7.430,   7.266,   7.109,   6.959,   6.815,
  6.677,   6.545,   6.417,   6.295,   6.177,   6.064,   5.955,   5.849,
  5.748,   5.650,   5.555,   5.463,   5.375,   5.289,   5.206,   5.126,
  5.048,   4.973,   4.899,   4.828,   4.759,   4.692,   4.627,   4.564,
  4.503,   4.443,   4.385,   4.328,   4.273,   4.219,   4.167,   4.116,
  4.066,   4.017,   3.970,   3.924,   3.879,   3.834,   3.791,   3.749,
  3.708,   3.668,   3.629,   3.590,   3.553,   3.516,   3.480,   3.445,
  3.410,   3.377,   3.344,   3.311,   3.279,   3.248,   3.218,   3.188,
  3.159,   3.130,   3.102,   3.074,   3.047,   3.020,   2.994,   2.968,
  2.943,   2.918,   2.894,   2.870,   2.847,   2.824,   2.801,   2.779,
  2.757,   2.735,   2.714,   2.693,   2.673,   2.652,   2.633,   2.613,
  2.594,   2.575,   2.556,   2.538,   2.520,   2.502,   2.485,   2.468,
  2.451,   2.434,   2.418,   2.401,   2.385,   2.370,   2.354,   2.339,
  2.324,   2.309,   2.294,   2.280,   2.266,   2.252,   2.238,   2.224,
  2.211,   2.197,   2.184,   2.171,   2.159,   2.146,   2.134,   2.121,
  2.109,   2.097,   2.086,   2.074,   2.062,   2.051,   2.040,   2.029,
  2.018,   2.007,   1.996,   1.986,   1.976,   1.965,   1.955,   1.945,
  1.935,   1.925,   1.916,   1.906,   1.897,   1.887,   1.878,   1.869,
  1.860,   1.851,   1.842,   1.834,   1.825,   1.817,   1.808,   1.800,
  1.792,   1.784,   1.776,   1.768,   1.760,   1.752,   1.744,   1.737,
  1.729,   1.722,   1.714,   1.707,   1.700,   1.693,   1.686,   1.679,
  1.672,   1.665,   1.658,   1.651,   1.645,   1.638,   1.632,   1.625,
  1.619,   1.613,   1.607,   1.600,   1.594,   1.588,   1.582,   1.576,
  1.570,   1.565,   1.559,   1.553,   1.548,   1.542,   1.536,   1.531,
  1.526,   1.520,   1.515,   1.510,   1.504,   1.499,   1.494,   1.489,
  1.484,   1.479,   1.474,   1.469,   1.464,   1.460,   1.455,   1.450,
  1.446,   1.441,   1.436,   1.432,   1.427,   1.423,   1.419,   1.414,
  1.410,   1.406,   1.401,   1.397,   1.393,   1.389,   1.385,   1.381,
  1.377,   1.373,   1.369,   1.365,   1.361,   1.357,   1.353,   1.350,
  1.346,   1.342,   1.339,   1.335,   1.331,   1.328,   1.324,   1.321,
  1.317,   1.314,   1.310,   1.307,   1.304,   1.300,   1.297,   1.294,
  1.290,   1.287,   1.284,   1.281,   1.278,   1.275,   1.272,   1.268,
  1.265,   1.262,   1.259,   1.257,   1.254,   1.251,   1.248,   1.245,
  1.242,   1.239,   1.237,   1.234,   1.231,   1.228,   1.226,   1.223,
  1.220,   1.218,   1.215,   1.213,   1.210,   1.208,   1.205,   1.203,
  1.200,   1.198,   1.195,   1.193,   1.191,   1.188,   1.186,   1.184,
  1.181,   1.179,   1.177,   1.175,   1.172,   1.170,   1.168,   1.166,
  1.164,   1.162,   1.160,   1.157,   1.155,   1.153,   1.151,   1.149,
  1.147,   1.145,   1.143,   1.141,   1.140,   1.138,   1.136,   1.134,
  1.132,   1.130,   1.128,   1.127,   1.125,   1.123,   1.121,   1.120,
  1.118,   1.116,   1.114,   1.113,   1.111,   1.109,   1.108,   1.106,
  1.105,   1.103,   1.101,   1.100,   1.098,   1.097,   1.095,   1.094,
  1.092,   1.091,   1.089,   1.088,   1.087,   1.085,   1.084,   1.082,
  1.081,   1.080,   1.078,   1.077,   1.076,   1.074,   1.073,   1.072,
  1.071,   1.069,   1.068,   1.067,   1.066,   1.064,   1.063,   1.062,
  1.061,   1.060,   1.059,   1.058,   1.056,   1.055,   1.054,   1.053,
  1.052,   1.051,   1.050,   1.049,   1.048,   1.047,   1.046,   1.045,
  1.044,   1.043,   1.042,   1.041,   1.040,   1.039,   1.038,   1.038,
  1.037,   1.036,   1.035,   1.034,   1.033,   1.033,   1.032,   1.031,
  1.030,   1.029,   1.029,   1.028,   1.027,   1.026,   1.026,   1.025,
  1.024,   1.024,   1.023,   1.022,   1.022,   1.021,   1.020,   1.020,
  1.019,   1.018,   1.018,   1.017,   1.017,   1.016,   1.015,   1.015,
  1.014,   1.014,   1.013,   1.013,   1.012,   1.012,   1.011,   1.011,
  1.010,   1.010,   1.010,   1.009,   1.009,   1.008,   1.008,   1.008,
  1.007,   1.007,   1.006,   1.006,   1.006,   1.005,   1.005,   1.005,
  1.005,   1.004,   1.004,   1.004,   1.003,   1.003,   1.003,   1.003,
  1.002,   1.002,   1.002,   1.002,   1.002,   1.002,   1.001,   1.001,
  1.001,   1.001,   1.001,   1.001,   1.001,   1.000,   1.000,   1.000,
  1.000,   1.000,   1.000,   1.000,   1.000,   1.000,   1.000,   1.000 }; 
  
/* end cosec array, 512 items long (for Sints 001 to 512) */


float  LR_learn_default(max_wts)
int  max_wts;
/*
----------------------------------------------------------------------
  Given a layer's maximum number of incoming weights, this routine will
   calculate a "good" first cut at a learning rate for use as a default
   when prompting the user.
   
  The constants used in this routine are entirely heuristic in nature.
   The 15.0 (for "learn") comes from the formula for delta weight. 
   Note:
   
     dW = learn_rate * delta * output
     
   for "typical" delta and output values (specifically, delta=.125 at
   output=.5), a dW of 1.0 arises from a learning rate of 16 (I picked
   15 to yield slightly more conservative learning rates). Dividing
   this by the number of weights tells how much the learning rate 
   should be.
----------------------------------------------------------------------
*/
BEGIN
   float  temp;

   temp = 15.0 / (float)max_wts;
   return( ((temp > 2.5) ? 2.5 : temp) );

END /* LR_learn_default */


float  LR_momentum_default(max_wts)
int  max_wts;
/*
----------------------------------------------------------------------
  Given some maximum number of incoming weights, this routine will
   calculate a "good" first cuts at a momentum value for use as default
   when prompting the user.
   
  The constants used in this routine are entirely heuristic in nature.
   The momentum scale is my own makeshift linear relation between momentum
   and max number of weights. It assumes that a momentum of 1.0 corresponds
   to 0 max_wts, and a momentum of 0.0 corresponds to 2250 max_wts.
----------------------------------------------------------------------
*/
BEGIN
   float  temp;

   if (max_wts > 2250)
      temp = 0.1;
   else
      temp = 1.0 - (0.0004 * (float)max_wts);
   
   return( ((temp > 0.9) ? 0.9 : temp) );

END /* LR_momentum_default */


float  LR_scale_default(learn)
float  learn;
/*
----------------------------------------------------------------------
  Given a layer's maximum number of incoming weights and base learning
   rate, this routine will calculate a "good" first cut at a scaling 
   factor for use as a default when prompting the user.
   
  The constants used in this routine are entirely heuristic in nature.
   The 2.5 and 30.836 for the scale come from two different places.
   First, the 2.5 is the learning rate needed to get a minimum dW 
   (.001 for NETS) at the minimum output (.2) and the minimum error 
   (.01 which yields a delta of .002). The formula in LR_learn_default
   is used to calculate a min learning rate of 2.5 with these values. 
   Then, the Don Woods function for calculating a new learning rate:
   
      learn = SCALE * cosecant(err) + MIN_LEARN_RATE - SCALE
   
   can be worked backwords to solve for scale. The result is the formula
   for the "scale" variable with the cosecant(.01) - 1.0 = 30.836
----------------------------------------------------------------------
*/
BEGIN
   return( (2.5 - learn) / 30.836 );

END /* LR_scale_default */


float  LR_learn_from_scale(scale)
float  scale;
/*
----------------------------------------------------------------------
  This routine is the exact opposite from the LR_scale_default and
  gives the learning rate from scale information.
----------------------------------------------------------------------
*/
BEGIN
   return( 2.5 - (30.836 * scale) );

END /* LR_learn_from_scale */


D_Sint  LR_calc_learn_rate(learn_base, learn_scale, avg_err)
float  learn_base, learn_scale;
Sint   avg_err;
/*
----------------------------------------------------------------------
  Given the average error for one (or more) IO pairs, this routine 
  returns a learning rate according to the function:
  
       scale * cosecant(error * pi) + (base - scale);
       
  To get the appropriate value from the table, one need only access the
  cosec array using the Sint avg_err value. Note that since the cosecant
  function is symetric about pi/2, I have only stored the cosecant 
  values for .001 to .5; thus if the incoming error is greater than 
  512 (which is SINT_SCALE / 2), then simply subtract it from SINT_SCALE
  and then reference the cosec array.
 NOTE THAT VALUES IN COSEC ARRAY are stored at indicies STARTING WITH 0.
  Thus, you must subtract 1 from the incoming avg_err parameter in order
  to reference the correct place in the cosec array. 
     ... however ...
  Note also that some errors reported by avg_err can be 0. This would
  correspond to an infinity value for the cosecant function. Thus, rather
  than subtract 1 from the avg_err, it is simply left alone. As a result,
  every reference to cosec is automatically made at +1 to the avg_err.
----------------------------------------------------------------------
*/
BEGIN
#if  USE_SCALED_INTS
   static int  max_csc = SINT_SCALE / 2;
   float       csc;
   
   /*-------------------------------------------*/
   /* if the error is greater than .5, subtract */
   /* from 1 (as a Sint) and then consult cosec */
   /*-------------------------------------------*/
   if (avg_err > max_csc)
      csc = cosec[SINT_SCALE - avg_err];
   else
      csc = cosec[avg_err];
      
   return((D_Sint) 
          C_float_to_Sint(learn_scale * csc + learn_base - learn_scale));
#else
   if ((avg_err <= 0) || (avg_err >= 1.0))
      return((D_Sint)learn_base);
   else
      return((D_Sint)
             ((learn_scale * 1.0 / sin(PI * avg_err)) + learn_base - learn_scale) );
#endif

END /* LR_calc_learn_rate */
