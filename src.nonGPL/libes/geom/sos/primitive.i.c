/* sos/primitive.i.c */

/* #include "primitive.i" replaces some macros defined in
   #include "primitive.h" by static routines to cut down the
   size of object files.

   USAGE:  
   
   #include "basic.h"
   #include "sos.h"
   #include "internal.h"
   #include "primitive.h"
 
   Static_Declarations ( , , , );

   #include "primitive.i.c"

   */

/*--------------------------------------------------------------------------*/

#undef Epsilon_Term
static void Epsilon_Term (t)
     int t;
{
  result.depth = (t);
  counter[t] ++;
  result.two_k = 0;
}

#undef Epsilon
static void Epsilon (LAMBDA, KAPPA)
     int LAMBDA, KAPPA;
{
  result.epsilon[result.two_k++] = (LAMBDA);
  result.epsilon[result.two_k++] = (KAPPA);
}
