#include "gis.h"
#include "gmath.h"
#include "numerical.h"

/*****************************************************************************/
/* MAX_POW2 : finds least power of 2 greater than or equal to number         */
/*                                                                           */
/* Input arguments: n - unsigned integer, the number                         */
/*                                                                           */
/* Output is an integer power of 2                                           */
/*                                                                           */
/*****************************************************************************/

long max_pow2 (long n)

{
        long p2, n1;

        n1 = n >> 1;
        p2 = 1;
        while (n1 > 0)
        {
                n1 >>= 1;
                p2 <<= 1;
        }
        if (p2 < n) p2 <<=1;
        return(p2);
}   /* end max_pow2 */

/*****************************************************************************/
/* MAX_POW2 : finds largest power of 2 less than or equal to number         */
/*                                                                           */
/* Input arguments: n - unsigned integer, the number                         */
/*                                                                           */
/* Output is an integer power of 2                                           */
/*                                                                           */
/*****************************************************************************/
long 
min_pow2 (long n)

{
        long p2, n1;

        n1 = n >> 1;
        p2 = 1;
        while (n1 > 0)
        {
                n1 >>= 1;
                p2 <<= 1;
        }
        return(p2);
}   /* end min_pow2 */
