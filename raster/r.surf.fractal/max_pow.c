/*****************************************************************************/
/* MAX_POW2 : finds least power of 2 greater than or equal to number         */
/* Input arguments: n - unsigned integer, the number                         */
/* Output is an integer power of 2                                           */
/*****************************************************************************/

int 
max_pow2 (int n)
{
    int p2, n1;

    n1 = n >> 1;
    p2 = 1;
 
    while (n1 > 0)
    {
    	n1 >>= 1;
    	p2 <<= 1;
    }
    if (p2 < n) 
	p2 <<=1;

    return(p2);
} 

