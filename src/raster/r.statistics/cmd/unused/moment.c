/***********************************************************************
*
*  Given an array of data[1...n], this routine returns its mean ave,
*  average deviation adev, standard deviation sdev, variance var,
*  skewnes skew, and kurtosis kurt.
*
************************************************************************/

int 
stats (double *data, int n, double *ave, double *adev, double *sdev, double *var, double *skew, double *kurt)
{      
 int i;
 double ep, s, p;

   if(n <= 1)
   {
    fprintf(stderr,"o_moment: No data in array\n");
    return (1);
   }

   *adev = 0.0;
   *var  = 0.0;
   *skew = 0.0;
   *kurt = 0.0;
   ep = 0.0;
   s  = 0.0;

   for(i = 1; i <= n; i++)              /* First pass to get the mean     */
      s += data[i];
   *ave = s / n;

   for (i = 1; i <=n; i++)             /* Second pass to get the first (absolute), */
   {                                   /* second, third and fourth moments of the  */
      *adev += fabs(s = data[i] - (*ave));  /* deviation from the mean             */
      *var  += (p = s * s);
      *skew += (p *= s);
      *kurt += (p *= s);
      ep    += s;
   }

   *adev /= n;
   *var  = (*var - ep * ep / n) / (n - 1);  /* Corrected 2-pass formula */
   *sdev = sqrt(*var);
   if(*var)
   {                                     /* Put the pieces together according to  */
      *skew /= (n * (*var) * (*sdev));   /* the conventional definitions          */
      *kurt  = (*kurt) / (n * (*var) * (*var)) - 3.0;
   }
   else
   {
      fprintf(stderr,"o_moment: No skew/kurtosis when variance = 0\n");
      return(2);
   }

   return(0);
}