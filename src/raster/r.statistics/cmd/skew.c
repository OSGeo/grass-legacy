#include <stdio.h>
#include <math.h>

#define MEM  1024

#define MDEBUG(a) fprintf(stderr,"%s\n",a);

#define VAL  500
#define SKEW 100

int main (int argc, char **argv)
{
  char array[1024];
  FILE *stats;
  int first, mem, i, count;
  long basecat, covercat, catb, catc;
  double value, var, x;
  double tab[100000];
      
    
    
    i=0;
    stats=fopen("gauss.dat","r");
    while(fgets(array,1023,stats))
    {
      sscanf(array,"%lf %lf",&var,&x);
      tab[i++] = x;
      fprintf(stdout,"%lf\n",x);
    }
    kurt(tab,i-1,&value);
    
    
    
    fprintf(stderr,"kurtosis: %lf   Nr. of Values: %d\n",value,i); 
    
    return 0;                                        
}


/***********************************************************************
*
*  Given an array of data[1...n], this routine returns its kurtosis
*
************************************************************************/

int 
kurt (double *data, int n, double *kurto)
{      
 double ave, ep, var, s;
 int i;

   if(n < 1)
   {
    fprintf(stderr,"o_kurto: No data in array\n");
    return (1);
   }

   *kurto = 0.0;
   var    = 0.0;
   ep     = 0.0;
   s      = 0.0;
   
   
   for(i = 0; i < n; i++)              /* First pass to get the mean     */
      s += data[i];
   ave = s / n;
  
   for (i = 0; i < n; i++)             
   {                     
       s   = data[i] - ave;     
       var  += s * s;
       ep += s;
   }
   
   var = (var - ep * ep / n) / (n -  1);
   
   for(i = 0; i < n; i++)
   {
     s       = (data[i] - ave) / sqrt(var);
     *kurto += s * s * s * s;
   }
   *kurto = (*kurto / n) - 3;
   
   return(0);
}


/***********************************************************************
*
*  Given an array of data[1...n], this routine returns its skewness
*
************************************************************************/

int 
skew (double *data, int n, double *skewn)
{      
 double ave, ep, var, s;
 int i;

   if(n < 1)
   {
    fprintf(stderr,"o_skew: No data in array\n");
    return (1);
   }

   *skewn = 0.0;
   var    = 0.0;
   ep     = 0.0;
   s      = 0.0;
   
   
   for(i = 0; i < n; i++)              /* First pass to get the mean     */
      s += data[i];
   ave = s / n;
  
   for (i = 0; i < n; i++)             
   {                     
       s   = data[i] - ave;     
       var += s * s;
       ep  += s;
   }
   
   var = (var - ep * ep / n) / (n -  1);
   
   fprintf(stderr,"sdev: %lf  ep2: %lf\n",sqrt(var),ep * ep / n);
   for(i = 0; i < n; i++)
   {
     s       = (data[i] - ave) / sqrt(var);
     *skewn += s * s * s;
   }
  
   *skewn /= n;


/*   *skewn /= (n*var*sqrt(var)); */

     
   return(0);
}
