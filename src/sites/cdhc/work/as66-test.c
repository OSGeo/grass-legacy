#include<stdio.h>
main()
{
 int i;
 double alnorm();
 for(i=-500;i<500;i+=10)
   fprintf (stdout,"%g\n",alnorm((double)i/100.0,1));
}
