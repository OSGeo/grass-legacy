#include<stdio.h>
main()
{
 int i;
  double ppnd();

 for(i=0;i<100;++i)
   fprintf (stdout,"%.2f -> %.7f\n",(double)i/100.0, ppnd((double)i/100.0));
}
