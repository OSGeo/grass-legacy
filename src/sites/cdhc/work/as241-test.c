#include<stdio.h>
main()
{
 int i,j;
 double ppnd();
 double ppnd7();
 double ppnd16();

 for(i=50;i<100;++i)
   fprintf (stdout,"%.2f -> %.16f %.16f\n",(double)i/100.0, 
          ppnd((double)i/100.0),
          ppnd16((double)i/100.0,&j));
}
