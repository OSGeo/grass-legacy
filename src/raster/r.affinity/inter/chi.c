#include <stdio.h>
#include <math.h>


#define ITMAX 1000
#define EPS 3.0e-7




double gammln(xx)
double xx;

{

  double x,tmp,ser;
  static double cof[6]={76.18009173, -86.50532033, 24.01409822,
     -1.231739516, 0.120858003e-2, -0.536382e-5};
  int j;

  x=xx-1.0;
  tmp=x+5.5;
  tmp-=(x+0.5)*log(tmp);
  ser=1.0;
  for(j=0;j<=5;j++){
    x +=1.0;
    ser+=cof[j]/x;
   }
 return (-tmp+log(2.50662827465*ser));

}





gser(gamser,a,x,gln)
double a,x,*gamser,*gln;

{
 int n;
 double sum,del,ap;
 
 *gln=gammln(a);
 if (x <= 0.0) {
   *gamser=0.0;
  fprintf(stderr,"x less than 0 in GSER");
  return -1;
  }

 else{
  ap=a;
  del=sum=1.0/a;
  for(n = 1; n<=ITMAX; n++){
     ap+=1.0;
     del *=x/ap;
     sum +=del;
     if (fabs(del) < fabs(sum)*EPS){
       *gamser=sum*exp(-x+a*log(x)-(*gln));
       return 0;
      }
   }
  fprintf(stderr,"a too large, ITMAX too small in GSER");
  return -2;
 }

}


gcf(gammcf,a,x,gln)
double a,x,*gammcf,*gln;
{
  int n;
  double gold=0.0, g, fac=1.0, b1=1.0;
  double b0=0.0, anf, ana, an, a1, a0=1.0;

  *gln=gammln(a);
  a1=x;
  for (n=1; n<=ITMAX; n++) {
     an=(double) n;
     ana=an-a;
     a0=(a1+a0*ana)*fac;
     b0=(b1+b0*ana)*fac;
     anf=an*fac;
     a1=x*a0+anf*a1;
     b1=x*b0+anf*b1;
     if (a1) {
        fac=1.0/a1;
        g=b1*fac;
        if (fabs((g-gold)/g) < EPS) {
          *gammcf=exp(-x+a*log(x)-(*gln))*g;
          return 0;
         }
        gold=g;
     }
   }
   fprintf(stderr,"a too large, ITMAX too small in GCF");
   return -1;

}
   




double gammq(a,x)
double a,x;
{

double gamser, gammcf,gln;
int ret;

/* fprintf(stderr,"\na: %lf, x: %lf",a,x); */

if ( (x<0.0) || (a <= 0.0)) {
  fprintf(stderr,"invlid argument in GAMMQ");
  return -1;
 }

if ( x < (a+1.0)){
  ret=gser(&gamser,a,x,&gln);
/*  fprintf(stderr,"gserret: %lf",1.0-gamser); */
  if (ret>=0)  return (1.0-gamser);
  else return -1;
}

else {

  ret=gcf(&gammcf,a,x,&gln);
/* fprintf(stderr,"gcfret: %lf",gammcf); */
  if (ret>=0)  return gammcf;
  else return -1;

}

}

            
 
