
#include "all.h"

/**************************************************************/
void ddsqoy(bottom,sslope,sslope2,strick,delx,yp,bel,nx1,dqdy,ltype,chn_dep,
            mk0,pk0,mk1,pk1,xk,numhts,ht_spc,table_num,NUMBPTS)

/*
 *    THIS SUBROUTINE CALCULATES THE DOWNSTREAM Q(Y) B.C.
 *
 */

   float *bottom,*sslope,*sslope2,*strick,delx,*yp,*bel,*dqdy,*chn_dep;
   float *mk0,*pk0;
   float *mk1,*pk1;
   int *nx1;
   int *ltype;
   /* added with breakpoint channel xsect descript. FLO 10-8-94 */
   float *xk,*ht_spc;
   int *numhts,*table_num,NUMBPTS;

{
   int nl,endaddr;
   float dslope,depth1,area,per,rhydr,cz;
   float ocdep,yabf;
   int tnum,interv,last;
   float slope,ck;

   nl=nlinks;
   endaddr=nx1[nl]+nl*NODES;
   dslope=(bel[nx1[nl]+nl*NODES-1]-bel[nx1[nl]+nl*NODES])/delx;
   depth1=yp[endaddr]-bel[endaddr];

   if(ltype[nl]==1)
      {
      /* do trapezoidal calculations */
      area=bottom[endaddr]*depth1+sslope[endaddr]*depth1*depth1;
      cz=2.0*sqrt((double)(1.0+sslope[endaddr]*sslope[endaddr]));
      per=bottom[endaddr]+2.0*depth1*sqrt(1.0+sslope[endaddr]*
          sslope[endaddr]);
      rhydr=area/per;
      *dqdy=sqrt((double)dslope)*strick[endaddr]*
   	    (bottom[endaddr]+2.0*sslope[endaddr]*depth1)*
   	    pow(rhydr,(double)(2./3.))+sqrt((double)dslope)*
	    strick[endaddr]*area*2./3.*pow(rhydr,(double)(-1./3.))*
	    (((bottom[endaddr]+2.0*sslope[endaddr]*depth1)/per)-
	    (area*cz)/(per*per));
      }
   if(ltype[nl]==5)
      {
      /* do APFA type II calculations */
      *dqdy=sqrt(dslope)*pk0[endaddr]*mk0[endaddr]*
	    pow((double)depth1,(double)(pk0[endaddr]-1.0));
      }
   if(ltype[nl]==6)
      {
      /* do APFA type III calculations */
      ocdep=chn_dep[endaddr];
      if(depth1>ocdep)
         {
         yabf=depth1-ocdep;
         *dqdy=sqrt(dslope)*pk1[endaddr]*mk1[endaddr]*
               pow((double)yabf,(double)(pk1[endaddr]-1.0));
         }
      else
         {
         *dqdy=sqrt(dslope)*pk0[endaddr]*mk0[endaddr]*
               pow((double)depth1,(double)(pk0[endaddr]-1.0));
         }
      }
   if(ltype[nl]==8)
     {
     /* this is a break-point cross section. */
     tnum=table_num[endaddr];
     interv=(int)(depth1/ht_spc[tnum]);
     if(interv>=numhts[tnum])
        {
        /* outside the range of data, EXTRAPOLATE */
        last=numhts[tnum];
        slope=(xk[last+tnum*NUMBPTS]-xk[last-1+tnum*NUMBPTS])/ht_spc[tnum];
        }
     else
        {
        /* This point is inside the range of table data */
        if(interv>0)
          {
          /* not in the first interval, use normal interpolation */
          slope=(xk[interv+1+tnum*NUMBPTS]-xk[interv+tnum*NUMBPTS])/
                ht_spc[tnum];
          }
        else
          {
          /* in the first interval, use 5/3 pwr interpolation */
          ck=xk[1+tnum*NUMBPTS]/pow(ht_spc[tnum],(5.0/3.0));
          slope=ck*pow(depth1,(2.0/3.0));
          }
        }
     *dqdy=sqrt(dslope)*slope;
     }
   if(ltype[nl]==9)
     {
     /* do multiple side slope calculations */
     ocdep=chn_dep[endaddr];
     if(depth1>ocdep)
	{
	yabf=depth1-ocdep;
	area=ocdep*(bottom[endaddr]+ocdep*sslope[endaddr])+
	     0.5*yabf*(bottom[endaddr]+2.0*ocdep*sslope[endaddr]+
	     bottom[endaddr]+2.0*ocdep*sslope[endaddr]+2.0*sslope2[endaddr]*yabf);
        per=bottom[endaddr]+2.0*ocdep*sqrt((double)(1.0+sslope[endaddr]*
	    sslope[endaddr]))+2.*yabf*sqrt((double)(1.0+sslope2[endaddr]*
	    sslope2[endaddr]));
        rhydr=area/per;
	*dqdy=(5./3.)*sqrt((double)dslope)*strick[endaddr]*
              pow((double)rhydr,(double)0.6666667)*
	      (bottom[endaddr]+2.0*ocdep*sslope[endaddr]+2.0*sslope2[endaddr]*yabf)-
              (4./3.)*sqrt((double)dslope)*strick[endaddr]*
	      pow((double)rhydr,(double)1.6666667)*
	      sqrt((double)(1.0+sslope2[endaddr]*sslope2[endaddr]));
        }
      else
	{
        area=bottom[endaddr]*depth1+sslope[endaddr]*depth1*depth1;
        cz=2.0*sqrt((double)(1.0+sslope[endaddr]*sslope[endaddr]));
        per=bottom[endaddr]+2.0*depth1*sqrt(1.0+sslope[endaddr]*
            sslope[endaddr]);
        rhydr=area/per;
        *dqdy=sqrt((double)dslope)*strick[endaddr]*
     	    (bottom[endaddr]+2.0*sslope[endaddr]*depth1)*
   	    pow(rhydr,(double)(2./3.))+sqrt((double)dslope)*
	    strick[endaddr]*area*2./3.*pow(rhydr,(double)(-1./3.))*
	    (((bottom[endaddr]+2.0*sslope[endaddr]*depth1)/per)-
	    (area*cz)/(per*per));

	}
   }
   return;
}
