
#include "all.h"
	  
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void INF_NODIST(itime,vect,dt,vinf,frate,yes_w_inf_rate,
		K,H,P,M,hov)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

int   itime;
double dt;
int   vect;
int   yes_w_inf_rate;

float *K,*H,*P,*M,*vinf,*frate;

double *hov;

{
double  smd,p1,p2,rinf,sqrt();

if((*hov) < 1e-10) {
   if(yes_w_inf_rate) frate[vect]=0;
   return;
}

smd=P[vect]-M[vect];

p1=K[vect]*dt-2.*vinf[vect];
p2=K[vect]*(vinf[vect]+H[vect]*smd);
rinf=(p1+sqrt(p1*p1+8.*p2*dt))/(2.*dt);
if(((*hov)/dt)<=rinf) {
  rinf=(*hov)/dt;
  (*hov)=0.;
  }
else {
  (*hov)=(*hov)-rinf*dt;
  }

vinf[vect]=vinf[vect]+rinf*dt;
if(yes_w_inf_rate) frate[vect]=rinf;

}
