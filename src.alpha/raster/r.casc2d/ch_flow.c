#include "all.h"

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
void CH_FLOW(link,node,itmp,vect,vect2,ltype,elev1,elev2,yes_chn_unif,bottom,
	     chn_dep,sslope,strick,hch,dqch,dt)
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

float  *dqch,*hch;
float  *strick;
float  *chn_dep;
float  *bottom;
float  *sslope;
int    *ltype;

int     itmp,vect,vect2;			
int     link,node;
int     yes_chn_unif;
double  dt;
double  elev1,elev2;

{

   double  s0,dhdx,sf,delh;
   double  depth1,depth2,depth,width,z,man;
   double  htmp,wetper,area,dq;
   double  pow(),sqrt(),base,power;

   depth1=chn_dep[node+link*NODES];
   if(itmp!=link) 
      {
      depth2=chn_dep[1+itmp*NODES];     
      }
   else
      {
      depth2=chn_dep[node+link*NODES+1];     
      }

   s0=(elev1-depth1-elev2+depth2)/w;
   if(itmp!=link) 
      {
      dhdx=(hch[1+itmp*NODES]-hch[node+link*NODES])/w;
      }
   else 
      {
      dhdx=(hch[node+link*NODES+1]-hch[node+link*NODES])/w;
      }
  
   sf=s0-dhdx;
   if(itmp!=link && ltype[itmp]==4) sf=((elev1-depth1+hch[node+link*NODES])-
				     lake_el[lake_cat[vect2]])/w;
				   
   if(abs(sf)<1e-20) return;
   if(itmp!=link && ltype[itmp]==4 && sf<0) 
      {
      htmp=hch[node+link*NODES];
      width=bottom[node+link*NODES];
      depth=chn_dep[node+link*NODES];
      z=sslope[node+link*NODES];
      if(htmp<depth && z>1e-6) 
         {
         delh=(sqrt(pow((double)(width+2.*z*htmp),
               (double)2.0)+4.*z*w*abs(sf)*w)-(width+2.*z*htmp))/(2.*z);
         }
      else
         {
         if(htmp>depth) delh=(w*abs(sf)*w)/(w+width+2.*z*depth);
         if(htmp<depth) delh=(w*abs(sf)*w)/(w+width+2.*z*htmp);
         }
      dq=(abs(sf)*w-delh)*w*w/dt;   
      dqch[node+link*NODES]=dqch[node+link*NODES]+dq;
      qtolake[lake_cat[vect2]]=qtolake[lake_cat[vect2]]-dq;
      return;
      }

  if(sf > 0) 
      {
      htmp=hch[node+link*NODES];
      width=bottom[node+link*NODES];     
      depth=chn_dep[node+link*NODES];     
      z=sslope[node+link*NODES];     
      man=1./strick[node+link*NODES];     
      }
   else 
      {
      if(itmp==link) 
         {
         htmp=hch[node+link*NODES+1];
         width=bottom[node+link*NODES+1];     
         depth=chn_dep[node+link*NODES+1];     
         z=sslope[node+link*NODES+1];     
         man=1./strick[node+link*NODES+1];     
         }
      else 
         {
         htmp=hch[1+itmp*NODES];
         width=bottom[1+itmp*NODES];     
         depth=chn_dep[1+itmp*NODES];     
         z=sslope[1+itmp*NODES];     
         man=1./strick[1+itmp*NODES];     
         }
      }

   if(htmp <= depth) 
      {
      wetper=width+2.*htmp*sqrt((double)(1.+z*z));
      area=htmp*(width+z*htmp);
      }
   else 
      {
      wetper=width+2.*depth*sqrt((double)(1.+z*z));
      area=depth*(width+z*depth)+(htmp-depth)*(width+2.*z*depth);
      }

   dq=sign(sf)*(1./man)*pow(base=area,power=1.667)*
      pow(base=(1./wetper),power=0.667)*sqrt(abs(sf));
   dqch[node+link*NODES]=dqch[node+link*NODES]-dq;

   if(itmp!=link) 
      {
      dqch[1+itmp*NODES]=dqch[1+itmp*NODES]+dq;
      if(ltype[itmp]==4) qtolake[lake_cat[vect2]]=qtolake[lake_cat[vect2]]+dq;
      }
   else 
      {
      dqch[node+link*NODES+1]=dqch[node+link*NODES+1]+dq;
      }
   
   return;
}

