#include <math.h>
#include "global.h"

static double affin_index[] =
  {0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.12,0.14,0.16,
   0.18,0.20,0.25,0.30,0.40,0.50,1.0
   };


int next_same(int layer, int cluster, int index,int *nsame)
{
    CELL ai,bi;
    double a, b;
    long d1,d2;
    int i;
    int same;
    int type;

type=types[layer]; 

if (type==1) {   /* quantitative data  */
    ai = stats[index].greyv;
    bi = stats[index+1].greyv;
    a=(double) ai;
    b=(double) bi;
    a-=means[layer][cluster];
    b-=means[layer][cluster];
    if  (fabs(a)!=fabs(b)) return (0);
 
    d1=0;
    d2=0;
    
    if (ai<bi) 
      {
      for (i=0;i<val_num[layer];i++) { 
         if (stats[i].greyv<=ai) d1+=stats[i].count_c;
         if (stats[i].greyv>=bi) d2+=stats[i].count_c;    
        }
      }
    else {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=bi) d2+=stats[i].count_c;
         if (stats[i].greyv>=ai) d1+=stats[i].count_c;    
         }
      }  
    if (d1!=d2) return (0);
    *nsame=1;
    return (1);

   }
 else if (type==2){   /* quanlitative data   */
    same=0;
    for(i=index+1;i<val_num[layer];i++) 
        if (stats[i].count_c==stats[index].count_c)
                       same++;
        else break;
    if(same==0) return (0);
    *nsame=same;
    return (1);

   }
 else {               /* ranked data  */
   ai = stats[index].greyv;;
   bi = stats[index+1].greyv;;
 
   a = stats[index].greyv;
   b = stats[index+1].greyv;
   a-=(int)means[layer][cluster];
   b-=(int)means[layer][cluster];
   if  (fabs(a)!=fabs(b)) return (0);
   d1=0;
   d2=0;
    
   if (ai<bi) 
      {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=ai) d1+=stats[i].count_c;
         if (stats[i].greyv>=bi) d2+=stats[i].count_c;    
         }
      }
   else {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=bi) d2+=stats[i].count_c;
         if (stats[i].greyv>=ai) d1+=stats[i].count_c;    
         }
      }  
   if (d2!=d1) return (0);
   *nsame=1;
   return (1);
  
  }  


}



classify (class, reject, ncols)
    CELL *class;
    CELL *reject;

{
    int i,j;
    int nfiles;
    int band;
    int col;
    double minp;
    int minprn;

    double gammq(double,double);


    /* Tian */
    CELL cellv;
    double pi[MAX_BANDNUMBER];
    int greyi;
    double sump[MAX_CLUSTERS];
    double xx,PP;
    int rn;
    double logp,tempd;
    int same;
    long pos;
    int all_zero;

    nfiles = Ref.nfiles;

    for (col = 0; col < ncols; col++)
    {   
        all_zero=1;
        for(i=0;i<nfiles;i++) 
	     if (cell[i][col]!=0) {
		all_zero=0;
                break;
                }
	if (all_zero)	/* all zeros - becomes category 0 */
	{
	    *class++ = 0;
	    if (reject)
		*reject++ = 0;
	    continue;
	}

        for (rn = 0; rn < clusternum; rn++)	
        {    
  	  sump[rn]=0.0;
            	 
          for (band = 0; band < nfiles; band++){

              cellv = cell[band][col];
              pi[band] = 0.0;

              if (cellv==0) continue;

              greyi=0;
              pos=((long)rn)*((long)val_num[band]);

              switch (band){
                case 0:
                   stats=&(statsb0[pos]);
                   break;
                  
                 case 1:
                   stats=&(statsb1[pos]);             
                   break;

                 case 2:
                   stats=&(statsb2[pos]);
                   break;
                 case 3:
                   stats=&(statsb3[pos]);
                   break;

                 case 4:
                   stats=&(statsb4[pos]);
                   break;

                 case 5:
                   stats=&(statsb5[pos]);
                   break;
                 case 6:
                   stats=&(statsb6[pos]);
                   break;

                }  /* switch */

             while  (stats[greyi].greyv!=cellv){
                       pi[band]+=stats[greyi].count_all;
                       greyi++;
                      }
         
             pi[band]+=stats[greyi].count_all;
          
             if (next_same(band,rn,greyi,&same)) 
                        for(i=0;i<same;i++)
                             pi[band]+=stats[greyi+i+1].count_all;  
            
             pi[band]=pi[band]/pixelnum[band];

 
            }  /* for band */

 
           xx=0.0; 
   
           for (band = 0; band < nfiles; band++){
                if (pi[band]==0.0) continue;
                logp=log(pi[band]);
                xx+= logp;
             } 

           xx=(-1.0)*xx;


           logp=(double)nfiles;

/*fprintf(stderr,"xx: %f",xx);*/

            if (xx==0.0) PP=1.0;
            else PP=gammq(logp,xx);
 
/*fprintf(stderr,"PP: %lf",PP);*/
    
           sump[rn]=PP;

        } /* for rn */   
      
      /* decide to which the pixel will be assined  */
        minp=sump[0];
        minprn=0;
        for (rn=1; rn <clusternum; rn++)
           if (sump[rn]<minp) {
                minp=sump[rn];
                minprn=rn;
             }
        
        
        *class++ =cluster_val[ minprn];
     

    	if (reject)
	{
	    for (i = 0; i < 20; i++)
		if (minp <= affin_index[i])
		    break;
	    *reject++ = i+1;
	}

      } /* for col */

}

 
               
 
