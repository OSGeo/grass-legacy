#include "global.h"

int Node_stats_compare(p,q)
    struct Node_stats  *p;
    struct Node_stats  *q;
    
{
    CELL ai,bi;
    double a, b;
    long d1,d2;
    int i;
    int type;


 type=types[layernum];
/* printf("\n in compare mean: %f", mean); */
 if (type==1) {
    ai = p->greyv;
    bi = q->greyv;
    a=(double) ai;
    b=(double) bi;
    a-=mean;
    b-=mean;
    if  (fabs(a)>fabs(b)) return (1);
    if  (fabs(a)<fabs(b)) return (-1);
    d1=0.0;
    d2=0.0;
   
    if (ai<bi) 
      {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=ai) d1+=stats[i].count_c;
         if (stats[i].greyv>=bi) d2+=stats[i].count_c;    
         }
      if (d1>d2) return (-1);
      if (d1<d2) return (1); 
      return (0);
      }
    else {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=bi) d2+=stats[i].count_c;
         if (stats[i].greyv>=ai) d1+=stats[i].count_c;    
         }
      if (d2>d1) return (1);
      if (d2<d1) return (-1);
      return (0);
      }  
    return 0;
   }
 else if (type==2){
  if (p->count_c > q->count_c) return (-1);
  if (p->count_c < q->count_c) return (1);
  return (0);

   }
 else {
   ai = p->greyv;
   bi = q->greyv;
   a = p->greyv;
   b = q->greyv;
   a-=(int)mean;
   b-=(int)mean;
    if  (fabs(a)>fabs(b)) return (1);
    if  (fabs(a)<fabs(b)) return (-1);
    d1=0;
    d2=0;
    if (ai<bi) 
      {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=ai) d1+=stats[i].count_c;
         if (stats[i].greyv>=bi) d2+=stats[i].count_c;    
         }
      if (d1>d2) return (-1);
      if (d1<d2) return (1); 
      return (0);
      }
    else {
      for (i=0;i<val_num[layernum];i++) { 
         if (stats[i].greyv<=bi) d2+=stats[i].count_c;
         if (stats[i].greyv>=ai) d1+=stats[i].count_c;    
         }
      if (d2>d1) return (1);
      if (d2<d1) return (-1);
      return (0);
      }  
    return 0;

  
  }  
    
 }



sort_all_stats()

{

 int rn,i,tempi;
 double sum,tempd;
 int countc;
 long pos;
 double mean1;

 for (rn=0;rn<clusternum;rn++){
     switch (Ref.nfiles) {

      case 7:
            layernum=6;
            pos=((long)rn)*((long)val_num[6]);

            if (types[6]==1) {
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[6];i++){
                        tempd=(double)(statsb6[pos+i].count_c*statsb6[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb6[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[6]==3)  {
                 
                 mean1=statsb6[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[6];i++){   
                   if(statsb6[pos+i].count_c>mean1) {
                       mean1=statsb6[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb6[pos+tempi].greyv;

             }
            
           /*  printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb6[pos]);
             qsort (&(statsb6[pos]), val_num[6], sizeof(struct Node_stats), Node_stats_compare);

      case 6: 
            layernum=5;
            pos=((long)rn)*((long)val_num[5]);

            if (types[5]==1) {
                   
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[5];i++){
                        tempd=(double)(statsb5[pos+i].count_c*statsb5[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb5[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[5]==3) {
               
                 mean1=statsb5[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[5];i++){   
                   if(statsb5[pos+i].count_c>mean1) {
                       mean1=statsb5[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb5[pos+tempi].greyv;

             }

           /* printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
       mean=means[layernum][rn];
       stats=&(statsb5[pos]);
       qsort (&(statsb5[pos]),val_num[5], sizeof(struct Node_stats), Node_stats_compare);
      case 5: 
            layernum=4;
            pos=((long)rn)*((long)val_num[4]);
            if (types[4]==1) {
                    
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[4];i++){
                        tempd=(double)(statsb4[pos+i].count_c*statsb4[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb4[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[4]==3)  {
                 
                 mean1=statsb4[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[4];i++){   
                   if(statsb4[pos+i].count_c>mean1) {
                       mean1=statsb4[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb4[pos+tempi].greyv;

             }
             

          /*    printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb4[pos]);
             qsort (&(statsb4[pos]),val_num[4], sizeof(struct Node_stats), Node_stats_compare);
      case 4: 
            layernum=3;
            pos=((long)rn)*((long)val_num[3]);

            if (types[3]==1) {
                    
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[3];i++){
                        tempd=(double)(statsb3[pos+i].count_c*statsb3[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb3[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[3]==3)  {
                 
                 mean1=statsb3[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[3];i++){   
                   if(statsb3[pos+i].count_c>mean1) {
                       mean1=statsb3[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb3[pos+tempi].greyv;

             }
             
             /* printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb3[pos]);             
             qsort (&(statsb3[pos]), val_num[3], sizeof(struct Node_stats), Node_stats_compare);
      case 3: 
            layernum=2;
            pos=((long)rn)*((long)val_num[2]);   

            if (types[2]==1) {
                    
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[2];i++){
                        tempd=(double)(statsb2[pos+i].count_c*statsb2[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb2[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[2]==3)  {
                 
                 mean1=statsb2[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[2];i++){   
                   if(statsb2[pos+i].count_c>mean1) {
                       mean1=statsb2[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb2[pos+tempi].greyv;

             }
          /*    printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb2[pos]);            
             qsort (&(statsb2[pos]),val_num[2], sizeof(struct Node_stats), Node_stats_compare);
      case 2: 

            layernum=1;
            pos=((long)rn)*((long)val_num[1]); 

            if (types[1]==1) {
                    
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[1];i++){
                        tempd=(double)(statsb1[pos+i].count_c*statsb1[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb1[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[1]==3)  {
                 
                 mean1=statsb1[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[1];i++){   
                   if(statsb1[pos+i].count_c>mean1) {
                       mean1=statsb1[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb1[pos+tempi].greyv;

             }   
             
            /*  printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb1[pos]);         
             qsort (&(statsb1[pos]), val_num[1], sizeof(struct Node_stats), Node_stats_compare);
      case 1: 
            layernum=0;
            pos=((long)rn)*((long)val_num[0]); 
            if (types[0]==1) {
                   
                    sum=0.0;
                    countc=0;
                    for(i=0;i<val_num[0];i++){
                        tempd=(double)(statsb0[pos+i].count_c*statsb0[pos+i].greyv);                      
                        sum+=tempd; 
                        countc+=statsb0[pos+i].count_c;    
                     }
                     tempd=(double)countc;
                     means[layernum][rn]=sum/tempd;
                 }
             else if (types[0]==3)  {
                 
                 mean1=statsb0[pos].count_c;
                 tempi=0;
                 i=0;
                 for(i=1;i<val_num[0];i++){   
                   if(statsb0[pos+i].count_c>mean1) {
                       mean1=statsb0[pos+i].count_c;
                       tempi=i;
                     }
                   }
                  means[layernum][rn]=statsb0[pos+tempi].greyv;
             }           
          /*   printf("\nsorting for layer:%d and cluster:%d", layernum,rn); */
             mean=means[layernum][rn];
             stats=&(statsb0[pos]);
             qsort (&(statsb0[pos]), val_num[0], sizeof(struct Node_stats), Node_stats_compare);
      }
    
  }
 

}
