#include <math.h>
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/imagery.h>
#include "local_proto.h"

#define PI M_PI
#define ZERO 1e-10
#define SMALLEST_SUBCLUST 1

static void seed();
static double refine_clusters();
static int reestimate();
static double regroup();
static void reduce_order();
static double loglike();
static double distance();
static int compute_constants();
static void add_SubSigs();
static void copy_ClassSig();
static void copy_SubSig();
static void list_Sig();
static void print_class();

static int total_nulls, *n_nulls;

void 
subcluster (struct SigSet *S, int Class_Index, int *Max_num, int maxsubclasses)
{
    int nparams_clust;
    int ndata_points;
    int min_i,min_j;
    int nbands;
    double rissanen;
    double min_riss;
    struct ClassSig *Sig;

    static int first=1;
    static struct SigSet min_S;
    static struct ClassSig *min_Sig;

    /* set class pointer */
    Sig = &(S->ClassSig[Class_Index]);

    /* set number of bands */
    nbands = S->nbands;

    /* allocate scratch class first time subroutine is called */
    if(first)
    {
      int i;
      I_InitSigSet (&min_S);
      I_SigSetNBands (&min_S, nbands);
      min_Sig = I_NewClassSig(&min_S);

      /* allocate enough subsignatures in scratch space */
      for (i = 0; i < maxsubclasses; i++)
	  I_NewSubSig (&min_S, min_Sig);

      first = 0;
    }

    /* compute number of parameters per cluster */
    nparams_clust = 1+nbands+0.5*(nbands+1)*nbands;

    /* compute number of data points */
    ndata_points = Sig->ClassData.npixels*nbands - total_nulls;
    if(ndata_points <= 1)
       G_fatal_error("Not enough data points!");

    /* Check for too few pixels */
    *Max_num = (ndata_points+1)/nparams_clust - 1;
    if (maxsubclasses > *Max_num / 2)
	maxsubclasses = *Max_num / 2;
    if (maxsubclasses < 1)
    {
      fprintf(stderr, "\nWarning: Not enough pixels in class %d\n",Class_Index+1);
      Sig->nsubclasses = 0;
      Sig->used = 0;
      return;
    }

    /* check for too many subclasses */
    if(Sig->nsubclasses > maxsubclasses )
    {
      Sig->nsubclasses = maxsubclasses;
      fprintf(stderr, "\nWarning: Too many subclasses for class index %d\n",Class_Index+1);
      fprintf(stderr, "         number of subclasses set to %d\n\n",Sig->nsubclasses);
    }


    /* initialize clustering */
    seed(Sig,nbands);

    /* EM algorithm */
    min_riss = refine_clusters(Sig,nbands);
    fprintf(stderr, "Subclasses = %d; Rissanen = %f; ",Sig->nsubclasses,min_riss);
    copy_ClassSig(Sig,min_Sig,nbands);
    while(Sig->nsubclasses>1) {
      reduce_order(Sig,nbands,&min_i,&min_j); 
      fprintf(stderr, "Combining Subclasses (%d,%d)\n",min_i+1,min_j+1);

      rissanen = refine_clusters(Sig,nbands);
      fprintf(stderr, "Subclasses = %d; Rissanen = %f; ",Sig->nsubclasses,rissanen);
      if(rissanen<min_riss)
      {
        min_riss = rissanen;
        copy_ClassSig(Sig,min_Sig,nbands);
      }
    }

    copy_ClassSig(min_Sig,Sig,nbands);
}


static void 
seed (struct ClassSig *Sig, int nbands)
{
     int     i,b1,b2;
     double  period;
     double  *mean,**R;

     /* Compute the mean of variance for each band */
     mean = G_alloc_vector(nbands);
     R = G_alloc_matrix(nbands,nbands);
     n_nulls = (int *) G_calloc(nbands, sizeof(int));

     total_nulls = 0;
     for(b1=0; b1<nbands; b1++) {
       n_nulls[b1] = 0;
       mean[b1] = 0.0;
       for(i=0; i<Sig->ClassData.npixels; i++) {
	 if(G_is_d_null_value(& Sig->ClassData.x[i][b1]))
	 {
	    n_nulls[b1]++;
	    total_nulls++;
         }
         else
         mean[b1] += Sig->ClassData.x[i][b1];
       }
       mean[b1] /= (double)(Sig->ClassData.npixels - n_nulls[b1]);
     }

     for(b1=0; b1<nbands; b1++) 
     for(b2=0; b2<nbands; b2++) {
       R[b1][b2] = 0.0;
       for(i=0; i<Sig->ClassData.npixels; i++) {
	 if(!G_is_d_null_value(&Sig->ClassData.x[i][b1]) &&
	    !G_is_d_null_value(&Sig->ClassData.x[i][b2]))
            R[b1][b2] += (Sig->ClassData.x[i][b1])*(Sig->ClassData.x[i][b2]);
       }
       R[b1][b2] /= (double) (Sig->ClassData.npixels-n_nulls[b1]- n_nulls[b2]);
       R[b1][b2] -= mean[b1]*mean[b2];
     }

     /* Compute the sampling period for seeding */
     if(Sig->nsubclasses>1) {
       period = (Sig->ClassData.npixels-1)/(Sig->nsubclasses-1.0);
     }
     else period =0;


     /* Seed the means and set the diagonal covariance components */
     for(i=0; i<Sig->nsubclasses; i++) {
       for(b1=0; b1<nbands; b1++) {
	 if(G_is_d_null_value(&Sig->ClassData.x[(int)(i*period)][b1]))
	    G_set_d_null_value(&Sig->SubSig[i].means[b1], 1);
         else
            Sig->SubSig[i].means[b1] = Sig->ClassData.x[(int)(i*period)][b1];
       }

       for(b1=0; b1<nbands; b1++)
       for(b2=0; b2<nbands; b2++) {
         Sig->SubSig[i].R[b1][b2] = R[b1][b2];
       }
       Sig->SubSig[i].pi = 1.0/Sig->nsubclasses;
     }

     G_free_vector(mean);
     G_free_matrix(R);

     compute_constants(Sig,nbands);
}


static double 
refine_clusters (
/* Computes ML clustering of data using Gaussian Mixture model.  */
/* Returns the values of the Rissen constant for the clustering. */
/* If all clusters are singular, the Sig data structure is       */
/* returned with Sig->nsubclasses==0 .                           */
    struct ClassSig *Sig,
    int nbands
)
{
     int nparams_clust;
     int num_params;
     int ndata_points;
     int singular;
     int repeat=0;
     double rissanen_const;
     double change,ll_new,ll_old;
     double epsilon;

     /* compute number of parameters per cluster */
     nparams_clust = 1+nbands+0.5*(nbands+1)*nbands;

     /* compute number of data points */
     ndata_points = Sig->ClassData.npixels*nbands - total_nulls;

     /* compute epsilon */
     epsilon = nparams_clust*log((double)ndata_points);
     epsilon *= 0.01;

     /* Perform initial regrouping */
     ll_new = regroup(Sig,nbands);

     /* Perform EM algorithm */
     change = 2*epsilon;
     do {
       ll_old = ll_new;
       singular = reestimate(Sig,nbands);

       if(singular==0) {
         ll_new = regroup(Sig,nbands);
         change = ll_new-ll_old;
         repeat = change>epsilon;
       }
       if(singular==1) {
         ll_new = regroup(Sig,nbands);
         repeat = 1;
       }
       if(singular==2) {
         repeat = 0;
       }

     } while(repeat);

     /* compute Rissanens expression */
     if(Sig->nsubclasses>0) {
       num_params = Sig->nsubclasses*nparams_clust - 1;
       rissanen_const = -ll_new + 0.5*num_params*log((double)ndata_points);
       rissanen_const /= ndata_points;
       return(rissanen_const);
     }
     else {
       return((double)0);
     }
}


static int 
reestimate (struct ClassSig *Sig, int nbands)
{
     int i;
     int s;
     int b1,b2;
     int singular;
     double pi_sum;
     double diff1,diff2;
     struct ClassData *Data;

     /* set data pointer */
     Data = &(Sig->ClassData);
     
     /* Compute N */
     for(i=0; i<Sig->nsubclasses; i++) 
     {
       Sig->SubSig[i].N = 0;
       for(s=0; s<Data->npixels; s++)
         Sig->SubSig[i].N += Data->p[s][i];
       Sig->SubSig[i].pi = Sig->SubSig[i].N;
     }




     /* Compute means and variances for each subcluster, */
     /* and remove small clusters.                       */
     for(i=0; i<Sig->nsubclasses; i++) 
     {
       /* For large subclusters */
       if(Sig->SubSig[i].N>SMALLEST_SUBCLUST)
       {
       /* Compute mean */
         for(b1=0; b1<nbands; b1++) 
         {
           Sig->SubSig[i].means[b1] = 0;
           for(s=0; s<Data->npixels; s++)
 	      if(!G_is_d_null_value(&Data->x[s][b1]))
		 Sig->SubSig[i].means[b1] += Data->p[s][i]*Data->x[s][b1];
           Sig->SubSig[i].means[b1] /= (Sig->SubSig[i].N);
         }
	
       /* Compute R */
         for(b1=0; b1<nbands; b1++) 
         for(b2=b1; b2<nbands; b2++)
         {
           Sig->SubSig[i].R[b1][b2] = 0;
           for(s=0; s<Data->npixels; s++)
           {
	     if(!G_is_d_null_value(&Data->x[s][b1])
		&& !G_is_d_null_value(&Data->x[s][b2]))
	     {
                diff1 = Data->x[s][b1] - Sig->SubSig[i].means[b1];
                diff2 = Data->x[s][b2] - Sig->SubSig[i].means[b2];
                Sig->SubSig[i].R[b1][b2] += Data->p[s][i]*diff1*diff2;
	     }
           }
           Sig->SubSig[i].R[b1][b2] /= (Sig->SubSig[i].N);
           Sig->SubSig[i].R[b2][b1] = Sig->SubSig[i].R[b1][b2];
         }
       }
       /* For small subclusters */
       else
       {
         fprintf(stderr, "Warning: subsignature %d only contains ",i);
         fprintf(stderr, " %f pixels\n", Sig->SubSig[i].N);

         Sig->SubSig[i].pi = 0;

         for(b1=0; b1<nbands; b1++) 
           Sig->SubSig[i].means[b1] = 0;

         for(b1=0; b1<nbands; b1++) 
         for(b2=0; b2<nbands; b2++)
           Sig->SubSig[i].R[b1][b2] = 0;
       }	
     }


     /* Normalize probabilities for subclusters */
     pi_sum = 0;
     for(i=0; i<Sig->nsubclasses; i++) pi_sum += Sig->SubSig[i].pi;
     if(pi_sum>0) {
       for(i=0; i<Sig->nsubclasses; i++) Sig->SubSig[i].pi /= pi_sum;
     }
     else {
       for(i=0; i<Sig->nsubclasses; i++) Sig->SubSig[i].pi = 0;
     }


     /* Compute constants and reestimate if any singular subclusters occur */
     singular = compute_constants(Sig,nbands);
     return(singular);
}


static double 
regroup (struct ClassSig *Sig, int nbands)
{
   int s;
   int i;
   double tmp;
   double maxlike= 0.0;
   double likelihood;
   double subsum;
   struct ClassData *Data;

   /* set data pointer */
   Data = &(Sig->ClassData);

   /* compute likelihoods */
   likelihood = 0;
   for(s=0; s<Data->npixels; s++)
   {

     for(i=0; i<Sig->nsubclasses; i++)
     {
       tmp = loglike(Data->x[s],&(Sig->SubSig[i]),nbands);
       Data->p[s][i] = tmp;
       if(i==0) maxlike = tmp;
       if(tmp>maxlike) maxlike = tmp;
     }

     subsum = 0;
     for(i=0; i<Sig->nsubclasses; i++)
     {
       tmp = exp( Data->p[s][i]-maxlike )*Sig->SubSig[i].pi;
       subsum += tmp;
       Data->p[s][i] = tmp;
     }
     likelihood += log(subsum) + maxlike;

     for(i=0; i<Sig->nsubclasses; i++)
       Data->p[s][i] /= subsum;
   }

   return(likelihood);
}


static void 
reduce_order (struct ClassSig *Sig, int nbands, int *min_ii, int *min_jj)
{
    int i,j;
    int min_i=0,min_j=0;
    double dist;
    double min_dist=0;
    struct SubSig *SubSig1,*SubSig2;

    static int first=1;
    struct SigSet S;
    static struct ClassSig *Sig3;
    static struct SubSig *SubSig3;

    /* allocate scratch space first time subroutine is called */
    if(first)
    {
      I_InitSigSet (&S);
      I_SigSetNBands (&S, nbands);
      Sig3 = I_NewClassSig(&S);
      I_NewSubSig (&S, Sig3);
      SubSig3 = Sig3->SubSig;
      first = 0;
    }

    if(Sig->nsubclasses>1)
    {
      /* find the closest subclasses */
      for(i=0; i<Sig->nsubclasses-1; i++)
      for(j=i+1; j<Sig->nsubclasses; j++)
      {
        dist = distance(&(Sig->SubSig[i]),&(Sig->SubSig[j]),nbands);
        if((i==0)&&(j==1))
        {
          min_dist = dist;
          min_i = i;
          min_j = j;
        }
        if(dist<min_dist)
        {
          min_dist = dist;
          min_i = i;
          min_j = j;
        }
      }

      *min_ii = min_i; *min_jj = min_j;

      /* Combine Subclasses */
      SubSig1 = &(Sig->SubSig[min_i]);
      SubSig2 = &(Sig->SubSig[min_j]);
      add_SubSigs(SubSig1,SubSig2,SubSig3,nbands);
      compute_constants(Sig3,nbands);
      copy_SubSig(SubSig3,SubSig1,nbands);

      /* remove extra subclass */
      for(i=min_j; i<Sig->nsubclasses-1; i++)
        copy_SubSig(&(Sig->SubSig[i+1]),&(Sig->SubSig[i]),nbands);

      /* decrement number of Subclasses */
      Sig->nsubclasses--;
    }
}


static double 
loglike (DCELL *x, struct SubSig *SubSig, int nbands)
{
    int b1,b2;
    double diff1,diff2;
    double sum;

    sum = 0;
    for(b1=0; b1<nbands; b1++) 
    for(b2=0; b2<nbands; b2++)
    {
      if(G_is_d_null_value(&x[b1])
        ||G_is_d_null_value(&x[b2]))
           continue;
      diff1 = x[b1]-SubSig->means[b1];
      diff2 = x[b2]-SubSig->means[b2];
      sum += diff1*diff2*SubSig->Rinv[b1][b2];
    }

    sum = -0.5*sum + SubSig->cnst;
    return(sum);
}


static double 
distance (struct SubSig *SubSig1, struct SubSig *SubSig2, int nbands)
{
    double dist;

    static int first=1;
    struct SigSet S;
    static struct ClassSig *Sig3;
    static struct SubSig *SubSig3;


    /* allocate scratch space first time subroutine is called */
    if(first)
    {
      I_InitSigSet (&S);
      I_SigSetNBands (&S, nbands);
      Sig3 = I_NewClassSig(&S);
      I_NewSubSig (&S, Sig3);
      SubSig3 = Sig3->SubSig;
      first = 0;
    }

    /* form SubSig3 by adding SubSig1 and SubSig2 */
    add_SubSigs(SubSig1,SubSig2,SubSig3,nbands);

    /* compute constant for SubSig3 */
    compute_constants(Sig3,nbands);

    /* compute distance */
    dist = SubSig1->N*SubSig1->cnst + SubSig2->N*SubSig2->cnst
           - SubSig3->N*SubSig3->cnst;

    return(dist);
}


static int 
compute_constants (
/* invert matrix and compute Sig->SubSig[i].cnst          */
/* Returns singular=1 if a singluar subcluster was found. */
/* Returns singular=2 if all subclusters were singular.   */
/* When singular=2 then nsubclasses=0.                    */
    struct ClassSig *Sig,
    int nbands
)
{
   int i,j;
   int b1,b2;
   int singular;
   double det;
   double pi_sum;

   static int first=1;
   static int *indx;
   static double **y;
   static double *col;


   /* allocate memory first time subroutine is called */
   if(first)
   {
     indx = G_alloc_ivector(nbands);
     y = G_alloc_matrix(nbands,nbands); 
     col = G_alloc_vector(nbands);
     first = 0;
   }

   /* invert matrix and compute constant for each subclass */
   i=0;
   singular = 0;
   do {
     for(b1=0; b1<nbands; b1++)
     for(b2=0; b2<nbands; b2++)
       Sig->SubSig[i].Rinv[b1][b2] = Sig->SubSig[i].R[b1][b2];

     invert(Sig->SubSig[i].Rinv,nbands,&det,indx,y,col);
     if(det<=ZERO)
     {
       if(Sig->nsubclasses==1)
       {
         Sig->nsubclasses--;
         singular = 2;
         fprintf(stderr,"\nError: unreliable clustering\n");
         fprintf(stderr,"try a smaller initial number of clusters\n");
       }
       else {
         for(j=i; j<Sig->nsubclasses-1; j++)
           copy_SubSig(&(Sig->SubSig[j+1]),&(Sig->SubSig[j]),nbands);
         Sig->nsubclasses--;
         singular = 1;
         fprintf(stderr, "Warning: Removed a singular subsignature; number %d;",i+1);
         fprintf(stderr, " %d remain\n", Sig->nsubclasses);
         if(Sig->nsubclasses < 0)  /* MN added 12/2001: to avoid endless loop */
         	Sig->nsubclasses=1;
       }
     }
     else
     {
       Sig->SubSig[i].cnst = (-nbands/2.0)*log(2*PI) - 0.5*log(det);
       i++;
     }
   } while(i<Sig->nsubclasses);

   /* renormalize pi */
   pi_sum = 0;
   for(i=0; i<Sig->nsubclasses; i++) pi_sum += Sig->SubSig[i].pi;
   for(i=0; i<Sig->nsubclasses; i++) Sig->SubSig[i].pi /= pi_sum;

   return(singular);
}


static void 
add_SubSigs (
/* add SubSig1 and SubSig2 to form SubSig3 */
    struct SubSig *SubSig1,
    struct SubSig *SubSig2,
    struct SubSig *SubSig3,
    int nbands
)
{
    int b1,b2;
    double wt1,wt2;
    double tmp;

    wt1 = SubSig1->N/(SubSig1->N + SubSig2->N);
    wt2 = 1 - wt1;

    /* compute means */
    for(b1=0; b1<nbands; b1++)
      SubSig3->means[b1] = wt1*SubSig1->means[b1] + wt2*SubSig2->means[b1];

    /* compute covariance */
    for(b1=0; b1<nbands; b1++)
    for(b2=b1; b2<nbands; b2++)
    {
      tmp = (SubSig3->means[b1]-SubSig1->means[b1])
            *(SubSig3->means[b2]-SubSig1->means[b2]);
      SubSig3->R[b1][b2] = wt1*(SubSig1->R[b1][b2] + tmp);
      tmp = (SubSig3->means[b1]-SubSig2->means[b1])
            *(SubSig3->means[b2]-SubSig2->means[b2]);
      SubSig3->R[b1][b2] += wt2*(SubSig2->R[b1][b2] + tmp);
      SubSig3->R[b2][b1] = SubSig3->R[b1][b2];
    }

    /* compute pi and N */
    SubSig3->pi = SubSig1->pi + SubSig2->pi;
    SubSig3->N = SubSig1->N + SubSig2->N;
}

static void 
copy_ClassSig (
/* copy Sig1 to Sig2 */
    struct ClassSig *Sig1,
    struct ClassSig *Sig2,
    int nbands
)
{
    int i;

    Sig2->classnum = Sig1->classnum;
    Sig2->title = Sig1->title;
    Sig2->used = Sig1->used;
    Sig2->type = Sig1->type;
    Sig2->nsubclasses = Sig1->nsubclasses;
    for(i=0; i<Sig1->nsubclasses; i++)
      copy_SubSig(&(Sig1->SubSig[i]),&(Sig2->SubSig[i]),nbands);
}


static void 
copy_SubSig (
/* copy SubSig1 to SubSig2 */
    struct SubSig *SubSig1,
    struct SubSig *SubSig2,
    int nbands
)
{
    int b1,b2;

    SubSig2->N = SubSig1->N;
    SubSig2->pi = SubSig1->pi;
    SubSig2->cnst = SubSig1->cnst;
    SubSig2->used = SubSig1->used;

    for(b1=0; b1<nbands; b1++)
      SubSig2->means[b1] = SubSig1->means[b1];

    for(b1=0; b1<nbands; b1++)
    for(b2=0; b2<nbands; b2++)
    {
      SubSig2->R[b1][b2] = SubSig1->R[b1][b2];
      SubSig2->Rinv[b1][b2] = SubSig1->Rinv[b1][b2];
    }
}

static void list_Sig (struct ClassSig *Sig, int nbands)
{
    int i,j,k;

    for(i=0; i<Sig->nsubclasses; i++)
    {
      fprintf(stderr, "Subclass %d: pi = %f, ",i+1,Sig->SubSig[i].pi);
      fprintf(stderr, "cnst = %f\n",Sig->SubSig[i].cnst);
      for(j=0; j<nbands; j++)
      {
        fprintf(stderr, "%f;    ",Sig->SubSig[i].means[j]);
        for(k=0; k<nbands; k++)
          fprintf(stderr, "%f ",Sig->SubSig[i].R[j][k]);
        fprintf(stderr, "\n");
      }
      fprintf(stderr, "\n");
    }
}


static void print_class (struct ClassSig *Sig, char *fname)
{
    FILE *fp;
    int s,i;


    if((fp=fopen(fname,"w"))==NULL) {
      fprintf(stderr,"can't open data file\n"); exit(1);}

    for(s=0; s<Sig->ClassData.npixels; s++)
    {
      /* fprintf(fp,"Pixel number %d:  ", s); */
      for(i=0; i<Sig->nsubclasses; i++)
        fprintf(fp,"%f  ",Sig->ClassData.p[s][i]);
      fprintf(fp,"\n");
    }

    fclose(fp);

}
