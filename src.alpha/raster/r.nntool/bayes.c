/*----------------------------------------------------------------------
This is a linear classifier.  The algorithm is described in Fukunaga's 
book [Eqns 4-15, 4-25 etc].  The algorithm briefly is as follows:
-- I strongly suggest that you see Fukunaga's book if you want to know
   more about whitening, diagonalizing, and classifiers.
0. The data is in vector X.
1. Find the mean value vector, M_1, M_2.
2. Find the variance-covariance matrix, Cov_1, Cov_2.
3. If interested in simultaneous diagonalization, then do it as:
		3.1 [Cov_1]^-1 [Cov_2] PSI^T = PSI LAMBDA
		3.2 Z = PSI^T X
		3.3 Cov_1z = I;	Cov_2z = LAMBDA.
4. Find the V matrix:
	V = [ 1/2  Cov_1z + 1/2 Cov_2z ]^-1 (M_1z - M_2z)
	sigma_1 = V^T(Cov_1z)V
	sigma_2 = V^T(Cov_2z)V
5. Find V_0 for s = 1/2 (Fisher Criteria):
               sigma_1^2 V^T M_1z + (1 - s) sigma_2^2 V^T M_2z }
       V_0 = ----------------------------------------------------
			S*sigma_1^2 + (1 - s) sigma_2^2
                      >                 omega_1
6. h(Z) = V^T*Z + V_0    0      Z member 
		      <                 omega_2

NOTE: IF YOU WANT TO CHANGE THE RESOLUTION TOO MUCH YOU HAVE TO ADJUST
      THE DIMENSIONS FOR THE ARRAYS!

      The divide by 90 and 10 for columns and rows rule is only assumed
      for 30 meter resolution.  For higher resolutions, don't use it!

NEED TO REDO FOR MULTICLASS CASE !!! -- consult Fukunaga!  Go with
k-nearest neighbor classification since it is easy to perform !!

------------------------------------------------------------------------*/
#include <stdio.h>
#include <math.h>
#include "/usr2/grass4.0/src/libes/gis.h"
#define NO 0
#define YES 1
#define skip 1    /* The sample from everyother "skip" row */

bayes() {
float X_1[1000][10],X_2[1000][10]; /*Training data from class 1 and 2*/
float X_in[10000][10], S_A, S_Aprime; /*Input data, and category weighting*/
int minin[10], maxin[10], kk; /*Min. and max. values from each map layer*/
int fd[10],row,col,getrow,i,getavg,k,nca; /*files and get data stuff */
int WEIGHT,FOUND,sum[10][40]; /* sum - used only for averaging */
int T_1[50],T_2[50],ncols,DIAG,fisher; /* Training areas for each class */
int getmask,getin,bayes; /* # to divide rows and colummns*/
struct ar {   /* data structure for value and count of each category */
	int num;
	int cnt;
};
struct ar A_T_1[20][25],A_T_2[20][25],AREA_I[20][25];/*Training areas */
long *pcell_1, *pavg, *pmask, *pcell_2;
int qstrt, qend,loop_1,nonloop_1=0,loopcnt_1,CONT_1=YES,CONT_2=YES;/* Loops */
int numfiles, ii,ct_1=0,ct_2=0; /* Cell counts and number of files */
int nrows,ncol,fmb,fmg,rcnt_1,rcnt_2,I_A[20],k_in;/* row counts */
int j,q,rt,clnt,no_in,rcnt=0,loop_2,nonloop_2=0,loopcnt_2,fout,scale;/*Loops*/
float T_area1[9][50][350],T_area2[9][50][350], inter;/* Area counts */
char *mapset,*lname[20],*outname,*G_ask_cell_new(),*mapsett;/*GRASS cell stuff*/
struct Colors color; /* Color stuff */
long *savg[10][500]; /* averaged cell values */
extern long **INPUT[10], **MAP[10];

  nrows = G_window_rows();
  ncols = G_window_cols();

  WEIGHT = G_yes("Do you want to weight the categories ? ",1);
  DIAG = G_yes("Do you want to diagonalize your data ?",1);
  fisher = G_yes("Do you want fisher classifier ?",1);
  bayes = G_yes("Do you want bayes' classifier ?",1);

  printf("How many files do you have as input ? ");
  scanf("%d",&numfiles);
  getchar();

  printf("Do you want to scale the data ? [0/1] -->: ");
  scanf("%d",&scale);
  getchar();

  for(i=0;i < numfiles;i++) {
    lname[i] = (char *) malloc(sizeof(char)*5);
    mapset = G_ask_cell_old("Enter cell file",lname[i]);
    fd[i] = G_open_cell_old(lname[i],mapset);
    if(mapset == NULL)
       exit(0);
  }
  fmg = G_open_cell_old("good.train",mapset);
  fmb = G_open_cell_old("bad.train",mapset);
  outname = (char *) malloc(sizeof(char)*20);

  mapsett = G_ask_cell_new("Please enter the output file name",outname);
  fout = G_open_cell_new(outname);

  fprintf(stderr,"HAVE O CUP A TEA, TAKE A SMOKE ...WHILE I DO MY");
  fprintf(stderr," THANG\n");
  fprintf(stderr,"Reading data files --> : ");
  for(i=0;i < numfiles;i++) {
    if(WEIGHT == YES) {
      for(k=0;k < 25;k++) {
        A_T_1[i][k].num = 0;
        A_T_1[i][k].cnt = 0;

        A_T_2[i][k].num = 0;
        A_T_2[i][k].cnt = 0;
      }
    }
    if(scale) {
      maxin[i] = 0; minin[i] = 1000;
    }
    else {
      maxin[i] = 1;  minin[i] = 1;
    }
  }

  pcell_1 = (long *) malloc(sizeof(long)*ncols);
  pcell_2 = (long *) malloc(sizeof(long)*ncols);
  pmask = (long *) malloc(sizeof(long)*ncols);

/*Note:Cut off once no more training information obtained after two rows!*/
  for(rcnt_1=0,rcnt_2=0,row=0;row < nrows;row++) {
      for(i=0;i < numfiles;i++) {
        savg[i][row] = (long *) malloc(sizeof(long)*ncols);
        G_zero_cell_buf(pavg);

        for(j=0;j < ncols;j++)
          savg[i][row][j] = INPUT[i][row][j];

        if(scale)
          for(col=0;col < ncols;col++)
	    if(pavg[col] > 0) {
	      if(maxin[i] < INPUT[i][row][col])
	        maxin[i] = INPUT[i][row][col];
	      if(minin[i] > INPUT[i][row][col])
	        minin[i] = INPUT[i][row][col];
	    }

	for(j=0;j < ncols;j++) {
	  if(MAP[1][row][j] != 0) pmask[j] = 127;
	  pcell_1[j] = pmask[j] & INPUT[i][row][j]; /* cell for "good" areas */
	}
	for(j=0;j < ncols;j++) {
	  if(MAP[2][row][j] != 0) pmask[j] = 127;
	  pcell_2[j] = pmask[j] & INPUT[i][row][j]; /* cell for "bad" areas */
	}
        if(getrow == -1 || getin == -1 || getmask == -1) {
          printf("error\n");
	  exit(0);
	}
	loop_1 = NO; nonloop_1++;

	if(CONT_1 == YES)
	  for(j=0;j < ncols;j++) {
	    T_area1[i][rcnt_1][j] = 0.;
	    if(pcell_1[j] > 0) {
		loopcnt_1++; nonloop_1 = 0;
		loop_1 = YES;
		break;
	    }
	  }
	if(loopcnt_1 > 0 && nonloop_1 > 0)
          CONT_1 = NO;

/* Sample at every other "skip" row of the training areas */
	if(loop_1 == YES)
	  if(row % skip == 0) {
	    for(col=0;col < ncols;col++)
	      if(pcell_1[col] > 0) {
	        if(i == 0)
                  ct_1++;
	        T_area1[i][rcnt_1][col] = (double)pcell_1[col];

		if(WEIGHT == YES) {
		  ii=0; FOUND = NO;
		  while(A_T_1[i][ii].num != 0) {
/* Contribution of each category towards the whole training area */
		     if(A_T_1[i][ii].num == pcell_1[col]) {
		       A_T_1[i][ii].cnt += 1;
		       FOUND = YES;
		     }
		     ii++;
		  }
                  if(FOUND == NO) {
		     A_T_1[i][ii].num = pcell_1[col];
		     A_T_1[i][ii].cnt += 1;
		  }
		}
	      }
	      rcnt_1++;
	  }
	  loop_2 = NO; nonloop_2++;
	  if(CONT_2 == YES)
	    for(j=0;j < ncols;j++) {
		T_area2[i][rcnt_2][j] = 0.;
	        if(pcell_2[j] > 0) {
		  loopcnt_2++; nonloop_2 = 0;
		  loop_2 = YES;
		  break;
		}
	    }
	  if(nonloop_2 > 0 && loopcnt_2 > 0)
		CONT_2 = NO;		
	  if(loop_2 == YES)
	    	if(row % skip == 0) {
	          for(col=0;col < ncols;col++)
		   if(pcell_2[col] > 0) {
		     if(i == 0) ct_2++;
		     T_area2[i][rcnt_2][col] = (double)pcell_2[col];

		     if(WEIGHT == YES) {
		        ii=0; FOUND = NO;
		        while(A_T_2[i][ii].num != 0) {
			  if(A_T_2[i][ii].num == pcell_2[col]) {
			     A_T_2[i][ii].cnt += 1;
			     FOUND = YES;
			  }
			  ii++;
			}
			if(FOUND == NO) {
			   A_T_2[i][ii].num = pcell_2[col];
			   A_T_2[i][ii].cnt += 1;
			}
		     }
		   }
		   rcnt_2++;
		}
		if(WEIGHT == YES) {
		  ii=0; FOUND = NO;
		  while(AREA_I[i][ii].num != 0) {
		    if(AREA_I[i][ii].num == pavg[col]) {
		      AREA_I[i][ii].cnt += 1;
		      FOUND = YES;
		    }
		    ii++;
		  }
		  if(FOUND == NO) {
		    AREA_I[i][ii].num = pavg[col];
		    AREA_I[i][ii].cnt += 1;
		  }
		  k = 0; T_1[i] = 0;
		  while(A_T_1[i][k].num != 0) {
		     T_1[i] += A_T_1[i][k].cnt;	/* Total area for class 1 */
		     k++;
		  }
		  k=0; T_2[i] = 0;
		  while(A_T_1[i][k].num != 0) {
		     T_2[i] += A_T_2[i][k].cnt; /* Total area for class 2 */
		     k++;
		  }
		  k=0; I_A[i] = 0;
		  while(AREA_I[i][k].num != 0) {
		     I_A[i] += AREA_I[i][k].cnt;  /* Total area of input */
		     k++;
		  }
		}
      }
      G_percent(row,nrows,1);
  }
  fprintf(stderr,"\n");

  if(WEIGHT == YES) {
      for(i=0;i < numfiles;i++) {
        for(row=0,k=0;row < rcnt_1;row++)
          for(col=0;col < ncols;col++)
	    if(T_area1[i][row][col] > 0) {
	        q = 0;
	        while(A_T_1[i][q].num != (int)T_area1[i][row][col])
	          q++;
              /* Category contribution in % */
	        S_A = (float)A_T_1[i][q].cnt/(float)T_1[i];

/*	        if(T_area1[i][row][col] - minin[i] > 0.0001)
	          X_1[k][i] = S_A*(T_area1[i][row][col] - minin[i])/
				  (maxin[i] - minin[i]);
	        else 
		  X_1[k][i] = S_A*T_area1[i][row][col]/
				(maxin[i] - minin[i]); */

                X_1[k][i] = S_A*T_area1[i][row][col]/maxin[i];
	        k++;
	    }
	    for(row=0,k=0;row < rcnt_2;row++)
	      for(col=0;col < ncols;col++)
		if(T_area2[i][row][col] > 0) {
		    q = 0;
		    while(A_T_2[i][q].num != (int)T_area2[i][row][col])
			q++;	
		    S_A = (float)A_T_2[i][q].cnt/(float)T_2[i];

/*		    if(T_area2[i][row][col] - minin[i] > 0.0001)
		      X_2[k][i] = S_A*(T_area2[i][row][col] - minin[i])/
					(maxin[i] - minin[i]);
		  else
		    X_2[k][i] = S_A*T_area2[i][row][col]/
					(maxin[i] - minin[i]); */

                  X_2[k][i] = S_A*T_area2[i][row][col]/maxin[i];
		  k++;
		}
      }
  }
  else {
	for(i=0;i < numfiles;i++) {

	  for(row=0,k=0;row < rcnt_1;row++)
	    for(col=0;col < ncols;col++)
	      if(T_area1[i][row][col] > 0) {

/*		 if(T_area1[i][row][col] - minin[i] > 0.0001)
		    X_1[k][i] = (T_area1[i][row][col] - minin[i])/
					(maxin[i] - minin[i]);
		 else
		    X_1[k][i] = T_area1[i][row][col]/
			 		(maxin[i] - minin[i]); */

                 X_1[k][i] = T_area1[i][row][col]/maxin[i];
		 k++;
	      }
	      for(k=0,row=0;row < rcnt_2;row++)
		for(col=0;col < ncols;col++)
		  if(T_area2[i][row][col] > 0) {

/*		    if(T_area2[i][row][col] - minin[i] > 0.0001)
			X_2[k][i] = (T_area2[i][row][col] - minin[i])/
					(maxin[i] - minin[i]);
		    else
			X_2[k][i] = T_area2[i][row][col]/
					(maxin[i] - minin[i]); */

		    X_2[k][i] = T_area2[i][row][col]/maxin[i];
		    k++;
		  }
	}
  }
	for(i=0;i < numfiles;i++)
	   for(row=0,k_in=0;row < nrows;row++)
		for(col=0;col < ncols;col++,k_in++) {
			inter = 1.0*(float)(savg[i][row][col]/(float)maxin[i]);
			X_in[k_in][i] = inter;
		}

/*	          if(savg[i][row][col] - minin[i] > 0.0001)
		    X_in[k_in][i] = (savg[i][row][col]-minin[i])/
					(maxin[i] - minin[i]);
		  else
		    X_in[k_in][i] = savg[i][row][col]/(maxin[i]-minin[i]);*/
  no_in = k_in;

  imsl: {
	long ido=0, iopt=0, icopt=0, mopt=1,*incd;
	long nrow_1, nrow_2, nsampo,nobs, nlayers=numfiles;
	long ifrq, iwt, nmiss, ipath=1;
	float *tcov_1,cov_1[20][20],cov_2[20][20],inv_cov_1[20],inv_cov_2[20];
	float inv_1[20][20], inv_2[20][20];
	float *cov_1z[20], *cov_2z[20], *tcov_2, *ttcov, *inv_ttcov;
	float conper=95., *mu_x1, *mu_x2, stat[11], sumwt, temp1[1][20];
	float *eval_C, Z_1[10][1000], Z_2[10][1000], *M_1z, *M_2z; 
	float SIGMA_1, SIGMA_2, scalar_1, scalar_2;
	float *temp_m, V_0, V[20][1],temp_x1[10][1000], sqrt();
	float tem1, tem2, s, *temp_cov[20], V_T[1][20], temp_x2[10][1000];
	float *inv_temp_cov[20],C[20],evec_C[20],t_evec[20][20];
	float *Z_in,*t_ar,num, var21,var22, *fac, det11,det12,det21,det22;
	float *temp_m1, *temp_m2, *temp_m1T, *temp_m2T, p1, p2, x1, x2;
	float *ipvt, log(), pow(), factor1, factor2, factor,epsilon,anordf_();
	int class, thistime, lastime, colcnt;
	CELL *putbuf;
	struct Categories cats;
	struct Colors colors;

	s = 0.5;	/* Fisher criterion number */
	nrow_1 =  ct_1; /* The # of data points for class 1 */
	mu_x1 = (float *) malloc(sizeof(float)*nlayers);

	nrow_2 = ct_2; /* The # of data points for class 2 */
	mu_x2 = (float *) malloc(sizeof(float)*nlayers);

	incd = (long *) malloc(sizeof(long)*nlayers*nlayers);
	tcov_1 = (float *) malloc(sizeof(float)*nlayers*nlayers);
	tcov_2 = (float *) malloc(sizeof(float)*nlayers*nlayers);
        fac = (float *) malloc(sizeof(float)*nlayers*nlayers);
        ipvt = (float *) malloc(sizeof(float)*nlayers);
        ttcov = (float *) malloc(sizeof(float)*nlayers*nlayers);
        inv_ttcov = (float *) malloc(sizeof(float)*nlayers*nlayers);
	temp_m1 = (float *) malloc(sizeof(float)*nlayers);
	temp_m2 = (float *) malloc(sizeof(float)*nlayers);
	temp_m1T = (float *) malloc(sizeof(float)*nlayers);
	temp_m2T = (float *) malloc(sizeof(float)*nlayers);

	for(i=0;i < nlayers;i++) {
	  cov_1z[i] = (float *) malloc(sizeof(float)*nlayers);
	  cov_2z[i] = (float *) malloc(sizeof(float)*nlayers);
	  inv_temp_cov[i] = (float *) malloc(sizeof(float)*nlayers);
	  temp_cov[i] = (float *) malloc(sizeof(float)*nlayers);

	   mu_x1[i] = 0.;	mu_x2[i] = 0.;
	}

/* Calculate the variance-covariance matrix for the two classes */
	for(i=0;i < nlayers;i++) {
	  for(row=0;row < nrow_1;row++) {
		mu_x1[i] += X_1[row][i];
	        temp_x1[i][row] = X_1[row][i]; /* There's really no need */
	  }					/* for this !! */

	  for(row=0;row < nrow_2;row++) {
	    mu_x2[i] += X_2[row][i];
	    temp_x2[i][row] = X_2[row][i]; /* Can do without this one too */
	  }				/* but you must use X_2[][] instead */
	}
	for(i=0;i < nlayers;i++) {
		for(j=0;j < nlayers;j++) {
			cov_1[i][j] = 0.;
			cov_2[i][j] = 0.;
		}
		mu_x1[i] = mu_x1[i]/nrow_1;
		mu_x2[i] = mu_x2[i]/nrow_2;
	}

	for(i=0;i < nlayers;i++)
	  for(j=0;j < nlayers;j++)
	    for(row=0;row < nrow_1;row++) {
		cov_1[i][j] += (X_1[row][i] - mu_x1[i])*
                               (X_1[row][j] - mu_x1[j]);
	    }

	for(i=0;i < nlayers;i++)
	  for(j=0;j < nlayers;j++)
	    for(row=0;row < nrow_2;row++) {
		cov_2[i][j] += (X_2[row][i] - mu_x2[i])*
                               (X_2[row][j] - mu_x2[j]);
            }

	for(i=0;i < nlayers;i++)
	  for(j=0;j < nlayers;j++) {
	    cov_1[i][j] = cov_1[i][j]/nrow_1;
	    cov_2[i][j] = cov_2[i][j]/nrow_2;
	  }

	k = 1; i = 0;
	for(j=0;j < nlayers*nlayers;j++) { /* To take care of IMSL */
	  if(j == k*nlayers) {		/* ideosynchracies */
	    k++;
	    i = 0;
	  }
	  tcov_1[j] = cov_1[k-1][i];
	  tcov_2[j] = cov_2[k-1][i];
	  i++;
	}

       linds_(&nlayers,tcov_1,&nlayers,inv_cov_1,&nlayers);
       linds_(&nlayers,tcov_2,&nlayers,inv_cov_2,&nlayers);

       lftrg_(&nlayers,tcov_1,&nlayers,fac,&nlayers,ipvt);
       lfdrg_(&nlayers,fac,&nlayers,ipvt,&det11,&det12); /* determinant */
       lftrg_(&nlayers,tcov_2,&nlayers,fac,&nlayers,ipvt);
       lfdrg_(&nlayers,fac,&nlayers,ipvt,&det21,&det22); /* determinant */

	k = 1;  i = 0;/* IMSL is very picky about using 2-d arrays from C */
        for(j=0;j < nlayers*nlayers;j++) {
                if(j == k*nlayers) {
                        k++;
                        i = 0;
                }
                inv_1[k-1][i] = inv_cov_1[j]; /* To go from 1-D to 2-D */
                inv_2[k-1][i] = inv_cov_2[j]; /* To go from 1-D to 2-D */
                i++;
        }

    /* For Sim. Diag: Invert Cov_1 */
    if(DIAG == YES) {
	linds_(&nlayers,tcov_1,&nlayers,inv_cov_1,&nlayers);

	/* Multiply inv_cov_1 with cov_2 */
	mrrrr_(&nlayers,&nlayers,inv_cov_1,&nlayers,&nlayers,&nlayers,tcov_2,&nlayers,&nlayers,&nlayers,C,&nlayers);

	/* Compute the eigenvalues and eigenvectors of C */
	eval_C = (float *) malloc(sizeof(float)*nlayers);
	evcsf_(&nlayers,C,&nlayers,eval_C,evec_C,&nlayers);

	k = 1;	i = 0;
	for(j=0;j < nlayers*nlayers;j++) {
		if(j == k*nlayers) {
			k++;
			i = 0;
		}
		t_evec[k-1][i] = evec_C[j]; /* To go from 1-D to 2-D */
		i++;
	}

	/* Since C is symmetric
   	Put the eigenvalues along the diagonal entries to get lambda */

	for(i=0;i < nlayers;i++) {
		cov_1z[i][i] = 1.;
		cov_2z[i][i] = eval_C[i];
	}

	/* Z = [PSI]^T [X] */
	matrixm(nlayers,nlayers,t_evec,nlayers,nrow_1,temp_x1,Z_1);
	matrixm(nlayers,nlayers,t_evec,nlayers,nrow_2,temp_x2,Z_2);

	/* Calculate M_z1 and M_z2 */
	M_1z = (float *) malloc(sizeof(float)*nlayers);
	M_2z = (float *) malloc(sizeof(float)*nlayers);

	for(i=0;i < nlayers;i++) { /* Calculate mean for each layer */
              M_1z[i]  = 0.;
              for(kk=0;kk < nrow_1;kk++)
                M_1z[i] += Z_1[i][kk];
              M_1z[i] = M_1z[i]/nrow_1;
		
              M_2z[i] = 0.;
              for(kk=0;kk < nrow_2;kk++)
                M_2z[i] += Z_2[i][kk];
              M_2z[i] = M_2z[i]/nrow_2;
        }

	/* Find the inverse of temp_cov[][] */
	for(i=0;i < nlayers;i++)
	  inv_temp_cov[i][i] = 1./(cov_1z[i][i] + cov_2z[i][i]);
   }
   if(fisher) {
        if(DIAG == NO) {
	  /* Calculate M_z1 and M_z2 */
          M_1z = (float *) malloc(sizeof(float)*nlayers);
          M_2z = (float *) malloc(sizeof(float)*nlayers);

	  for(i=0;i < nlayers;i++) {
		for(k=0;k < nlayers;k++) {
			cov_1z[i][k] = cov_1[i][k];
			cov_2z[i][k] = cov_2[i][k];
		}
		M_1z[i] = mu_x1[i];
		M_2z[i] = mu_x2[i];
	  }

	  for(i=0;i < nlayers;i++)
		for(k=0;k < nlayers;k++)
			temp_cov[i][k] = cov_1z[i][k] + cov_2z[i][k];

	  k = 1; i = 0;
          for(j=0;j < nlayers*nlayers;j++) {
            if(j == k*nlayers) {
              k++;
              i = 0;
            } 
            ttcov[j] = temp_cov[k-1][i];
            i++;
          }

	  linrg_(&nlayers,ttcov,&nlayers,inv_ttcov,&nlayers); /* inverse */
  
	  k = 1;  i = 0;
          for(j=0;j < nlayers*nlayers;j++) {
                if(j == k*nlayers) {
                        k++;
                        i = 0;
                }
                inv_temp_cov[k-1][i] = inv_ttcov[j]; /*To go from 1-D to 2-D*/
                i++;
          }
        }

        /* Calculate V */
        temp_m = (float *) malloc(sizeof(float)*nlayers);

        for(i=0;i < nlayers;i++)
	    temp_m[i] = M_1z[i] - M_2z[i]; /* Difference between two means */

        nca = 1;
        /* [temp_cov]^-1 * [temp_m] = [V] */
        if(DIAG == YES) {
           for(i=0;i < nlayers;i++)
               V[i][0] = inv_temp_cov[i][i]*temp_m[i];
        }
        else  {
            for(i=0;i < nlayers;i++) {
              V[i][0] = 0.;
              for(j=0;j < nlayers;j++)
                V[i][0] += inv_temp_cov[i][j]*temp_m[j];
            }
        }

        /* The V -> V_T and [V_T] * [cov_1z] = [temp1] step */
	for(i=0;i < nlayers;i++) {
	  V_T[0][i] = V[i][0];
	  printf("V_T[0][%d] -->: %5.4f\n",i, V_T[0][i]);
	}

	for(col=0;col < nlayers;col++) {
	  num = 0.;
	  for(row=0;row < nlayers;row++)
		num += V_T[0][row]*cov_1z[row][col];
	  temp1[0][col] = num;
	}

        /* [temp1]*[V] = SIGMA_1 */
	SIGMA_1 = 0.;
	for(i=0;i < nlayers;i++)
	  SIGMA_1 += temp1[0][i]*V[i][0];

        /* [V_T]*[cov_2z] = [temp1] */
	for(col=0;col < nlayers;col++){
	   num = 0.;
	   for(row=0;row < nlayers;row++)
	     num += V_T[0][row]*cov_2z[row][col];
	   temp1[0][col] = num;
	}

        /* [temp1]*[V] = SIGMA_2 */
	SIGMA_2 = 0.;
	for(i=0;i < nlayers;i++)
          SIGMA_2 += temp1[0][i]*V[i][0];

        tem1 = SIGMA_1*SIGMA_1*s;
        tem2 = SIGMA_2*SIGMA_2*(1 - s);

        /* [V]^T * M_1z = scalar_1 */
	scalar_1 = 0.; scalar_2 = 0.;
	for(i=0;i < nlayers;i++) {
          scalar_1 += V_T[0][i]*M_1z[i];
	  scalar_2 += V_T[0][i]*M_2z[i];
	}
	V_0 = -1.*(tem1*scalar_1 + tem2*scalar_2)/(tem1 + tem2);

	var21 = V_T[0][0]*M_1z[0] + V_T[0][1]*M_1z[1] + V_0;
        var22 = V_T[0][0]*M_2z[0] + V_T[0][1]*M_2z[1] + V_0;
/*	printf("**********************************************************\n");
        printf("Loc_M1: %5.4f %5.4f  Sigma1: %5.4f\n",M_1z[0],M_1z[1],SIGMA_1);
        printf("Loc_M2: %5.4f %5.4f  Sigma2: %5.4f\n",M_2z[0],M_2z[1],SIGMA_2);
        printf("Mean1: %5.4f  n_1/s_1 ---> : %5.4f\n", var21,var21/SIGMA_1);
        printf("Mean2: %5.4f  n_2/s_2 ---> : %5.4f\n", var22,var22/SIGMA_2);
	printf("*********************************************************\n");
	printf("V_0 --->: %5.4f\n",V_0); */

	x1 = (-var21/sqrt(SIGMA_1) - var21)/sqrt(SIGMA_1);
	x2 = (-var22/sqrt(SIGMA_2) - var22)/sqrt(SIGMA_2);
	p1 = anordf_(&x1);
	p2 = anordf_(&x2);

	epsilon = 0.5*(1 - p1) + 0.5*p2;
	printf("Minimum error of classification --> %5.4f\n",epsilon);

/* For new input vectors, Z */ 

        t_ar = (float *) malloc(sizeof(float)*nlayers);
        Z_in = (float *) malloc(sizeof(float)*nlayers);
	colcnt = 0;

	printf("Finding out classification for input ---->: ");
	for(k=1;k <= no_in;k++) {
          if(k == 1) {
	    putbuf = G_allocate_cell_buf();
	    G_zero_cell_buf(putbuf);
	  }
	  for(i=0;i < nlayers;i++)
	    t_ar[i] = X_in[k][i];

	  if(DIAG == YES)
	    for(row=0;row < nlayers;row++) {
	      num = 0.;
	      for(col=0;col < nlayers;col++)
	        num += t_evec[row][col]*t_ar[col];
              Z_in[row] = num;
	    }
	  else
	    for(i=0;i < nlayers;i++)
	       Z_in[i] = t_ar[i];

	  scalar_1 = 0.;
	  for(i=0;i < nlayers;i++)
            scalar_1 += V_T[0][i]*Z_in[i];

	  if(scalar_1 + V_0 < 0)
	    class = 0;
	  else
	    class = 1;
	  colcnt++;

	  thistime = colcnt;
	  for(col=lastime;col < thistime;col++)
	    putbuf[col] = 100*class;
	  lastime = colcnt;

	  if(k % ncols == NULL) {
	    for(i=0;i < nrows;i++)
		G_put_map_row(fout,putbuf);
	    colcnt = 0;
	    lastime = 0;
	    G_zero_cell_buf(putbuf);
	  }
	  G_percent(k,no_in,1);
	}
	printf("\n");
    }
    if(bayes) {
        colcnt = 0;
	factor1 = det11*pow(10.,det12);
        factor2 = det21*pow(10.,det22);
	factor = log(factor1/factor2);
      for(k=0;k <= no_in;k++) {
	if(k == 0) {
            putbuf = G_allocate_cell_buf();
            G_zero_cell_buf(putbuf);
        }
	for(j=0;j < nlayers;j++) {
	  temp_m1T[j] = X_in[k][j] - mu_x1[j]; 
	  temp_m1[j] = X_in[k][j] - mu_x1[j];
          temp_m2T[j] = X_in[k][j] - mu_x2[j];
	  temp_m2[j] = X_in[k][j] - mu_x2[j];
	}
	p1=p2=0.;
        for(ii=0;ii < nlayers;ii++) {
	  for(j=0;j < nlayers;j++) {
	    p1 += temp_m1T[j]*inv_1[j][ii];
	    p2 += temp_m2T[j]*inv_2[j][ii];
	  }
	  p1 = p1*temp_m1[ii];
	  p2 = p2*temp_m2[ii];
	}

        if(0.5*p1 - 0.5*p2 + 0.5*factor > 0)
	  class = 0;
        else
	  class = 1;

	colcnt++;
        thistime = colcnt;
        for(col=lastime;col < thistime;col++)
          putbuf[col] = 100*class;
        lastime = colcnt;
 
        if(k % ncols == NULL) {
           for(i=0;i < nrows;i++)
              G_put_map_row(fout,putbuf);
           colcnt = 0;
           lastime = 0;
           G_zero_cell_buf(putbuf);
        }
        G_percent(k,no_in,1);
      }
    }

    G_close_cell(fout);
    G_init_colors(&colors);
    G_set_color((CELL)0,255,0,0,&colors);
    G_set_color((CELL)100,0,255,0,&colors);
    G_write_colors(outname,mapsett,&colors);

    G_init_cats((CELL)0,"linear site selection",&cats);
    G_set_cat((CELL)0,"BAD area",&cats);
    G_set_cat((CELL)100,"GOOD area",&cats);
    G_write_cats(outname,&cats);

    R_erase();
    Dcell(outname,mapsett,1);
  } /* end of imsl */
} /* end of bayes() */

matrixm(nrow1,ncol1,X1,nrow2,ncol2,X2,X3)
int nrow1, ncol1, nrow2, ncol2;
float X1[20][20], X2[10][1000], X3[10][1000];
{
int row1, row2;
int col1, col2;
float num;

	for(row1=0;row1 < nrow1;row1++)
	  for(col2=0;col2 < ncol2;col2++) {
	    num = 0.;
	    for(row2=0,col1=0;row2 < nrow2;row2++,col1++)
	       num += X1[row1][col1]*X2[row2][col2];
	    X3[row1][col2] = num;
	  }
}
